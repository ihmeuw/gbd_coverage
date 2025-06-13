









#' @param df.adjust [data.table]

#' @return [data.table] df.adjusted with imputed dpt1 survey data, removing

#' @export

#' @examples
impute_dpt1_survey_for_bias <- function(df.adjust){
   
   
   
   varnames_og <- copy(names(df.adjust))
   
   varnames_usable <- sub("\\.x", "_admin", sub("\\.y", "_survey", varnames_og))
   
   dpt1 <- df.adjust[me_name == "vacc_dpt1"]
   dpt3 <- df.adjust[me_name == "vacc_dpt3"]
   setnames(dpt1, varnames_og, varnames_usable)
   setnames(dpt3, varnames_og, varnames_usable)
   
   
   
   varnames_merge <- c("ihme_loc_id", "year_id", "sex_id", "age_group_id")
   dpt <- merge(
      x = dpt1
      , y = dpt3[!is.na(data_admin) & !is.na(data_survey), ]
      , by = varnames_merge
      , suffixes = c("_dpt1", "_dpt3")
      , all = TRUE
   )
   
   
   dpt <- dpt[!is.na(me_name_dpt1) & !is.na(me_name_dpt3)] 
   
   
   
   dpt_combos_to_predict <- dpt[is.na(data_survey_dpt1) & !is.na(data_survey_dpt3), .(ihme_loc_id, year_id, sex_id, age_group_id)]
   dpt_combos_to_predict <- dpt_combos_to_predict[!duplicated(dpt_combos_to_predict)]
   
   dpt_to_predict <- dpt[, on = dpt_combos_to_predict]
   
   dpt_to_model <- dpt[!is.na(data_admin_dpt3) & !is.na(data_survey_dpt3) & !is.na(data_admin_dpt1) & !is.na(data_survey_dpt1)]
   dpt_to_model[, `:=`(dpt3_survey_admin_ratio = (data_survey_dpt3 / data_admin_dpt3), dpt1_survey_admin_ratio = (data_survey_dpt1 / data_admin_dpt1))]
   
   dpt_mod        <- lm(log(dpt1_survey_admin_ratio) ~ log(dpt3_survey_admin_ratio), data = dpt_to_model)
   dpt_mod_params <- as.data.table(broom::tidy(summary(dpt_mod)))
   intercept      <- dpt_mod_params[term == "(Intercept)", estimate]
   slope          <- dpt_mod_params[term == "log(dpt3_survey_admin_ratio)", estimate]
   
   
   dpt_to_predict[, dpt3_survey_admin_ratio := data_survey_dpt3 / data_admin_dpt3]
   dpt_to_predict[, dpt3_log_ratio := log(dpt3_survey_admin_ratio)]
   dpt_to_predict[, dpt1_survey_admin_ratio := exp(intercept + slope * dpt3_log_ratio)]
   dpt_to_predict[, data_survey_dpt1 := pmin(data_admin_dpt1 * dpt1_survey_admin_ratio, 1)]
   
   
   varnames_keep <- c(
      
      varnames_merge,
      "me_name_dpt1",
      "data_survey_dpt1",
      "data_admin_dpt1",
      "variance_admin_dpt1",
      "standard_error_admin_dpt1",
      "nid_admin_dpt1",
      "nid_survey_dpt1",
      
      "standard_error_survey_dpt3",
      "variance_survey_dpt3",
      "survey_name_dpt3",
      "year_start_dpt3"
   )
   dpt1_predicted <- dpt_to_predict[, ..varnames_keep]
   
   
   substrings_to_clip <- paste0("_dpt", c(1,3))
   varnames_clipped   <- sub(paste(substrings_to_clip, collapse = "|"), "", varnames_keep)
   setnames(dpt1_predicted, varnames_keep, varnames_clipped)
   varnames_reverted <- sub("_admin", ".x", sub("_survey", ".y", names(dpt1_predicted)))
   setnames(dpt1_predicted, names(dpt1_predicted), varnames_reverted)
   dpt1_predicted[, survey_name := paste0(survey_name, "_DPT1_IMPUTED")]
   setcolorder(dpt1_predicted, varnames_og)
   
   
   df.adjust_dpt1 <- df.adjust[me_name == "vacc_dpt1"]
   df.adjust_other <- df.adjust[me_name != "vacc_dpt1"]
   df.dpt1_surveys_imputed <- rbind(
      dplyr::anti_join(df.adjust_dpt1, dpt1_predicted, by = varnames_merge), 
      dpt1_predicted
   )
   df.adjust_dpt1_surveys_imputed <- rbind(df.adjust_other, df.dpt1_surveys_imputed)
   setorderv(df.adjust_dpt1_surveys_imputed, c("me_name", varnames_merge))

   return(df.adjust_dpt1_surveys_imputed)
}


