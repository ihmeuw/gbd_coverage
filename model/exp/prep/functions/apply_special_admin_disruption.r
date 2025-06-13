#' @title apply_special_disruption

#' @description For countries that show disruption in administrative data but not official data,







#' @param df (data.table) data being prepped by prep_exp.r
#' @param vacc.admin (data.table) JRF administrative data
#' @param countries_with_admin_disruption (chr) vector of `ihme_loc_ids` e.g. c("COM", "IDN", "MLI", "MOZ", "TZA", "MUS")
#' @param adjust_dpt12_conditional (lgl) used in GBD model, not currently in age model (mcv1 only)
apply_special_admin_disruption <- function(df, vacc.admin, countries_with_admin_disruption, adjust_dpt12_conditional = TRUE) {
  
  
  admin_data <- readRDS(vacc.admin)
  message("Applying admin disruptions for: ", toString(countries_with_admin_disruption))
  admin_data <- admin_data[(ihme_loc_id %in% countries_with_admin_disruption & year_id %in% c(2019:2021))|
                             (ihme_loc_id=='MOZ' & year_id %in% 2022:2023), .(ihme_loc_id, me_name, data, year_id)]

  
  admin_data[, year_id := paste0("admin_", year_id)]
  admin_data_wide <- dcast(admin_data, formula = "ihme_loc_id + me_name ~ year_id", value.var = "data")

  
  admin_data_wide[, disruption_2020 := admin_2020 / admin_2019]
  admin_data_wide[, disruption_2021 := admin_2021 / admin_2019]
  admin_data_wide[, disruption_2022 := admin_2022 / admin_2019]
  admin_data_wide[, disruption_2023 := admin_2023 / admin_2019]

  
  admin_data_wide <- admin_data_wide[, .(ihme_loc_id, me_name, disruption_2020, disruption_2021,disruption_2022,disruption_2023)]
  admin_disruption_ratio <- melt(admin_data_wide,
                                 id.vars = c("ihme_loc_id", "me_name"),
                                 measure_vars = c("disruption_2020", "disruption_2021","disruption_2022","disruption_2023"),
                                 variable.name = "year_of_application", value.name = "admin_disruption_ratio")

  
  
  admin_disruption_ratio[, year_of_application := as.integer(gsub("disruption_", "", year_of_application))]
  admin_disruption_ratio$year_id <- 2019
  admin_disruption_ratio$nid     <- 203321

  
  admin_adjusted_data <- merge(df[, .(ihme_loc_id, me_name, year_id, nid, data, cv_admin_orig, cv_outlier)],
                               admin_disruption_ratio,
                               by = c("ihme_loc_id", "me_name", "year_id", "nid"))

  
  
  
  admin_adjusted_data[, data := data * admin_disruption_ratio]
  admin_adjusted_data[, year_id := year_of_application]
  admin_adjusted_data$year_of_application <- NULL

  if(adjust_dpt12_conditional){
     
     
     
     
     admin_adjusted_dpt12_cond <- admin_adjusted_data[grepl("dpt", me_name), ]
     admin_adjusted_dpt12_cond[me_name == "vacc_dpt3", data := cv_admin_orig]
     admin_adjusted_dpt12_cond[me_name == "vacc_dpt3", data := data * admin_disruption_ratio]
     admin_adjusted_dpt12_cond_wide <- dcast(admin_adjusted_dpt12_cond, formula = "ihme_loc_id + year_id ~ me_name", value.var = "data")
     admin_adjusted_dpt12_cond_wide[, vacc_dpt12_cond := ((vacc_dpt1 - vacc_dpt3)/(1-vacc_dpt3))]
     admin_adjusted_dpt12_cond <- melt(admin_adjusted_dpt12_cond_wide,
                                       id.vars = c("ihme_loc_id", "year_id"),
                                       measure_vars = c("vacc_dpt1", "vacc_dpt3", "vacc_dpt12_cond"),
                                       variable.name = "me_name", value.name = "data")
     admin_adjusted_dpt12_cond <- admin_adjusted_dpt12_cond[me_name == "vacc_dpt12_cond", ]
     
     
     admin_adjusted_dpt12_cond$nid <- 203321
     admin_adjusted_data <- rbind(admin_adjusted_data, admin_adjusted_dpt12_cond, fill = TRUE)
  }
  
  admin_adjusted_data <- admin_adjusted_data[, .(ihme_loc_id, me_name, year_id, nid, admin_adjusted_data = data)]

  
  
  df <- merge(df, admin_adjusted_data, all.x = TRUE, by = c("ihme_loc_id", "me_name", "year_id", "nid"))
  df[!is.na(admin_adjusted_data) & !is.na(cv_outlier), admin_adjusted_data := NA]
  df[!is.na(admin_adjusted_data), data := admin_adjusted_data]
  df$admin_adjusted_data <- NULL

  
  df[data >= 1, data := .999]
  df[data <= 0, data := .001]


  
  return(df)
}
