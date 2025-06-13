





























#' @param dtvc [data.table] data table of vaccine coverage (`df` object in

#' @param to_model_dir [chr] e.g.

#' @param path_outliers_ref [chr] Path to hand-selected outlier table e.g.


#' @param gbd [lgl] TRUE for GBD runs, not for LBD runs
#' @param dpt_timeliness [lgl] TRUE to apply DPT3 timeliness outliers
#' @param loc_table [data.table] with columns `ihme_loc_id`, `location_id`,

#' @param sections_to_run [chr] a section of this function you want to run
#' @param remove_outlier_method_vars [lgl] TRUE to remove all `outlier_method_x` columns you've accrued over the course of data prep

#' @return [data.table] `df` object from `prep_exp` with all outliering applied.


#' @export

#' @examples
build_outlier_table <- function(
      dtvc,
      to_model_dir, 
      path_outliers_ref,
      gbd,
      dpt_timeliness,
      loc_table, 
      sections_to_run,
      remove_outlier_method_vars
) {
   
   valid_sections <- c(
      "outlier_sheet_decision"
      , "survey_decision"
      , "data_decision_dpt3"
      , "intro_year_decision"
      , "fhs_hotfixes"
      , "small_sample_size"
   )
   
   if(!all(sections_to_run %in% valid_sections)){
      stop("Choose a valid section: ", toString(valid_sections))
   }
   
   varnames_og <- names(dtvc) 
   
   
   
   
   
   
   path_dt_outlier <- file.path(to_model_dir, "reference/outliers_applied.csv")
   
   
   if(!"outlier" %in% names(dtvc)) dtvc[, outlier := 0]
   if(!"cv_outlier" %in% names(dtvc)) dtvc[, cv_outlier := NA_real_]
   

   
         
   
   
   if("outlier_sheet_decision" %in% sections_to_run){
      
      message("Applying hand-selected outliers from reference repo outlier sheet.")
      
      
      dto <- as.data.table(readr::read_csv(path_outliers_ref)) %>% unique
      dto[, year_id := as.integer(year_id)]
      if (dpt_timeliness) {
         
         df.time <- dto[me_name == "vacc_dpt3"]
         df.time <- df.time[, me_name := "vacc_dpt3_timeliness_ratio"]
         dto <- rbind(dto, df.time)
      }
      
      if (gbd) {
         dto <- dto[gbd == 1,]
      }
      
      dto <- dto[, outlier := 1]
      
      message("Applying outlier decisions across vaccines associated with any reference (denominator) vaccine.")
      
      for(ref in c('vacc_dpt3', 'vacc_dpt1', 'vacc_mcv1')){
         message(" -- ", ref)
         
         if(ref == 'vacc_dpt3'){
            dto_sub <- dto[me_name == ref]
            dto_sub[, me_name := NULL]
            dto_sub <- data.table(
               dto_sub, 
               me_name = rep(
                  c(
                     'vacc_hepb3',
                     'vacc_hib3',
                     'vacc_pcv3',
                     'vacc_rotac',
                     'vacc_hepb3_dpt3_ratio',
                     'vacc_hib3_dpt3_ratio',
                     'vacc_pcv3_dpt3_ratio',
                     'vacc_rotac_dpt3_ratio',
                     'vacc_dpt1',
                     'vacc_dpt12_cond'
                  ), 
                  each = nrow(dto_sub)
               ))
            
            dto <- unique(rbind(dto, dto_sub))
         }
         
         if(ref == 'vacc_dpt1'){
            dto_sub <- dto[me_name == ref]
            dto_sub[, me_name := NULL]
            dto_sub <- data.table(dto_sub, me_name = rep(c('vacc_dpt12_cond'), each = nrow(dto_sub)))
            
            dto   <- unique(rbind(dto, dto_sub))
            
            dto_sub <- dto[me_name == ref & nid != 203321] 
            dto_sub[, me_name := NULL]
            dto_sub <- data.table(dto_sub, me_name = rep(c('vacc_dpt3'), each = nrow(dto_sub)))
            
            dto   <- unique(rbind(dto, dto_sub))
         }
         
         if(ref == 'vacc_mcv1'){
            dto_sub <- dto[me_name == ref]
            dto_sub[, me_name := NULL]
            dto_sub <- data.table(dto_sub, me_name = rep(
               c(
                  'vacc_mcv2',
                  'vacc_rcv1',
                  'vacc_mcv2_mcv1_ratio',
                  'vacc_rcv1_mcv1_ratio'
               ),
               each = nrow(dto_sub)
            ))
            
            dto <- unique(rbind(dto, dto_sub))
         }
         
      }
      
      
      conditions <- list(
         
         list(
            
            
            
            
            cond           = 'is_empty(me_name)',
            vars           = c("nid"),
            outlier_method = "outlier_sheet_decision - NID"
         ),
         
         list(
            
            cond           = 'batch_outlier == 1 & !is_empty(me_name)',
            vars           = c("me_name", "nid"),
            outlier_method = "outlier_sheet_decision - location_NID"
         ),
         
         list(
            cond           = 'batch_outlier == 0',
            vars           = c("me_name", "nid", "ihme_loc_id", "year_id"),
            outlier_method = "outlier_sheet_decision - location_NID_year_vaccine"
         )
      )
      
      
      message("Applying outlier decisions by condition:")
      for (cond_i in conditions) {
         cond     <- cond_i[["cond"]]
         vars     <- cond_i[["vars"]]
         o_method <- cond_i[["outlier_method"]]
         message("  ", o_method)
         
         dto_sub <- dto[eval(parse(text = cond)), (vars), with = FALSE]
         .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
         dto_sub[, (.varname_outlier_method) := o_method]
         
         
         dtvc <- merge(dtvc, unique(dto_sub), by = vars, all.x = TRUE)
      }
      
      
      dtvc <- .apply_outliers_append_and_write(dtvc, path_dt_outlier)
      
   }
   
   
   
   
   
   
   
   
   
   if("survey_decision" %in% sections_to_run){
      message("Applying outlier decisions by survey name:")
      
      
      .outlier_method <- "survey_decision - Outlier select WHO_WHS, nid != 21535"
      message("  ", .outlier_method)
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      dtvc[
         survey_name %like% "WHO_WHS" & !is.na(data) & !nid %in% c(21535), 
         (.varname_outlier_method) := .outlier_method
      ]
      
      .outlier_method <- "survey_decision - Outlier SUSENAS"
      message("  ", .outlier_method)
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      dtvc[
         survey_name %like% "SUSENAS" & !is.na(data), 
         (.varname_outlier_method) := .outlier_method
      ]
      
      .outlier_method <- "survey_decision - Outlier IND/HMIS"
      message("  ", .outlier_method)
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      dtvc[
         survey_name %like% "IND/HMIS" & !is.na(data), 
         (.varname_outlier_method) := .outlier_method
      ]
      if (dpt_timeliness) {
         
         .outlier_method <- "survey_decision - Outlier IND/HMIS for DPT3 timeliness"
         message("  ", .outlier_method)
         .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
         dtvc[
            survey_name %like% "IND/HMIS" & me_name == "vacc_dpt3_timeliness_ratio" & !is.na(data), 
            (.varname_outlier_method) := .outlier_method
         ]
      }
      
      
      .outlier_method <- "survey_decision - Outlier MEX/INEGI"
      message("  ", .outlier_method)
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      dtvc[
         survey_name %like% "MEX/INEGI" & me_name == "vacc_dpt3" & !is.na(data), 
         (.varname_outlier_method) := .outlier_method
      ]
      if (dpt_timeliness) {
         .outlier_method <- "survey_decision - Outlier MEX/INEGI DPT3 timeliness"
         message("  ", .outlier_method)
         .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
         dtvc[survey_name %like% "MEX/INEGI" & me_name == "vacc_dpt3_timeliness_ratio" & !is.na(data), 
              (.varname_outlier_method) := .outlier_method
         ]
      }
      
      
      .outlier_method <- "survey_decision - Outlier RUS/LMS 2 year olds"
      message("  ", .outlier_method)
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      dtvc[
         survey_name %like% "RUS/LONGITUDINAL" & age_year != 2 & !is.na(data), 
         (.varname_outlier_method) := .outlier_method
      ]
      
      
      dtvc <- .apply_outliers_append_and_write(dtvc, path_dt_outlier)
   }
   
   
   
   
   
   
   
   
   
   if("data_decision_dpt3" %in% sections_to_run){
      message("Applying outlier decisions for DPT1 and DPT3:")
      
      dpt      <- copy(dtvc)[me_name %in% c("vacc_dpt1", "vacc_dpt3") & !is.na(data) & cv_admin == 1][, .(me_name, nid, ihme_loc_id, year_id, age_year, age_length, survey_name, data)]  
      
      gbr_subs <- loc_table[grep("GBR_", ihme_loc_id), ihme_loc_id]
      dpt      <- dpt[!ihme_loc_id %in% gbr_subs]  
      
      
      dpt_wide <- data.table::dcast(dpt, ... ~ me_name, value.var = "data")
      dpt_wide <- dpt_wide[!is.na(vacc_dpt1) & !is.na(vacc_dpt3)]  
      
      
      dpt_wide[, dpt3_greater := vacc_dpt3 > vacc_dpt1]
      dpt_outlier <- dpt_wide[dpt3_greater == TRUE, outlier_dpt3 := 1][outlier_dpt3 == 1, ]
      dpt_outlier[, id := paste0("vacc_dpt3", nid, ihme_loc_id, year_id, age_year, age_length)]
      
      
      dtvc[, id := paste0(me_name, nid, ihme_loc_id, year_id, age_year, age_length)]
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      .outlier_method <- "data_decision - Outlier DPT3 > DPT1"
      message("  ", .outlier_method)
      dtvc[
         id %in% unique(dpt_outlier$id), 
         (.varname_outlier_method) := .outlier_method
      ]
      
      dtvc[, id := NULL]
      
      
      dtvc <- .apply_outliers_append_and_write(dtvc, path_dt_outlier)
   }
   
   
   
   
   
   
   
   if("intro_year_decision" %in% sections_to_run){
      message("Applying outlier decisions for intro years:")
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      .outlier_method <- "intro_decision - year_id < cv_intro"
      dtvc[
         year_id < cv_intro &
            !is.na(data) &
            !is.na(cv_intro) &
            (cv_survey == 1 | cv_lit == 1), 
         
         (.varname_outlier_method) := .outlier_method]
      
      
      dtvc <- .apply_outliers_append_and_write(dtvc, path_dt_outlier)
   }
   
   
   
   
   
   
   if("small_sample_size" %in% sections_to_run){
      message("Applying outlier decisions for sample size < 20:")
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      .outlier_method <- "small_sample_size < 20"
      dtvc[
         sample_size < 20 & !is.na(data), 
         (.varname_outlier_method) := .outlier_method
      ]
      
      
      dtvc <- .apply_outliers_append_and_write(dtvc, path_dt_outlier)
   }
   
   
   
   
   
   
   
   
   
   
   
   if("fhs_hotfixes" %in% sections_to_run){
      message("Applying outlier decisions for FHS hotfixes:")
      .varname_outlier_method <- .get_next_outlier_method_varname(dtvc)
      .outlier_method <- "fhs_hotfixes"
      dtvc[ihme_loc_id %like% "CHN_" & me_name %in% c("vacc_pcv3_dpt3_ratio", "vacc_hib3_dpt3_ratio"), (.varname_outlier_method) := .outlier_method]
      
      
      .apply_outliers_append_and_write(dtvc, path_dt_outlier)
   }
   
   
   
   
   
   varnames_outlier_methods <- .get_outlier_method_varnames(dtvc)
   varnames_return <- setdiff(
      c(varnames_og, varnames_outlier_methods, "cv_outlier", "outlier", "n_outlier_methods")      
      , "id"
   )
   
   if(remove_outlier_method_vars){
      varnames_return <- setdiff(varnames_return, c(varnames_outlier_methods, "n_outlier_methods"))
   }
   
   dtvc <- dtvc[, ..varnames_return]
   
   varnames_head  <- c("me_name", "ihme_loc_id", "year_id", "data", "nid", "survey_name")
   varnames_cv    <- sort(grep("^cv_", names(dtvc), value = TRUE))
   
   
   setcolorder(dtvc, c(varnames_head, varnames_cv))
   setorderv(dtvc, varnames_head)
   
   return(dtvc)
}












#' @param dtvc [data.table] with columns `data`, `cv_outlier`, `outlier`

#' @return [data.table] with `cv_outlier` column set to `data` for all `outlier == 1`, and `data` set to NA
#' @export

#' @examples
.convert_data_to_cv_outlier <- function(dtvc){
   stopifnot(is.data.table(dtvc))
   stopifnot("data" %in% names(dtvc))
   stopifnot("cv_outlier" %in% names(dtvc))
   stopifnot("outlier" %in% names(dtvc))
   
   message("Setting outliered data to `cv_outlier` column, and setting `data` to NA.")
   dtvc[outlier == 1 & !is.na(data), `:=`(cv_outlier = data, data = NA_real_)]
   return(dtvc)
}










#' @param dtvc 

#' @return
#' @export

#' @examples
.get_next_outlier_method_varname <- function(dtvc){
   
   last_outlier_method <- names(dtvc)[grep("outlier_method_", names(dtvc))][length(grep("outlier_method_", names(dtvc)))]
   last_outlier_method <- ifelse(length(last_outlier_method), last_outlier_method, "outlier_method_0")
   last_idx            <- as.integer(sub("_", "", stringr::str_extract(last_outlier_method, "_\\d*$")))
   return(paste0("outlier_method_", last_idx + 1))
}

.get_outlier_method_varnames <- function(dtvc){
   varnames_outlier_method <- grep("outlier_method_\\d+", names(dtvc), value = TRUE)
}

.read_outliers_applied <- function(path){
   tryCatch(as.data.table(readr::read_csv(path)), error = function(e) return(data.table()))
}



#' @param dtvc [data.table] `df` object from prep_exp
#' @param path_dt_outlier [chr] path to `outliers_applied.csv` e.g. file.path(to_model_dir, "reference/outliers_applied.csv")

#' @return [data.table] `df` object with outliers applied
#' @export

#' @examples
.apply_outliers_append_and_write <- function(dtvc, path_dt_outlier){
   
   
   assert_data_schema(dtvc)
   varnames_outlier_method <- grep("outlier_method_\\d+", names(dtvc), value = TRUE)
   
   dtvc[apply(dtvc[, ..varnames_outlier_method], 1, function(x) any(!is.na(x))), outlier := 1]
   
   outlier_count <- rowSums(!is.na(dtvc[, ..varnames_outlier_method]))
   dtvc[, n_outlier_methods := outlier_count]
   
   .convert_data_to_cv_outlier(dtvc)
   
   
   
   
   
   varnames_keep_outlier_table <- c(
      "ihme_loc_id",
      "me_name",
      "year_id",
      "nid",
      "age_year",
      "age_length",
      "survey_name",
      "data",
      "cv_outlier",
      "outlier",
      "cv_intro",
      varnames_outlier_method,
      "n_outlier_methods"
   )
   
   varnames_keep_outlier_table <- intersect(varnames_keep_outlier_table, names(dtvc))
   
   
   message("Writing outliers_applied.csv to ", path_dt_outlier)
   outlier_table_applied <- dtvc[outlier == 1, ..varnames_keep_outlier_table]
   setorderv(outlier_table_applied, c("ihme_loc_id", "me_name", "year_id", "nid"))
   
   
   dt_outlier_disk <- .read_outliers_applied(path_dt_outlier)
   dt_outlier_applied <- rbindlist(list(dt_outlier_disk, outlier_table_applied), fill = TRUE)
   
   
   readr::write_csv(x = outlier_table_applied, file = path_dt_outlier)
   
   return(dtvc)
}


















#' @param dtvc [data.table] with `data`, `cv_outlier`, `invert_outlier` columns

#' @return [data.table] with `data` and `cv_outlier` columns updated
#' @export

#' @examples
.invert_cv_outlier <- function(dtvc){
   stopifnot(is.data.table(dtvc))
   varnames_req <- c("data", "cv_outlier", "invert_outlier")
   if(!all(varnames_req %in% names(dtvc))){
      stop("Missing required columns in dtvc: ", toString(setdiff(varnames_req, names(dtvc))))
   }
   message("Inverting outliered data from `cv_outlier` column, and setting `cv_outlier`to NA where invert_outlier == 1.")
   dtvc[invert_outlier == 1, `:=`(data = cv_outlier, cv_outlier = NA_real_)]
   return(dtvc)
}







#' @param dtvc [data.table] with `data`, `cv_outlier`, `invert_outlier` columns

#' @return [data.table] with `data` and `cv_outlier` columns updated
#' @export

#' @examples
.reinstate_inverted_outlier <- function(dtvc){
   stopifnot(is.data.table(dtvc))
   varnames_req <- c("data", "cv_outlier", "invert_outlier")
   if(!all(varnames_req %in% names(dtvc))){
      stop("Missing required columns in dtvc: ", toString(setdiff(varnames_req, names(dtvc))))
   }
   message("Reversing outlier inversion and removing invert_outlier column.")
   dtvc[invert_outlier == 1, `:=`(cv_outlier = data, data = NA_real_)][, invert_outlier := NULL]
   return(dtvc)
}






.invert_cv_outlier_by_method <- function(dtvc, outlier_method_text){
   stopifnot(is.data.table(dtvc))
   stopifnot("data" %in% names(dtvc))
   stopifnot("cv_outlier" %in% names(dtvc))
   stopifnot(length(outlier_method_text) == 1)
   
   varname_dpt_outlier <- find_column_by_val(dtvc, outlier_method_text)
   stopifnot(length(varname_dpt_outlier) == 1)
   stopifnot(varname_dpt_outlier %in% names(dtvc))
   stopifnot("n_outlier_methods" %in% names(dtvc))
   
   dtvc[get(varname_dpt_outlier) == outlier_method_text & n_outlier_methods == 1, invert_outlier := 1]
   dtvc[is.na(invert_outlier), invert_outlier := 0]
   
   message("Inverting outliered data where outlier_method_n equals `", outlier_method_text, "` and only one method was used to outlier the data.")
   .invert_cv_outlier(dtvc)
   return(dtvc)
}








#' @param df [data.table] `df` object from prep_exp just after dpt3 > dpt1 outliers have been applied
#' @param out_path [chr] path to save the output table in a `to_model` subdirectory e.g. file.path(to_model_dir,"reference/outliers_dpt3_admin_survey_lit_matches.csv")

#' @return [none] side effect - write table to disk
#' @export

#' @examples
.find_dpt3_admin_outliers_paired_with_surveys <- function(
      df,
      out_path
){
   
   varnames_req <- c("n_outlier_methods", "data", "cv_admin", "me_name")
   if(!all(varnames_req %in% names(df))){
      stop("Missing required columns in `df`: ", toString(varnames_req))
   }
   
   
   varname_outlier_dpt3 <- find_column_by_val(df, "data_decision - Outlier DPT3 > DPT1")
   
   df_out_dpt3 <- df[!is.na(get(varname_outlier_dpt3))  & n_outlier_methods == 1]
   
   df_dpt3_survey_lit <- df[!is.na(data) & (cv_survey == 1 | cv_lit == 1) & me_name == "vacc_dpt3", ]
   
   varnames_merge <- c("me_name", "ihme_loc_id", "year_id")
   varnames_keep <- c(varnames_merge, "data", "cv_outlier", "variance", "sample_size", "cv_admin", "cv_survey", "cv_lit", "nid")
   
   df_dpt3_admin_survey_match <- merge(x     = df_out_dpt3[, ..varnames_keep], 
                                       y     = df_dpt3_survey_lit[, ..varnames_keep], 
                                       by    = varnames_merge,
                                       suffixes = c("_adm", "_svy"))
   setnames(df_dpt3_admin_survey_match, "nid_adm", "nid") 
   message("Writing table of dpt3 admin points opposed by surveys/lit to:\n   ", out_path)
   readr::write_csv(df_dpt3_admin_survey_match, out_path)
   
   
   
   
   
   
   
   
   
   
}











#' @param dtvc [data.table] with columns `cv_outlier`, `n_outlier_methods`, and

#' @param fpath_outlier_invert_table [chr] path to table built by


#' @param fpath_outlier_exceptions [chr] (optional, default `NULL`) path to



#' @return [data.table] with `cv_outlier` values replaced by `data` values where

#' @export

#' @examples
.invert_cv_outlier_from_table <- function(
      dtvc, 
      fpath_outlier_invert_table, 
      fpath_outlier_exceptions = NULL
){
   stopifnot(is.data.table(dtvc))
   varnames_req <- c("data", "cv_outlier", "n_outlier_methods", "year_id")
   stopifnot(all(varnames_req %in% names(dtvc)))
   
   dt_invert <- as.data.table(readr::read_csv(fpath_outlier_invert_table))
   varnames_req <- c("me_name",	"ihme_loc_id", "year_id", "nid")
   stopifnot(all(varnames_req %in% names(dt_invert)))
   
   if(!is.null(fpath_outlier_exceptions)){
      dt_except <- as.data.table(readr::read_csv(fpath_outlier_exceptions))
      assert_no_na(dt_except, "invert_outlier")
      varnames_req <- c("me_name",	"ihme_loc_id", "year_id", "nid", "invert_outlier")
      stopifnot(all(varnames_req %in% names(dt_except)))
      
      if("review_later" %in% names(dt_except) & nrow(dt_except[review_later == 1, ])){
         message("Some records in the outlier inversion exception table have been marked for review later.")
         message(paste(capture.output(dt_except[review_later == 1, ]), collapse = '\n'))
         dt_except <- dt_except[is.na(review_later) | review_later == 0]
      }
      
      
      varnames_merge <- c("me_name", "ihme_loc_id", "year_id", "nid")
      dt_invert <- merge(
         x       = dt_invert
         , y     = dt_except
         , all.x = TRUE
         , by    = varnames_merge
      )
   } else {
      
      dt_invert[, invert_outlier := 1]
   }
   
   varnames_keep <- c("me_name",	"ihme_loc_id", "year_id", "nid", "invert_outlier")
   dt_invert <- dt_invert[is.na(invert_outlier), invert_outlier := 1][, ..varnames_keep]
   
   
   varnames_merge <- c("me_name", "ihme_loc_id", "year_id", "nid")
   dtvc <- merge(
      x       = dtvc
      , y     = dt_invert
      , all.x = TRUE
      , by    = varnames_merge
   )
   dtvc[is.na(invert_outlier), invert_outlier := 0]
   
   message("Inverting outliers from reference table: ", fpath_outlier_invert_table)
   if(!is.null(fpath_outlier_exceptions)){
      message("  with exceptions from: ", fpath_outlier_exceptions)
   }
   dtvc <- .invert_cv_outlier(dtvc)
   
   return(dtvc)
}
