#' @title Split dataset

#' @description Split metadata out for documentation of model runs

#' @param df (data.table) data being prepped by prep_exp.r

#' @return named list with values "meta", "df.mod" and "df". 




#' @concept split_and_save_functions
split_meta <- function(df) {
  
  
  cols.id <- c('nid', 'ihme_loc_id', 'year_start', 'year_end', 'year_id', 'survey_name', 'survey_module', 'file_path')
  cols.meta <- c(cols.id[cols.id %in% names(df)], grep("cv_", names(df), value=TRUE), "stockout")  
  cols.data <- c('me_name', 'data', 'variance', 'sample_size')
  
  
  meta <- df[, cols.meta, with=FALSE] %>% unique
  df.mod <- df[, c("ihme_loc_id", "year_id", "age_group_id", "sex_id", "nid",  "cv_id", "cv_intro", "cv_intro_years", "stockout", "cv_stockout",  "cv_stockout_ratio", cols.data), with=FALSE]
  
  
  return(list(meta=meta, df.mod=df.mod, df=df))
}