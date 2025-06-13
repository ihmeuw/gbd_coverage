#' @title Clean data

#' @description Standard cleaning operations run after data processing

#' @param df (data.table) data being prepped by prep_exp.r

#' @return df after cleaning operations run

#' @concept data_cleaning_functions
clean_data <- function(df) {
  
  
  cvs <- grep("cv_", names(df), value=TRUE)
  
  i <- lapply(cvs, function(x) length(setdiff(unique(df[!is.na(get(x))][[x]]), c(0, 1)))==0) %>% unlist
  cvs <- cvs[i]
  
  for (var in cvs) df[is.na(get(var)), (var) := 0]
  
  df <- df[, year_id := as.numeric(as.character(year_id))]
  
  df <- df[, sample_size := as.numeric(sample_size)]
  
  df <- df[data <= 0, data := 0.001]
  df <- df[data >= 1, data := 0.999]
  
  df <- df[, `:=` (age_group_id=22, sex_id=3)]
  
  df <- df[, bundle := "vaccination"]
  
  df <- df[order(me_name, ihme_loc_id, year_id)]
  return(df)
  
}