#' @title Outlier pre-intro year data

#' @description Pre-intro year ratio data cleaning after add in intro years for new vaccines





#' @concept outlier_functions
pre_intro_outliers <- function(df) {
  
  
  df[year_id < cv_intro & is.na(cv_outlier) & !is.na(data), `:=` (cv_outlier=data, data=NA)] 
  
  return(df)
}