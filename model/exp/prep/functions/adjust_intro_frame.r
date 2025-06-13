#' @title Adjust intro frame

#' @description By vaccine and location find the earliest year for where there is 



#' @param df (data.table) data being prepped by prep_exp.r

#' @return df modified as described above

#' @concept intro_year_functions
adjust_intro_frame <- function(df) {

  df <- df[, cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]
  return(df)
  
}