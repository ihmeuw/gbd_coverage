#' @title Create id

#' @description  Function to calculate uncertainty (variance) in administrative data

#' @param df (data.table) data being prepped by prep_exp.r

#' @return df with an additional column n_survey, and variance for admin data

#' @concept admin_bias_correction_functions
set_admin_variance <- function(df) {

  
  df <- df[, n_survey := length(data[!is.na(data) & cv_survey==1]), by=c("ihme_loc_id", "year_id", "me_name")]
  df <- df[n_survey > 0 & cv_admin==1, variance := data * (1 - data) / 10]
  df <- df[n_survey==0 & cv_admin==1, variance := data * (1 - data) / 50]

}