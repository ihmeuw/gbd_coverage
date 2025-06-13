#' @title Cap estimates

#' @description This function caps estimates to a maximum of 99% coverage.

#' @param df A data frame with draw columns containing estimates to be capped.

#' @return A modified data frame with the estimates capped at 99% coverage.

#' @concept prep_draws_individual_functions
cap_est <- function(df) {

  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[, (cols) := lapply(.SD, function(x) ifelse(x >= 1, 0.99, x)), .SDcols=cols]
  
  
  
  
  
  return(df)

}
