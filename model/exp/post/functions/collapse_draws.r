#' @title Collapse draws

#' @description Collapse draws to mean, upper, and lower values (gpr_mean, gpr_upper, gpr_lower)

#' @param df A data.table with draws_* columns

#' @return A data.table with collapsed draws

#' @concept prep_draws_individual_functions
collapse_draws <- function(df) {

  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[, gpr_mean := rowMeans(.SD), .SDcols=cols]
  df$gpr_lower <- apply(df[, (cols), with=F], 1, quantile, probs=0.025)
  df$gpr_upper <- apply(df[, (cols), with=F], 1, quantile, probs=0.975)
  df <- df[, -(cols), with=FALSE]
  return(df)

}
