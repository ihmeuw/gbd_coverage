#' @title Collapse draws
#' @description Given data table of draws, collapse estimates

#' @param draws data table of draws

#' @concept logit_raking
collapse_draws <- function(draws) {
  cols <- grep("draw_", names(draws), value=TRUE)
  draws <- draws[, gpr_mean := rowMeans(.SD), .SDcols=cols]
  draws$gpr_lower <- apply(draws[, (cols), with=F], 1, quantile, probs=0.025)
  draws$gpr_upper <- apply(draws[, (cols), with=F], 1, quantile, probs=0.975)
  draws <- draws[, -(cols), with=FALSE]
  return(draws)
}
