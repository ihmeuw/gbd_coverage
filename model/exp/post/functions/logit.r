#' @title Logit
#' @description Logit transformation `log(x/(1-x))`

#' @param x a numeric vector containing values between 0 and 1

#' @return `x` in logit space
#' @export

#' @concept raking
logit <- function(x) {
  log(x / (1 - x))
}
