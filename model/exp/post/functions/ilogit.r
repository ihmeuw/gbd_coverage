#' @title Inverse Logit
#' @description Inverse logit transformation `exp(x)/(1+exp(x))`

#' @param x a numeric vector of values in logit space

#' @return `x` in linear space
#' @export

#' @concept raking
ilogit <- function(x) {
  exp(x) / (1 + exp(x))
}
