#' @title Invlogit

#' @description exp(x)/(1+exp(x))

#' @concept utilities
invlogit <- function(x) {
  exp(x)/(1+exp(x))
}