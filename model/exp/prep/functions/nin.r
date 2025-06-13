#' @title Not in

#' @description inverse of %in%

#' @concept utilities
'%!in%' <- function(x,y) {!('%in%'(x,y))}
