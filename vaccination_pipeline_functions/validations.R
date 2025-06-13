

#' @param x [obj] some R object

#' @return [lgl] TRUE / FALSE
is_an_error <- function(x) {
   
   (inherits(x, "simpleError") | inherits(x, "try-error"))
} 