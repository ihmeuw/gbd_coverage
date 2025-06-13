




#' @param path [chr] a full path on the file system to validate

#' @return [chr/NULL] full path to alternate path, or NULL if it does not exist

#' @examples







validate_path <- function(path = NULL){
   
   if (missing(path)) path <- NULL
   
   path_name <- deparse(substitute(path))
   expr <- substitute(path)
   
   if (is.symbol(expr) && !exists(deparse(expr), envir = parent.frame(), inherits = TRUE)) {
      path <- NULL
   }
   
   if (is.null(path) || length(path) == 0) path <- NULL
   
   path <- tryCatch(
      expr = {
         normalizePath(path, mustWork = TRUE)
      }, error = function(e) {
         if(length(path) == 0) newline <- NULL
         message(e, "-- path (", path_name, ") does not exist, returning NULL")
         return(NULL)
      } 
   )
   return(path)
}

