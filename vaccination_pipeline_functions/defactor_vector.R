



#' @param vector [any] Vector to be converted from factor to original type

#' @return [any] Vector with original type
#' @export

#' @examples
defactor_vector <- function(vector) {
  if (is.factor(vector)) {
    levels_vector <- levels(vector)
    vector_values <- as.numeric(vector)  
    
    
    type <- "character"  
    suppressWarnings({
      if (all(!is.na(as.numeric(levels_vector)))) {
        type <- "numeric"
      } else if (all(!is.na(as.integer(levels_vector)))) {
        type <- "integer"
      } else if (all(levels_vector %in% c("TRUE", "FALSE"))) {
        type <- "logical"
      }
    })
    
    
    vector <- switch(type,
                     numeric = as.numeric(levels_vector)[vector_values],
                     integer = as.integer(levels_vector)[vector_values],
                     logical = as.logical(levels_vector)[vector_values],
                     character = levels_vector[vector_values])
  }
  return(vector)
}

