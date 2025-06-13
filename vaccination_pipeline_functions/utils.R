

#' @param dt [data.table] A data.table object
#' @param value [chr] A value to search for

#' @return [chr] The name of the column containing the value
#' @export

#' @examples
find_column_by_val <- function(dt, value) {
   found_col <- unlist(
      lapply(colnames(dt), function(varname) {
         if (any(grepl(value, dt[[varname]]))) {
            return(varname)
         } else {
            return(NULL)
         }
      })
   )
   if(is.null(found_col)) stop(paste0("Value `", value, "` not found in any column."))
   null_idx <- unlist(lapply(found_col, is.null))
   return(found_col[!null_idx])
}



vec_to_comma_string <- function(vec){
   return(paste(vec, collapse = ","))
}



comma_string_to_vec <- function(string){
   return(unlist(strsplit(string, ",")))
}






#' @param pop [data.table] population data
#' @param location_ids [integer] location_ids to calculate proportions for
#' @param by_vars [chr] variables to group the population table by

#' @return [data.table] population data with a new column `pop_prop` that

#' @export

#' @examples
pop_proportion_calc <- function(pop, location_ids, by_vars = NULL){
   
   stopifnot(is.data.table(pop))
   stopifnot(is.integer(location_ids))
   req_vars <- c("location_id", "population", by_vars)
   stopifnot(all(req_vars %in% colnames(pop)))
   stopifnot(all(location_ids %in% pop$location_id))
   
   pop_sub <- pop[location_id %in% location_ids]
   pop_sub[, pop_agg := sum(population), by = by_vars]
   pop_sub[, pop_prop := population / pop_agg]
   return(pop_sub)
}



#' @param x [data.frame] data.frame to print

#' @return [chr] data.frame output as a single string
#' @export

#' @examples
prt_multiline <- function(x){
   paste(capture.output(x), collapse = "\n")
}



#' @param x [vector]
#' @param y [vector]

#' @return [vector] elements in `x` that are not in `y` _and_ vice versa
#' @export

#' @examples

anti_intersect <- function(x, y){
   return(setdiff(union(x, y), intersect(x, y)))
}









#' @param x [vector] A vector to check for emptiness

#' @return [lgl] A logical vector indicating whether each element is empty
#' @export

#' @examples
























is_empty <- function(x) {
   
   
   if (all(length(x)) == 0) return(TRUE)
   if (all(is.null(x)))     return(TRUE)
   if (all(is.na(x)))       return(TRUE)
   
   
   if (is.list(x)) {
      warning("x is a list - returning is_empty logical check as a list.  Try `sapply(list, is_empty)` instead.")
      return(lapply(x, is_empty))
   }
   
   
   
   chk <- sapply(x, function(item) {
      is.na(item) ||                                         
         length(item) == 0 ||                                
         (is.character(item) && nchar(trimws(item)) == 0)    
      
   })
   return(chk)
}



get_column_val <- function(x) {
  
  if (is.factor(x)) x <- as.character(x)
  
  if (is.data.table(x)) {
    
    if (ncol(x) > 1) stop('data.table has more than 1 column')
    x <- unique(x[!is.na(x)])
    
  } else if (is.data.frame(x)) {
    
    if (ncol(x) > 1) stop('data.frame has more than 1 column')
    x <- unique(as.vector(x)[!is.na(as.vector(x))])
    
  } else if (is.vector(x)) {
    
    x <- unique(x[!is.na(x)])
    
  }
  
  if (length(x) > 1) {
    
    stop(paste('Provided object has multiple unique values:', paste(x, collapse=' ')))
    
  } else if (length(x) == 0) {
    
    return(NA)
    
  } else {
    
    return(x)
    
  }
}
