#' @title Apply intro

#' @description Given data table of draws and year of introduction,


#' @param draws data.table of draws
#' @param intro_year the year_id for the intro year

#' @return modified `draws`
apply_intro <- function(draws, intro_year) {

  
  column_names_draws    <- names(draws)[grepl("draw", names(draws))]
  column_names_metadata <- names(draws)[!grepl("draw", names(draws))]
  draws_matrix          <- as.matrix(draws[, (column_names_draws), with = FALSE])

  
  year_start        <- min(draws$year_id)
  n_pre_intro_years <- intro_year - year_start

  
  draws_matrix[c(1:n_pre_intro_years), ] <- 0

  
  draws_intro_applied         <- as.data.table(draws_matrix)
  names(draws_intro_applied)  <- column_names_draws
  draws_intro_applied_full    <- cbind(draws[, (column_names_metadata), with = FALSE], draws_intro_applied)
  return(draws_intro_applied_full)
}
