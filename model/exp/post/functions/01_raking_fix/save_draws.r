#' @title Save draws

#' @description Saves out given draws at root directory, for the 01


#' @param draws Draws to be saved
#' @param root  Root directory to save location-specific draws
save_draws_01 <- function(draws, root) {
  loc_ids <- unique(draws$location_id)
  for(loc_id in loc_ids) {
    draws_location_id <- draws[location_id == loc_id, ]
    fwrite(draws_location_id, file = file.path(root, paste0(loc_id, ".csv")))
  }
}
