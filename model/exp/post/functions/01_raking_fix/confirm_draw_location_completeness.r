#' @title Confirm draw location completeness

#' @description Given draws, confirm subnational set is complete (all subnationals are present)


#' @param draws (data.table) - Draws with `location_id` attribute
#' @param locs (data.table) - Location metadata

#' @concept logit_raking
confirm_draw_location_completeness <- function(draws, locations = locs) {

  
  location_ids <- unique(draws$location_id)
  ihme_loc_ids <- locs[location_id %in% location_ids, unique(ihme_loc_id)]
  parent_ihme_loc_ids <- unique(substr(ihme_loc_ids, 1, 3))
  if(length(parent_ihme_loc_ids) != 1) {
    stop("Subnational draws are from more than one country")
  }

  
  ihme_loc_id_subnational_pattern <- paste0(parent_ihme_loc_ids, "_")
  subnational_location_ids        <- locs[grepl(ihme_loc_id_subnational_pattern, ihme_loc_id), unique(location_id)]
  if(!all(subnational_location_ids %in% location_ids)) {
    missing_location_id <- subnational_location_ids[subnational_location_ids %!in% location_ids]
    stop(paste0("Location id ", missing_location_id, " missing from unraked draws"))
  }
}
