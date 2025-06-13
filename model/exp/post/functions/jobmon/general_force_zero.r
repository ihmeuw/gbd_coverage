#' @title Force zero

#' @description This function forces certain estimates to zero where appropriate







#' @param me A character string representing the measure
#' @param type A character string representing the type of estimates to be forced to zero, "_covidfree" or "_covidimputed"
#' @param collapsed_estimate_path_root A character string representing the file path to the root folder where the collapsed estimates are stored
#' @param COVID_years [int] years that force_zero will NOT apply to `_covidfree` files

#' @return NULL

#' @concept general_tasks
force_zero <- function(me, type, collapsed_estimate_path_root, COVID_years){
  collapsed_estimate_path_full <- paste0(collapsed_estimate_path_root, type, ".rds")

  collapsed_estimate_path_save <- paste0(collapsed_estimate_path_root, type, "_pre_force_to_zero.rds")
  collapsed_estimate           <- readRDS(collapsed_estimate_path_full)
  saveRDS(collapsed_estimate, collapsed_estimate_path_save)
  
  true_zeros_vax_specific      <- true_zeros[me_name == me, .(location_id, me_name, year_id, force_to_zero = TRUE)]
  if (type == "_covidfree") {
    true_zeros_vax_specific <- true_zeros_vax_specific[!year_id %in% COVID_years | location_id==133, ] 
  }
  collapsed_estimate           <- merge(collapsed_estimate, true_zeros_vax_specific, by = c("location_id", "me_name", "year_id"), all.x = TRUE)
  collapsed_estimate[!is.na(force_to_zero), `:=`(gpr_mean = 0, gpr_lower = 0, gpr_upper = 0)]
  collapsed_estimate$force_to_zero <- NULL
  saveRDS(collapsed_estimate, collapsed_estimate_path_full)
}
