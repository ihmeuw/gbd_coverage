#' @title Get raked estimates

#' @description Get raked estimates. If model finished successfully, collapsed


#' @param run_id Run id of model to pull
#' @param parent_location_id Country location_id

#' @concept logit_raking
get_raked_estimates <- function(run_id, parent_location_id) {

  
  success <- check_stgpr_model_success(run_id)
  if(success) {
    
    
    raked_estimates <- get_estimates(version_id = run_id, entity = "final")
    
    
    
    
    
    
    
    
    
    
    
    raked_estimates <- raked_estimates[location_id == parent_location_id, ]
    
    
  } else {
    
    
    
    
    raked_draws     <- read_draws(run_id, location_ids = parent_location_id, raked = FALSE)
    raked_estimates <- collapse_draws(raked_draws)
  }
  
  
  wrong_names <- c("val", "lower", "upper") 
  right_names <- c("gpr_mean", "gpr_lower", "gpr_upper") 
  repl_idx    <- which(wrong_names %in% names(raked_estimates))
  wrong_names <- wrong_names[repl_idx]
  right_names <- right_names[repl_idx]
  setnames(raked_estimates, wrong_names, right_names)
  
  return(raked_estimates)
}
