#' @title Load Checkpoint Data

#' @description This is a convenience function for loading in checkpoint data produced by 










#' @param to_model_dir an absolute file path to the dated model directory
#' @param steps_to_load a vector or single number of which steps to load. The default will 


#' @return returns a named list of data.table(s)
load_checkpoint_data <- function(to_model_dir, steps_to_load = c(1, 2, 2.5, 3, 4, 5)) {

  checkpoints <- c("01_rbind_survey_lit_admin.rds", "02_after_decider_swaps.rds", 
                 "02.5_pre_dpt1_imputation.rds", "03_pre_bias_adj.rds", 
                 "04_post_bias_adj.rds", "05_ratios_and_intros_applied.rds")
  
  
  valid_steps <- c(1, 2, 2.5, 3, 4, 5)
  if (any(!(steps_to_load %in% valid_steps))) {
    invalid_steps <- steps_to_load[!(steps_to_load %in% valid_steps)]
    stop(paste0("Invalid step(s) passed to load_checkpoint_data: ", 
                paste(invalid_steps, collapse = ", "), 
                ". Valid steps are: ", paste(valid_steps, collapse = ", ")))
  }
  checkpoints <- checkpoints[valid_steps %in% steps_to_load]
  
  
  full_path_checkpoints <- file.path(to_model_dir, checkpoints)
  checkpoints_exist <- file.exists(full_path_checkpoints)
  missing_files <- checkpoints[!checkpoints_exist]
  if (length(missing_files) > 0) {
    message("The following checkpoint data was not found: ", paste(missing_files, collapse = ", "), ", and will not be loaded.")
  }
  
  paths_to_load <- full_path_checkpoints[checkpoints_exist]
  checkpoint_list <- lapply(paths_to_load, readRDS)
  names(checkpoint_list) <- checkpoints[checkpoints_exist]
  
  return(checkpoint_list)
}