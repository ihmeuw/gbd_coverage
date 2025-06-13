#' @title Estimate time varying bias


#' @description Prep outputs of MR-BRT admin bias adjustment cascade model as


#' @param to_model_bias_dir (str) path to model directory
#' @param j_root (str) platform dependent prefix for J drive
#' @param me_names (chr) e.g. c("vacc_dpt3", "vacc_mcv1", "vacc_polio3", "vacc_bcg")
#' @param share_model_dir (path) e.g. "FILEPATH" - 2024-03-03 for posterity, no longer used for processing

#' @return NULL

#' @concept mrbrt_bias
prep_mrbrt_output_custom_stage_1 <- function(to_model_bias_dir, share_model_dir, j_root, me_names) {

 
  for(me_name_i in me_names){

    
    model_dir <- file.path(to_model_bias_dir, me_name_i)

    
    results <- fread(file.path(model_dir, paste0("mrbrt_", me_name_i, "_results.csv")))

    
    results[, `:=` (age_group_id=22, sex_id=3)] %>% setnames(., "pred", "cv_custom_stage_1") %>% .[, mean_value := NULL]

    
    fwrite(results, file.path(to_model_bias_dir, paste0(me_name_i, "_mrbrt_custom_stage_1.csv")))
    fwrite(results, file.path(share_model_dir, "bias", "mrbrt_custom_stage_1", paste0(me_name_i, "_mrbrt_custom_stage_1.csv"))) 

  }

  message("All done prepping mrbrt_cascade outputs for ST-GPR")
}
