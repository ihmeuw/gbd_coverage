#' @title Estimate rati coverage stage 1


#' @description Prep outputs of MR-BRT me_i scale up cascade model as


#' @param to_model_dir (str) path to model directory e.g. file.path("FILEPATH", date)
#' @param share_model_dir (path) e.g. "FILEPATH"
#' @param mes (str) vector of me_i me names e.g.  c("vacc_hepb3_dpt3_ratio",'vacc_hib3_dpt3_ratio')
#' @param mrbrt_cs1_J_dir (path) e.g. file.path("FILEPATH", date)
#' @param to_model_coverage_ratio_custom_stage_1_dir (path) e.g. file.path(to_model_dir, "coverage_ratio")

#' @return NULL

#' @concept mrbrt_bias
prep_mrbrt_output_coverage_ratio_custom_stage_1 <- function(
    to_model_dir,
    share_model_dir,
    mes,
    mrbrt_cs1_J_dir,
    to_model_coverage_ratio_custom_stage_1_dir
) {
  
  stopifnot(dir.exists(to_model_dir))
  stopifnot(dir.exists(share_model_dir))
  stopifnot(is.character(mes))
  if (!dir.exists(mrbrt_cs1_J_dir)) {
    stop(paste0("MR-BRT run appears to have failed, ", mrbrt_cs1_J_dir, " does not exist"))
  }
  
  
  for(me_i in mes){
    message("Saving results for ", me_i)
    
    
    model_dir <- file.path(mrbrt_cs1_J_dir, me_i)
    
    
    results <- fread(file.path(model_dir, paste0("mrbrt_custom_stage_1_", me_i, "_results.csv")))
    results <- unique(results[, list(year_id, location_id, me_name = me_i, age_group_id = 22, sex_id = 3, cv_custom_stage_1 = preds)])
    
    
    
    message(" -- to ", to_model_coverage_ratio_custom_stage_1_dir)
    fwrite(results, file.path(to_model_coverage_ratio_custom_stage_1_dir, paste0(me_i, "_mrbrt_coverage_ratio_custom_stage_1.csv")))
    message(" -- to ", share_model_dir)
    fwrite(results, file.path(share_model_dir, "mrbrt_coverage_ratio_custom_stage_1", paste0(me_i, "_mrbrt_coverage_ratio_custom_stage_1.csv")))
    
  }
  
  message("All done prepping mrbrt_cascade outputs for ST-GPR")
}