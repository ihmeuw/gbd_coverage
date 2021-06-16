#-------------------Header------------------------------------------------
# Author: USERNAME
# Date: 8/13/2020
# Purpose: Prep outputs of MR-BRT admin bias adjustment cascade model as input to ST-GPR
#***************************************************************************


to_model_bias_dir <- file.path(to_model_dir, "admin_bias")
share_model_dir <- "FILEPATH"

mes <- c("vacc_dpt3", "vacc_mcv1", "vacc_polio3", "vacc_bcg")

work_dir <- paste0(j_root, "FILEPATH/", date)  # Outputs will be stored here


for(ratio in mes){
  
  # where are the estimates saved?
  model_dir <- file.path(work_dir, ratio)
  
  # read in the file
  results <- fread(file.path(model_dir, paste0("mrbrt_", ratio, "_results.csv")))
  
  # column clean
  results[, `:=` (age_group_id=22, sex_id=3)] %>% setnames(., "pred", "cv_custom_stage_1") %>% .[, mean_value := NULL]
  
  # save to date-version to_model_dir and share_model_dir
  fwrite(results, file.path(to_model_bias_dir, paste0(ratio, "_mrbrt_custom_stage_1.csv")))
  fwrite(results, file.path(share_model_dir, "bias/mrbrt_custom_stage_1", paste0(ratio, "_mrbrt_custom_stage_1.csv")))
  
}

message("All done prepping mrbrt_cascade outputs for ST-GPR")