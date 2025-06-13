








if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
  code_root <- Sys.getenv()[['CODE_ROOT']] 
} else {
  
  code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
}

message("code_root: ", code_root)

r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
library(vmTools, lib.loc = r_lib_team_dir) 

source(file.path(code_root, "init_list.r"))
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), recursive = FALSE, modifiedOnly = FALSE)

PARAMS <- list(
  launch_cv_prep    = FALSE
  , prep_configs    = FALSE
  , launch_pipeline = FALSE
  , launch_metrics  = TRUE
)

fpath_config_repo <- file.path(code_root, "pipeline_config.yaml")
pipeline_config <- se$read_file(fpath_config_repo)

stopifnot(identical(init_list$constants$gbd_cycle
                    , pipeline_config$cross_validation$gbd_cycle))

message("Creating cross-validation folder in the dx_share_root: ", init_list$file_roots$dx_share_root)


gbd_cycle       <- pipeline_config$cross_validation$gbd_cycle
dv_cv_base      <- pipeline_config$cross_validation$date_version

cv_root_gbdxx   <- file.path(init_list$file_roots$dx_share_root, "cross_validation", gbd_cycle)
slt <- SLT$new(user_root_list = list(output_root = cv_root_gbdxx)
               , user_central_log_root = cv_root_gbdxx)
slt$create_date_version_folders_with_logs(date_version = dv_cv_base)

output_root <- slt$return_dynamic_fields()$VERS_PATHS$output_root



if (PARAMS$launch_cv_prep == TRUE){
  
  
  
  
  
  fpath_config_dv <- file.path(output_root, "pipeline_config.yaml")
  if(file.exists(fpath_config_dv)) archive_single_file(fpath_config_dv)
  file.copy(from = fpath_config_repo, to = fpath_config_dv, overwrite = TRUE)
  
  job_id_cv_prep <-
    se$submit_job(script_path            = file.path(code_root, "FILEPATH/cross_val_pre_modeling.R")
                  , threads              = 1L
                  , mem                  = "4G"
                  , runtime_min          = 5
                  , partition            = "all.q,long.q"
                  , account              = "proj_cov_vpd"
                  , send_email           = FALSE
                  , console_style_log_tf = TRUE
                  , args_list            = list(code_root     = code_root
                                                , config_path = fpath_config_dv)
    )
  
  se$wait_on_slurm_job_id(job_id_cv_prep, cycle_sleep_sec = 10)
}


if(PARAMS$prep_configs == TRUE){
  message("Building config files:")
  for (fold in 1:pipeline_config$cross_validation$k_folds){
    
    
    message("Fold: ", fold)
    output_dir <- file.path(output_root, sprintf("fold_%s", fold))
    stopifnot(dir.exists(output_dir))
    config_cv <- data.table::copy(pipeline_config)
    
    dv_cv          <- paste0(dv_cv_base, "_fold_", fold)
    n_draws        <- pipeline_config$cross_validation$n_draws
    Note           <- paste0("Cross-validation fold ", fold, " of ", pipeline_config$cross_validation$k_folds)
    fpath_outliers <- file.path(output_dir, "outliers_cross_val.csv")
    stopifnot(file.exists(fpath_outliers)) 
    
    
    
    dv_cv ->
      config_cv$prep_exp$date ->
      config_cv$prep_exp_diagnostics$run_date_new ->
      config_cv$compare_data_versions$date_new_ ->
      config_cv$`_save_results`$date ->
      config_cv$global_aggregates$run_date ->
      config_cv$`_plot_vaccines`$date ->
      config_cv$`_save_results_diagnostics`$run_date_new
    
    n_draws ->
      config_cv$prep_exp$n_draws_coverage ->
      config_cv$`_save_results`$draws_count
    
    fpath_outliers ->
      config_cv$prep_exp$fpath_outliers_config
    
    Note ->
      config_cv$prep_exp$NOTES ->
      config_cv$`_save_results`$run_note
    
    config_path_cv <- file.path(output_dir, "cross_val_config.yaml")
    if (file.exists(config_path_cv)) archive_single_file(config_path_cv)
    se$save_file(config_cv, config_path_cv)
  }
}



if (PARAMS$launch_pipeline == TRUE) {
  message("Launching:")
  run_ids <- c()
  for (fold in 1:pipeline_config$cross_validation$k_folds){
    
    
    
    message("Fold: ", fold)
    output_dir <- file.path(output_root, sprintf("fold_%s", fold))
    stopifnot(dir.exists(output_dir))
    config_path_cv <- file.path(output_dir, "cross_val_config.yaml")
    stopifnot(file.exists(config_path_cv))
    
    run_id_i <- se$submit_job(script_path = file.path(code_root, "vaccination_pipeline.r")
                              , job_name             = sprintf("vaccination_pipeline_cv_%s", fold)
                              , threads              = 1L
                              , mem                  = "1G"
                              
                              
                              , runtime_min          = 60 * 48
                              , partition            = "all.q,long.q"
                              , account              = "proj_cov_vpd"
                              , send_email           = TRUE
                              , console_style_log_tf = TRUE
                              , args_list            = list(code_root     = code_root
                                                            , config_path = config_path_cv))
    run_ids <- c(run_ids, run_id_i)
  }
  se$wait_on_slurm_job_id(run_ids, cycle_sleep_sec = 300)
}


if (PARAMS$launch_metrics == TRUE) {
  
  se$submit_job(script_path            = file.path(code_root, "FILEPATH/cross_val_post_modeling.R")
                , threads              = 10L
                , mem                  = "50G"
                
                
                , runtime_min          = 60
                , partition            = "all.q,long.q"
                , account              = "proj_cov_vpd"
                , send_email           = TRUE
                , console_style_log_tf = TRUE
                
                , args_list = list(
                  code_root                = code_root
                  , config_path            = clean_path(output_root, "pipeline_config.yaml", mustWork = TRUE)
                  , output_root            = output_root
                  , prepare_draws_ratios   = TRUE
                  , prepare_draws_straight = TRUE
                  , prepare_mrbrt_draws    = TRUE))
  
}


message("Done.")

