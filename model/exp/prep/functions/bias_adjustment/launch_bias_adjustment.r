#' @title Launch bias adjustment


#' @description Model time-varying administrative data bias adjustment



#' @param ref_data_repo (str) absolute path to reference repo, from init.R
#' @param to_model_dir (str) absolute path to dated run folder
#' @param to_model_bias_dir (str) absolute path to admin_bias folder within dated



#' @param date (str) run date, e.g. "2023-01-10"
#' @param n_draws (int) number of draws to use in ST-GPR
#' @param run_bias_correction_via_bundle (bool) TRUE when want to run ST-GPR models via


#' @param bundle.version.dir (str) Path to bundle version directory 

#' @param straight_models (str vector) straight models to launch
#' @param NOTES (str) notes for prep_exp.r run, set in config at top of script
#' @param proj (str) cluster project to run on, default 'proj_cov_vpd'
#' @param mark_best (int) make this run best in tracking log? default 0
#' @param nparallel (int) cores to parallelize the ST-GPR run over, default 50
#' @param rel_id (int) GBD release_id, default release_id

#' @return run_ids of ST-GPR models

#' @concept mrbrt_bias_functions
launch_bias_adjustment <- function(
    ref_data_repo,
    to_model_dir,
    to_model_bias_dir,
    date,
    n_draws,
    run_bias_correction_via_bundle,
    bundle.version.dir,
    straight_models,
    NOTES,
    proj      = "proj_cov_vpd",
    mark_best = 0,
    nparallel = 50,
    rel_id    = release_id,
    location_set_id,
    year_start,
    year_end
) {
  
     my_me_names        <- paste0("bias_", straight_models)
     names(my_me_names) <- my_me_names
     
     if (run_bias_correction_via_bundle) {
       
      stop("2024 Mar 08 - no bundle created for dpt1 since its addition to the group of straight models - launch via bundle not possible.")
        
        stgpr_config_section <- "ADMIN_BIAS_MODEL_LAUNCH_VIA_BUNDLE"
        config_stgpr_dt      <- load_vacc_model_db(section = stgpr_config_section)
        config_stgpr_dt      <- config_stgpr_dt[release_id == rel_id, ]
        
        my_me_ids          <- config_stgpr_dt[me_name %in% my_me_names, modelable_entity_id]
        mod_list           <- stgpr_quota_assert(me_ids     = my_me_ids,
                                                 release_id = as.integer(rel_id),
                                                 n_draws    = n_draws)
        
        xw_ids <- find_bundle_crosswalk_version_ids(
          date_version            = date,
          my_me_names             = my_me_names,
          path_bundle_version_log = bundle.version.dir
        )
        
        message("Building custom ST-GPR config for admin bias via bundle ...")        
        
        stgpr_config_custom_dt <- data.table(
           me_name                   = my_me_names,
           release_id                = rel_id,
           location_set_id           = location_set_id,
           year_start                = year_start,
           year_end                  = year_end,
           path_to_data              = NA,
           path_to_custom_stage_1    = file.path(to_model_bias_dir, paste0(straight_models, "_mrbrt_custom_stage_1.csv")),
           path_to_custom_covariates = NA,
           path_to_custom_inputs     = NA,
           author_id                 = Sys.info()[["user"]],
           gpr_draws                 = n_draws,
           crosswalk_version_id      = xw_ids
        )
        
        message("Saving custom ST-GPR config for admin bias ...")
        path_stgpr_config <- make_stgpr_custom_config(stgpr_config_custom_dt = stgpr_config_custom_dt
                                                      , path_varnames        = c("path_to_custom_stage_1")
                                                      , stgpr_config_section = stgpr_config_section
                                                      , save_dir             = to_model_bias_dir)
        
     } else {
        
        stgpr_config_section <- "ADMIN_BIAS_MODEL_LAUNCH"
        config_stgpr_dt      <- load_vacc_model_db(section = stgpr_config_section)
        config_stgpr_dt      <- config_stgpr_dt[release_id == rel_id, ]
        
        my_me_ids <- config_stgpr_dt[me_name %in% my_me_names, modelable_entity_id]
        mod_list  <- stgpr_quota_assert(me_ids     = my_me_ids,
                                        release_id = as.integer(rel_id),
                                        n_draws    = n_draws)
        
        message("Building custom ST-GPR config for admin bias (non-bundle) ...")        
        
        stgpr_config_custom_dt <- data.table(
           me_name                   = my_me_names,
           path_to_data              = file.path(to_model_bias_dir, paste0(straight_models, ".csv")),
           path_to_custom_stage_1    = file.path(to_model_bias_dir, paste0(straight_models, "_mrbrt_custom_stage_1.csv")),
           path_to_custom_covariates = NA,
           path_to_custom_inputs     = NA,
           author_id                 = Sys.info()[["user"]],
           gpr_draws                 = n_draws,
           crosswalk_version_id      = NA
        )
        
        message("Saving custom ST-GPR config for admin bias ...")
        path_stgpr_config <- make_stgpr_custom_config(stgpr_config_custom_dt = stgpr_config_custom_dt
                                                      , path_varnames        = c("path_to_data", "path_to_custom_stage_1")
                                                      , stgpr_config_section = stgpr_config_section
                                                      , save_dir             = to_model_bias_dir)
        
     }
     
  
  

  cat("\n
  cat("Bias Adjustment ST-GPR\n", file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)
  
  RUNS <- NULL
  
  
  for (idx in 1:length(straight_models)) {

    me_name     <- straight_models[idx]
    data_path   <- file.path(to_model_bias_dir, paste0(me_name, ".csv"))
    my_me_name  <- paste0("bias_", me_name)
    my_model_id <- config_stgpr_dt[me_name == my_me_name & is_best == 1, model_index_id]
    my_me_id    <- config_stgpr_dt[me_name == my_me_name & is_best == 1, modelable_entity_id]


    
    
    run_id <- register_stgpr_model(path_to_config = path_stgpr_config,
                                   model_index_id = my_model_id)

    stgpr_sendoff(run_id = run_id, project = proj, nparallel = nparallel)


    
    
    logs_path <- file.path(ref_data_repo, "gbd_model/bias_run_log.csv")
    print_date <- gsub("-", "_", date) %>% as.character
    logs_df   <- data.frame("date"         = format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M"),
                            "me_name"      = me_name,
                            "my_model_id"  = my_model_id,
                            "run_id"       = run_id[[1]],
                            "data_id"      = "",
                            "model_id"     = "",
                            "data_path"    = data_path,
                            "is_best"      = mark_best,
                            "notes"        = NOTES,
                            "status"       = "",
                            "date_version" = print_date)
    logs_file <- fread(logs_path) %>% rbind(., logs_df, fill=TRUE)
    fwrite(logs_file, logs_path, row.names=FALSE)
    message(paste0("Log file saved for ", me_name, " under run_id ", run_id[[1]]))

    log_msg <- paste0("Logs for ", me_name, " (", run_id, "):\n   /mnt/share/covariates/ubcov/model/output/", run_id, "/logs\n")
    cat(log_msg, file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)

    
    append_to_stgpr_run_id_log(date                  = date
                               , me_name             = me_name
                               , run_id              = run_id
                               , model_type          = "admin_bias"
                               , modelable_entity_id = my_me_id
                               , to_model_dir        = to_model_dir)
    
    RUNS <- c(RUNS, run_id)

  }

  message("Sleeping for 2 min while models run before checking to see if complete")
  Sys.sleep(120)  
  wait_on_stgpr(RUNS)

  return(RUNS)
}
