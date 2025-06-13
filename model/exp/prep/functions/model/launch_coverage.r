#' @title Launch coverage models


#' @description Run ST-GPR for GBD vaccine coverage (indicators to be launched must be




#' @param ref_data_repo (str) absolute path to reference repo, from init.R
#' @param to_model_dir (str) absolute path to dated run folder
#' @param to_model_cv_dir (str) absolute path to dated run covariate subfolder
#' @param root_cs1 (str) absolute path to dated run for custom stage 1 models (contains models by me_name, as well as sub-folders)


#' @param date (str) run date, e.g. "2023-01-10"
#' @param n_draws (int) number of draws to use in ST-GPR
#' @param run_via_bundle (bool) TRUE when want to run ST-GPR models via


#' @param bundle.version.dir (str) Path to bundle version directory 

#' @param mes_to_launch (str vector) indicators to launch models for
#' @param mes_straight (str vector) vaccines modeled as "straight" for purposes of custom ST-GPR config e.g. custom_covars
#' @param mes_ratio (str vector) vaccines modeled as "ratio" for purposes of custom ST-GPR config e.g. custom_covars
#' @param NOTES (str) notes for prep_exp.r run, set in config at top of script
#' @param proj (str) cluster project to run on, default 'proj_cov_vpd'
#' @param mark_best (int) make this run best in tracking log? default 0
#' @param fhs_run_TF (int) is this run for FHS forecasting?
#' @param nparallel (int) cores to parallelize the ST-GPR run over, default 50
#' @param location_set_id (int) e.g. 22L
#' @param year_start (int) e.g. 1980
#' @param year_end (int) e.g. 202x for GBD modeling, 2100 for FHS modeling
#' @param rel_id (int) release id, e.g. 16L

#' @return run_ids of ST-GPR models

#' @concept model_functions
launch_coverage <- function(
    ref_data_repo,
    to_model_dir,
    to_model_cv_dir,
    root_cs1,
    date,
    n_draws,
    run_via_bundle,
    bundle.version.dir,
    mes_to_launch,
    mes_straight, 
    mes_ratio,
    NOTES,
    proj = "proj_cov_vpd",
    mark_best = 0,
    nparallel = 50,
    rel_id,
    location_set_id,
    fhs_run_TF,
    year_start,
    year_end
) {
    
     my_me_names        <- mes_to_launch
     names(my_me_names) <- my_me_names
     
     if (run_via_bundle) {
       
        stgpr_config_section <- "MAIN_COVERAGE_MODEL_LAUNCH_VIA_BUNDLE"
        config_stgpr_dt      <- load_vacc_model_db(section = stgpr_config_section)
        config_stgpr_dt      <- config_stgpr_dt[release_id == rel_id, ]
        
        my_me_ids  <- config_stgpr_dt[me_name %in% mes_to_launch, modelable_entity_id]
        mod_list   <- stgpr_quota_assert(me_ids     = my_me_ids,
                                         release_id = as.integer(rel_id),
                                         n_draws    = n_draws)
        
        non_bundle_uploads <- c("vacc_bcg", "vacc_polio3")
        bundle_uploads     <- setdiff(mes_to_launch, non_bundle_uploads)
        
        xw_ids <- find_bundle_crosswalk_version_ids(date_version = date, my_me_names = bundle_uploads, path_bundle_version_log = bundle.version.dir)
        
        message("Building custom ST-GPR config for coverage via bundle ...")
        
        stgpr_config_custom_dt_bundle <- data.table(
           me_name                   = bundle_uploads,
           path_to_data              = NA,
           path_to_custom_stage_1    = NA,
           path_to_custom_covariates = file.path(to_model_dir, "covariates", paste0(bundle_uploads, "_cv_covariates.csv")),
           path_to_custom_inputs     = NA,
           author_id                 = Sys.info()[["user"]],
           gpr_draws                 = n_draws,
           crosswalk_version_id      = xw_ids
        )
        
        stgpr_config_custom_dt_non_bundle <- data.table(
           me_name                   = non_bundle_uploads,
           path_to_data              = NA,
           path_to_custom_stage_1    = NA,
           path_to_custom_covariates = file.path(to_model_dir, "covariates", paste0(non_bundle_uploads, "_cv_covariates.csv")),
           path_to_custom_inputs     = NA,
           author_id                 = Sys.info()[["user"]],
           gpr_draws                 = n_draws,
           crosswalk_version_id      = NA
        )
        
        
        if(is.null(bundle_uploads)     | length(bundle_uploads) == 0)     stgpr_config_custom_dt_bundle     <- NULL
        if(is.null(non_bundle_uploads) | length(non_bundle_uploads) == 0) stgpr_config_custom_dt_non_bundle <- NULL
        
        stgpr_config_custom_dt <- rbind(stgpr_config_custom_dt_bundle,
                                        stgpr_config_custom_dt_non_bundle)
        
        message("Saving custom ST-GPR config for coverage (via bundle) ...")
        path_stgpr_config <- make_stgpr_custom_config(stgpr_config_custom_dt = stgpr_config_custom_dt
                                                      , path_varnames        = c("path_to_custom_covariates")
                                                      , stgpr_config_section = stgpr_config_section
                                                      , save_dir             = to_model_dir)
        
     } else if(fhs_run_TF) {
       stgpr_config_section <- "FORECASTING_MODEL_LAUNCH"
       config_stgpr_dt      <- load_vacc_model_db(section = stgpr_config_section)

       my_me_ids  <- config_stgpr_dt[me_name %in% mes_to_launch, modelable_entity_id]
       mod_list   <- stgpr_quota_assert(me_ids = my_me_ids, release_id = as.integer(rel_id), n_draws = draws_count)

       message("Building custom ST-GPR config for coverage (non-bundle) ...")
       
       stgpr_config_custom_dt_straight <- data.table(
         me_name                   = mes_straight,
         release_id                = rel_id,
         location_set_id           = location_set_id,
         year_start                = year_start,
         year_end                  = year_end,
         path_to_data              = file.path(to_model_dir, paste0(mes_straight, ".csv")),
         path_to_custom_stage_1    = NA,
         path_to_custom_covariates = file.path(to_model_cv_dir, paste0(mes_straight, "_cv_covariates.csv")),
         path_to_custom_inputs     = NA,
         author_id                 = Sys.info()[["user"]],
         gpr_draws                 = draws_count,
         crosswalk_version_id      = NA
       )

       stgpr_config_custom_dt_ratios <- data.table(
         me_name                   = mes_ratio,
         release_id                = rel_id,
         location_set_id           = location_set_id,
         year_start                = year_start,
         year_end                  = year_end,
         path_to_data              = file.path(to_model_dir, paste0(mes_ratio, ".csv")),
         path_to_custom_stage_1    = file.path(root_cs1, paste0(mes_ratio, "_mrbrt_coverage_ratio_custom_stage_1.csv")),
         path_to_custom_covariates = NA,
         path_to_custom_inputs     = NA,
         author_id                 = Sys.info()[["user"]],
         gpr_draws                 = draws_count,
         crosswalk_version_id      = NA
       )

       
       if(is.null(mes_straight) | length(mes_straight) == 0) stgpr_config_custom_dt_straight <- NULL
       if(is.null(mes_ratio)    | length(mes_ratio) == 0)    stgpr_config_custom_dt_ratios   <- NULL

       stgpr_config_custom_dt <- rbind(stgpr_config_custom_dt_straight,
                                       stgpr_config_custom_dt_ratios)

       message("Saving custom ST-GPR config for coverage (non-bundle) ...")
       path_stgpr_config <- make_stgpr_custom_config(stgpr_config_custom_dt = stgpr_config_custom_dt
                                                     , path_varnames        = c("path_to_data", "path_to_custom_stage_1", "path_to_custom_covariates")
                                                     , stgpr_config_section = stgpr_config_section
                                                     , save_dir             = to_model_dir)
       
     } else {
        
        stgpr_config_section <- "MAIN_COVERAGE_MODEL_LAUNCH"
        config_stgpr_dt      <- load_vacc_model_db(section = stgpr_config_section)
        
        my_me_ids  <- config_stgpr_dt[me_name %in% mes_to_launch, modelable_entity_id]
        mod_list   <- stgpr_quota_assert(me_ids     = my_me_ids,
                                         release_id = as.integer(rel_id),
                                         n_draws    = n_draws)
        
        message("Building custom ST-GPR config for coverage (non-bundle) ...")
        
        stgpr_config_custom_dt_straight <- data.table(
           me_name                   = mes_straight,
           release_id                = rel_id,
           location_set_id           = location_set_id,
           year_start                = year_start,
           year_end                  = year_end,
           path_to_data              = file.path(to_model_dir, paste0(mes_straight, ".csv")),
           path_to_custom_stage_1    = NA,
           path_to_custom_covariates = file.path(to_model_cv_dir, paste0(mes_straight, "_cv_covariates.csv")),
           path_to_custom_inputs     = NA,
           author_id                 = Sys.info()[["user"]],
           gpr_draws                 = n_draws,
           crosswalk_version_id      = NA
        )
        
        stgpr_config_custom_dt_ratios <- data.table(
           me_name                   = mes_ratio,
           release_id                = rel_id,
           location_set_id           = location_set_id,
           year_start                = year_start,
           year_end                  = year_end,
           path_to_data              = file.path(to_model_dir, paste0(mes_ratio, ".csv")),
           path_to_custom_stage_1    = file.path(root_cs1, paste0(mes_ratio, "_mrbrt_coverage_ratio_custom_stage_1.csv")),
           path_to_custom_covariates = NA,
           path_to_custom_inputs     = NA,
           author_id                 = Sys.info()[["user"]],
           gpr_draws                 = n_draws,
           crosswalk_version_id      = NA
        )
        
        
        if(is.null(mes_straight) | length(mes_straight) == 0) stgpr_config_custom_dt_straight <- NULL
        if(is.null(mes_ratio)    | length(mes_ratio) == 0)    stgpr_config_custom_dt_ratios   <- NULL
        
        stgpr_config_custom_dt <- rbind(stgpr_config_custom_dt_straight,
                                        stgpr_config_custom_dt_ratios)
        
        message("Saving custom ST-GPR config for coverage (non-bundle) ...")
        path_stgpr_config <- make_stgpr_custom_config(stgpr_config_custom_dt = stgpr_config_custom_dt
                                                      , path_varnames        = c("path_to_data", "path_to_custom_stage_1", "path_to_custom_covariates")
                                                      , stgpr_config_section = stgpr_config_section
                                                      , save_dir             = to_model_dir)
     }


  
  
  all_me_name     <- mes_to_launch
  all_my_model_id <- NULL
  all_my_me_id    <- NULL
  all_my_model_id <- lapply( 1:length(all_me_name), function(x) c(all_my_model_id, config_stgpr_dt[me_name==all_me_name[x], model_index_id]) )
  all_my_me_id    <- lapply( 1:length(all_me_name), function(x) c(all_my_me_id, config_stgpr_dt[me_name==all_me_name[x], modelable_entity_id]) )
  all_data_notes  <- rep(c(NOTES), length(all_me_name))


  
  

  cat("\n
  cat("Vaccine Coverage ST-GPR\n", file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)

  RUNS <- NULL
  
  for (idx in 1:length(all_me_name)) {

    me_name     <- all_me_name[idx]
    my_model_id <- all_my_model_id[[idx]]
    my_me_id    <- all_my_me_id[[idx]]
    data_notes  <- all_data_notes[idx]
    data_path   <- file.path(to_model_dir, paste0(me_name, ".csv"))


    
    
    message("|||"); message("|||"); message(paste0("Starting launch for ", me_name))
    run_id <- register_stgpr_model(path_to_config = path_stgpr_config,
                                   model_index_id = my_model_id)
    stgpr_sendoff(run_id = run_id, project = proj, nparallel = nparallel)


    
    
    logs_path <- file.path(ref_data_repo, "gbd_model/vaccination_run_log.csv")
    print_date <- gsub("-", "_", date) %>% as.character

    logs_df   <- data.frame("date"         = format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M"),
                            "me_name"      = me_name,
                            "my_model_id"  = my_model_id,
                            "run_id"       = run_id[[1]],
                            "data_id"      = "",
                            "model_id"     = "",
                            "data_path"    = data_path,
                            "is_best"      = 0,
                            "notes"        = data_notes,
                            "status"       = "",
                            "date_version" = print_date)
    logs_file <- fread(logs_path) %>% rbind(., logs_df, fill=TRUE)
    fwrite(logs_file, logs_path, row.names=FALSE)
    message(paste0("Log file saved for ", me_name, " under run_id (", run_id[[1]], ")")); message("|||"); message("|||")

    log_msg <- paste0("Logs for ", me_name, " (", run_id, "):\n   /mnt/share/covariates/ubcov/model/output/", run_id, "/logs\n")
    cat(log_msg, file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)
    
    
    append_to_stgpr_run_id_log(date                  = date
                               , me_name             = me_name
                               , run_id              = run_id
                               , model_type          = "vaccine_coverage"
                               , modelable_entity_id = my_me_id
                               , to_model_dir        = to_model_dir)

    RUNS <- c(RUNS, run_id)
    
  }

  return(RUNS)
}
