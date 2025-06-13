#' @title Launch stockout models


#' @description This function launches an ST-GPR on the vaccine coverage administrative






#' @param ref_data_repo (str) absolute path to reference repo, from init.R
#' @param to_model_dir (str) absolute path to dated run folder
#' @param to_model_stockout_dir (str) absolute path to dated run folder for stockout models


#' @param date (str) run date, e.g. "2023-01-10"
#' @param n_draws (int) number of draws to use in ST-GPR
#' @param stockout_vacc_to_launch (str vector) vaccines to launch stockout models for
#' @param NOTES (str) notes for prep_exp.r run, set in config at top of script
#' @param proj (str) cluster project to run on, default 'proj_cov_vpd'
#' @param mark_best (int) make this run best in tracking log? default 0
#' @param nparallel (int) cores to parallelize the ST-GPR run over, default 50
#' @param rel_id (int) GBD release_id, default release_id
#' @param stgpr_config_section (chr) e.g "STOCKOUT_MODEL_LAUNCH_STRAIGHT" - ratios and straight models launch separately

#' @return run_ids of ST-GPR models

#' @concept stockout_functions
launch_stockout <- function(
    ref_data_repo,
    to_model_dir,
    to_model_stockout_dir,
    date,
    n_draws,
    stockout_vacc_to_launch,
    NOTES,
    proj      = "proj_cov_vpd",
    stgpr_config_section, 
    mark_best = 0,
    nparallel = 50,
    rel_id    = release_id,
    location_set_id,
    year_start,
    year_end
) {
  
   message("Loading ST-GPR model database file...")
  
   stockout_vacc_to_launch <- unique(stockout_vacc_to_launch)
   
   config_stgpr_dt      <- load_vacc_model_db(section = stgpr_config_section)
   config_stgpr_dt      <- config_stgpr_dt[release_id == rel_id, ]

  RUNS <- NULL
  
  
  my_me_names <- paste0("admin_", stockout_vacc_to_launch)
  my_me_ids   <- config_stgpr_dt[me_name %in% my_me_names, modelable_entity_id]
  
  mod_list    <- stgpr_quota_assert(me_ids     = my_me_ids,
                                    release_id = as.integer(rel_id),
                                    n_draws    = n_draws)
  
  
  
  message("Building custom ST-GPR config for stockout models...")
  
  stgpr_config_custom_dt <- data.table(
     me_name                   = my_me_names,
     release_id                = rel_id,
     location_set_id           = location_set_id,
     year_start                = year_start,
     year_end                  = year_end,
     path_to_data              = file.path(to_model_stockout_dir, paste0(stockout_vacc_to_launch, ".csv")),
     path_to_custom_stage_1    = NA,
     path_to_custom_covariates = NA,
     path_to_custom_inputs     = NA,
     author_id                 = Sys.info()[["user"]],
     gpr_draws                 = n_draws
  )
  
  path_stgpr_config <- make_stgpr_custom_config(stgpr_config_custom_dt = stgpr_config_custom_dt
                                                , path_varnames        = c("path_to_data")
                                                , stgpr_config_section = stgpr_config_section
                                                , save_dir             = to_model_stockout_dir)
  
  
  
  cat("\n
  cat("Stockout\n", file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)
  
  message("Registering ST-GPR model(s) and sending-off.")

    
  for (idx in 1:length(stockout_vacc_to_launch)) {

    me_name     <- stockout_vacc_to_launch[idx]
    data_path   <- file.path(to_model_stockout_dir, paste0(me_name, ".csv"))
    my_me_name  <- paste0("admin_", me_name)
    my_model_id <- config_stgpr_dt[me_name == my_me_name & is_best == 1, model_index_id]
    my_me_id    <- config_stgpr_dt[me_name == my_me_name & is_best == 1, modelable_entity_id]


    
    run_id <- register_stgpr_model(path_to_config = path_stgpr_config,
                                   model_index_id = my_model_id)

    stgpr_sendoff(run_id = run_id, project = proj, nparallel = nparallel)


    
    
    logs_path <- file.path(ref_data_repo, "gbd_model/stockout_run_log.csv")
    print_date <- gsub("-", "_", date) %>% as.character
    logs_df   <- data.frame("date"           = format(lubridate::with_tz(Sys.time(), tzone = "America/Los_Angeles"), "%m_%d_%y_%H%M"),
                            "me_name"        = me_name,
                            "model_index_id" = my_model_id,
                            "me_id"          = my_me_id,
                            "run_id"         = run_id[[1]],
                            "data_id"        = "",
                            "model_id"       = "",
                            "data_path"      = data_path,
                            "is_best"        = mark_best,
                            "notes"          = NOTES,
                            "status"         = "",
                            "date_version"   = print_date)
    logs_file <- fread(logs_path) %>% rbind(., logs_df, fill=TRUE)
    fwrite(logs_file, logs_path, row.names = FALSE)
    print(paste0("Log file saved for ", me_name, " under run_id ", run_id[[1]]))

    log_msg <- paste0("Logs for ", me_name, " (", run_id, "):\n   /mnt/share/covariates/ubcov/model/output/", run_id, "/logs\n")
    cat(log_msg, file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)
    
    
    append_to_stgpr_run_id_log(date                  = date
                               , me_name             = me_name
                               , run_id              = run_id
                               , model_type          = "stockout"
                               , modelable_entity_id = my_me_id
                               , to_model_dir        = to_model_dir)

    RUNS <- c(RUNS, run_id)

  }

  
  wait_on_stgpr(RUNS)

  return(RUNS)
}
