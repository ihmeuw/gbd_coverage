#' @title Read draws

#' @description modified read_draws function with added functionality

#' @param run_id Model run_id (ignored if custom draw path is provided in `special`)
#' @param location_ids list of location ids to read. If NULL, read all
#' @param raked TRUE grabs (regular) raked draws. FALSE attempts to read unraked draws, 

#' @param special Directory path used to read in custom draws
#' @param offset if T, substracts 0.01 from draw cols
#' @param use_cc_get_draws [lgl] read draws using central functions instead of

#' @param to_model_dir [path] date-version folder where initial models were run,

#' @param rel_id [int] gbd release_id - sourced with init.r

#' @return draws as data.table
read_draws <- function(
      run_id           = NULL,
      location_ids     = NULL,
      raked            = TRUE,
      special          = NULL,
      offset           = FALSE,
      use_cc_get_draws = FALSE,
      to_model_dir     = get("input_root", envir = .GlobalEnv),
      rel_id           = get("release_id", envir = .GlobalEnv)
) {
   
   key_vars <- c("location_id", "year_id", "age_group_id", "sex_id")
   
   
   
   
   if(use_cc_get_draws && raked){
      source("FILEPATH/get_draws.R")
      
      stgpr_run_log_dt <- read_stgpr_best_runs(root = to_model_dir, model_type = "vaccine_coverage")
      .run_id          <- copy(run_id) 
      stgpr_run_me_dt  <- stgpr_run_log_dt[run_id == .run_id, .(run_id, me_name)]
      
      config_stgpr_dt  <- load_vacc_model_db(section = "MAIN_COVERAGE_MODEL_LAUNCH")
      lookup_dt        <- merge(stgpr_run_me_dt, config_stgpr_dt[, .(me_name, modelable_entity_id)], by = "me_name")
      df               <- get_draws(gbd_id_type = "modelable_entity_id", 
                                    gbd_id      = lookup_dt$modelable_entity_id, 
                                    source      = "stgpr", 
                                    release_id  = rel_id, 
                                    version_id  = lookup_dt$run_id)
      varnames_keep <- c(key_vars, grep("draw_", names(df), value = TRUE))
      df <- df[, ..varnames_keep]
      
   } else {
      
      
      if (!is.null(special)) {
         path <- special
         if(!dir.exists(path)) {
            stop("Special raking directory `special` does not exist")
         }
      } else if (raked){
         path <- paste0("FILEPATH", run_id, "/draws_temp_0")
      } else {
         path <- paste0("FILEPATH", run_id, "FILEPATH")
         if(!dir.exists(path)) {
            stop("Unraked draws unavailable. ST-GPR likely finished successfully (deleting unraked draws)")
         }
      }
      
      
      if (is.null(location_ids)){
         files <- list.files(path, full.names=TRUE)
      } else {
         files <- file.path(path, paste0(location_ids, ".csv"))
      }
      
      
      if(any(!(file.exists(files)))) {
         stop(paste0("File doesn't exists: ", files[!(file.exists(files))]))
      }
      
      
      if (length(files)==0) stop(paste0("No draws for this run_id (", run_id, ")"))
      
      ncores <- Sys.getenv("OMP_NUM_THREADS")
      if (is.null(ncores) || ncores==""){
         ncores <- 2
      }
      df <- mclapply(files, fread, mc.cores=ncores) %>% rbindlist(., use.names=TRUE)
      
   }
   
  setkeyv(df, cols=key_vars)
  df <- unique(df)
  
  if (offset) {
    cols <- grep("draw_", names(df), value=TRUE)
    df <- df[, (cols) := .SD-0.01, .SDcols=cols]
  }
  
  
  df <- cap_est(df)

  return(df)

}
