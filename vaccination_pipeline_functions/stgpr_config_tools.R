


















#' @param stgpr_config_section [chr] a valid vaccine_model_db.csv section to load (see `load_vacc_model_db()` documentation)
#' @param stgpr_config_custom_dt [data.table] a data.table with columns `me_name` (ID column for merging), and any other fields of the stgpr_config.csv file to replace with custom entries e.g. `path_to_data`
#' @param save_dir [path] a valid _directory_ to save the config file e.g. `file.path(to_model_dir, "stockout")`

#' @return [path] full path to date_version-specific config file (`stgpr_config_<stgpr_config_section>.csv`) & writes that file to `save_dir`
#' @export

#' @examples
save_stgpr_config_subsection <- function(stgpr_config_section, stgpr_config_custom_dt, save_dir){
   
   
   if (!is.character(stgpr_config_section)) stop("stgpr_config_section must be a character string")
   if (!is.data.table(stgpr_config_custom_dt)) stop("stgpr_config_custom_dt must be a data.table")
   if (!"me_name" %in% names(stgpr_config_custom_dt)) stop("stgpr_config_custom_dt must have a column named 'me_name'")
   if (!is.character(save_dir)) stop("save_dir must be a character string")
   if (!dir.exists(save_dir)) stop("save_dir must be a valid directory")
   
   stgpr_config_dt <- load_vacc_model_db(section = stgpr_config_section)
   
   
   if (!all(names(stgpr_config_custom_dt) %in% names(stgpr_config_dt))) {
      invalid_varnames <- setdiff(names(stgpr_config_custom_dt), names(stgpr_config_dt))
      invalid_varnames <- paste(invalid_varnames, collapse = "\n  ")
      stop(paste0("All varnmes in stgpr_config_custom_dt must exist in vaccine_model_db.csv: \n", invalid_varnames))
   }
   
   
   drop_vars <- setdiff(names(stgpr_config_custom_dt), "me_name")
   stgpr_config_dt[, c(drop_vars) := NULL]
   
   stgpr_config_section_dt <- merge(stgpr_config_custom_dt, stgpr_config_dt, by = "me_name", all.x = TRUE)
   
   if (nrow(stgpr_config_section_dt) == 0) {
      message(paste(capture.output(stgpr_config_custom_dt), collapse = "\n"), "\n")
      stop("No rows in stgpr_config_section_dt after merging user-defined data.table by `me_name` - inspect \n\n")
   }
   
   path_stgpr_config <- file.path(save_dir, paste0("config_", tolower(stgpr_config_section), ".csv"))
   if (file.exists(path_stgpr_config)) {
      message("The ST-GPR Config file already exists - overwriting.")
   }
   
   message("Writing to disk: ", path_stgpr_config)
   fwrite(stgpr_config_section_dt, path_stgpr_config)
   return(path_stgpr_config)
}










#' @param stgpr_config_custom_dt [data.table] e.g. see below









#' @param path_varnames [chr] e.g. c("path_to_data", "path_to_custom_stage_1", "path_to_custom_covariates")
#' @param stgpr_config_section [chr] Section of the vaccination model database to load (exact, case-sensitive)
#' @param output_dir [path] folder to save the custom ST-GPR config file

#' @return
#' @export

#' @examples
make_stgpr_custom_config <- function(stgpr_config_custom_dt, 
                                     path_varnames,
                                     stgpr_config_section,
                                     save_dir
){
   
   if (!is.data.table(stgpr_config_custom_dt)) stop("stgpr_config_custom_dt must be a data.table")
   if (!is.character(path_varnames)) stop("path_varnames must be a character vector")
   if (!all(path_varnames %in% names(stgpr_config_custom_dt))) stop("Some path_varnames do not exist in stgpr_config_custom_dt\n",
                                                                    toString(setdiff(path_varnames, names(stgpr_config_custom_dt))))
   if (!dir.exists(save_dir)) stop("save_dir does not exist")
   
   chk_paths <- unlist(stgpr_config_custom_dt[, ..path_varnames])
   
   chk_paths <- chk_paths[!is.na(chk_paths)]
   assert_files_exist(chk_paths, verbose = FALSE)
   
   message("Saving custom ST-GPR config.")
   path_stgpr_config <- save_stgpr_config_subsection(
      stgpr_config_section   = stgpr_config_section,
      stgpr_config_custom_dt = stgpr_config_custom_dt,
      save_dir               = save_dir
   )
   
   return(path_stgpr_config)
   
}



#' @param date_version [chr] date_version for pipeline run
#' @param path_bundle_version_log [chr] full path to bundle version log e.g. "FILEPATH/gbd_covariate_bundle_versions.csv"
#' @param my_me_names [chr] vector of modelable entity names to find crosswalk ids for e.g. "bias_vacc_mcv1"

#' @return [int] vector of crosswalk_version_ids
#' @export

#' @examples
find_bundle_crosswalk_version_ids <- function(date_version, my_me_names, path_bundle_version_log){
   
   
   if (!is.character(date_version)) stop("date_version must be a character string")
   if (!is.character(my_me_names)) stop("my_me_names must be a character string")
   assert_files_exist(path_bundle_version_log, verbose = FALSE)
   
   bundle_version_dt  <- fread(path_bundle_version_log)
   
   xw_ids <- unlist(lapply(my_me_names, function(my_me_name){
      xw_id <- bundle_version_dt[me_name == my_me_name & run_date == date_version, crosswalk_version_id]
      if(!length(xw_id)) stop(paste("No crosswalk_version_id for:", my_me_name, "for", date_version, "in", path_bundle_version_log))
      
      xw_id  <- as.integer(max(xw_id, na.rm = TRUE))
      if(is.na(xw_id) || is.infinite(xw_id)) stop(paste("crosswalk_version_id is missing for:", my_me_name, "for", date_version, "in", path_bundle_version_log))
      return(xw_id)
   }))
   
   return(xw_ids)
}









#' @param to_model_dir [path] the directory to save the log
#' @param date [chr] date_version for the pipeline run
#' @param me_name [chr] modelable entity name e.g. "vacc_mcv1"
#' @param run_id [int] ST-GPR run id (returned by ST-GPR api registration)
#' @param model_type [chr] e.g. "vaccine_coverage"
#' @param me_id [int] modelable entity id

#' @return [none] appends to file on disk
#' @export

#' @examples
append_to_stgpr_run_id_log <- function(date,
                                       me_name,
                                       run_id,
                                       model_type, 
                                       modelable_entity_id ,
                                       to_model_dir){
   
   if (!is.character(date)) stop("date must be a character string")
   if (!is.character(me_name)) stop("me_name must be a character string")
   if (!is.character(model_type)) stop("model_type must be a character string")
   if (!dir.exists(to_model_dir)) stop("to_model_dir does not exist: ", to_model_dir)
   
   path_stgpr_log <- ifelse(is.na(run_id), "NA", file.path("FILEPATH", run_id, "logs"))  
   
   dt_runs <- data.table(
      date_version          = date
      , timestamp_pst       = format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M%S")
      , me_name             = me_name
      , run_id              = as.integer(run_id)
      , model_type          = model_type
      , modelable_entity_id = modelable_entity_id 
      , path_stgpr_log      = path_stgpr_log
      , is_best             = 0L 
   )
   
   run_id_log_path <- file.path(to_model_dir, "log_stgpr_run_ids.csv")
   fwrite(dt_runs, run_id_log_path, append = TRUE)
} 






#' @param root [path] e.g."FILEPATH"

#' @return [none] side effect: write log_stgpr_runs with new logical column `model_succeeded` 
#' @export

#' @examples
record_stgpr_model_status <- function(root, stgpr){
   stopifnot(exists("stgpr", envir = globalenv()))
   
   log_stgpr_runs <- read_vc_logs(root, "stgpr_runs")
   message("Recording ST-GPR model status.")
   for(idx in 1:nrow(log_stgpr_runs)){
      log_stgpr_runs$model_succeeded[[idx]] <- as.integer(
         tryCatch(
            
            stgpr$get_model_status(log_stgpr_runs$run_id[[idx]])
            , error = function(e) NA_integer_
         )
      )
   }
   
   log_stgpr_runs[, model_succeeded := ifelse(model_succeeded == 1, TRUE, FALSE)]
   fwrite(log_stgpr_runs, file.path(root, "log_stgpr_run_ids.csv"))
}









#' @param root [path] e.g.

#' @param model_type [chr] if NULL (default), do not subset the log.  If not


#' @return [data.table] stgpr_runs log created by stgpr_config_tools.R
#' @export

#' @examples
read_stgpr_best_runs <- function(root, model_type = NULL){
   
   
   stopifnot(dir.exists(root))
   stopifnot(is.character(model_type) | is.null(model_type))
   stopifnot(length(model_type) == 1 | is.null(model_type))
   
   log <- read_vc_logs(root, "stgpr_runs")
   
   model_types_found <- unique(log$model_type)
   if(length(model_types_found) == 0) stop("No model_types found in log.")
   
   
   
   if (is.null(model_type)){
      
   } else if (!is.null(model_type) && model_type %in% model_types_found) {
      .model_type <- model_type 
      log <- log[model_type == .model_type, ]
   } else {
      stop("Model type ", model_type, " not found in log. Found model_types: ", toString(model_types_found))
   }
   
   
   
   
   
   
   dupe_combos      <- log[, .N, by = .(me_name, model_type)][N > 1, ]
   log_dupes        <- log[dupe_combos[, .(me_name, model_type)], on = c("me_name", "model_type")]
   log_dupes_best   <- log_dupes[is_best == 1, ]
   
   log_no_dupes     <- log[!dupe_combos[, .(me_name, model_type)], on = c("me_name", "model_type")]
   
   
   log_best         <- rbind(log_no_dupes, log_dupes_best)
   
   if(nrow(log_best) == 0){
      stop(
         "No non-duplicate rows found in ST-GPR run log by me_name and model_type: \n",
         file.path(root, "log_stgpr_run_ids.csv"), "/n",
         "All rows require unique me_name and model_type combo.\n",
         "If you re-ran models, select rows with is_best = 1"
      )
   }
   
   
   dupe_combos_best <- log_best[, .N, by = .(me_name, model_type)][N > 1, ]
   
   if (nrow(dupe_combos_best) > 0) {
      stop("Duplicated rows found in ST-GPR run log: ", file.path(root, "log_stgpr_run_ids.csv"), "\n", 
           "All rows require unique me_name and model_type combo.  
           If there are duplicate rows, select only one combination with is_best = 1.
           Found duplicate rows:\n ", 
           paste(capture.output(dupe_combos_best), collapse = "\n"))
   }
   
   return(log_best)
}

