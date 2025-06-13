



#' @param step [chr] which pipeline stage to load
#' @param config_path [path] absolute path to "config_age_specific.yaml"

#' @return [stderr] error if loader assumptions are violated
validate_config_file <- function(step, config_path){
   
   
   
   accepted_steps <- c(
      "pipeline",
      "survey_processing",
      "generate_lbd_csvs",
      "check_completeness",
      "compare_gbd_extraction_versions",
      "prep_exp",
      "prep_exp_diagnostics",
      "compare_data_versions",
      "compare_gbd_lbd_inputs",
      "launch_aggregate",
      "_save_results",
      "_save_results_diagnostics",
      "global_aggregates",
      "_upload_results",
      "_plot_vaccines"
   )
   
   if (step %!in% accepted_steps) {
      stop(paste0("Step ", step, " not supported by config. Must be one of: \n  ", paste0(accepted_steps, collapse = "\n  ")))
   }
   
   if (!file.exists(config_path)) {
      stop(paste0("config_path not found: ", config_path))
   }
}

#' @title Load Pipeline Config

#' @description Loads in the pipeline config yaml file that contains the parameters for





#' @param step Which part of the yaml file to load. Corresponds with the sections of the file,

#' @param config_path the path to config to load in

#' @return NULL

#' @importFrom yaml read_yaml
#' @export
load_pipeline_config <- function(step, config_path) {
   
   validate_config_file(step = step, config_path = config_path)
   pipeline_config <- yaml::read_yaml(config_path) 
   step_config     <- pipeline_config[[step]]
   
   message("Assigning objects to global env:")
   list2env(step_config, envir = globalenv())
   
   step_config_display <- lapply(step_config, function(step) ifelse(is.null(step), "NULL", step) )
   message(paste(capture.output(print.data.frame(utils::stack(step_config_display)[2:1], right = FALSE)), collapse = '\n'))
   
   
   
   
   
   for (object in names(step_config)) {
      
      
      if (is.null(step_config[[object]])) {
         next
      }
      
      
      
      
      
      step_config[[object]] <- gsub("\\", "/", step_config[[object]], fixed = T)
      
      
      if(grepl(pattern = "(?<!/)[0-9]{4}[_-][0-9]{2}[_-][0-9]{2}", step_config[[object]], perl=T)) {
         next
      }
      
      
      
      parsed <- tryCatch({
         eval(parse(text = step_config[[object]]))
      }, error = function(e) {
         NULL
      })
      
      
      
      if (is.function(parsed)) {
         next
      }
      
      if (is.null(parsed)) {
         next
      }
      
      
      assign(object, parsed, envir = globalenv())
   }
   
   return(invisible())
}






#' @param step [chr] valid pipeline step found in `validate_config_file()`
#' @param config_path [chr] where does config.yaml live?
#' @param save_root [chr] output folder for .yaml subset of config to be saved
#' @param list_pattern [chr] e.g. "run_" each successive run of a process will be named in order from 1 to n
#' @param add_list [list] named list of extra items to append to config metadata (e.g. other derived config args resulting from pipeline control-flow)
#' @param copy_to_archive [lgl] save a copy of the config to an archive directory (`dir_archive`)?
#' @param dir_archive [chr] path to archive directory for saving a copy of the config
#' @param run_date [chr] pipeline run date-version e.g. 2024_07_02

#' @return [none] saves a `config_{step}.yaml` file to disk (and an archive folder if `copy_to_archive` is TRUE)
#' @export

#' @importFrom yaml write_yaml
#' @importFrom yaml read_yaml

#' @examples
save_config <- function(step, config_path, save_root, add_list = NULL, list_pattern = "run_", copy_to_archive = FALSE, dir_archive, run_date){
   
   if(!dir.exists(save_root)) stop("save_root does not exist: ", save_root)
   stopifnot(is.character(list_pattern))
   
   if(!is.null(add_list)){
      .err_msg <- "add_list must be a named list (not whitespace)"
      if(!is.list(add_list))                   stop(.err_msg)
      if(is.null(names(add_list)))             stop(.err_msg)
      if(any(is.na(names(add_list))))          stop(.err_msg)
      names(add_list) <- trimws(names(add_list))
      if(any(nchar(names(add_list)) == 0))     stop(.err_msg)
   }
   
   validate_config_file(step = step, config_path = config_path)
   pipeline_config <- yaml::read_yaml(config_path)
   
   config_metadata <- append(
      list(username   = Sys.info()[["user"]]
           , time_stamp = as.character(Sys.time()))
      , pipeline_config[[step]]
   )
   
   
   config_metadata <- lapply(config_metadata, function(item){
      message(item)
      if(is.null(item)) return(NULL)
      if(grepl("^file\\.path", item)) {
         item <- try(normalizePath(eval(parse(text = item))))
      } 
      item
   })
   
   config_metadata[["add_list"]] <- add_list
   
   config_metadata_path <- file.path(save_root, paste0("config_", step, ".yaml"))
   
   if(!file.exists(config_metadata_path)){
      
      
      run_name                         <- paste0(list_pattern, "1")
      config_metadata_yaml             <- list()
      config_metadata_yaml[[run_name]] <- config_metadata
      
   } else {
      
      
      config_metadata_yaml             <- yaml::read_yaml(config_metadata_path)
      run_number                       <- sum(grepl(list_pattern, names(config_metadata_yaml))) + 1
      run_name                         <- paste0(list_pattern, run_number)
      config_metadata_yaml[[run_name]] <- config_metadata
   }
   
   yaml::write_yaml(config_metadata_yaml, config_metadata_path)
   message("Saved ", step, " ", run_name, " config to ", config_metadata_path)
   
   
   if(copy_to_archive){
      dir.create(dir_archive, showWarnings = FALSE, recursive = TRUE, mode = "0775")
      path_archive <- file.path(dir_archive, paste0("config_", step, "_", run_date, ".yaml"))
      file.copy(config_metadata_path, path_archive)
      message("Copied config to ", path_archive)
   }
   
}
