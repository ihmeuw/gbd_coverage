#' @title Get arguments for argparse

CLUSTER_NAME

#' @return vector of arguments as characters in the order passed to sbatch
get_args <- function() {
   
   args <- commandArgs(trailingOnly = TRUE)
   
   
   if (is.null(args) || length(args) == 0) {
      
      
      
      
      
      
      
      
      
      
      args <- commandArgs(trailingOnly = FALSE)
      idx <- match("--no-save", args)
      if (is.na(idx)) {
         message(sprintf("--no-save not detected in commandArgs() - cannot determine CLI args from '%s'", paste(commandArgs(), collapse = " ")))
         args <- character(0)
      } else if (idx == length(args)) {
         
         args <- character(0)
      } else {
         
         args <- args[-seq_len(idx + 1)]
      }
   }
   return(args)
}






tryCatch({
   sink(stdout(), type = "message")
   
   suppressMessages(library(data.table))
   suppressMessages(library(dplyr))
   suppressMessages(library(parallel))
   suppressMessages(library(readxl))
   suppressMessages(library(DBI))
   suppressMessages(library(magrittr))
   suppressMessages(library(ggplot2))
   r_lib_team_dir <- "FILEPATH"
   withr::with_libpaths(new = r_lib_team_dir, code = {
      se <- asNamespace("INDIVIDUAL_NAME") 
   })
   
   if(interactive()) {
      
      
      
      if(FALSE){
         
         args <- list(
            task              = "covid_adj_ratios",
            code_root         = file.path("FILEPATH", Sys.info()[["user"]], "vaccines"),
            location_id       = "6",
            vaccine           = "hib3",
            vaccine_ratio     = "vacc_hib3_dpt3_ratio",
            confirmation_file = "",
            files_to_check    = "",
            me                = "vacc_hib3",
            draw_root         = "FILEPATH",
            save_draws        = "False",
            save_collapsed    = "TRUE",
            me_db             = "FILEPATH/me_db.csv",
            offset_remove     = "False",
            run_date          = "2024_06_20_china_cap",
            stockout_ratios   = "True",
            
            year_start        = "1980",
            year_end          = "2024",
            gbd_cycle         = "gbd2023",
            output_file       = "FILEPATH"
         ) 
         
         args$save_root                    = file.path(args$draw_root, "exp", args$gbd_cycle, args$run_date)
         args$collapsed_estimate_path_root = file.path("FILEPATH", args$gbd_cycle, args$run_date, args$me)
         args$results_root                 = file.path("FILEPATH", args$gbd_cycle, args$run_date)
         
      }
      
      
      
      
      
      
      if(TRUE){
         args <- list()
         
         args[["code_root"]] <- "FILEPATH"
         args[["confirmation_file"]] <- ""
         args[["draw_root"]] <- ""
         args[["draws_read_root"]] <- "FILEPATH"
         args[["draws_root_gbdxx"]] <- ""
         args[["draws_save_root"]] <- "FILEPATH"
         args[["fhs_location_set_id"]] <- ""
         args[["fhs_n_draws"]] <- "500"
         args[["fhs_path_dpt3"]] <- "FILEPATH/vacc_dtp3.nc"
         args[["fhs_path_mcv1"]] <- "FILEPATH/vacc_mcv1.nc"
         args[["fhs_release_id"]] <- ""
         args[["files_to_check"]] <- ""
         args[["gbd_cycle"]] <- ""
         args[["location_id"]] <- ""
         args[["location_set_id_gbd"]] <- ""
         args[["me"]] <- "vacc_mcv2_mcv1_ratio"
         args[["me_db"]] <- ""
         args[["n_cores"]] <- ""
         args[["n_draws_vc"]] <- "1000"
         args[["offset_remove"]] <- ""
         args[["output_file"]] <- "FILEPATH"
         args[["release_id_gbd"]] <- ""
         args[["results_root"]] <- "FILEPATH"
         args[["run_date"]] <- ""
         args[["run_date_gbd_best"]] <- ""
         args[["save_collapsed"]] <- ""
         args[["save_draws"]] <- ""
         args[["task"]] <- "fhs_resample"
         args[["vaccine"]] <- ""
         args[["vaccine_ratio"]] <- ""
         args[["year_end"]] <- ""
         args[["year_end_gbd"]] <- ""
         
         
      }    
      
   } else {
      
      
      message("INDIVIDUAL_NAME parser")
      args <- se$parse_all_named_cli_args()
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      message("To run this section interactively, copy these parameters to the interactive() block in execute_fhs_tasks.R")
      for(arg in names(args)) {
         message("args[[\"", arg, "\"]] <- \"", args[[arg]], "\"")
      }
      message("")
   }
   
   
   task_to_run  <- args[["task"]]
   code_root    <- args[["code_root"]]
   results_root <- args[["results_root"]]
   tmp_env      <- file.path(results_root, "jobmon_env/tmp_env_jobmon.RData")
   if(!file.exists(tmp_env)){
      stop("Could not find the tmp environment path!")
   }
   
   load(file = tmp_env)
   
   start_task_time <- Sys.time()
   
   
   
   output_conn <- file(args[["output_file"]], open="a+b")
   sink(file=output_conn, type = "message")
   
   if (task_to_run == "fhs_resample"){
      
      message("------ FHS - Resampling FHS draws")
      
      source(file.path(code_root, "FILEPATH/fhs_resample_task.R"))
      
      fhs_resample_task(
         me                  = args[["me"]],
         draws_read_root     = args[["draws_read_root"]], 
         draws_save_root     = args[["draws_save_root"]], 
         fhs_path_dpt3       = args[["fhs_path_dpt3"]],
         fhs_path_mcv1       = args[["fhs_path_mcv1"]],
         fhs_n_draws         = as.integer(args[["fhs_n_draws"]]),
         n_draws_vc          = as.integer(args[["n_draws_vc"]])
      )
      
      
   } else if (task_to_run == "fhs_straight") {
      
      message("------ FHS - Appending direct model forecasts to VC best results")
      
      fhs_append_straight_vaccines_task(
         code_root           = args[["code_root"]],
         results_root        = args[["results_root"]],
         draws_read_root     = args[["draws_read_root"]], 
         draws_save_root     = args[["draws_save_root"]], 
         run_date            = args[["run_date"]],
         year_end_gbd        = as.integer(args[["year_end_gbd"]]),
         fhs_path_dpt3       = args[["fhs_path_dpt3"]],
         fhs_path_mcv1       = args[["fhs_path_mcv1"]],
         fhs_release_id      = as.integer(args[["fhs_release_id"]]),
         fhs_location_set_id = as.integer(args[["fhs_location_set_id"]]),
         release_id_gbd      = as.integer(args[["release_id_gbd"]]),
         location_set_id_gbd = as.integer(args[["location_set_id_gbd"]]),
         n_cores             = as.integer(args[["n_cores"]])
      )
      
   } else if (task_to_run == "fhs_fixes") {
      
      message("------ FHS - Applying custom FHS fixes")
      
      fhs_fixes_task(
         code_root           = args[["code_root"]],
         results_root         = args[["results_root"]],
         draws_read_root     = args[["draws_read_root"]], 
         draws_save_root     = args[["draws_save_root"]], 
         run_date            = args[["run_date"]],
         year_end_gbd        = as.integer(args[["year_end_gbd"]]),
         fhs_release_id      = as.integer(args[["fhs_release_id"]]),
         fhs_location_set_id = as.integer(args[["fhs_location_set_id"]]),
         release_id_gbd      = as.integer(args[["release_id_gbd"]]),
         location_set_id_gbd = as.integer(args[["location_set_id_gbd"]])
      )
      
      
   } else if (task_to_run == "fhs_ratio") {
      
      message("------ FHS - vaccine ratio calculations")
      
      fhs_calculate_vaccine_ratio_task(
         me_ratio     = args[["me"]],
         run_date     = args[["run_date"]]
      )
   } 
   
   sink(stdout(), type = "message")
   
   
   tryCatch({
      suppressWarnings(cat(readLines(output_conn)))
   }, error = function(e) {
      
      message(paste0('Encountered the following error when writing task messages back to stdout: ', e))
   })
   
   message(paste0("Task finished! Total runtime = ", Sys.time() - start_task_time))
   sink(NULL, type = "message")
   
}, error = function(e) {
   sink(NULL,type = "message")
   
   
   stop(paste0('Error: ', e), call.=FALSE)
}, finally={
   try(close(output_conn), 
       silent = TRUE)
})
