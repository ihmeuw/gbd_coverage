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

  if(interactive()) {
    
    
    
    if(FALSE){
      
      args <- list(
        task               = "covid_adj_ratios",
        code_root          = file.path("FILEPATH", Sys.info()[["user"]], "vaccines"),
        run_id             = "",
        location_id        = "8",
        intro_year         = "", 
        max_intro_year     = "",
        min_intro_year     = "",
        draw_path          = "",
        raked_successfully = "",
        me_outdir          = "",
        run_date           = "2023-09-29_01_SB",
        stockout_ratios    = "True",
        
        gbd_cycle          = "gbd2023",
        output_file        = "FILEPATH",
      ) 
      
      args$results_root    = file.path("FILEPATH", args$gbd_cycle, args$run_date)
      
    }
    
    
    
    
    
    
    if(TRUE){
      args <- list()
      
      args[["code_root"]] <- "FILEPATH"
      args[["draw_path"]] <- "FILEPATH"
      args[["input_root"]] <- ""
      args[["intro_year"]] <- ""
      args[["location_id"]] <- "4841,43908,43872,4842,43909,43873,4843,43910,43874,4844,43911,43875,4846,43913,43877,4849,43916,43880,4850,43917,43881,4851,43918,43882,4852,43919,43883,4853,43920,43884,4854,43921,43885,4855,43922,43886,4856,43923,43887,4857,43924,43888,4859,43926,43890,4860,43927,43891,4861,43928,43892,4862,43929,43893,4863,43930,43894,4864,43931,43895,4865,43932,43896,44538,44539,44540,4867,43934,43898,4868,43935,43899,4869,43936,43900,4870,43937,43901,4871,43938,43902,4872,43939,43903,4873,43940,43904,4874,43941,43905,4875,43942,43906"
      args[["max_intro_year"]] <- "2024"
      args[["me_outdir"]] <- "FILEPATH"
      args[["min_intro_year"]] <- "1980"
      args[["ndraws"]] <- ""
      args[["output_file"]] <- "FILEPATH"
      args[["raked_successfully"]] <- ""
      args[["results_root"]] <- "FILEPATH"
      args[["run_id"]] <- "220989"
      args[["task"]] <- "rake_draws"
      args[["vaccine"]] <- ""
      args[["year_end"]] <- ""
      args[["year_start"]] <- ""
      
    }    
    
  } else {

    parser <- argparse::ArgumentParser()
    
    parser$add_argument(
      "--task",
      choices = c(
        "unrake_draws",
        "unrake_failed_draws",
        "rake_draws",
        "move_draws",
        "ysi_to_years"
      ),
      default = "unrake_draws",
      help = "Specifies the task to run"
    )
    parser$add_argument("--code_root", default          = NULL, help = "Gives the path to the root of the repo. Required by all")
    parser$add_argument("--results_root", default       = NULL, help = "Gives the path to the 'modeled/gbdxxxx/date_version folder.  Required by all.")
    parser$add_argument("--input_root", default         = NULL, help = "Gives the path to the 'to_model/gbdxxxx/date_version folder.")
    parser$add_argument("--run_id", default             = NULL, help = "Gives the model run id for which to run the specified task, Required by all 01 subtasks")
    parser$add_argument("--location_id", default        = NULL, help = "The location id, or location_ids for which to run the specified task. unrake_draws and unrake_failed_draws require a single location_id. rake_draws requires a comma separated list of all subnational location_ids")
    parser$add_argument("--intro_year", default         = NULL, help = "Gives the year_id for the intro year for which to run task. Required for unrake_draws, and unrake_failed_draws")
    parser$add_argument("--max_intro_year", default     = NULL, help = "Gives the year_id for the max intro year for which to run task. Required for rake_draws")
    parser$add_argument("--min_intro_year", default     = NULL, help = "Gives the year_id for the min intro year for which to run task. Required for rake_draws")
    parser$add_argument("--draw_path", default          = NULL, help = "Gives the path from which to load draws for rake_draws")
    parser$add_argument("--raked_successfully", default = NULL, help = "Gives a boolean for move_draws task.")
    parser$add_argument("--me_outdir", default          = NULL, help = "Gives the ouput path for each task. (Required for all tasks)")
    parser$add_argument("--output_file", default        = NULL, help = "Gives the filename into which task related log should be written to. After workflow all of these files will be consolidated")
    parser$add_argument("--ndraws", default             = NULL, help = "Number of draws")
    parser$add_argument("--vaccine", default            = NULL, help = "vaccine being raked.")
    parser$add_argument("--year_start", default         = NULL, help = "First year in time series")
    parser$add_argument("--year_end", default           = NULL, help = "Last year in time series")
    
    args <- parser$parse_args(get_args())

    message("To run this section interactively, copy these parameters to the interactive() block in execute_staggered_subnat_intro_tasks.R")
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
  
  
  
  
  load(file=tmp_env)
  message("RETICULATE_PYTHON is ", Sys.getenv("RETICULATE_PYTHON"))
  source("FILEPATH/public.R")
  
  
  year_start <- year_start %>% as.numeric 
  year_end   <- year_end %>% as.numeric

  start_task_time <- Sys.time()
  
  
  
  output_conn <- file(args[["output_file"]], open='a+b')
  sink(file = output_conn, type = "message")
  
  if (task_to_run == "unrake_draws") {
    
    stop("unrake_draws currently unsupported - issues with function.")
    
  } else if (task_to_run == "unrake_failed_draws") {
    
    message("Unraking draws that failed stgpr raking")
    
    unrake_failed_draws_task(
      failed_run_id = as.integer(args[["run_id"]])
      , location_id = as.integer(args[["location_id"]])
      , intro_year  = as.integer(args[["intro_year"]])
      , me_outdir   = args[["me_outdir"]]
    )
    
  } else if (task_to_run == "rake_draws") {
    
    message("Raking draws")
    
    rake_draws_task(
      run_id            = as.integer(args[["run_id"]])
      , loc_ids         = as.integer(unlist(strsplit(args[['location_id']], ",")))
      , rake_year_end   = as.integer(args[["max_intro_year"]])
      , rake_year_start = as.integer(args[["min_intro_year"]])
      , draw_path       = args[["draw_path"]]
      , me_outdir       = args[["me_outdir"]]
    )
    
  } else if (task_to_run == "move_draws") {
    
    message("Moving draws to the 02_raked_draws_intros_applied folder")
    
    move_draws_task(
      run_id               = as.integer(args[["run_id"]])
      , raked_successfully = as.logical(args[["raked_successfully"]])
      , me_outdir          = args[["me_outdir"]]
      , stgpr_run_log_root = args[["input_root"]]
    )
    
  } else if (task_to_run == "ysi_to_years") {
    
    message("Converting YSI to years")
    
    ysi_to_years(
      vaccine      = args[["vaccine"]]
      , me_outdir  = args[["me_outdir"]]
      , ndraws     = as.integer(args[["ndraws"]])
      , year_start = as.integer(args[["year_start"]])
      , year_end   = as.integer(args[["year_end"]])
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
  sink(NULL, type = "message")

  
  stop(paste0('Error: ', e), call.=FALSE)
}, finally={
  try(close(output_conn), 
      silent = TRUE)
})

