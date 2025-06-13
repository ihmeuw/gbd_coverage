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
    args[["fhs_run_TF"]] <- "False"
    args[["files_to_check"]] <- ""
    args[["gbd_cycle"]] <- "gbd2023"
    args[["location_id"]] <- ""
    args[["location_set_id"]] <- "22"
    args[["me"]] <- ""
    args[["me_db"]] <- ""
    args[["offset_remove"]] <- ""
    args[["output_file"]] <- "FILEPATH"
    args[["release_id"]] <- "16"
    args[["results_root"]] <- "FILEPATH"
    args[["run_date"]] <- "2024_08_20_pipeline_test"
    args[["save_collapsed"]] <- ""
    args[["save_draws"]] <- ""
    args[["task"]] <- "rake_to_stock_adjusted"
    args[["vaccine"]] <- "pcv3"
    args[["vaccine_ratio"]] <- ""
    args[["year_end"]] <- "2024"
    
    }    
    
  } else {

    parser <- argparse::ArgumentParser()
    
    parser$add_argument("--task",
                        choices = c(
                          "apply_china_stock_adjustment",
                          "rake_to_stock_adjusted",
                          "back_calculate_ratio",
                          "delete_numerator_draws",
                          "save_confirmation",
                          "prep_draws"
                        ),
                        default = "apply_china_stock_adjustment",
                        help = "Specifies the task to run")
    parser$add_argument("--code_root", default=NULL, help = "Gives the path to the root of the repo. Required by all")
    parser$add_argument("--results_root", default=NULL, help = "Gives the path to the 'modeled/gbdxxxx/date_version folder. Required by all.")
    parser$add_argument("--location_id", default=NULL, help = "The location id for which to run the specified task.")
    parser$add_argument("--year_end", default=NULL, help = "Gives the year_id for the year_end for which to run task.")
    parser$add_argument("--gbd_cycle", default=NULL, help = "Gives the gbd_cycle for which to run the task.")
    parser$add_argument("--vaccine", default=NULL, help = "Gives the vaccine for which to run the task.")
    parser$add_argument("--vaccine_ratio", default=NULL, help = "Gives the vaccine ratio for which to run the task.")
    parser$add_argument("--run_date", default=NULL, help = "Gives the run_date for the task.")
    parser$add_argument("--confirmation_file", default=NULL, help = "Gives the path of the confirmation_file, for the save_confirmation ask.")
    parser$add_argument("--files_to_check", default=NULL, help = "Gives the path to check if the prep draws should be run, for the prep_draws ask.")
    parser$add_argument("--me",
                        choices = c(
                          "vacc_dpt1",
                          "vacc_dpt3",
                          "vacc_mcv1",
                          "vacc_bcg",
                          "vacc_polio3",
                          "vacc_dpt12_cond",
                          "vacc_mcv2_mcv1_ratio",
                          "vacc_hib3_dpt3_ratio",
                          "vacc_rotac_dpt3_ratio",
                          "vacc_rcv1_mcv1_ratio",
                          "vacc_pcv3_dpt3_ratio",
                          "vacc_hepb3_dpt3_ratio"
                        ),
                        help = "Gives the specific vaccine model, or ratio for the task")
    parser$add_argument("--draw_root", help = "The path from which to load draws. Required by prep_draws, prep_ratios, and china_stock_adjustment tasks")
    parser$add_argument("--save_draws", help = "A boolean specifying if draws should be saved. Required by prep_draws and prep_ratios")
    parser$add_argument("--save_collapsed", help = "A boolean specifying if collapsed results should be saved. Required by prep_draws and prep_ratios")
    parser$add_argument("--me_db", help = "A path to the me_db. Required by prep_draws")
    parser$add_argument("--offset_remove", help = "A boolean required by all tasks")
    parser$add_argument("--output_file", default=NULL, help = "Gives the filename into which task related log should be written to. After workflow all of these files will be consolidated")
    
    parser$add_argument("--fhs_run_TF", help = "[lgl] is this an FHS run or not?")
    parser$add_argument("--release_id", help = "[int] how to call hierarchy & populations for gbd vs. fhs runs")
    parser$add_argument("--location_set_id", help = "[int] how to call hierarchy for gbd vs. fhs runs")
    


    args <- parser$parse_args(get_args())

    message("To run this section interactively, copy these parameters to the interactive() block in execute_china_stock_adjustment_tasks.R")
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

  start_task_time <- Sys.time()
  
  
  
  output_conn <- file(args[["output_file"]], open="a+b")
  sink(file=output_conn, type = "message")
  
  if (task_to_run == "apply_china_stock_adjustment") {
    
    message("------ Applying national-level stock data adjustment")
    
    
    apply_china_stock_adj_task(
      gbd_cycle = args[["gbd_cycle"]],
      vaccine   = args[["vaccine"]],
      run_date  = args[["run_date"]],
      loc_id    = as.integer(args[["location_id"]]),
      year_end  = as.integer(args[["year_end"]])
    )
    
  } else if (task_to_run == "rake_to_stock_adjusted") {
    
    message("------ Raking subnational draws to stock-adjusted national level ")
    
    rake_to_stock_adjusted_task(
      gbd_cycle       = args[["gbd_cycle"]],
      vaccine         = args[["vaccine"]],
      run_date        = args[["run_date"]],
      year_end        = as.integer(args[["year_end"]]),
      fhs_run_TF      = as.logical(args[["fhs_run_TF"]]),
      location_set_id = as.integer(args[["location_set_id"]]),
      release_id      = as.integer(args[["release_id"]])
    )
    
  } else if (task_to_run == "back_calculate_ratio") {
    
    message("------ Back-calculating ratio draws from stock-adjusted draws")
    
    back_calculate_ratio_draws_task(
      vaccine       = args[["vaccine"]],
      vaccine_ratio = args[["vaccine_ratio"]],
      run_date      = args[["run_date"]],
      loc_id        = as.integer(args[["location_id"]])
    )
    
  } else if (task_to_run == "delete_numerator_draws"){
    
    message("------ Deleting Numerator Vaccine Draws")
    
    numerator_vaccine_draw_folder <- file.path("FILEPATH",
                                               args[["gbd_cycle"]],
                                               args[["run_date"]],
                                               paste0("vacc_", args[["vaccine"]]))
    unlink(numerator_vaccine_draw_folder, recursive = TRUE)
    
  } else if (task_to_run == "save_confirmation"){
    
    message("-- China stock data adjustment: Success")
    file.create(args[["confirmation_file"]])
    
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

