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

suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(parallel))
suppressMessages(library(readxl))
suppressMessages(library(DBI))
suppressMessages(library(magrittr))
suppressMessages(library(EnvStats, lib.loc="FILEPATH"))
r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})





tryCatch({
  sink(stdout(), type = "message")

  if(interactive()) {
    
    
    
    
    

    

    

    if(FALSE){
      args <- list(
        task             = "covid_adj_ratios",
        code_root        = file.path("FILEPATH", Sys.info()[["user"]], "vaccines"),
        me               = "vacc_hepb3_dpt3_ratio",
        draws_root       = "FILEPATH",
        save_draws       = "False",
        save_collapsed   = "TRUE",
        me_db            = "FILEPATH/me_db.csv",
        offset_remove    = "False",
        run_date         = "2023-09-29_01_SB",
        stockout_ratios  = "True",
        save_ratio_draws = "False",
        
        year_start       = "1980",
        year_end         = "2022",
        gbd_cycle        = "gbd2023",
        type             = "_covidimputed",
        message          = "interactive debug",
        output_file      = "FILEPATH",
        ndraws           = "100.0"
      )
      
      args$save_root                    = file.path(args$draws_root, "exp", args$gbd_cycle, args$run_date)
      args$collapsed_estimate_path_root = file.path("FILEPATH", args$gbd_cycle, args$run_date, args$me)
      args$results_root                 = file.path("FILEPATH", args$gbd_cycle, args$run_date)
      args$input_root                   = file.path("FILEPATH", args$gbd_cycle, args$run_date)
    }

    
    
    
    

    if(TRUE){
      
      args  <- list()
      
      args[["COVID_years"]] <- ""
      args[["code_root"]] <- "FILEPATH"
      args[["collapsed_estimate_path_root"]] <- ""
      args[["draw_root"]] <- ""
      args[["gbd_cycle"]] <- ""
      args[["input_root"]] <- "FILEPATH"
      args[["me"]] <- "vacc_polio3"
      args[["me_db"]] <- ""
      args[["me_names_epi_intro"]] <- ""
      args[["message"]] <- ""
      args[["ndraws"]] <- "100"
      args[["offset_remove"]] <- "False"
      args[["output_file"]] <- "FILEPATH"
      args[["results_root"]] <- "FILEPATH"
      args[["run_date"]] <- "2025_02_07_dhs5y_v2"
      args[["s1_cascade_run_date"]] <- ""
      args[["s1_stockout_ratios"]] <- ""
      args[["save_collapsed"]] <- ""
      args[["save_draws"]] <- ""
      args[["save_ratio_draws"]] <- ""
      args[["save_root"]] <- ""
      args[["stockout_ratios"]] <- "True"
      args[["task"]] <- "covid_adjustment"
      args[["type"]] <- ""
      args[["year_end"]] <- ""
      args[["year_start"]] <- ""
      
    }


  } else {

    parser <- argparse::ArgumentParser()

    parser$add_argument("--task",
                        choices = c("prep_draws",
                                    "covid_adjustment",
                                    "prep_ratios",
                                    "prep_dpt_ratio",
                                    "covid_adj_ratios",
                                    "covid_adj_dpt_ratios",
                                    "dpt1_coverage",
                                    "force_zero",
                                    "send_message"),
                        default = "prep_draws",
                        help    = "Specifies the task to run")
    parser$add_argument("--code_root", default=NULL, help = "Gives the path to the root of the repo. Required by all")
    parser$add_argument("--results_root", default=NULL, help = "Gives the path to the 'modeled/gbdxxxx/date_version folder. Required by all.")
    parser$add_argument("--input_root", default=NULL, help = "Gives the path to the 'to_model/gbdxxxx/date_version folder.")
    parser$add_argument("--me",
                        choices = c(
                          "vacc_dpt1",
                          "vacc_dpt3",
                          "vacc_mcv1",
                          "vacc_bcg",
                          "vacc_polio3",
                          "vacc_hepb3",
                          "vacc_pcv3",
                          "vacc_rotac",
                          "vacc_hib3",
                          "vacc_mcv2",
                          "vacc_mcv2_mcv1_ratio",
                          "vacc_hib3_dpt3_ratio",
                          "vacc_rotac_dpt3_ratio",
                          "vacc_rcv1_mcv1_ratio",
                          "vacc_pcv3_dpt3_ratio",
                          "vacc_hepb3_dpt3_ratio",
                          "vacc_dpt3_dpt1_ratio"
                        ),
                        help = "Gives the specific vaccine model, or ratio for the task")
    parser$add_argument("--me_names_epi_intro", help = "Vaccines modeled as 'straight' from prep_exp that need an intro applied in prep_draws. e.g. 'vacc_dpt1,vacc_mcv1' ")
    parser$add_argument("--draw_root", help = "The path from which to load draws. Required by prep_draws, prep_ratios, and china_stock_adjustment tasks")
    parser$add_argument("--save_draws", help = "A boolean specifying if draws should be saved. Required by prep_draws and prep_ratios")
    parser$add_argument("--save_collapsed", help = "A boolean specifying if collapsed results should be saved. Required by prep_draws and prep_ratios")
    parser$add_argument("--me_db", help = "A path to the me_db. Required by prep_draws")
    parser$add_argument("--offset_remove", help = "A boolean required by all tasks")
    parser$add_argument("--run_date", help = "The gbd run date. Required by all tasks")
    parser$add_argument("--stockout_ratios", help = "A boolean required by covid_adjustment and prep_ratios")
    parser$add_argument("--save_ratio_draws", help = "A boolean required by prep_ratios")
    parser$add_argument("--save_root", help = "Path in which to save results. Required by china_stock_adjustment")
    parser$add_argument("--year_start", help = "Start year_id required by china_stock_adjustment")
    parser$add_argument("--year_end", help = "End year_id required by china_stock_adjustment")
    parser$add_argument("--gbd_cycle", help = "specifies the gbd cycle. Required by dpt1_coverage tasks")
    parser$add_argument("--type", choices=c("_covidfree", "_covidimputed"), help="Required by force_zero task")
    parser$add_argument("--collapsed_estimate_path_root", help="Path to collapsed_estimates. Required by force_zero task")
    parser$add_argument("--message", help = "Message that should be written to output_file")
    parser$add_argument("--output_file", default=NULL, help = "Gives the filename into which task related log should be written to. After workflow all of these files will be consolidated")
    parser$add_argument("--ndraws", default=1000L, help = "Gives the number of draws specified for this model")
    parser$add_argument("--s1_stockout_ratios", help = "Should betas for stockouts used in calculating ratio covid adjustments come from the st-gpr stage 1 model, or a custom mrbrt stage 1 model?")
    parser$add_argument("--s1_cascade_run_date", help = "What run date was used for ratio stage 1 modeling")
    parser$add_argument("--COVID_years", help = "What years do covid adjustments apply to?")

    args <- parser$parse_args(get_args())
    message("To run this section interactively, copy these parameters to the interactive() block in execute_general_tasks.R")
    for(arg in names(args)) {
      message("args[[\"", arg, "\"]] <- \"", args[[arg]], "\"")
    }
    message("")
  }
  
  {
    task_to_run  <- args[["task"]]
    code_root    <- args[["code_root"]]
    results_root <- args[["results_root"]]
    
    
    
    
    tmp_env <- file.path(results_root, "jobmon_env/tmp_env_jobmon.RData")
    if(!file.exists(tmp_env)){
      stop("Could not find the tmp environment path!")
    }
    load(file = tmp_env)
    
    start_task_time <- Sys.time()
  }
    
  
  
  output_conn <- file(args[["output_file"]], open="a+b")
  sink(file=output_conn, type = "message")
  
  if (task_to_run == "prep_draws") {
    

    message(paste0("-- Prepping draws for vaccine ", args[["me"]]))
    
    prep_draws(
      me                   = args[["me"]]
      , me_names_epi_intro = unlist(strsplit(x = args[["me_names_epi_intro"]], split = ","))
      , run_date           = args[["run_date"]]
      , special_draw_root  = args[["draw_root"]]
      , save_draws         = as.logical(args[["save_draws"]])
      , save_collapsed     = as.logical(args[["save_collapsed"]])
      , me_db              = fread(args[["me_db"]])
      , offset_remove      = as.logical(args[["offset_remove"]])
    )


  } else if (task_to_run == "covid_adjustment") {
    

    message(paste0("-- Applying covid adjustment for vaccine ", args[["me"]]))
    
    apply_covid_adjustment(
      me                 = args[["me"]]
      , run_date         = args[["run_date"]]
      , ndraws           = as.integer(args[["ndraws"]])
      , results_root     = args[["results_root"]]
      , offset_remove    = as.logical(args[["offset_remove"]])
      , stockout_ratios  = as.logical(args[["stockout_ratios"]])
      , covid_years      = COVID_years 
      , to_model_dir     = args[["input_root"]]
    )

  } else if (task_to_run %in% c("prep_ratios", "prep_dpt_ratio")) {
    
    
    message(paste0("---- Prepping vaccine ratios for ", args[["me"]]))

    prep_ratio(
      me                   = args[["me"]]
      , gbd_run_date       = args[["run_date"]]
      , special_draw_root  = args[["draw_root"]]
      , save_draws         = as.logical(args[["save_draws"]])
      , save_collapsed     = as.logical(args[["save_collapsed"]])
      , save_ratio_draws   = as.logical(args[["save_ratio_draws"]])
      , offset_remove      = as.logical(args[["offset_remove"]])
      , stockout_ratios    = as.logical(args[["stockout_ratios"]])
      , me_db              = fread(file.path(code_root, "reference/me_db.csv"))
    )

  } else if (task_to_run %in% c("covid_adj_ratios", "covid_adj_dpt_ratios")) {
    
    
    message(paste0("-- Applying covid adjustment for vaccine ratios ", args[["me"]]))

    apply_covid_adjustment_ratios(
      me                    = args[["me"]]
      , ndraws              = as.integer(args[["ndraws"]])
      , save_collapsed      = as.logical(args[["save_collapsed"]])
      , offset_remove       = as.logical(args[["offset_remove"]])
      , gbd_run_date        = args[["run_date"]]
      , covid_years         = COVID_years 
      , s1_stockout_ratios  = as.logical(args[["s1_stockout_ratios"]])
      , s1_cascade_run_date = args[["s1_cascade_run_date"]]
      , to_model_dir        = args[["input_root"]]
    )

  } else if (task_to_run == "force_zero"){
    

    message(paste0("Forcing to zero for vaccine ", args[["me"]]))

    force_zero(
      me                           = args[["me"]],
      type                         = args[["type"]],
      collapsed_estimate_path_root = args[["collapsed_estimate_path_root"]],
      COVID_years                  = COVID_years 
    )

  } else if (task_to_run == "send_message"){
    
    
    
    message(args[["message"]])
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
