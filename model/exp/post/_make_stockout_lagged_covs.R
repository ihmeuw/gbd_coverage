



r_lib_team_dir <- "FILEPATH" 
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})

if(!interactive()){
   se$parse_all_named_cli_args(required_args = list(
      parallel_threads    = "integer"
      , code_root         = "character"
      , config_path       = "character"
      , run_date          = "character"
      , scenario_suffixes = "character"
      , make_lagged_means = "logical"
      , me_names_means    = NA 
      , make_lagged_draws = "logical" 
      , me_names_draws    = NA 
   )) 
} else {
   code_root   <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines")
   config_path <- file.path(code_root, "pipeline_config.yaml")
   
   parallel_threads = system(SYSTEM_COMMAND)
   run_date = "2023-11-02"
   scenario_suffixes = paste0(
      c(
         "_covidimputed" 
         
      )
      , collapse = ","
   )
   make_lagged_means = FALSE
   make_lagged_draws = TRUE
   me_names_means <- c(
      "vacc_bcg",
      "vacc_dpt3",
      "vacc_mcv1",
      "vacc_mcv2",
      "vacc_polio3",
      "vacc_rcv1",
      "vacc_hib3",
      "vacc_hepb3",
      "vacc_rotac",
      "vacc_pcv3"
   )
   me_names_draws <- c("vacc_dpt3")
}






scenario_suffixes <- unlist(strsplit(scenario_suffixes, split = ","))

valid_scenario_suffixes <- c("_covidfree", "_covidimputed")
if(!all(scenario_suffixes %in% valid_scenario_suffixes)){
   stop("scenario_suffixes must correspond to file names and paths - valid options : ",  toString(valid_scenario_suffixes))
}






library(data.table)
library(zoo)
library(magrittr)
library(parallel)
library("pbmcapply", lib.loc = r_lib_team_dir)
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"))
source(file.path(code_root, "init.r"))
source(file.path(code_root, "FILEPATH/make_stockout_lagged_cov_funcs.R"))




if (make_lagged_means == TRUE){
   
   message("Writing lagged summary (mean, 95%CI) rolling average covariates to the 'FILEPATH' folder for these scenarios : ", "'", toString(scenario_suffixes), "'")
   
   
   me_names_means <- sub("vacc_", "", me_names_means)
   
   lag_dir <- paste0("FILEPATH", gbd_cycle, "/", run_date, "/lagged")
   dir.create(path = lag_dir, showWarnings = FALSE, recursive = TRUE)
   
   
   for(scenario_suffix in scenario_suffixes){
      for (me in me_names) {
         fname_to_read  <- paste0("vacc_", me, scenario_suffix, ".rds") 
         fname_to_write <- paste0("lagged_", me, "_coverage_prop", scenario_suffix, ".rds")
         message(paste("Making mean lags for", fname_to_write))
         me_dt  <- readRDS(file.path("FILEPATH", gbd_cycle, run_date, fname_to_read))
         me_dt2 <- lag_means(me_dt) 
         saveRDS(me_dt2, file = file.path(lag_dir, fname_to_write))
      } 
   }
   
   message("Summary (mean, 95%CI) rolling average covariates saved to : ", lag_dir)
   
}





if (make_lagged_draws) {
  
  message("Making draw-level rolling average covariates to /ihme/covariates/vaccines/draws/exp/ .")

  
  
  for(scenario_suffix in scenario_suffixes){
     for (me in me_names_draws) {
        
        output_root <- file.path("FILEPATH", gbd_cycle, paste0(run_date, scenario_suffix), paste0("lagged_", me))
        input_root  <- clean_path("FILEPATH", gbd_cycle, paste0(run_date, scenario_suffix),  me, mustWork = TRUE)  
        me_files    <- list.files(input_root, full.names = TRUE)
        
        dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
        message("Making draw-level lags in ", output_root)
        
        
        draw_dts <- pbmclapply(
           mc.cores = parallel_threads
           , X      = me_files
           , FUN    = function(loc_fname, output_root){
              
              df   <- fread(loc_fname)
              df   <- setorderv(df, c("location_id", "age_group_id", "sex_id", "year_id"))
              df2  <- lag_draws(df)
              fwrite(df2, file.path(output_root, basename(loc_fname)))
              return(paste(basename(loc_fname), "saved."))
           }
           , output_root = output_root
        )
     }
  }
  
  message("Draw-level rolling average covariates saved to : ", output_root)
  
}


message("Done with lagged (rolling average) coverage file production.")
