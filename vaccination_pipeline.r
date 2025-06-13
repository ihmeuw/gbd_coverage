#----HEADER------------------------------------------------------------------------------------------------------------
# Purpose: Code-based sequence of full GBD vaccination estimation pipeline



#---- PARSER --------------------------------------------------------------------------------------------------------------
# Config flow
# 1. Prefer parse CLI args if submitted
# 2. Look for .Renviron defined CODE_ROOT
#    - called by project-level .Rprofile file - multiple projects allow multiple roots
# 3. Finally, default to standard code_root if others are undefined

r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
   message("Starting arg-parser.")
   se$parse_all_named_cli_args(required_args = list(code_root = "character"
                                                    , config_path = "character"))
} else {
   if(!is.na(Sys.getenv()[['CODE_ROOT']])){ 
      code_root <- Sys.getenv()[['CODE_ROOT']]
   } else {
      code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
   }
   config_path <- file.path(code_root, "pipeline_config.yaml")
}


message("code_root: ", code_root)
message("config_path: ", config_path)
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), recursive = FALSE, modifiedOnly = FALSE)
load_pipeline_config(step = "pipeline", config_path)
start_time <- proc.time()



#----STEP 1------------------------------------------------------------------------------------------------------------

# 1 - Prep literature data

# if any new report data have been extracted, locations changed in the Google
# Sheet, or GBD location hierarchy has changed, run this code to re-proccess the
# literature report extractions from the Google Sheet.
if (run_prep_gbd_literature_data) {
   message("Running GBD literature prep.")
   source(file.path(code_root, "extraction/extract_literature_gbd.R"), echo = TRUE)
}


# Produces output at PATH/resampled with info on dropped data from resampling
if (run_prep_lbd_literature_data) {
   clean_pipeline_environment()
   message("Running LBD literature prep.")
   source(file.path(code_root, "extraction/extract_literature_lbd.R"), echo = TRUE)
}





#----STEP 2------------------------------------------------------------------------------------------------------------
   
   # 2 - prep reference datasets (JRF coverage data and other JRF ancillary
   # data, e.g. stockouts, introductions, schedules, etc.)
   
   # from raw reference data inputs, this code creates square, prepped datasets
   # with GBD location hierarchy and years that are compatible with subsequent
   # coverage estimation/prep infrastructures. Re-run with JRF/any supplementary
   # data updates to reference datasets OR the location hierarchy / estimation
   # years changed for GBD:
   
if (run_prep_reference) {
   clean_pipeline_environment()
   message("Running reference prep.")
   source(file.path(code_root, "FILEPATH/prep_reference.r"), echo = TRUE)
}


#----STEP 3------------------------------------------------------------------------------------------------------------

# 3 - Process Extracted survey data

# From raw extracted data, processes data for input to gbd and lbd models.

# Looks for nids that have failed processing and alerts for any that are not set to be included
# in this round of processing according to the survey_processing step of the vaccination_config

if (run_survey_processing) {
   clean_pipeline_environment()
   load_pipeline_config(step = "survey_processing", config_path)
   identify_failed_nids(nid)
   message("Running survey processing prep.")
   source(file.path(code_root, "FILEPATH/00_processing_wrapper.R"), echo = TRUE)
   identify_failed_nids()
}



# Combines processed survey microdata and lit extractions
if (run_generate_lbd_csvs) {
   clean_pipeline_environment()
   message("Running LBD surey mirodata + lit extraction combination.")
   source(file.path(code_root, "FILEPATH/generate_lbd_csvs.R"), echo = TRUE)
}


# reads in output of generate_lbd_csvs, checks for completeness
# and compares against prior runs
if (run_check_completeness) {
   clean_pipeline_environment()
   message("Running LBD completeness check.")
   source(file.path(code_root, "FILEPATH/check_completeness.R"), echo = TRUE)
}




#----STEP 4------------------------------------------------------------------------------------------------------------

# 4 - prepare to model coverage data. Saves out to dated `to_model` folder.
if (run_prep_exp) {
   message("Running prep_exp.")
   jobid_prep_exp <- se$submit_job(
      script_path            = file.path(code_root, "FILEPATH/prep_exp.r")
      , threads              = 1L
      , mem                  = "8G"
      , runtime_min          = 60 * 24
      , partition            = "all.q,long.q"
      , account              = "proj_cov_vpd"
      , send_email           = TRUE
      , console_style_log_tf = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
   wait_on_slurm_job_id(job_id = jobid_prep_exp, break_on_failure = TRUE, cycle_sleep_sec = 300)
}

# Run diagnostics between prep_exp versions
if(run_prep_exp_diagnostics){
   message("Running prep_exp diagnostics, old vs. new")
   jobid_prep_exp_diagnostics <- se$submit_job(
      script_path            = file.path(code_root, "FILEPATH/launch_dx_prep_exp.R")
      , threads              = 1L
      , mem                  = "1G"
      , runtime_min          = 30
      , partition            = "all.q,long.q"
      , account              = "proj_cov_vpd"
      , send_email           = FALSE
      , console_style_log_tf = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
}


# Use to compare input data for different runs of prep_exp. Will output
# 3 csvs to the outdir set in the config. The first merges the two files
# and compares the data column, sample size, and outliering. The second
# shows any new data, and the third shows any dropped data.
if (run_compare_data_versions) {
   message("Running GBD prep_exp data comparisons.")
   jobid_compare_data_versions <- se$submit_job(
      script_path            = file.path(code_root, "extraction/compare_data_versions.R")
      , threads              = 2L
      , mem                  = "10G"
      , runtime_min          = 60
      , partition            = "all.q,long.q"
      , account              = "proj_cov_vpd"
      , send_email           = FALSE
      , console_style_log_tf = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
}


# Use to compare the input data for GBD and LBD models. Outputs 3 csvs, the first
# has comparisons of data and sample size for data in both inputs, the second has
# a list of nid-me pairs present in GBD data but not LBD data, and the third is pairs
# present in LBD data but not GBD data. The second and third files also contain columns
# with additional checks to narrow down the issue.
if (run_compare_gbd_lbd_inputs) {
   message("Running GBD/LBD input data comparisons.")
   jobid_compare_gbd_lbd_inputs <- submit_job(
      script_path            = file.path(code_root, "extraction/compare_gbd_lbd_inputs.R")
      , threads              = 2L
      , mem_G                = "10G"
      , runtime_min          = 10
      , partition            = "all.q,long.q"
      , console_style_log_tf = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
}

# Use to compare data for different GBD vaccine lit extractions. Will output
# 3 csvs to the outdir set in the config. The first merges the two files
# and compares the data column, sample size, and outliering. The second
# shows any new data, and the third shows any dropped data.
if (run_compare_gbd_extraction_versions) {
   clean_pipeline_environment()
   message("Running comparison of GBD lit extraction versions.")
   source(file.path(code_root, "extraction/compare_extraction_versions.R"), echo = TRUE)
}











#----STEP 5------------------------------------------------------------------------------------------------------------

# 5 - Run Post-Processing on modeled estimates

if (run_save_results) {
   message("Running _save_results.")
   jobid_save_results <- se$submit_job(
      script_path            = file.path(code_root, "FILEPATH/_save_results.r")
      , threads              = 1L
      , mem                  = "6G"
      , runtime_min          = 60 * 24
      , account              = "proj_cov_vpd"
      , partition            = "all.q,long.q"
      , console_style_log_tf = TRUE
      , send_email           = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
   wait_on_slurm_job_id(jobid_save_results, break_on_failure = TRUE, cycle_sleep_sec = 300)
}






# Plot standard coverage vs. time plots for all locations with modeled
# estimates, administrative and survey data points.
if (run_plot_vaccines) {
   message("Running vaccine coverage plots.")
   job_id_plot_vaccines <- se$submit_job(
      script_path            = file.path(code_root, "FILEPATH/_plot_all_vaccines.r")
      , mem                  = "10G"
      , partition            = "all.q,long.q"
      , account              = "proj_cov_vpd"
      , threads              = 3
      , runtime_min          = 90
      , console_style_log_tf = TRUE
      , send_email           = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
}


# Plot diagnostics of post-processed estimates, current best vs. previous:
# - scatters
if(run_save_results_diagnostics){
   message("Running launch_dx_save_results.R.")
   jobid_save_results_diagnostics <- se$submit_job(
      script_path            = file.path(code_root, "FILEPATH/launch_dx_save_results.R")
      , threads              = 1L
      , mem                  = "1G"
      , runtime_min          = 20
      , partition            = "all.q,long.q"
      , account              = "proj_cov_vpd"
      , console_style_log_tf = TRUE
      , send_email           = FALSE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path)
   )
}

# aggregate to region/super region/global
if(run_global_aggregates){
   message("Running global aggregates.")
   n_cores <- 10L
   jobid_global_aggregates <- se$submit_job(
      script_path            = file.path(code_root, "FILEPATH/submit_global_coverage_aggregations.R")
      , threads              = n_cores
      , mem                  = "10G"
      , runtime_min          = 20
      , partition            = "all.q"
      , account              = "proj_cov_vpd"
      , console_style_log_tf = TRUE
      , send_email           = TRUE
      , args_list            = list(code_root     = code_root
                                    , config_path = config_path
                                    , n_cores     = n_cores)
   )
   wait_on_slurm_job_id(jobid_global_aggregates, break_on_failure = TRUE, cycle_sleep_sec = 90)
}


# Upload modeled estimates to IHME central database.
if (run_upload_results) {
   clean_pipeline_environment()
   message("Running _upload_results.")
   
   jobid_upload_results <- submit_job(
      script_path            = file.path(code_root, "FILEPATH/_upload_results.R")
      , threads              = 1L
      , mem_G                = "10G"
      , runtime_min          = 30
      , partition            = "all.q,long.q"
      , console_style_log_tf = TRUE
      , args_list            = list(code_root = code_root)
   )
}


steps_run <- names(Filter(isTRUE, mget(grep("^run_", ls(envir = globalenv()), value = TRUE))))
message("Done with pipeline steps: ", toString(steps_run))
message(format((proc.time() - start_time)[3]/60, digits=3), " minutes")
