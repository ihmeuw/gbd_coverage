










































r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
  message("Starting arg-parser.")
  se$parse_all_named_cli_args(required_args = list(code_root     = "character"
                                                   , config_path = "character"))
} else {
  if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
    code_root <- Sys.getenv()[['CODE_ROOT']]
  } else {
    code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
  }
  config_path <- file.path(code_root, "pipeline_config.yaml")
}
message("code_root : ", code_root)






library(data.table)
library(dplyr)
library(parallel)
library(readxl)
library(DBI)
library(magrittr)
library(arm)
library(vmTools, lib.loc = r_lib_team_dir) 



source(file.path(code_root, "init.r"))
source("FILEPATH/utility.r")
source(file.path(cc_r_lib_root, "get_location_metadata.R"))
source(file.path(cc_r_lib_root, "get_age_metadata.R"))
source(file.path(cc_r_lib_root, "get_sdg_indicator_component_index.R"))
source(file.path(cc_r_lib_root, "get_population.R"))
source(file.path(cc_r_lib_root, "get_draws.R"))
source(file.path(code_root, "FILEPATH/03_pch_and_si_as_function.R"))
source(file.path(code_root, "vaccination_pipeline_functions/submit_job.R"))

R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), recursive = FALSE, modifiedOnly = FALSE)
R.utils::sourceDirectory(file.path(code_root, "FILEPATH"), recursive = TRUE, modifiedOnly=FALSE)


core_repo  <- file.path('FILEPATH', Sys.info()[["user"]], 'lbd_core/mbg_central')
indic_repo <- file.path('FILEPATH', Sys.info()[["user"]], 'vaccine')

source("FILEPATH/gbd_logit_raking_function.R")
source(file.path(indic_repo,'functions/misc_vaccine_functions.R'))
source(file.path(code_root, "FILEPATH/helper_functions.R"))






source(file.path(code_root, "vaccination_pipeline_functions/load_pipeline_config.r"))
load_pipeline_config(step = "_save_results", config_path = config_path)






if(all(stages_to_run == "all")) stages_to_run <- stages_allowed

if(fhs_run_TF)              stages_to_run <- c(stages_to_run, stages_fhs_main)
if(fhs_run_TF & fhs_fix_TF) stages_to_run <- c(stages_to_run, stages_fhs_fixes)






if (is.null(date)) {
  stop("date must be set in the config file and match a successful prep_exp run")
}
run_date           <- paste0(date)  
input_root         <- file.path(to_model_root, run_date)
to_model_dir       <- file.path(to_model_root, run_date)
results_root       <- file.path(modeled_root, run_date)
save_root          <- file.path(draws_root_gbdxx, run_date)
adjusted_draw_root <- file.path(save_root, "02_raked_draws_intros_applied")

if(dir.exists(save_root)) stop(
  paste0(
    "`save_root` already exists, run `scrub_outputs$save_results() on ", run_date,
    "\n - save_root: ", save_root,
    "\n - Post-processing must be run from a clean slate, or the staggered subnational introduction section will inappropriately move draws."
  )
)
slt_post <- SLT$new(user_root_list = list(modeled_root = modeled_root), 
                    user_central_log_root = modeled_root)
slt_post$create_date_version_folders_with_logs(run_date); rm(slt_post)
dir.create(save_root,    recursive = TRUE, showWarnings = FALSE)

vacc_intro_path <- file.path(to_model_dir, "reference/vaccine_intro.rds")
vacc_intro_for_outro_path <- file.path(to_model_dir, "reference/vaccine_intro_for_outro.rds")







me_db         <- fread(file.path(ref_data_repo, "me_db.csv"))
model_run_log <- read_stgpr_best_runs(root = to_model_dir, model_type = "vaccine_coverage")
locations     <- get_location_metadata(location_set_id=location_set_id, release_id=release_id)
locs          <- locations[level >= 3, ]






metadata <- se$build_metadata_shell(code_root = code_root)
save_config(
  step            = "_save_results",
  config_path     = config_path,
  save_root       = results_root,
  copy_to_archive = TRUE,
  dir_archive     = file.path(modeled_root, "archive_configs"),
  run_date        = date,
  add_list        = list(
    metadata  = metadata,
    init_list = init_list
  )
)







if(fhs_run_TF){
  year_end_gbd        <- year_end
  year_end            <- fhs_year_end
  location_set_id_gbd <- location_set_id
  location_set_id     <- fhs_location_set_id
  release_id_gbd      <- release_id
  release_id          <- fhs_release_id
}












if(!is.null(stgpr_custom_env)){
  Sys.setenv("RETICULATE_PYTHON"=file.path("FILEPATH", stgpr_custom_env, "bin/python"))
} else {
  Sys.setenv("RETICULATE_PYTHON"="FILEPATH") 
}
source("FILEPATH/public.R") 
if("jobmonr" %in% (.packages())) detach("package:jobmonr", unload=TRUE) 
attachNamespace("jobmonr") 

tool = jobmonr::tool(name="Vaccines Post Processing Pipeline")

source(file.path(code_root, "FILEPATH/templates_staggered_subnat_intro.R"))
source(file.path(code_root, 'FILEPATH/templates_china_stock_adjustments.R'))
source(file.path(code_root, "FILEPATH/templates_general.R"))
if(fhs_run_TF) source(file.path(code_root, "FILEPATH/templates_fhs.R"))



workflow_run <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d_%H:%M")

log_dir <- paste0("FILEPATH", Sys.info()[["user"]], "FILEPATH", workflow_run)
dir.create(file.path(log_dir), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(log_dir, "output"), showWarnings = FALSE)
dir.create(file.path(log_dir, "errors"), showWarnings = FALSE)



jobmon_env_root <- file.path(results_root, "/jobmon_env")
if(!dir.exists(jobmon_env_root)) {
  
  dir.create(jobmon_env_root, recursive = FALSE)
}

workflow_args <- paste0("vacc_gbd_post_", date, "_", workflow_run)
message("workflow_args: ", workflow_args)
workflow <- jobmonr::workflow(tool = tool,
                              workflow_args = workflow_args,
                              name = "Vaccine Coverage GBD Post Processing")

jobmonr::set_default_workflow_resources(workflow=workflow,
                                        default_cluster_name = "slurm",
                                        resources=list("project"='proj_cov_vpd',
                                                       "constraints"="archive",
                                                       "working_dir"=getwd(),
                                                       "stdout"=paste0(log_dir, "/output"),
                                                       "stderr"=paste0(log_dir, "/errors")))







if(commit_run_log){ 
  git_commit_repo(repo_root        = ref_data_repo
                  , add_files      = "gbd_model/vaccination_run_log.csv"
                  , commit_message = "automated _save_results update - bumping gbd_model/vaccination_run_log.csv history"
                  , dryrun         = FALSE)
}


start_time <- Sys.time()







if("stage_staggered_intros" %in% stages_to_run){
  
  message("Running: stage_staggered_intros")
  tasks_staggered_subnationals <- apply_staggered_subnational_introduction(
    save_root       = save_root,
    vacc_intro_path = vacc_intro_for_outro_path,
    ndraws          = draws_count,
    year_start      = year_start,
    year_end        = year_end,
    results_root    = results_root,
    input_root      = input_root
  )
  
  
  unrake_tasks       <- tasks_staggered_subnationals$jobmon_unrake_tasks
  rake_tasks         <- tasks_staggered_subnationals$jobmon_rake_tasks
  move_tasks         <- tasks_staggered_subnationals$jobmon_move_tasks
  ysi_to_years_tasks <- tasks_staggered_subnationals$jobmon_ysi_to_years_tasks
}








if("stage_fhs_resample" %in% stages_to_run){
  
  message("Running: FHS - Resampling draws")
  
  
  me_names_modeled     <- model_run_log$me_name
  me_names_ignore      <- c("vacc_dpt3", "vacc_dpt12_cond")
  me_names_disk        <- list.dirs(adjusted_draw_root, full.names = FALSE, recursive = FALSE)
  me_names_to_resample <- setdiff(me_names_modeled, me_names_ignore)
  stopifnot(all(me_names_to_resample %in% me_names_disk))
  
  task_resample_fhs <- list()
  
  for(me in me_names_to_resample){
    task_resample_fhs[[me]] <- jobmonr::task(
      task_template     = template_task_fhs_resample,
      cluster_name      = "slurm",
      compute_resources = resources_fhs_resample,
      name              = paste0('fhs_resample_', me),
      nthreads          = format(resources_fhs_resample$cores, nsmall=0),
      r_shell           = r_shell,
      scriptname        = paste0(code_root, "FILEPATH/execute_fhs_tasks.R"),
      code_root         = code_root,
      results_root      = results_root,
      me                = me,
      draws_read_root   = adjusted_draw_root,
      draws_save_root   = adjusted_draw_root,
      fhs_path_dpt3     = fhs_path_dpt3,
      fhs_path_mcv1     = fhs_path_mcv1, 
      fhs_n_draws       = fhs_n_draws,
      n_draws_vc        = draws_count,
      output_file       = file.path(log_dir, "output", paste0('resample_fhs_task_', me, ".out"))
    )
  }
  
  message("\nResetting draws_count to fhs_n_draws for all subsequent tasks: ", fhs_n_draws, "\n")
  draws_count <- fhs_n_draws
  
}









if("stage_prep_direct" %in% stages_to_run){
  
  message("Running: stage_prep_direct")
  
  me_db <- fread(file.path(ref_data_repo, "me_db.csv"))
  
  tasks_prep_direct <- list()
  me_names_epi_intro <- paste(c(
      "vacc_bcg",
      "vacc_dpt1",
      "vacc_dpt3",
      "vacc_mcv1",
      "vacc_polio3"
  ), collapse = ",")
  
  
  
  
  for(me in c("vacc_mcv1", "vacc_bcg", "vacc_polio3", "vacc_dpt1", "vacc_dpt3_dpt1_ratio")){
    
    tasks_prep_direct[[me]] <- jobmonr::task(
      task_template      = template_task_prep_draws,
      cluster_name       = "slurm",
      compute_resources  = resources_task_prep_draws,
      name               = paste0('prep_draws_task_', me),
      nthreads           = format(resources_task_prep_draws$cores, nsmall=0),
      r_shell            = r_shell,
      scriptname         = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
      code_root          = code_root,
      results_root       = results_root,
      me                 = me,
      me_names_epi_intro = me_names_epi_intro,
      draw_root          = adjusted_draw_root,
      save_draws         = "TRUE",
      save_collapsed     = "TRUE",
      me_db              = file.path(ref_data_repo, "me_db.csv"),
      offset_remove      = manual_offset_remove,
      run_date           = run_date,
      output_file        = file.path(log_dir, "output", paste0('prep_draws_task_', me, ".out"))
    )
  }
  
}











if("stage_prep_dpt_ratio" %in% stages_to_run){
  
  message("Running: stage_prep_dpt_ratio")
  
  me <- 'vacc_dpt3_dpt1_ratio'
  task_prep_ratios_dpt <- list()
  task_prep_ratios_dpt[[1]] <- jobmonr::task(task_template     = template_task_prep_dpt_ratio,
                                             cluster_name      = "slurm",
                                             compute_resources = resources_task_prep_ratios,
                                             name              = paste0('prep_dpt_ratio_task_', me),
                                             nthreads          = format(resources_task_prep_ratios$cores, nsmall=0),
                                             r_shell           = r_shell,
                                             scriptname        = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                                             code_root         = code_root,
                                             results_root      = results_root,
                                             me                = me,
                                             draw_root         = adjusted_draw_root,
                                             save_draws        = "TRUE",
                                             save_collapsed    = "TRUE",
                                             run_date          = run_date,
                                             save_ratio_draws  = "TRUE",
                                             offset_remove     = manual_offset_remove,
                                             stockout_ratios   = stockout_ratios,
                                             output_file       = file.path(log_dir, "output", paste0('prep_ratios_task_', me, ".out")))
}







if("stage_fhs_straight" %in% stages_to_run){
  
  message("Running: FHS - Appending direct model forecasts to VC best results")
  
  
  
  tasks_fhs_straight <- list()
  
  
  tasks_fhs_straight[["dpt3_and_mcv1"]] <- jobmonr::task(
    task_template       = template_task_fhs_straight,
    cluster_name        = "slurm",
    compute_resources   = resources_fhs_straight,
    name                = paste0('fhs_straight_', me),
    nthreads            = format(resources_fhs_straight$cores, nsmall=0),
    r_shell             = r_shell,
    scriptname          = file.path(code_root, "FILEPATH/execute_fhs_tasks.R"),
    code_root           = code_root,
    results_root        = results_root,
    run_date            = run_date,
    year_end_gbd        = fhs_year_end_gbd,
    draws_read_root     = file.path(draws_root_gbdxx, fhs_run_date_gbd_best), 
    draws_save_root     = save_root, 
    fhs_path_dpt3       = fhs_path_dpt3, 
    fhs_path_mcv1       = fhs_path_mcv1, 
    fhs_release_id      = fhs_release_id,
    fhs_location_set_id = fhs_location_set_id,
    release_id_gbd      = release_id_gbd,
    location_set_id_gbd = location_set_id_gbd,
    n_cores             = format(resources_fhs_straight$cores, nsmall=0),
    output_file         = file.path(log_dir, "output", paste0('fhs_straight_task.out'))
  )
  
  
}










if("stage_fhs_fixes" %in% stages_to_run){
  
  message("Running: FHS - applying hierarchy fixes")
  
  tasks_fhs_fixes <- list()
  
  tasks_fhs_fixes[["dpt3_and_mcv1"]] <- jobmonr::task(
    task_template       = template_task_fhs_fixes,
    cluster_name        = "slurm",
    compute_resources   = resources_fhs_fixes,
    name                = paste0('fhs_ratio_hierarchy_fix_', me),
    nthreads            = format(resources_fhs_fixes$cores, nsmall=0),
    r_shell             = r_shell,
    scriptname          = file.path(code_root, "FILEPATH/execute_fhs_tasks.R"),
    code_root           = code_root,
    results_root        = results_root,
    draws_read_root     = save_root, 
    draws_save_root     = save_root, 
    run_date            = run_date,
    year_end_gbd        = fhs_year_end_gbd,
    fhs_release_id      = fhs_release_id,
    fhs_location_set_id = fhs_location_set_id,
    release_id_gbd      = release_id_gbd,
    location_set_id_gbd = location_set_id_gbd,
    output_file         = file.path(log_dir, "output", paste0('fhs_fixes_task.out'))
  )
  
}




















stock_vaccines   <- data.table(vaccine       = c("rotac", "hib3", "pcv3"),
                               vaccine_ratio = c("vacc_rotac_dpt3_ratio",
                                                 "vacc_hib3_dpt3_ratio",
                                                 "vacc_pcv3_dpt3_ratio"))


stock_limits_applied <- FALSE

if ("stage_china_cap" %in% stages_to_run) {
  
  
  source(file.path(code_root, "FILEPATH/chn_cap.R"))
  
  commondir    <- sprintf('FILEPATH')
  package_list <- c(t(read.csv(sprintf('%s/package_list.csv', commondir), header = FALSE)))
  package_list <- package_list[package_list!= "tictoc"]
  invisible(lapply(package_list, library, character.only=TRUE, quietly=TRUE))
  for (i in (list.files(path = core_repo, pattern = "functions.R"))) {
    suppressMessages(suppressWarnings(source(file.path(core_repo, i))))
  }
  suppressMessages(invisible(library(
    lbd.loader,
    lib.loc = sprintf(
      "FILEPATH",
      R.version$major,
      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]
    )
  )))
  suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))
  
  
  stock_limits_applied <- TRUE
  
  message("Running: stage_china_cap")
  confirmation_task <- list()
  tasks_china_stock_adj <- apply_china_stock_adjustment(
    save_root         = save_root,
    results_root      = results_root,
    run_date          = run_date,
    special_draw_root = adjusted_draw_root,
    year_start        = year_start,
    year_end          = year_end,
    stock_vaccines    = stock_vaccines, 
    fhs_run_TF        = fhs_run_TF,
    location_set_id   = location_set_id,
    release_id        = release_id,
    locations         = locations
  )
  
  china_stock_tasks <- tasks_china_stock_adj$main_tasks
  confirmation_task <- tasks_china_stock_adj$confirmation_task
}









if("stage_prep_ratios" %in% stages_to_run){
  
  message("Running: stage_prep_ratios")
  
  
  ratio_grid_stock_adjusted <- data.table(me_name = stock_vaccines$vaccine_ratio,
                                          draw_read_root = ifelse(stock_limits_applied, save_root, adjusted_draw_root))
  ratio_grid_not_stock_adjusted <- data.table(me_name = c("vacc_mcv2_mcv1_ratio",
                                                          "vacc_hepb3_dpt3_ratio",
                                                          "vacc_rcv1_mcv1_ratio"),
                                              draw_read_root = adjusted_draw_root)
  ratio_grid <- rbind(ratio_grid_stock_adjusted, ratio_grid_not_stock_adjusted)
  
  ratios_to_prep <- c("vacc_mcv2_mcv1_ratio",
                      "vacc_hib3_dpt3_ratio",
                      "vacc_rotac_dpt3_ratio",
                      "vacc_rcv1_mcv1_ratio",
                      "vacc_pcv3_dpt3_ratio",
                      "vacc_hepb3_dpt3_ratio")
  
  stopifnot(all(ratios_to_prep %in% ratio_grid$me_name))
  
  tasks_prep_ratios <- list()
  
  for (me in ratios_to_prep){
    
    tasks_prep_ratios[[me]] <- jobmonr::task(task_template     = template_task_prep_ratios,
                                             cluster_name      = "slurm",
                                             compute_resources = resources_task_prep_ratios,
                                             name              = paste0('prep_ratios_task_', me),
                                             nthreads          = format(resources_task_prep_ratios$cores, nsmall=0),
                                             r_shell           = r_shell,
                                             scriptname        = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                                             code_root         = code_root,
                                             results_root      = results_root,
                                             me                = me,
                                             draw_root         = ratio_grid[me_name == me, draw_read_root],
                                             save_draws        = "TRUE",
                                             save_collapsed    = "TRUE",
                                             run_date          = run_date,
                                             save_ratio_draws  = "TRUE",
                                             offset_remove     = manual_offset_remove,
                                             stockout_ratios   = stockout_ratios,
                                             output_file       = file.path(log_dir, "output", paste0('prep_ratios_task_', me, ".out")))
  }
}








if("stage_covid_direct" %in% stages_to_run){
  
  message("Running: stage_covid_direct")
  
  tasks_covid_direct <- list()
  
  for(me in c("vacc_dpt1", "vacc_mcv1", "vacc_bcg", "vacc_polio3")){
    tasks_covid_direct[[me]] <- jobmonr::task(task_template     = template_task_covid_adjustment,
                                              cluster_name      = "slurm",
                                              compute_resources = resources_covid_adjustment,
                                              name              = paste0('covid_adjustment_task_', me),
                                              nthreads          = format(resources_covid_adjustment$cores, nsmall=0),
                                              r_shell           = r_shell,
                                              scriptname        = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                                              code_root         = code_root,
                                              input_root        = input_root,
                                              me                = me,
                                              results_root      = results_root,
                                              offset_remove     = manual_offset_remove,
                                              stockout_ratios   = stockout_ratios,
                                              run_date          = run_date,
                                              output_file       = file.path(log_dir, "output", paste0('covid_adjustment_task_', me, ".out")),
                                              ndraws            = draws_count)
  }
}








if("stage_covid_ratios" %in% stages_to_run){
  
  message("Running: stage_covid_ratios")
  
  
  me <- 'vacc_dpt3_dpt1_ratio'
  task_covid_ratios_dpt <- list()
  task_covid_ratios_dpt[[1]] <- jobmonr::task(task_template       = template_task_covid_adj_dpt_ratios,
                                              cluster_name        = "slurm",
                                              compute_resources   = resources_covid_adj_ratios,
                                              name                = paste0('covid_adj_dpt_ratios_task_', me),
                                              nthreads            = format(resources_covid_adj_ratios$cores, nsmall=0),
                                              r_shell             = r_shell,
                                              scriptname          = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                                              code_root           = code_root,
                                              input_root          = input_root,
                                              results_root        = results_root,
                                              me                  = me,
                                              save_collapsed      = "TRUE",
                                              run_date            = run_date,
                                              offset_remove       = manual_offset_remove,
                                              output_file         = file.path(log_dir, "output", paste0('covid_adj_dpt_ratios_task_', me, ".out")),
                                              ndraws              = draws_count,
                                              s1_stockout_ratios  = FALSE,
                                              s1_cascade_run_date = NULL)
  tasks_covid_ratios <- list()
  
  ratios_to_covid_adjust <- c("vacc_mcv2_mcv1_ratio",
                              "vacc_hib3_dpt3_ratio",
                              "vacc_rotac_dpt3_ratio",
                              "vacc_rcv1_mcv1_ratio",
                              "vacc_pcv3_dpt3_ratio",
                              "vacc_hepb3_dpt3_ratio")
  
  for (me in ratios_to_covid_adjust){
    tasks_covid_ratios[[me]] <- jobmonr::task(task_template       = template_task_covid_adj_ratios,
                                              cluster_name        = "slurm",
                                              compute_resources   = resources_covid_adj_ratios,
                                              name                = paste0('covid_adj_ratios_task_', me),
                                              nthreads            = format(resources_covid_adj_ratios$cores, nsmall=0),
                                              r_shell             = r_shell,
                                              scriptname          = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                                              code_root           = code_root,
                                              input_root          = input_root,
                                              results_root        = results_root,
                                              me                  = me,
                                              save_collapsed      = "TRUE",
                                              run_date            = run_date,
                                              offset_remove       = manual_offset_remove,
                                              output_file         = file.path(log_dir, "output", paste0('covid_adj_ratios_task_', me, ".out")),
                                              ndraws              = draws_count,
                                              s1_stockout_ratios  = s1_stockout_ratios,
                                              s1_cascade_run_date = s1_cascade_run_date)
  }
}













if("stage_true_zeros" %in% stages_to_run){
  
  message("Running: stage_true_zeros")
  
  prepped_model_input <- readRDS(file.path(to_model_dir, "vaccination.rds"))

  
  true_zeros <- prepped_model_input[nid == 203321 & data < .01 & !grepl("cond|ratio", me_name),
                                    .(ihme_loc_id, location_id, year_id, me_name, nid, data, drop = 0)][order(ihme_loc_id, me_name, year_id),]
  true_zeros[ihme_loc_id == "ASM" & me_name == "vacc_hepb3" & year_id == 2007, drop := 1]
  true_zeros <- rbind(true_zeros, data.table(ihme_loc_id = 'IRQ', location_id = 143, year_id = 2022, me_name = 'vacc_pcv3', nid = NA, data = NA, drop = 0))
  true_zeros <- rbind(true_zeros, expand.grid(ihme_loc_id = "VEN", location_id = 133, year_id = 2018:2023, me_name = c('vacc_pcv3','vacc_rotac'), nid = NA, data = NA, drop = 0))

  
  true_zeros[, denom_vax := me_name]
  true_zeros[me_name %in% c('vacc_hib3', 'vacc_pcv3', 'vacc_hepb3', 'vacc_rotac'), denom_vax := 'vacc_dpt3']
  true_zeros[me_name %in% c('vacc_mcv2', 'vacc_rcv1'), denom_vax := 'vacc_mcv1']
  true_zeros[me_name %in% c('vacc_dpt3'), denom_vax := 'vacc_dpt1']
  
  prepped_model_input <- prepped_model_input[me_name %in% unique(true_zeros$denom_vax), 
                                             .(denom_vax = me_name, ihme_loc_id, location_id, year_id, nid, cv_outlier)]
  true_zeros <- merge(
    x     = true_zeros, 
    y     = prepped_model_input, 
    by    = c('denom_vax', 'ihme_loc_id', 'location_id', 'year_id', 'nid'), 
    all.x = TRUE
  )
  true_zeros[!is.na(cv_outlier), drop := 1]

  true_zeros$cv_outlier <- NULL
  true_zeros$denom_vax <- NULL

  
  subzero <- rbindlist(lapply(
    unique(true_zeros[location_id %in% locs$parent_id]$me_name),
    function(me_name_parent){
      
      subzero <- rbindlist(lapply(
        
        unique(true_zeros[me_name == me_name_parent & location_id %in% locs$parent_id]$location_id), 
        function(loc_id_parent){
          subzero <- expand.grid(location_id = locs[parent_id == loc_id_parent]$location_id, 
                                 year_id     = true_zeros[me_name == me_name_parent & location_id == loc_id_parent]$year_id,
                                 me_name     = me_name_parent, 
                                 nid         = NA, 
                                 data        = NA, 
                                 drop        = 0)
        }
      ))
      
    }
  ))
  subzero[, me_name := defactor_vector(me_name)] 
  subzero    <- merge(subzero, locs[, list(location_id, ihme_loc_id)], by = 'location_id')
  true_zeros <- rbind(true_zeros, subzero)
  
  
  true_zeros <- true_zeros[drop == 0, ]
  true_zeros$drop <- NULL

  
  tasks_true_zeros <- list()
  for(me in unique(true_zeros$me_name)) {
    collapsed_estimate_path_root <- file.path("FILEPATH", gbd_cycle, run_date, me)
    for(type in c("_covidfree", "_covidimputed")) {
      message(paste(me, type))
      
      subtask <- jobmonr::task(task_template                = template_task_force_zero,
                               cluster_name                 = "slurm",
                               compute_resources            = resources_task_force_zero,
                               name                         = paste0('task_force_zero_', me, type),
                               nthreads                     = format(resources_task_force_zero$cores, nsmall=0),
                               r_shell                      = r_shell,
                               scriptname                   = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                               code_root                    = code_root,
                               results_root                 = results_root,
                               me                           = me,
                               type                         = type,
                               collapsed_estimate_path_root = collapsed_estimate_path_root,
                               output_file                  = file.path(log_dir, "output", paste0('task_force_zero_', me, type, ".out")))
      
      tasks_true_zeros <- c(tasks_true_zeros, subtask)
      
    }
  }
}







if("stage_fhs_ratio" %in% stages_to_run){
  
  message("Running: FHS - vaccine ratio calculations")
  
  mes_fhs_ratio <- c(
    "vacc_mcv2_mcv1_ratio"
    , "vacc_rotac_dpt3_ratio"
    , "vacc_hib3_dpt3_ratio"
    , "vacc_pcv3_dpt3_ratio"
  )
  
  for(me in mes_fhs_ratio) {
    
    tasks_fhs_ratio[[me]] <- jobmonr::task(
      task_template       = template_task_fhs_ratio,
      cluster_name        = "slurm",
      compute_resources   = resources_fhs_ratio,
      name                = paste0('fhs_ratio_', me),
      nthreads            = format(resources_fhs_ratio$cores, nsmall=0),
      r_shell             = r_shell,
      scriptname          = paste0(code_root, "FILEPATH/execute_fhs_tasks.R"),
      me_ratio            = me,
      run_date            = run_date,
      output_file         = file.path(log_dir, "output", paste0('fhs_ratio_task_', me, ".out"))
    )
  }
}
























message("Building task dependencies")

tasks <- list()

if("stage_fhs_ratio" %in% stages_to_run){
  for(task_name in names(tasks_fhs_ratio)) lapply(tasks, tasks_fhs_ratio[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(tasks_fhs_ratio)))
}

if("stage_true_zeros" %in% stages_to_run) tasks <- c(tasks, rev(tasks_true_zeros))

if("stage_covid_ratios" %in% stages_to_run){
  for(task_name in names(tasks_covid_ratios)) lapply(tasks, tasks_covid_ratios[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(tasks_covid_ratios)))
  
  lapply(tasks,  task_covid_ratios_dpt[[1]]$add_downstream)
  tasks <- c(tasks,  task_covid_ratios_dpt)
}

if("stage_covid_direct" %in% stages_to_run) {
  for(task_name in names(tasks_covid_direct)) lapply(tasks, tasks_covid_direct[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(tasks_covid_direct)))
}

if("stage_prep_ratios" %in% stages_to_run){
  for(task_name in names(tasks_prep_ratios)) lapply(tasks, tasks_prep_ratios[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(tasks_prep_ratios)))
}

if("stage_china_cap" %in% stages_to_run){
  
  lapply(tasks, confirmation_task[[1]]$add_downstream)
  for(task in china_stock_tasks) lapply(tasks, task$add_downstream)
  
  
  tasks <- c(tasks, confirmation_task, rev(china_stock_tasks)) 
}

if("stage_fhs_fixes" %in% stages_to_run){
for(task_name in names(tasks_fhs_fixes)) lapply(tasks, tasks_fhs_fixes[[task_name]]$add_downstream)
tasks <- c(tasks, rev(unname(tasks_fhs_fixes)))
}

if("stage_fhs_straight" %in% stages_to_run){
  for(task_name in names(tasks_fhs_straight)) lapply(tasks, tasks_fhs_straight[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(tasks_fhs_straight)))
}

if("stage_prep_dpt_ratio" %in% stages_to_run){
  lapply(tasks, task_prep_ratios_dpt[[1]]$add_downstream)
  tasks <- c(tasks, task_prep_ratios_dpt)
}

if("stage_prep_direct" %in% stages_to_run){
  for(task_name in names(tasks_prep_direct)) lapply(tasks, tasks_prep_direct[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(tasks_prep_direct)))
}

if("stage_fhs_resample" %in% stages_to_run){
  for(task_name in names(task_resample_fhs)) lapply(tasks, task_resample_fhs[[task_name]]$add_downstream)
  tasks <- c(tasks, rev(unname(task_resample_fhs)))
}

if("stage_staggered_intros" %in% stages_to_run){
  for(task in unrake_tasks)       lapply(rake_tasks, task$add_downstream)
  for(task in rake_tasks)         lapply(move_tasks, task$add_downstream)
  for(task in move_tasks)         lapply(ysi_to_years_tasks, task$add_downstream)
  for(task in ysi_to_years_tasks) lapply(tasks, task$add_downstream)

  tasks <- c(tasks, rev(ysi_to_years_tasks), rev(move_tasks), rev(rake_tasks), rev(unrake_tasks))
}








me_db <- fread(file.path(ref_data_repo, "me_db.csv"))

message("Running workflow. This will take a long time...")
message("Logs for jobmon jobs can be found at ", log_dir)

workflow <- jobmonr::add_tasks(workflow, tasks)


run_time <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%H:%M:%S")


jobmon_env_path  <- file.path(jobmon_env_root, "tmp_env_jobmon.RData")
save.image(file = jobmon_env_path)

if(run_jobmon_workflow){
  invisible(capture.output(status <- jobmonr::run(workflow = workflow,
                                                  resume = FALSE,
                                                  seconds_until_timeout = 86400)))
   
   
  
  
  if(is.null(delete_jobmon_temp_env)){
    delete_jobmon_temp_env <- ifelse(status == "D", TRUE, FALSE)
  }
  
  
  tidy_after_jobmon(
    workflow            = workflow
    , start_time        = start_time
    , tasks             = tasks
    , log_dir           = log_dir
    , status            = status
    , jobmon_env_root   = jobmon_env_root
    , delete_empty_logs = FALSE
    , delete_temp_env   = delete_jobmon_temp_env 
  ) 

  if (status != "D"){
    stop("The workflow failed. Total runtime = ", Sys.time() - start_time)
  } else {
    message(paste0("Workflow Completed! Total runtime = ", Sys.time() - start_time))
  }
}








if(submit_plot_job){ 
  submit_job(
    script_path   = file.path(code_root, "FILEPATH/_plot_all_vaccines.r")
    , job_name    = "plot_gpr_output_vax"
    , mem_G       = "10G"
    , threads     = 3
    , runtime_min = 150
    , args_list   = list(code_root = code_root)
  )
}





