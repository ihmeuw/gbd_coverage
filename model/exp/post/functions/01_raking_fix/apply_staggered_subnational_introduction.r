


























#' @title Apply staggered subnational introduction

#' @param save_root Model run directory to work out of
#' @param vacc_intro_path path to the correct vaccine intros file in to_model_dir
#' @param ndraws Number of draws to use

#' @param input_root [path] e.g. `to_model_dir` - used to find a date-version specific stgpr run log
#' @param year_start [int] e.g. 1980L
#' @param year_end [int] e.g. 2024L
apply_staggered_subnational_introduction <- function(
    save_root,
    vacc_intro_path,
    ndraws,
    results_root,
    input_root,
    year_start,
    year_end
)
{
  


  stopifnot(is.integer(year_start))
  stopifnot(is.integer(year_end))

  
  locs <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)[level >= 3, ]

  
  models_of_interest <- c(
    paste0(
      "vacc_",
      c(
        "mcv1",
        "dpt3_dpt1_ratio",
        "dpt1",
        "bcg",
        "polio3",
        "hib3_dpt3_ratio",
        "pcv3_dpt3_ratio",
        "rotac_dpt3_ratio",
        "rcv1_mcv1_ratio",
        "hepb3_dpt3_ratio",
        "mcv2_mcv1_ratio"
      )
    )
  )
  vaccine_ratio_dictionary <- c(
    "vacc_mcv1"   = "vacc_mcv1",
    "vacc_dpt1"   = "vacc_dpt1",
    "vacc_dpt3"   = "vacc_dpt3_dpt1_ratio",
    "vacc_bcg"    = "vacc_bcg",
    "vacc_polio3" = "vacc_polio3",
    "vacc_hepb3"  = "vacc_hepb3_dpt3_ratio",
    "vacc_rotac"  = "vacc_rotac_dpt3_ratio",
    "vacc_hib3"   = "vacc_hib3_dpt3_ratio",
    "vacc_mcv2"   = "vacc_mcv2_mcv1_ratio",
    "vacc_pcv3"   = "vacc_pcv3_dpt3_ratio",
    "vacc_rcv1"   = "vacc_rcv1_mcv1_ratio"
  )

  
  
  message("Reading intros from : ", vacc_intro_path, "\n")
  intros <- readRDS(vacc_intro_path)

  message("Identifying countries with staggered subnational introduction:\n")
  intros <- unique(intros[!grepl("ratio", me_name), .(ihme_loc_id, location_id, me_name, cv_intro)])
  intros[, country := unlist(substr(ihme_loc_id, 1, 3))]
  intros[, n_intros := length(unique(cv_intro)), by = c("country", "me_name")]
  country_staggered_intros <- unique(intros[n_intros > 1, .(me_name, country)])[order(country)]
  country_staggered_intros <- merge(country_staggered_intros, locs[, .(country = ihme_loc_id, location_id)], all.x = TRUE, by = c("country"))
  setnames(country_staggered_intros, "me_name", "me_name_short")
  for(me in unique(country_staggered_intros$me_name_short)) {
    country_staggered_intros[me_name_short == me, me_name := vaccine_ratio_dictionary[[me]]]
  }
  
  message(prt_multiline(country_staggered_intros), '\n')

  
  
  
  
  run_log <- read_stgpr_best_runs(root = input_root, model_type = "vaccine_coverage")
  model_runs <- run_log[me_name %in% models_of_interest, .(me_name, run_id)]
  for(me in unique(model_runs$me_name)) {
    model_runs[me_name == me, me_name_short := names(vaccine_ratio_dictionary)[unname(vaccine_ratio_dictionary) == me]]
  }

  
  vaccines_with_staggered_intros <- unique(country_staggered_intros$me_name)
  model_runs[me_name %in% vaccines_with_staggered_intros, staggered_intro := TRUE]
  model_runs[me_name %!in% vaccines_with_staggered_intros, staggered_intro := FALSE]

  
  
  model_runs$success <- unlist(lapply(model_runs$run_id, check_stgpr_model_success))

  
  
  
  
  
  unrake_tasks <- c()
  
  message("Successful runs")
  
  for(successful_run_id in model_runs[success == TRUE & staggered_intro == TRUE, run_id]) {

    message('\n', prt_multiline(model_runs[run_id == successful_run_id, .(me_name, me_name_short, run_id, success)]))
    
    
    me_name_full <- model_runs[run_id == successful_run_id, me_name]
    me_name_short <- model_runs[run_id == successful_run_id, me_name_short]
    me_outdir <- file.path(save_root, "01_unraked_draws_intros_applied", me_name_full)
    dir.create(me_outdir, recursive = TRUE, showWarnings = FALSE)

    
    
    unrake_tasks <- c(unrake_tasks,
                      write_message_to_workflow(message=paste0("-- Unrake and force to zero in years prior to intro: ", me_name_short)))

    
    unraked_results <- get_estimates(version_id = successful_run_id, entity = "gpr")
    raked_results   <- get_estimates(version_id = successful_run_id, entity = "final")

    
    raked_vs_unraked_results <- merge(raked_results[, .(year_id, location_id, raked = val)],
                                      unraked_results[, .(year_id, location_id, unraked = val)],
                                      all = TRUE, by = c("year_id", "location_id"))[order(location_id, year_id)]
    raked_vs_unraked_results <- cbind(raked_vs_unraked_results, model_runs[run_id == successful_run_id, .(me_name, run_id)])

    
    raked_vs_unraked_results[, k := logit(pmin(0.999, pmax(0.001, raked))) - logit(unraked)]

    
    country_me_staggered_intros <- country_staggered_intros[me_name == me_name_full, ]

    
    
    
    
    for(i in 1:nrow(country_me_staggered_intros)) {

      message('\n', paste0("-- ", country_me_staggered_intros[i, country]))
      unrake_tasks <- c(unrake_tasks,
                        write_message_to_workflow(message=paste0("---- ", country_me_staggered_intros[i, country])))

      
      subnational_country_in_loop  <- country_me_staggered_intros[i, country]
      subnationals                 <- locs[grepl(paste0(subnational_country_in_loop, "_"), ihme_loc_id), .(ihme_loc_id, location_id, level)]
      raked_vs_unraked_subnational <- raked_vs_unraked_results[location_id %in% subnationals$location_id, ]

      
      
      for(loc_id in unique(raked_vs_unraked_subnational$location_id)) {
         
        message(paste0("---- ", locs[location_id == loc_id, ihme_loc_id]))
        
        intro_year    <- intros[me_name == me_name_short & location_id == loc_id, cv_intro]
        raked_draws <- read_draws(run_id = successful_run_id, location_id = loc_id)

        
        raked_draws_with_k <- merge(raked_draws, raked_vs_unraked_subnational[location_id == loc_id, .(year_id, location_id, k)],
                                    all.x = TRUE, by = c("year_id", "location_id"))

        
        draw_names <- grep("draw_", names(raked_draws), value = TRUE)
        unraked_draws <- copy(raked_draws_with_k)
        unraked_draws[, (draw_names) := lapply(.SD, function(x) invlogit(logit(x) - k)), .SDcols = draw_names]

        
        unraked_draws$k <- NULL

        
        
        df_zeros <- copy(unraked_draws)
        df_zeros[, (draw_names) := 0]
        df_zeros <- df_zeros[year_id < intro_year]

        
        unraked_draws[, ysi := year_id - year_start]
        unraked_draws[, year_id := intro_year + ysi]
        unraked_draws <- unraked_draws[year_id <= year_end]
        unraked_draws[, ysi := NULL]

        unraked_draws <- rbind(df_zeros, unraked_draws)

        unraked_draws_intro_applied <- apply_draw_range_check(unraked_draws)
        
        out_path <- file.path(me_outdir, paste0(loc_id, ".csv"))
        
        fwrite(unraked_draws_intro_applied, file = out_path)
      }
    }
  }

  

  
  
  
  
  message("Failed runs")
  
  for(failed_run_id in model_runs[success == FALSE & staggered_intro == TRUE, run_id]) {

    
    me_name_full  <- model_runs[run_id == failed_run_id, me_name]
    me_name_short <- model_runs[run_id == failed_run_id, me_name_short]
    me_outdir <- file.path(save_root, "01_unraked_draws_intros_applied", me_name_full)
    if(!dir.exists(me_outdir)) {
      dir.create(me_outdir, recursive = TRUE)
    }

    
    
    unrake_tasks <- c(unrake_tasks,
                      write_message_to_workflow(message=paste0("-- Force unraked draws to zero in years prior to intro: ", me_name_short)))

    
    country_me_staggered_intros <- country_staggered_intros[me_name == me_name_full, ]

    
    for(i in 1:nrow(country_me_staggered_intros)) {

      message(paste0("---- ", country_me_staggered_intros[i, country]))
      unrake_tasks <- c(unrake_tasks,
                        write_message_to_workflow(message=paste0("---- ", country_me_staggered_intros[i, country])))

      
      subnational_country_in_loop  <- country_me_staggered_intros[i, country]
      subnationals                 <- locs[grepl(paste0(subnational_country_in_loop, "_"), ihme_loc_id), .(ihme_loc_id, location_id, level)]

      
      for(loc_id in unique(subnationals$location_id)) {
        message(paste0("------ ", locs[location_id == loc_id, ihme_loc_id]))
        intro_year    <- intros[me_name == me_name_short & location_id == loc_id, cv_intro]

        
        subtask <- jobmonr::task(
          task_template    = template_task_unrake_failed,
          cluster_name     = "slurm",
          compute_resources= resources_task_unrake_failed,
          name             = paste0('unrake_failed_task_', subnational_country_in_loop, '_', failed_run_id, '_', intro_year, '_', loc_id),
          nthreads         = format(resources_task_unrake_failed$cores, nsmall=0),
          r_shell          = r_shell,
          scriptname       = paste0(code_root, "FILEPATH/execute_staggered_subnat_intro_tasks.R"),
          code_root        = code_root,
          results_root     = results_root,
          run_id           = failed_run_id,
          location_id      = loc_id,
          intro_year       = intro_year,
          me_outdir        = me_outdir,
          output_file      = file.path(log_dir, "output", paste0('unrake_failed_task_', subnational_country_in_loop, '_', failed_run_id, '_', intro_year, '_', loc_id, ".out"))
        )
        unrake_tasks <- c(unrake_tasks, subtask)

      }
    }
  }


  

  
  
  
  
  
  rake_tasks <- c(write_message_to_workflow(message="Raking unraked draws, with staggered subnational rollout applied, to national"))
  for(run_id_in_loop in model_runs[staggered_intro == TRUE, run_id]) {

    
    me_name_full  <- model_runs[run_id == run_id_in_loop, me_name]
    me_name_short <- model_runs[run_id == run_id_in_loop, me_name_short]
    me_outdir <- file.path(save_root, "02_raked_draws_intros_applied", me_name_full)
    dir.create(me_outdir, recursive = TRUE, showWarnings = FALSE)

    
    
    rake_tasks <- c(rake_tasks,
                    write_message_to_workflow(message=paste0("-- ", me_name_short)))

    
    country_me_staggered_intros <- country_staggered_intros[me_name == me_name_full, ]

    
    draw_path <- file.path(save_root, "01_unraked_draws_intros_applied", me_name_full)

    
    for(i in 1:nrow(country_me_staggered_intros)) {

      
      subnational_country_in_loop  <- country_me_staggered_intros[i, country]
      subnationals                 <- locs[grepl(paste0(subnational_country_in_loop, "_"), ihme_loc_id), .(ihme_loc_id, location_id, level)]
      
      rake_tasks <- c(rake_tasks,
                      write_message_to_workflow(message=paste0("---- ", subnational_country_in_loop)))

      loc_ids <-  unique(subnationals$location_id)
      loc_ids_str <- paste(loc_ids, collapse=',')

      
      min_intro_year <- year_start 
      max_intro_year <- year_end

      
      subtask <- jobmonr::task(
        task_template    = template_task_rake_draws,
        cluster_name     = "slurm",
        compute_resources= resources_task_rake_draws,
        name             = paste0('rake_draws_task_', run_id_in_loop, '_', min_intro_year, '_', max_intro_year),
        nthreads         = format(resources_task_rake_draws$cores, nsmall=0),
        r_shell          = r_shell,
        scriptname       = paste0(code_root, "FILEPATH/execute_staggered_subnat_intro_tasks.R"),
        code_root        = code_root,
        results_root     = results_root,
        run_id           = run_id_in_loop,
        location_id      = loc_ids_str,
        max_intro_year   = max_intro_year,
        min_intro_year   = min_intro_year,
        draw_path        = draw_path,
        me_outdir        = me_outdir,
        output_file      = file.path(log_dir, "output", paste0('rake_draws_task_', run_id_in_loop, '_', min_intro_year, '_', max_intro_year, ".out"))
      )

      rake_tasks <- c(rake_tasks, subtask)
    }
  }

  
  
  
  move_tasks <- c()
  for(run_id_in_loop in model_runs$run_id) {

    
    me_name_full  <- model_runs[run_id == run_id_in_loop, me_name]
    me_name_short <- model_runs[run_id == run_id_in_loop, me_name_short]
    me_outdir <- file.path(save_root, "02_raked_draws_intros_applied", me_name_full)
    if(!dir.exists(me_outdir)) {
      dir.create(me_outdir, recursive = TRUE)
    }

    raked_successfully <- model_runs[run_id == run_id_in_loop, success]
    
    subtask <- jobmonr::task(
      task_template     = template_task_move_draws,
      cluster_name      = "slurm",
      compute_resources = resources_task_move_draws,
      name              = paste0('move_draws_task_', run_id_in_loop),
      nthreads          = format(resources_task_move_draws$cores, nsmall=0),
      r_shell           = r_shell,
      scriptname        = paste0(code_root, "FILEPATH/execute_staggered_subnat_intro_tasks.R"),
      code_root         = code_root,
      results_root      = results_root,
      run_id            = run_id_in_loop,
      raked_successfully= raked_successfully,
      me_outdir         = me_outdir,
      input_root        = input_root,
      output_file       = file.path(log_dir, "output", paste0('move_draws_task_', run_id_in_loop, ".out"))
    )

    move_tasks <- c(move_tasks, subtask)
  }
  
  
  ysi_to_years_tasks <- c()
  for(vaccine in c(
    'vacc_hib3_dpt3_ratio',
    'vacc_hepb3_dpt3_ratio',
    'vacc_pcv3_dpt3_ratio',
    'vacc_rotac_dpt3_ratio',
    'vacc_rcv1_mcv1_ratio',
    'vacc_mcv2_mcv1_ratio'
  )){

    ndraws    <- draws_count
    me_outdir <- file.path(save_root, "02_raked_draws_intros_applied", vaccine, '/')

    subtask <- jobmonr::task(
      task_template     = template_task_ysi_to_years,
      cluster_name      = "slurm",
      compute_resources = resources_task_ysi_to_years,
      name              = paste0('ysi_to_years_task_', vaccine),
      nthreads          = format(resources_task_ysi_to_years$cores, nsmall=0),
      r_shell           = r_shell,
      scriptname        = paste0(code_root, "FILEPATH/execute_staggered_subnat_intro_tasks.R"),
      code_root         = code_root,
      results_root      = results_root,
      vaccine           = vaccine,
      ndraws            = ndraws,
      year_start        = year_start,
      year_end          = year_end,
      me_outdir         = me_outdir,
      output_file       = file.path(log_dir, "output", paste0('ysi_to_years_task_', vaccine, ".out"))
    )

    ysi_to_years_tasks <- c(ysi_to_years_tasks, subtask)

  }


  

  
  
  
  return(list(
    jobmon_unrake_tasks       = unrake_tasks,
    jobmon_rake_tasks         = rake_tasks,
    jobmon_move_tasks         = move_tasks,
    jobmon_ysi_to_years_tasks = ysi_to_years_tasks
  ))
}
