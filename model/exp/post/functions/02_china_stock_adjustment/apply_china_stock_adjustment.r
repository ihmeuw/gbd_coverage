#' @title Apply China stock adjustment
#' @description
























#' @param save_root Path used to save prepped draws
#' @param run_date Date used to save prepped draws
#' @param special_draw_root Pull draws from `special_draw_root` to incorporate properly raked subnational intros?
#' @param year_start start year, for raking
#' @param results_root [path] e.g. 'modeled' output root
#' @param stock_vaccines [data.table] grid of stock-limited vaccines e.g. data.table(vaccine       = c("rotac", "hib3", "pcv3"),



#' @param year_end [int] end year, for raking
#' @param locations [data.table] location hierarchy 
#' @param fhs_run_TF [lgl] TRUE if running FHS, FALSE otherwise
#' @param location_set_id [int] location set id
#' @param release_id [int] release id


#' @return list of jobmon tasks
apply_china_stock_adjustment <- function(
    save_root,
    results_root,
    run_date,
    special_draw_root,
    year_start,
    year_end,
    stock_vaccines,
    fhs_run_TF,
    location_set_id,
    release_id,
    locations
) {

  stopifnot(all(c("vaccine", "vaccine_ratio") %in% names(stock_vaccines)))
  

  
  
  
  
  

  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  


  

  confirmation_file <- file.path(save_root, "china_stock_adjustments_applied.txt")

  if(file.exists(confirmation_file)) {
    message("China stock data adjustments have already been applied. Skipping function.")
    return(list(main_tasks=list(),
                confirmation_task=list()))
  }

  

  
  
  
  

  
  
  


  
  
  tasks            <- list()
  last_stage_tasks <- list()
  stock_vaccines   <- data.table(vaccine     = c("rotac", "hib3", "pcv3"),
                               vaccine_ratio = c("vacc_rotac_dpt3_ratio",
                                                 "vacc_hib3_dpt3_ratio",
                                                 "vacc_pcv3_dpt3_ratio"))
  for(i in 1:nrow(stock_vaccines)) {

    vaccine       <- stock_vaccines[i, vaccine]
    vaccine_ratio <- stock_vaccines[i, vaccine_ratio]

    
    
    ratio_task <- jobmonr::task(task_template     = template_task_prep_ratios,
                                cluster_name      = "slurm",
                                compute_resources = resources_task_prep_ratios,
                                name              = paste0('prep_ratios_task_china_', vaccine_ratio),
                                nthreads          = format(resources_task_prep_ratios$cores, nsmall=0),
                                r_shell           = r_shell,
                                scriptname        = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
                                code_root         = code_root,
                                results_root      = results_root,
                                me                = vaccine_ratio,
                                draw_root         = special_draw_root,
                                save_draws        = "TRUE",
                                save_collapsed    = "TRUE",
                                run_date          = run_date,
                                save_ratio_draws  = "TRUE",
                                offset_remove     = manual_offset_remove,
                                stockout_ratios   = stockout_ratios,
                                output_file       = file.path(log_dir, "output", paste0('prep_ratios_task_china_', vaccine_ratio, ".out")))

    
    loc_id  <- 6

    
    
    stock_adjustment_applied <- file.exists(paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", loc_id, "_orig.csv"))
    if(!stock_adjustment_applied) {
      apply_stock_task <- jobmonr::task(task_template     = template_task_apply_china_stock,
                                        cluster_name      = "slurm",
                                        compute_resources = resources_task_apply_china_stock,
                                        name              = paste0('apply_china_stock_task_', vaccine),
                                        nthreads          = format(resources_task_apply_china_stock$cores, nsmall=0),
                                        r_shell           = r_shell,
                                        scriptname        = paste0(code_root, "FILEPATH/execute_china_stock_adjustment_tasks.R"),
                                        code_root         = code_root,
                                        results_root      = results_root,
                                        gbd_cycle         = gbd_cycle,
                                        vaccine           = vaccine,
                                        run_date          = run_date,
                                        location_id       = loc_id,
                                        year_end          = year_end,
                                        output_file       = file.path(log_dir, "output", paste0('apply_china_stock_task_', vaccine, ".out")),
                                        upstream_tasks    = c(ratio_task))
      
    } else {
      if(stock_adjustment_applied) {
        
        last_stage_tasks <- c(last_stage_tasks, ratio_task)
        break
      }
    }

    
    
    rake_to_stock_adjusted_task <- jobmonr::task(task_template     = template_task_rake_to_stock_adjusted,
                                                 cluster_name      = "slurm",
                                                 compute_resources = resources_task_rake_to_stock_adjusted,
                                                 name              = paste0('rake_to_stock_adjusted_task_', vaccine),
                                                 nthreads          = format(resources_task_rake_to_stock_adjusted$cores, nsmall=0),
                                                 r_shell           = r_shell,
                                                 scriptname        = paste0(code_root, "FILEPATH/execute_china_stock_adjustment_tasks.R"),
                                                 code_root         = code_root,
                                                 results_root      = results_root,
                                                 gbd_cycle         = gbd_cycle,
                                                 vaccine           = vaccine,
                                                 run_date          = run_date,
                                                 year_end          = year_end,
                                                 fhs_run_TF        = fhs_run_TF, 
                                                 location_set_id   = location_set_id,
                                                 release_id        = release_id,
                                                 output_file       = file.path(log_dir, "output", paste0('rake_to_stock_adjusted_task_', vaccine, ".out")),
                                                 upstream_tasks    = c(apply_stock_task))

    

    
    china_loc_ids <- se$children_of_parents(parent_loc_ids = 6, hierarchy = locations, include_parent = TRUE, output = "loc_ids")
    

    

    
    ratio_back_calc_tasks <- list()
    for(loc_id in china_loc_ids){
      ratio_back_calc_tasks <- c(ratio_back_calc_tasks, 
                                 jobmonr::task(task_template     = template_task_back_calculate_ratio_draws,
                                               cluster_name      = "slurm",
                                               compute_resources = resources_task_back_calculate_ratio_draws,
                                               name              = paste0('back_calculate_ratio_draws_task_', vaccine, '_', loc_id),
                                               nthreads          = format(resources_task_back_calculate_ratio_draws$cores, nsmall=0),
                                               r_shell           = r_shell,
                                               scriptname        = paste0(code_root, "FILEPATH/execute_china_stock_adjustment_tasks.R"),
                                               code_root         = code_root,
                                               results_root      = results_root,
                                               vaccine           = vaccine,
                                               vaccine_ratio     = vaccine_ratio,
                                               run_date          = run_date,
                                               location_id       = loc_id,
                                               output_file       = file.path(log_dir, "output", paste0('back_calculate_ratio_draws_task_', vaccine, '_', loc_id, ".out")),
                                               upstream_tasks    = c(rake_to_stock_adjusted_task))
      )
    }
    
    
    
    delete_numerator_draws_task <- jobmonr::task(task_template     = template_task_delete_numerator_draws,
                                                 cluster_name      = "slurm",
                                                 compute_resources = resources_task_delete_numerator_draws,
                                                 name              = paste0('delete_numerator_draws_task_', vaccine),
                                                 nthreads          = format(resources_task_delete_numerator_draws$cores, nsmall=0),
                                                 r_shell           = r_shell,
                                                 scriptname        = paste0(code_root, "FILEPATH/execute_china_stock_adjustment_tasks.R"),
                                                 code_root         = code_root,
                                                 results_root      = results_root,
                                                 gbd_cycle         = gbd_cycle,
                                                 run_date          = run_date,
                                                 vaccine           = vaccine,
                                                 output_file       = file.path(log_dir, "output", paste0('delete_numerator_draws_task_', vaccine, ".out")),
                                                 upstream_tasks    = ratio_back_calc_tasks)

    tasks            <- c(tasks, ratio_task, apply_stock_task, rake_to_stock_adjusted_task, ratio_back_calc_tasks)
    last_stage_tasks <- c(last_stage_tasks, delete_numerator_draws_task)
  }

  
  
  save_confirmation_task <- jobmonr::task(task_template     = template_task_save_confirmation,
                                          cluster_name      = "slurm",
                                          compute_resources = resources_task_save_confirmation,
                                          name              = paste0('save_confirmation_task'),
                                          nthreads          = format(resources_task_save_confirmation$cores, nsmall=0),
                                          r_shell           = r_shell,
                                          scriptname        = paste0(code_root, "FILEPATH/execute_china_stock_adjustment_tasks.R"),
                                          code_root         = code_root,
                                          results_root      = results_root,
                                          confirmation_file = confirmation_file,
                                          output_file       = file.path(log_dir, "output", paste0("save_confirmation_task.out")),
                                          upstream_tasks    = last_stage_tasks)

  return(list(main_tasks        = c(tasks, last_stage_tasks),
              confirmation_task = c(save_confirmation_task)))
}
