#' @title Move draws task

#' @description Part 4/4 of the 01 raking task in the post pipeline.





#' @param run_id Model run_id
#' @param raked_successfully T/F, was the model run raked



#' @param me_outdir directory to save outputs (typically model run date folder)
#' @param stgpr_run_log_root [chr] e.g. to_model_dir - where is the date_version specific stgpr run log saved? 

#' @return NULL

#' @concept 01_post_raking
move_draws_task <- function(run_id,
                            raked_successfully,
                            me_outdir,
                            stgpr_run_log_root
) {
  
  
  draws <- read_draws(run_id, raked = raked_successfully)

  
  
  staggered_intro_locs <- gsub(".csv", "", list.files(me_outdir)) %>% as.integer()
  draws_locs_with_non_staggered_intros <- draws[location_id %!in% staggered_intro_locs, ]

  
  if(!raked_successfully){
    print('Need to manually rake, is going to take a very long time')
    locs1 <- locs[, list(location_id, parent_id, level, ihme_loc_id = substr(ihme_loc_id, 1, 3))]

    draws_locs_with_non_staggered_intros <- merge(draws_locs_with_non_staggered_intros, locs1[, list(location_id, parent_id, level, ihme_loc_id)], by = 'location_id')

    
    run_log <- read_stgpr_best_runs(root = stgpr_run_log_root, model_type = "vaccine_coverage")
    r <- run_id
    m <- run_log[run_id==r]$me_name

    
    if(m %in% c('vacc_hib3_dpt3_ratio','vacc_pcv3_dpt3_ratio','vacc_rotac_dpt3_ratio')){
      natl_draws_locs_with_non_staggered_intros <- draws_locs_with_non_staggered_intros[level <= 3 | ihme_loc_id=='CHN']
    }else{
      natl_draws_locs_with_non_staggered_intros <- draws_locs_with_non_staggered_intros[level <= 3]
    }
    natl_draws_locs_with_non_staggered_intros[, level := NULL]
    natl_draws_locs_with_non_staggered_intros[, parent_id := NULL]
    natl_draws_locs_with_non_staggered_intros[, ihme_loc_id := NULL]
    save_draws_01(natl_draws_locs_with_non_staggered_intros, root = me_outdir)

    
    if(m %in% c('vacc_hib3_dpt3_ratio','vacc_pcv3_dpt3_ratio','vacc_rotac_dpt3_ratio')){
      subnat_draws_locs_with_non_staggered_intros <- draws_locs_with_non_staggered_intros[level > 3 & ihme_loc_id != 'CHN']
    }else{
      subnat_draws_locs_with_non_staggered_intros <- draws_locs_with_non_staggered_intros[level >3 ]
    }


    
    intros <- readRDS(vacc_intro_path)
    intros <- unique(intros[me_name==m, .(ihme_loc_id, location_id, me_name, cv_intro)])
    raked_subnat_draws <- rbindlist(lapply(unique(subnat_draws_locs_with_non_staggered_intros$ihme_loc_id), function(p){
      print(p)
      rake_year_start <- pmin(pmax(intros[ihme_loc_id==p]$cv_intro, year_start), year_end)
      df <- subnat_draws_locs_with_non_staggered_intros[ihme_loc_id==p]
      df[, level := NULL]
      df[, parent_id := NULL]
      df[, ihme_loc_id := NULL]
      d<-rake_draws(df, run_id = run_id, rake_year_start, rake_year_end = year_end)
      return(d)
    }))

    
    save_draws_01(draws = raked_subnat_draws, root = me_outdir)
  }else{
    
    save_draws_01(draws = draws_locs_with_non_staggered_intros, root = me_outdir)
  }
}
