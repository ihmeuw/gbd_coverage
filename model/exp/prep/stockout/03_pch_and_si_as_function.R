



















































library(EnvStats, lib.loc='FILEPATH')
libraries_to_load <- c("lme4", "nlme", "ggplot2", "data.table", "viridis", "gridExtra", "arm", "tictoc")
invisible(lapply(libraries_to_load, library, character.only = TRUE))

covidfree_counterfactual_and_covidimputation <- function(
    gbd_run_date,              
    calculate_counterfactuals, 
    copy_covid_files,          
    age_specific             = FALSE,
    age_specific_run_date    = NULL,
    stockout_ratios          = TRUE,   
    ndraws                   = n_draws,
    stage_1_stockout_ratios  = FALSE,
    stage_1_cascade_run_date = NULL,
    v,                        
    covid_years,              
    to_model_dir,               
    pre_years = 2015:2019
){                        
  run_id <- try(read_stgpr_best_runs(root = to_model_dir, model_type = "vaccine_coverage")[me_name == v, run_id])
  
  
  
  
  
  
  run_date_specific_stockout_model_path       <- paste0('FILEPATH', gbd_cycle, '/', gbd_run_date, '/full_detected_stockouts_stgpr_', gbd_run_date, '.csv')
  run_date_specific_ratio_stockout_model_path <- paste0('FILEPATH', gbd_cycle, '/', gbd_run_date, '/full_ratio_detected_stockouts_stgpr_', gbd_run_date, '.csv')
  run_date_specific_stockout_model            <- file.exists(run_date_specific_stockout_model_path)
  run_date_specific_ratio_stockout_model      <- file.exists(run_date_specific_ratio_stockout_model_path)
  if(!run_date_specific_stockout_model) {
    message("---- COVID IMPUTATION: Since run.stockout was set to FALSE in prep_exp.R, stockout model wasn't run for this run date. Grabbing previous version")
  }
  if(run_date_specific_stockout_model){
    s0 <- fread(run_date_specific_stockout_model_path)
  } else {
    s0 <- fread("FILEPATH/full_detected_stockouts_stgpr.csv")
  }
  
  
  
  if(!age_specific) {
    if (stockout_ratios) {
      if(run_date_specific_ratio_stockout_model) {
        s0_ratios <- fread(paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'/full_ratio_detected_stockouts_stgpr_',gbd_run_date,'.csv'))
      } else {
        s0_ratios <- fread(paste0("FILEPATH/full_ratio_detected_stockouts_stgpr.csv"))
      }
      s0 <- rbind(s0[v %in% c("vacc_bcg", "vacc_dpt1", "vacc_mcv1", "vacc_polio3")],
                  s0_ratios)
    } else {
      s0[me_name %in% c('vacc_pcv3','vacc_rotac','vacc_hepb3','vacc_hib3'),me_name:= paste0(me_name, '_dpt3_ratio')]
      s0[me_name %in% c('vacc_mcv2','vacc_rcv1'),me_name:= paste0(me_name, '_mcv1_ratio')]
      s0[me_name == 'vacc_dpt1',me_name:= paste0(me_name, '2_cond')]
      s0[me_name == 'vacc_dpt3',me_name:= paste0(me_name, '_dpt1_ratio')]
    }
  }
  s0 <- s0[ihme_loc_id!='']
  countries <- unique(s0[!grep('_',s0$ihme_loc_id)]$ihme_loc_id)
  countries <- countries[countries!=''] 
  
  
  s1 <- s0[year_id %in% c(covid_years,pre_years)]
  s1 <- s1[me_name == v]
  
  
  s1[, country_iso := ihme_loc_id]
  s1[, country_iso := gsub('_[0-9]*', '', country_iso)]
  
  
  s1[ihme_loc_id %in% countries, level := 'national']
  s1[!(ihme_loc_id %in% countries), level := 'subnational']
  
  s1 <- s1[, list(ihme_loc_id, country_iso, location_id, year_id, me_name, cov = diff, level, who_stockout)]
  
  
  
  out <- readRDS(paste0('FILEPATH', gbd_cycle, '/', gbd_run_date, '/vaccination.rds'))
  
  out <- out[!is.na(data) & cv_admin == 1 & me_name == v]
  for(ihme_loc in unique(out$ihme_loc_id)){
    years <- out[ihme_loc_id == ihme_loc]$year_id
    for (yr_i in c(covid_years,pre_years)) {
      if (length(years[years < yr_i]) == 0) {
        if (yr_i %in% years)
          s1[country_iso == ihme_loc & year_id == yr_i, cov := NA]
      }
    }
  }
  
  
  
  
  intros <- readRDS(file.path(to_model_dir, "reference/vaccine_intro_for_outro.rds"))
  
  intros[me_name %in% c('vacc_pcv3', 'vacc_rotac', 'vacc_hepb3', 'vacc_hib3'), me_name := paste0(me_name, '_dpt3_ratio')]
  intros[me_name %in% c('vacc_mcv2', 'vacc_rcv1'), me_name := paste0(me_name, '_mcv1_ratio')]
  
  
  intros <- intros[me_name == v]
  intros <- intros[cv_intro_years > 0]
  
  
  never_introd <- as.character(vector())
  if(!(v %in% c('vacc_bcg', 'vacc_dpt3_dpt1_ratio', 'vacc_mcv1', 'vacc_polio3', 'vacc_dpt1'))){
    for(iso_i in unique(s1$country_iso)){
      if(nrow(intros[ihme_loc_id == iso_i & year_id %in% c(pre_years,covid_years) & cv_intro_years > 0 & (is.na(cv_outro) | cv_outro == 9999)]) == 0) {never_introd <- c(never_introd, iso_i)}
    }
  }
  
  
  locations_dropped_due_to_non_introduction <- data.table()
  if(!(v %in% c('vacc_bcg', 'vacc_dpt3_dpt1_ratio', 'vacc_mcv1', 'vacc_polio3', 'vacc_dpt1'))) {
    for(iso_i in unique(s1$country_iso)){
      for(yr_i in c(covid_years,pre_years)){
        if(nrow(intros[ihme_loc_id == iso_i & year_id == yr_i]) == 0) {
          locations_dropped_due_to_non_introduction <- rbind(locations_dropped_due_to_non_introduction, s1[(country_iso == iso_i & year_id == yr_i)])
          s1 <- s1[!(country_iso == iso_i & year_id == yr_i)]
        }
      }
    }
  }
  
  
  s1[, cov_round := cov]
  s1[cov_round < 0, cov_round := 0]
  
  
  
  
  
  
  if(calculate_counterfactuals){
    
    
    
    
    if(!age_specific) {
      draw_root       <- paste0('FILEPATH', gbd_cycle, '/', gbd_run_date, '/' ,v, '/')
      covid_free_root <- paste0('FILEPATH', gbd_cycle, '/', gbd_run_date, '_covidfree/', v, '/')
    } else {
      draw_root       <- paste0('FILEPATH',gbd_cycle, 'FILEPATH', age_specific_run_date,'FILEPATH', v, '/')
      covid_free_root <- paste0('FILEPATH',gbd_cycle, 'FILEPATH', age_specific_run_date, '_covidfree/', 'FILEPATH', v, '/')
    }
    dir.create(covid_free_root, recursive = TRUE)
    
    
    
    
    
    if(stage_1_stockout_ratios){
      mr_brt_betas <- unique(fread(paste0('FILEPATH', stage_1_cascade_run_date,
                                          '/', v, '/mrbrt_custom_stage_1_', v, '_betas.csv'))[, list(location_id, stockout_beta)])
    } else {
      st_gpr_beta <- fread(paste0('FILEPATH', run_id, '/stage1_summary.csv'))[covariate == 'cv_stockout']$betas
    }
    
    message(paste0("---- Saving covid-free draws: ", covid_free_root))
    
    for(loc_id in unique(s1$location_id)){
      
      
      if(s1[location_id == loc_id]$level[1] == 'subnational'){
        l_nat <- s1[country_iso == unique(s1[location_id == loc_id]$country_iso) & level == 'national']$location_id[1]
      } else{
        l_nat <- loc_id
      }
      
      vacc_draws       <- fread(paste0(draw_root, loc_id, '.csv'))
      nat_draws        <- fread(paste0(draw_root, l_nat, '.csv'))
      draw_columns     <- names(vacc_draws)[grepl("draw_[0-9]*", names(vacc_draws))]
      metadata_columns <- names(vacc_draws)[!grepl("draw_[0-9]*", names(vacc_draws))]
      
      
      if(stage_1_stockout_ratios){
        
        beta <- mr_brt_betas[location_id == loc_id]$stockout_beta
        if(length(beta) > 1) stop(paste0('somehow have multiple mrbrt stockout betas for location ', loc_id, ': stop and inspect'))
        if(length(beta)==0){ 
          warning(paste0('no beta for location ', loc_id, '; setting beta to zero'))
          beta <- 0
        }
        
      } else{
        
        beta <- st_gpr_beta
      }
      
      
      
      
      if(!age_specific){
        
        library(arm)
        
        rel_diff_all=data.table()
        
        stockout_years <- unique(s1[location_id==l_nat & year_id %in% pre_years & who_stockout==TRUE]$year_id)
        
        for(yr_i in pre_years){
          if(yr_i %in% stockout_years){
          coverage_with_disruptions <- as.numeric(nat_draws[year_id == yr_i, draw_columns, with = F]) 
          coverage_with_disruptions <- pmax(coverage_with_disruptions, 0.001) 
          stockout <- s1[location_id == l_nat & year_id == yr_i]$cov_round
          coverage_pre_covid_counterfactual <- arm::invlogit(logit(coverage_with_disruptions) + beta * (0 - stockout))
          rel_diff <-(coverage_pre_covid_counterfactual - coverage_with_disruptions) / coverage_pre_covid_counterfactual
          rel_diff[which(is.nan(rel_diff))] <- 0
          rel_diff[which(is.na(rel_diff))] <- 0
          rel_diff[which(is.infinite(rel_diff))] <- 0
          }else{
            rel_diff <- rep(0,ndraws)
          }
          rel_diff_all <- cbind(rel_diff_all, rel_diff)
        }
        rel_diff_mean <- rowMeans(rel_diff_all)
        
        
        for(yr_i in covid_years){
          coverage_with_covid <- as.numeric(vacc_draws[year_id == yr_i, draw_columns, with = F])
          coverage_with_covid <- pmax(coverage_with_covid, 0.001) 
          stockout_with_covid <- s1[location_id == l_nat & year_id == yr_i]$cov_round
          coverage_counterfactual <- arm::invlogit(logit(coverage_with_covid) + beta * (0 - stockout_with_covid))
          coverage_counterfactual <- coverage_counterfactual - coverage_counterfactual*rel_diff_mean
          
          
          
          
          if(nrow(s1[location_id == loc_id & year_id == yr_i]) > 0) {
            if(!is.na(s1[location_id == l_nat & year_id == yr_i]$cov_round)) {
              vacc_draws[year_id == yr_i, (draw_columns) := as.list(coverage_counterfactual)]
            }
          }
        }
        
      } else {
        
        
        
        
        for(yr_i in covid_years){
          
          if(nrow(s1[location_id == loc_id & year_id == yr_i]) > 0) {
            if(!is.na(s1[location_id == l_nat & year_id == yr_i]$cov_round)) {
              
              
              stockout_with_covid <- s1[location_id==l_nat & year_id==yr_i]$cov_round
              
              
              
              
              
              vacc_draws          <- melt(vacc_draws, id.vars = metadata_columns, measure.vars = draw_columns, variable.name = "draws")
              vacc_draws[year_id == yr_i, value := arm::invlogit(logit(value) + beta * (0 - stockout_with_covid))]
              
              
              
              
              
              lhs        <- paste(metadata_columns, collapse = " + ")
              rhs        <- "draws"
              lhs_rhs    <- paste(c(lhs, rhs), collapse = " ~ ")
              vacc_draws <- dcast(vacc_draws, eval(parse(text = lhs_rhs)))
            }
          }
        }
      }
      assert_data_schema(vacc_draws)
      fwrite(vacc_draws, file = file.path(covid_free_root, paste0(loc_id, '.csv')))
    }
    
    
    for(loc_id in unique(s0[ihme_loc_id %in% never_introd]$location_id)){
      vacc_draws <- fread(file.path(draw_root, paste0(loc_id,'.csv')))
      assert_data_schema(vacc_draws)
      fwrite(vacc_draws, file = file.path(covid_free_root, paste0(loc_id, '.csv')))
    }
  }
  
  
  
  
  
  
  
  s1_all <- data.table()
  for(loc_id in unique(s1$location_id)){
    
    s_sub <- s1[location_id == loc_id]
    
    
    
    if(!age_specific) {
      c_draws  <- fread(paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'/',v,'/',loc_id,'.csv'))
      cf_draws <- fread(paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'_covidfree/',v,'/',loc_id,'.csv'))
    } else {
      c_draws  <- fread(paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, 'FILEPATH', v, '/', loc_id,'.csv'))
      cf_draws <- fread(paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, 'FILEPATH', v, '/' , loc_id, '.csv'))
    }
    
    c_draws  <- c_draws[year_id %in% covid_years]
    cf_draws <- cf_draws[year_id %in% covid_years]
    
    draw_columns <- names(c_draws)[grepl("draw_[0-9]*", names(c_draws))]
    
    
    
    c_draws[, mean := rowMeans(.SD), .SDcols = draw_columns]
    cf_draws[, mean := rowMeans(.SD), .SDcols = draw_columns]
    
    if(!age_specific) {
      c_draws  <- c_draws[, list(year_id, c_mean = mean)]
      cf_draws <- cf_draws[, list(year_id, cf_mean = mean)]
      merge_vars <- "year_id"
    } else {
      c_draws  <- c_draws[, list(year_id, age_group_id, c_mean = mean)]
      cf_draws <- cf_draws[, list(year_id, age_group_id, cf_mean = mean)]
      merge_vars <- c("year_id", "age_group_id")
    }
    
    c_cf <- merge(c_draws, cf_draws, by = merge_vars)
    c_cf[ , percentage_point_diff := cf_mean * 100 - c_mean * 100]
    c_cf[ , percent_diff := (cf_mean * 100 - c_mean * 100) / (cf_mean * 100)]
    c_cf[ is.nan(percent_diff)|is.na(percent_diff),percent_diff := 0]
    c_cf[ , location_id := loc_id]
    c_cf[ , me_name := v]
    
    s_sub  <- merge(s_sub, c_cf, by = c('location_id','me_name','year_id'))
    s1_all <- rbind(s1_all, s_sub)
    
  }
  
  
  s1_all[, p_binom := ifelse(percent_diff <= 0, 0, 1)]
  
  
  
  
  
  if(copy_covid_files){
    if(!age_specific) {
      dir.create(paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'_covidimputed/'))
    } else {
      dir.create(paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, '_covidimputed/'))
    }
    
    for(v in unique(s1_all$me_name)){
      if(!age_specific) {
        old_path <- paste0('FILEPATH',gbd_cycle,'/', gbd_run_date, '/', v, '/')
        new_path <- paste0('FILEPATH',gbd_cycle,'/', gbd_run_date, '_covidimputed/', v, '/')
      } else {
        old_path <- paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, 'FILEPATH', v, '/')
        new_path <- paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, 'FILEPATH', v, '/')
      }
      dir.create(new_path, recursive = TRUE)
      files <- list.files(old_path)
      file.copy(file.path(paste0(old_path, files)), new_path, overwrite = TRUE)
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  if(!age_specific) {
    params <- rbindlist(lapply(unique(s1_all$me_name), function(v){
      params <- rbindlist(lapply(covid_years, function(yr_i){
        q <- quantile(s1_all[year_id == yr_i & level == 'national']$percent_diff, .975, na.rm = TRUE)
        b <- ebinom(s1_all[year_id == yr_i & level == 'national']$p_binom)$parameters[["prob"]]
        if(nrow(s1_all[year_id == yr_i & percent_diff < q & percent_diff > 0 & level == 'national'])>0){ 
        e <- eexp(s1_all[year_id == yr_i & percent_diff < q & percent_diff > 0 & level == 'national']$percent_diff)$parameters[["rate"]]
        }else{
          e <- NA
        }
        params <- data.table(prob = b, rate = e, me_name = v, year_id = yr_i)
        return(params)
      }))
    }))
    
    
    
    missing <- s1_all[is.na(cov)]
    
    
    for(yr_i in covid_years){
      
      for(loc_id in unique(missing[year_id == yr_i & ihme_loc_id != country_iso]$location_id)){
        
        country <- missing[year_id == yr_i & location_id == loc_id]$country_iso
        if(nrow(missing[year_id == yr_i & ihme_loc_id == country]) == 0) {
          missing <- missing[!(year_id == yr_i & location_id == loc_id)]
        }
      }
    }
    
    message(paste0("---- Saving covid-imputed draws: ", 'FILEPATH',gbd_cycle,'/', gbd_run_date, '_covidimputed/',v, '/'))
    
    
    missing <- missing[c_mean != 0]
    
    
    
    
    
    
    
    
    
    for(ihme_loc_nat in unique(missing$country_iso)){
      
      
      
      loc_ids_nat_sub <- unique(missing[country_iso == ihme_loc_nat]$location_id) 
      imputed_percent_difference <- rbindlist(lapply(unique(missing[country_iso == ihme_loc_nat]$year_id), function(yr_id){
        q <- quantile(s1_all[year_id == yr_id & level == 'national']$percent_diff, .975, na.rm = TRUE)
      if(nrow(s1_all[year_id == yr_id & percent_diff < q & percent_diff > 0 & level == 'national'])>0){  
        DT <- as.data.table(
          t(
            rbinom(n = ndraws, size = 1, prob = params[year_id == yr_id]$prob) * 
              rexp(n = ndraws, rate = params[year_id == yr_id]$rate)
          )
        )
      }else{
        DT <- as.data.table(t(rep(0,ndraws)))
      }
        DT[, year_id := yr_id]
      }))
      
      for(loc_id in loc_ids_nat_sub) {
        
        vacc_draws <- fread(paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'/',v,'/',loc_id,'.csv'))
        covidfree_draws <- fread(paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'_covidfree/',v,'/',loc_id,'.csv'))
        varnames_draws <- grep("draw_[0-9]*", names(vacc_draws), value = TRUE)
        
        for(yr_id in unique(missing[location_id == loc_id]$year_id)){
          counterfactual_coverage <- as.numeric(covidfree_draws[year_id == yr_id, varnames_draws, with = FALSE])
          
          imputed_percent_difference_yr <- imputed_percent_difference[year_id == yr_id]
          imputed_percent_difference_yr[ , year_id := NULL]
          
          coverage_with_covid <- counterfactual_coverage - (imputed_percent_difference_yr * counterfactual_coverage)
          
          vacc_draws[year_id == yr_id, (varnames_draws) := as.list(coverage_with_covid)]
        }
        
        assert_data_schema(vacc_draws)
        fwrite(vacc_draws, paste0('FILEPATH',gbd_cycle,'/',gbd_run_date,'_covidimputed/',v,'/',loc_id,'.csv'))
      }
    }
    
    
    
    
    
    
    
    
    unintroduced_loc_ids           <- unique(locations_dropped_due_to_non_introduction$location_id)
    unintroduced_loc_id_file_names <- paste0(unintroduced_loc_ids, ".csv")
    
    covidfree_draw_path    <- paste0('FILEPATH',gbd_cycle,'/', gbd_run_date, '_covidfree/', v, '/')
    covidimputed_draw_path <- paste0('FILEPATH',gbd_cycle,'/', gbd_run_date, '_covidimputed/', v, '/')
    
    unintroduced_loc_id_file_names_covidfree    <- unintroduced_loc_id_file_names[unintroduced_loc_id_file_names %!in% list.files(covidfree_draw_path)]
    unintroduced_loc_id_file_names_covidimputed <- unintroduced_loc_id_file_names[unintroduced_loc_id_file_names %!in% list.files(covidimputed_draw_path)]
    
    if(length(unintroduced_loc_ids) > 0) {
      for(unintroduced_loc_id_file_name in unintroduced_loc_id_file_names_covidfree) {
        unintroduced_loc_id_file_path <- paste0('FILEPATH',gbd_cycle,'/', gbd_run_date,'/',v,'/', unintroduced_loc_id_file_name)
        file.copy(from = unintroduced_loc_id_file_path, to = covidfree_draw_path, overwrite = FALSE)
      }
      
      for(unintroduced_loc_id_file_name in unintroduced_loc_id_file_names_covidimputed) {
        unintroduced_loc_id_file_path <- paste0('FILEPATH',gbd_cycle,'/', gbd_run_date,'/',v,'/', unintroduced_loc_id_file_name)
        file.copy(from = unintroduced_loc_id_file_path, to = covidimputed_draw_path, overwrite = FALSE)
      }
    }
    
  } else if(age_specific) {
    
    
    
    covid_imputed_draw_path <- paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, 'FILEPATH', v, '/')
    message(paste0("---- Saving covid-imputed draws for each age_group "))
    
    age_groups <- unique(s1_all$age_group_id)
    
    for(age_group in age_groups) {
      
      message(paste0("------ Age group: ", age_group))
      
      
      s1_age_group <- s1_all[age_group_id == age_group]
      
      
      params <- rbindlist(lapply(unique(s1_age_group$me_name),function(v){
        params <- rbindlist(lapply(covid_years, function(yr_i){
          
          
          
          if(s1_age_group[year_id == yr_i, .N] > 0){
            q <- quantile(s1_age_group[year_id == yr_i & level == 'national']$percent_diff, .975, na.rm = TRUE)
            b <- ebinom(s1_age_group[year_id == yr_i & level == 'national']$p_binom)$parameters[["prob"]]
            e <- eexp(s1_age_group[year_id == yr_i & percent_diff < q & percent_diff > 0 & level == 'national']$percent_diff)$parameters[["rate"]]
            params <- data.table(prob = b, rate = e, me_name = v, year_id = yr_i, age_group_id = age_group)
            return(params)
          }
        }))
      }))
      
      
      missing <- s1_age_group[is.na(cov)]
      
      
      for(yr_i in covid_years){
        for(loc_id in unique(missing[year_id == yr_i & ihme_loc_id != country_iso]$location_id)){
          country <- missing[year_id == yr_i & location_id == loc_id]$country_iso
          
          if(nrow(missing[year_id == yr_i & ihme_loc_id == country]) == 0) {
            missing <- missing[!(year_id == yr_i & location_id == loc_id)]
          }
        }
      }
      
      
      
      missing <- missing[c_mean != 0]
      
      
      for(ihme_loc_nat in unique(missing$country_iso)){
        
        
        loc_ids_nat_sub <- unique(missing[country_iso == ihme_loc_nat]$location_id)
        imputed_percent_difference <- rbindlist(lapply(unique(missing[country_iso == ihme_loc_nat]$year_id), function(yr_i){
          DT <- as.data.table(
            t(
              rbinom(n = ndraws, size = 1, prob = params[year_id == yr_i]$prob) *
                rexp(n = ndraws, rate = params[year_id == yr_i]$rate)
            )
          )
          DT[ , year_id := yr_i]
        }))
        
        
        for(loc_id in loc_ids_nat_sub) {
          
          
          vacc_draws <- fread(paste0('FILEPATH',gbd_cycle,'FILEPATH', age_specific_run_date, 'FILEPATH', v, '/', loc_id, '.csv'))
          draw_columns <- names(vacc_draws)[grepl("draw_[0-9]*", names(vacc_draws))]
          
          for(yr_i in unique(missing[location_id == loc_id]$year_id)){
            counterfactual_coverage <- as.numeric(vacc_draws[age_group_id == age_group & year_id == yr_i, draw_columns,with = FALSE])
            
            imputed_percent_difference_yr <- imputed_percent_difference[year_id == yr_i]
            imputed_percent_difference_yr[, year_id := NULL]
            
            coverage_with_covid <- counterfactual_coverage - (imputed_percent_difference_yr * counterfactual_coverage)
            
            vacc_draws[age_group_id == age_group & year_id == yr_i, (draw_columns) := as.list(coverage_with_covid)]
          }
          
          assert_data_schema(vacc_draws)
          fwrite(vacc_draws, paste0(covid_imputed_draw_path, '/', loc_id, '.csv'))
        }
      }
    }
  }
}





read_covidfree_and_covidimputed_draws <- function(v, gbd_run_date, type) {
  
  
  draw_root <- paste0("FILEPATH",gbd_cycle,'/')
  if (type == "covid-free") {
    draw_path <- file.path(draw_root, paste0(gbd_run_date, "_covidfree/", v))
  } else if (type == "covid-imputed") {
    draw_path <- file.path(draw_root, paste0(gbd_run_date, "_covidimputed/", v))
  } else {
    stop(paste0('type must be "covid-free" or "covid-imputed"'))
  }
  
  
  files <- list.files(draw_path, full.names=TRUE)
  if (length(files)==0) {
    stop(paste0("No draws at ", draw_path))
  }
  df <- mclapply(files, fread, mc.cores=10) %>% rbindlist(., use.names=TRUE)
  key <- c("location_id", "year_id", "age_group_id", "sex_id")
  setkeyv(df, cols=key)
  df <- unique(df)
  return(df)
}




save_covidfree_and_covidimputed_draws <- function(df, me, gbd_run_date, type) {
  
  assert_data_schema(df)
  
  
  draw_root <- paste0("FILEPATH",gbd_cycle,'/')
  if (type == "covid-free") {
    draw_path <- file.path(draw_root, paste0(gbd_run_date, "_covidfree/", me))
  } else if (type == "covid-imputed") {
    draw_path <- file.path(draw_root, paste0(gbd_run_date, "_covidimputed/", me))
  } else {
    stop(paste0('type must be "covid-free" or "covid-imputed"'))
  }
  
  
  unlink(draw_path, recursive=TRUE)
  dir.create(draw_path, showWarnings=FALSE)
  location_ids <- unique(df$location_id)
  invisible(mclapply(location_ids, function(x) {
    fwrite(df[location_id == x], paste0(draw_path, "/", x, ".csv"), row.names=FALSE)
  }, mc.cores=10))
}




save_covidfree_and_covidimputed_collapsed <- function(df, me, type, results_root) {
  
  if (type == "covid-free") {
    file_name <- paste0(me, "_covidfree.rds")
  } else if (type == "covid-imputed") {
    file_name <- paste0(me, "_covidimputed.rds")
  } else {
    stop(paste0('type must be "covid-free" or "covid-imputed"'))
  }
  path <- paste0(results_root, "/", file_name)
  saveRDS(df, path)
}
