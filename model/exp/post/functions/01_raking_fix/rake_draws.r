#' @title Rake draws

#' @description Given the draws for all subnationals of a given country where the draws






#' @param draws unraked draws
#' @param run_id model run_id
#' @param rake_year_start the year_id for the min intro year
#' @param rake_year_end the year_id for the max intro year
#' @param special_raking_draw_root [path] default NULL - optional root to read draws from
#' @param convert_from_ysi [lgl] default FALSE - whether to convert from years-since-introduction to calendar years
#' @param stgpr_run_log_root [chr] e.g. to_model_dir - where is the date_version specific stgpr run log saved? 

#' @concept logit_raking
rake_draws <- function(draws,
                       run_id,
                       rake_year_start,
                       rake_year_end,
                       special_raking_draw_root = NULL,
                       convert_from_ysi         = FALSE,
                       stgpr_run_log_root       = input_root
){

  draw_names <- paste0('draw_', 0:(draws_count - 1))

  
  confirm_draw_location_completeness(draws) 

  
  loc_ids <- unique(draws$location_id)
  subnational_levels <- sort(locs[location_id %in% loc_ids, unique(level)])
  if(3 %in% subnational_levels) stop("National-level included in unraked draws. Unraked draws should be subnational only")
  if(6 %in% subnational_levels) stop("rake_draws not configured for level 6 subnationals. Please update function")

  intros <- readRDS(vacc_intro_for_outro_path)
  intros <- unique(intros[, list(ihme_loc_id, location_id, me_name, cv_intro)])
  run_log <- read_stgpr_best_runs(root = stgpr_run_log_root, model_type = "vaccine_coverage")[, .(r_id = run_id, me_name)]
  vaccine <- run_log[r_id == run_id]$me_name

  draws <- merge(draws, intros[me_name==vaccine, list(location_id, cv_intro)], by = 'location_id')

  
  
  raked_draws <- data.table()
  for(lvl in subnational_levels) {

    if(lvl == 4) message(paste0("------ Raking lvl 4 to lvl 3"))
    if(lvl == 5) message(paste0("------ Raking lvl 5 to lvl 4"))

    
    parent_location_ids <- locs[location_id %in% loc_ids & level == lvl, unique(parent_id)]

    for(parent_location_id in parent_location_ids) {

      
      index_in_loop    <- which(parent_location_ids == parent_location_id)
      percent_complete <- round(index_in_loop/length(parent_location_ids), digits = 2) * 100
      message(paste0("-------- ", percent_complete, "% - ", locs[location_id == parent_location_id, location_name], " (", parent_location_id, ")"))

      
      loc_ids_lvl         <- locs[parent_id == parent_location_id & level == lvl, unique(location_id)]

      
      if(lvl == 4) {
        if(is.null(special_raking_draw_root)) {
          parent_raked_estimates <- get_raked_estimates(run_id, parent_location_id)
        } else {
          parent_raked_estimates <- fread(paste0(special_raking_draw_root, '/', parent_location_id, '.csv'))
          cols <- grep("draw_", names(parent_raked_estimates), value=TRUE)
          parent_raked_estimates[, gpr_mean := rowMeans(.SD), .SDcols=cols]
        }
      } else if (lvl == 5) {
        parent_raked_estimates <- raked_estimates_lvl_4[location_id == parent_location_id, ]
      } else {
        stop("rake_draws only equipped to rake level 4 and 5")
      }

      
      if(convert_from_ysi  & lvl==4){
        print('converting from ysi')
        cols=c('gpr_mean','gpr_upper','gpr_lower') 
        intro_year <- intros[me_name==vaccine & location_id==parent_location_id]$cv_intro
        df_zeros <- copy(parent_raked_estimates)
        df_zeros[, (cols) := 0]
        df_zeros <- df_zeros[year_id < intro_year]

        
        parent_raked_estimates[, ysi := year_id - rake_year_start]
        parent_raked_estimates[, year_id := intro_year + ysi]
        parent_raked_estimates <- parent_raked_estimates[year_id <= rake_year_end]
        parent_raked_estimates[, ysi := NULL]
        
        parent_raked_estimates <- rbind(df_zeros, parent_raked_estimates)
      }

      
      parent_population <- get_population(
        age_group_id    = 1,
        sex_id          = 3,
        release_id      = release_id,
        year_id         = unique(draws$year_id),
        location_id     = parent_location_id,
        location_set_id = location_set_id,
        forecasted_pop  = fhs_run_TF
      )
      
      
      raking_factor_by_year <- data.table(year_id = unique(draws$year_id), k = 0)
      years <- rake_year_start:rake_year_end

      for(year in years) {

        
        parent_population_year   <- parent_population[year_id == year, population]
        parent_coverage_year     <- parent_raked_estimates[year_id == year, gpr_mean]
        parent_target_count_year <- parent_population_year * parent_coverage_year

        
        unraked_year <- draws[year_id == year & location_id %in% loc_ids_lvl & cv_intro <= year, ]
        if(nrow(draws[year_id == year & location_id %in% loc_ids_lvl & cv_intro <= year, ])==0) next
        k_year <- find_k_year(parent_target_count_year,unraked_year)
        raking_factor_by_year[year_id == year, k := k_year]
        k_year <- NULL
      }

      
      
      
      
      
      
      raked_draws_lvl <- draws[location_id %in% loc_ids_lvl & (year_id < rake_year_start | year_id > rake_year_end), ]
      for(year in years) {

        
        k <- raking_factor_by_year[year_id == year, k]

        
        un_introd <- draws[year_id == year & location_id %in% loc_ids_lvl & cv_intro > year]
        raked_draws_year <- draws[year_id == year & location_id %in% loc_ids_lvl & cv_intro <= year]
        if(nrow(raked_draws_year) > 0) raked_draws_year <- rake_draws_with_k(raked_draws_year, k)

        raked_draws_lvl  <- rbind(raked_draws_lvl, un_introd, raked_draws_year, fill = TRUE)
        remove(un_introd, raked_draws_year) 
      }

      
      
      raked_draws <- rbind(raked_draws, raked_draws_lvl, fill = TRUE)

      
      
      if(lvl == 4 & 5 %in% subnational_levels) {
        raked_estimates_lvl_4 <- collapse_draws(raked_draws)
        raked_draws$gpr_mean <- NULL
      }
    }
  }
  raked_draws[, cv_intro := NULL]
  return(raked_draws)
}
