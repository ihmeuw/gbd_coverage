#' @title Rake to stock adjustment Jobmon task

#' @description Rake China's subnational draws to the stock-adjusted national level

#' @param gbd_cycle string, i.e. "gbd2020"
#' @param vaccine vaccine to run the task for
#' @param run_date model run date
#' @param year_end end year
#' @param fhs_run_TF [lgl] TRUE if running FHS, FALSE otherwise
#' @param location_set_id [int] location set id
#' @param release_id [int] release id

#' @return NULL

#' @concept 02_china_stock_jobmon_tasks
rake_to_stock_adjusted_task <- function(
    gbd_cycle
    , vaccine
    , run_date
    , year_end
    
    , fhs_run_TF
    , location_set_id
    , release_id
) {
  
  gbd_env <- new.env()
  source("FILEPATH/gbd_logit_raking_function.R", local=gbd_env)
  raking_options <- list(
    
    zero_heuristic   = TRUE
    , iterate        = TRUE
    , approx_0_1     = TRUE
    , MaxJump        = 11
    , MaxIter        = 80
    , FunTol         = 1e-5
    , if_no_gbd      = "return_na"
    
    
    , yr   = c(year_start:year_end)
    , nyrs = length(c(year_start:year_end))
  )
  
  list2env(raking_options, gbd_env)

  
  read.files <- function(chn_sub, vaccine) {
    path <- paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", chn_sub, ".csv")
    file <- fread(path)
    return(file)
  }
  
  source("FILEPATH/get_location_metadata.R")
  locations <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)

  yr   <- c(year_start:year_end)
  nyrs <- length(yr)

  
  level4_loc_ids <- se$children_of_parents(6, locations[level == 4], "loc_ids") 
  level4_pops    <- get_population(
    age_group_id    = 22,
    location_id     = level4_loc_ids,
    year_id         = yr,
    release_id      = release_id,
    sex_id          = 3,
    location_set_id = location_set_id,
    forecasted_pop  = fhs_run_TF
  )
  colnames(level4_pops) <- c("age_group_id1",  "location_id", "year", "sex_id", "pop", "run_id1")
  level4_pops           <- level4_pops[order(location_id),]
  
  if(!all(yr %in% level4_pops$year)) stop("China cap - not all required years are in population file \n- missing years are: ", toString(setdiff(yr, level4_pops$year)))

  
  output_antigen <- stock_limit(gbd_cycle    = gbd_cycle,
                                vaccine      = vaccine,
                                run_date     = run_date,
                                loc_name     = "China",
                                quantile_max = 0.975,
                                last_data_yr = 2020,
                                last_est_yr  = year_end)

  stock_adjusted_collapsed_estimates <- output_antigen[[1]]
  stock_adjusted_draw_estimates      <- output_antigen[[2]]

  level3  <- stock_adjusted_collapsed_estimates
  rake_to <- level3[, .(name = location_id, year = year_id, value = mean_trans)]
  rake_to[value < 0.001, value := 0]
  rake_to[is.na(value), value := 0]

  level4  <- lapply(level4_loc_ids, function(x) read.files(x, vaccine)) %>% rbindlist(., use.names=TRUE)

  metadata_columns <- c("location_id", "year_id", "age_group_id", "sex_id", "measure_id", "covariate_id")
  draw_columns     <- names(level4)[names(level4) %!in% metadata_columns]

  level4_metadata  <- level4[, c(metadata_columns), with = FALSE]
  level4_draws     <- level4[, c(draw_columns), with = FALSE]

  raked_4_to_3_draws  <- gbd_env$gbd_raking_level4(rake_targets = rake_to,
                                                   gbd_loc_id   = 6,
                                                   cell_pred    = level4_draws,
                                                   location_ids = level4_loc_ids,
                                                   nyears       = nyrs,
                                                   year_list    = c(yr),
                                                   population   = c(level4_pops$pop))

  raked_4_to_3      <- cbind(level4_metadata, raked_4_to_3_draws)

  
  raked_subnationals <- raked_4_to_3
  
  
  
  
  
  level5_loc_ids_all <- se$children_of_parents(level4_loc_ids, locations, "loc_ids")
  
  
  if(length(level5_loc_ids_all)){
    
    
    level4_parent_loc_ids <- se$parents_of_children(child_loc_ids = level5_loc_ids_all, hierarchy = locations, parent_level = 4)
    
    for(parent_id in level4_parent_loc_ids){
      
      raked_parent_draws <- raked_4_to_3_draws[which(raked_4_to_3$location_id == parent_id), ]
      raked_parent_means <- rowMeans(raked_parent_draws)
      
      
      
      level5_loc_ids <- se$children_of_parents(parent_id, locations, "loc_ids")
      level5_pops <- get_population(
        age_group_id    = 22,
        location_id     = level5_loc_ids,
        year_id         = yr,
        release_id      = release_id,
        sex_id          = 3,
        location_set_id = location_set_id,
        forecasted_pop  = fhs_run_TF
      )
      colnames(level5_pops) <- c("age_group_id1",  "location_id", "year", "sex_id", "pop", "run_id1")
      
      if(!all(yr %in% level5_pops$year)) stop("China cap - not all required years are in population file \n- missing years are: ", toString(setdiff(yr, level5_pops$year)))
      
      name              <- rep(parent_id, nyrs)
      rake_to           <- data.table(cbind(name, yr, raked_parent_means))
      colnames(rake_to) <- c("name", "year", "value")
      rake_to[value < 0.001, value := 0]
      
      level5   <- lapply(level5_loc_ids, function(x) read.files(x, vaccine)) %>% rbindlist(., use.names=TRUE)
      
      metadata_columns <- c("location_id", "year_id", "age_group_id", "sex_id", "measure_id", "covariate_id")
      draw_columns     <- names(level5)[names(level4) %!in% metadata_columns]
      
      level5_metadata  <- level5[, c(metadata_columns), with = FALSE]
      level5_draws     <- level5[, c(draw_columns), with = FALSE]
      
      raked_5_to_4_draws <- gbd_env$gbd_raking(rake_targets = rake_to,
                                                    gbd_loc_id   = parent_id,
                                                    cell_pred    = level5_draws,
                                                    location_ids = level5_loc_ids,
                                                    nyears       = nyrs,
                                                    year_list    = c(yr),
                                                    population   = level5_pops$pop)
      
      raked_5_to_4 <- cbind(level5_metadata, raked_5_to_4_draws)
      
      
      raked_subnationals <- rbind(raked_subnationals, raked_5_to_4)
    }
    
    
  } 
  
  
  if(any(se$children_of_parents(6, locations, "loc_ids") %in% locations[level > 5, location_id])){
    stop("China Cap is not designed for level 6+ locations. There are still level 6+ children that have not been raked")
  }
  
  
  raked_subnational_location_ids <- unique(raked_subnationals$location_id)
  for(raked_subnational_location_id in raked_subnational_location_ids) {
    raked_subnational <- raked_subnationals[location_id == raked_subnational_location_id, ]
    file_path <- paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", raked_subnational_location_id, ".csv")
    fwrite(x = raked_subnational, file = file_path)
  }
  
}
