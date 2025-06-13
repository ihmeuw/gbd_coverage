#' @title Apply china stock adjustment Jobmon task

#' @description This function is called by jobmon as a part of the 02_china_stock_adjustment

#' @param gbd_cycle string, i.e. "gbd2020"
#' @param vaccine vaccine to run the task for
#' @param run_date model run date
#' @param loc_id location id to run task for
#' @param year_end end year

#' @return NULL

#' @concept 02_china_stock_jobmon_tasks
apply_china_stock_adj_task <- function(gbd_cycle, vaccine, run_date, loc_id, year_end) {
  
  output_antigen <- stock_limit(gbd_cycle    = gbd_cycle,
                                vaccine      = vaccine,
                                run_date     = run_date,
                                loc_name     = "China",
                                quantile_max = 0.975,
                                last_data_yr = 2020,
                                last_est_yr  = year_end)

  stock_adjusted_collapsed_estimates <- output_antigen[[1]]
  stock_adjusted_draw_estimates      <- output_antigen[[2]]
  
  vax_draws <- fread(paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", loc_id, ".csv"))
  fwrite(vax_draws, file = paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", loc_id, "_orig.csv"))

  
  fwrite(stock_adjusted_draw_estimates, file=paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", loc_id, ".csv"))
}
