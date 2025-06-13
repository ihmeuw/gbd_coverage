#' @title Back Calculate ratio draws Jobmon task

#' @description Back-calculate ratio draws from stock-adjusted draws as (stock_adjusted_draw / dpt3_draw)

#' @param run_date model run date
#' @param vaccine vaccine to run the task for
#' @param vaccine_ratio ratio vaccine to back-calculate
#' @param loc_id location to run

#' @return NULL

#' @concept 02_china_stock_jobmon_tasks
back_calculate_ratio_draws_task <- function(run_date, vaccine, vaccine_ratio, loc_id) {
  
  dpt3_china_draws <- fread(paste0('FILEPATH', gbd_cycle, "/", run_date,'FILEPATH/6.csv'))

  
  stock_adjusted_china_draws <- fread(paste0('FILEPATH', gbd_cycle, "/", run_date,'/vacc_', vaccine, '/', loc_id, '.csv'))

  
  draw_columns     <- names(stock_adjusted_china_draws)[grepl("draw_[0-9]*", names(stock_adjusted_china_draws))]
  non_draw_columns <- names(stock_adjusted_china_draws)[names(stock_adjusted_china_draws) %!in% draw_columns]

  stock_adjusted_china_draws_as_matrix <- as.matrix(stock_adjusted_china_draws[, draw_columns, with=F])
  dpt_china_draws_as_matrix            <- as.matrix(dpt3_china_draws[, draw_columns, with=F])

  if(any(dim(stock_adjusted_china_draws_as_matrix) != dim(dpt_china_draws_as_matrix))) {
    stop(paste0("Draw dimensions between DPT3 and ", vaccine, " unequal. Resolve issue in 'apply_China_stock_data_adjustment.R' before continuing"))
  }

  stock_adjusted_china_ratio_draws_as_matrix <- (stock_adjusted_china_draws_as_matrix / dpt_china_draws_as_matrix)
  stock_adjusted_china_ratio_draws           <- cbind(stock_adjusted_china_draws[, non_draw_columns, with=F],
                                                      as.data.table(stock_adjusted_china_ratio_draws_as_matrix))

  
  stock_adjusted_china_ratio_draws[, `:=`(measure_id = NULL, covariate_id = NULL)]

  
  fwrite(x = stock_adjusted_china_ratio_draws,
         file = paste0('FILEPATH', gbd_cycle, "/", run_date, '/', vaccine_ratio, '/', loc_id, '.csv'))
}
