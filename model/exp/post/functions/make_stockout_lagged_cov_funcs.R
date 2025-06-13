




#' @param data_table [data.table] e.g. readRDS(paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", me, ".rds"))
#' @param cols_to_roll_avg [chr] vector of colnames to apply rolling averages to
#' @param year_colname [chr] name of year_id column
#' @param roll_avg_window_years [int] how many years to apply rolling average over

#' @return [data.table] vaccine coverage with x-year rolling average smoothing
#' @export
lag_means <- function(data_table,
                      cols_to_roll_avg = c("gpr_mean", "gpr_lower", "gpr_upper"),
                      year_colname ="year_id",
                      roll_avg_window_years = 5) {
  
  dt <- copy(data_table)
  setorderv(dt, c("location_id", "age_group_id", "sex_id", "year_id"))
  valid_order_flag <- all.equal(dt, dt[order(location_id, age_group_id, sex_id, year_id)])
  
  
  
  
  
  if (valid_order_flag != TRUE) {
    stop(valid_order_flag, "
         Your input data is not ordered correctly! Ensure that it is sorted by location_id, age_group_id, sex_id, and year_id")
  }
  
  
  by_cols <- names(dt)[!(names(dt) %in% c(cols_to_roll_avg, year_colname))]
  
  
  dt[, (cols_to_roll_avg) := lapply(.SD, zoo::rollapply, 
                                    width   = roll_avg_window_years, 
                                    FUN     = "mean", 
                                    align   = "right", 
                                    partial = TRUE), 
     by = by_cols, 
     .SDcols = cols_to_roll_avg]
  
  return(dt)
}






#' @param df [data.table] one location of vaccine coverage for one me, draw level
#' @param year_col [chr] e.g. 'year_id'
#' @param year_lag [int] how many years to roll the average over

#' @return [data.table]
#' @export
lag_draws <- function(df,
                      year_col = "year_id",
                      year_lag = 5L) {
  
  df <- copy(df)
  
  cols <- grep("draw_", names(df), value = TRUE)
  
  
  
  
  
  if (nrow(setdiff(df, df[order(location_id, age_group_id, sex_id, year_id)])) > 0) {
    stop("Your input data is not ordered correctly! Ensure that it is sorted by location_id, age_group_id, sex_id, and year_id")
  }
  
  
  by_cols <- names(df)[!(names(df) %in% c(cols, year_col))]
  
  
  df[, (cols) := lapply(.SD, zoo::rollapply, 
                        width   = year_lag, 
                        FUN     = "mean", 
                        align   = "right", 
                        partial = TRUE), 
     by = by_cols, 
     .SDcols = cols]
  
  
  return(df)
}

