#' @title Make lags

#' @description This function creates lagged estimates of a given model by



#' @param me_name_read [chr] input coverage me name by scenario e.g. "vacc_hepb3_covidfree"
#' @param me_name_write [chr] output covarage me name by lag-year and scenario e.g. "vacc_hepb3_lag_5_covidfree"
#' @param lag_years [int, length = 1] The number of years to lag the data
#' @param input_root [path] e.g. "FILEPATH"
#' @param output_root [path] e.g. "FILEPATH"

#' @return None (the function saves the lagged estimates as an RDS file)

#' @concept prep_draws_custom_functions
make_lags <- function(me_name_read, me_name_write, lag_years, input_root, output_root) {
  
  assertthat::assert_that(assertthat::is.dir(input_root))
  assertthat::assert_that(assertthat::is.dir(output_root))
  assertthat::assert_that(assertthat::is.scalar(me))
  assertthat::assert_that(assertthat::is.scalar(lag_years))
  assertthat::assert_that(assertthat::is.scalar(me_name_read))
  assertthat::assert_that(assertthat::is.scalar(me_name_write))

  
  clean_data <- readRDS(file.path(input_root, paste0(me_name_read, ".rds")))
  
  assertable::assert_ids(clean_data,
                         list(location_id = unique(clean_data$location_id),
                              year_id     = unique(clean_data$year_id)))
  
  square <-
    CJ(
      location_id  = unique(clean_data$location_id),
      year_id      = year_start:year_end,
      age_group_id = 22,
      sex_id       = 3
    )
  
  data_lag <-
    merge(
      x     = square,
      y     = copy(clean_data)[, year_id := year_id + lag_years],
      by    = c("location_id", "year_id", "age_group_id", "sex_id"),
      all.x = TRUE
    )
  
  data_lag[is.na(gpr_mean), c("gpr_mean", "gpr_lower", "gpr_upper") := 0]
  data_lag[, me_name := me_name_write]
  data_lag[, measure_id := clean_data$measure_id %>% unique] 
  
  save_collapsed_general(df = data_lag, me = me_name_write, results_root = output_root)

}
