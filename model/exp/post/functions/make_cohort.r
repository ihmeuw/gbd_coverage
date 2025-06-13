#' @title Make Cohort

#' @description This function creates 5-year lagged estimates by age

#' @param me_name_read [chr] input coverage me name by scenario e.g. "vacc_hepb3_covidfree"
#' @param me_name_write [chr] output covarage me name by lag-year and scenario e.g. "vacc_hepb3_lag_5_covidfree"
#' @param input_root [path] e.g. "FILEPATH"
#' @param output_root [path] e.g. "FILEPATH"

#' @return NULL

#' @concept prep_draws_custom_functions
make_cohort <- function(me_name_read, me_name_write, input_root, output_root) {
  
  assertthat::assert_that(assertthat::is.scalar(me_name_read))
  assertthat::assert_that(assertthat::is.scalar(me_name_write))
  assertthat::assert_that(assertthat::is.dir(input_root))
  assertthat::assert_that(assertthat::is.dir(output_root))
  
  
  ages_except_1to4 <- c(2:3, 388:389, 238, 34, 6:20, 30:32, 235)
  clean_data <- readRDS(file.path(input_root, paste0(me_name_read, ".rds")))
  
  assertable::assert_ids(clean_data,
                         list(location_id = unique(clean_data$location_id),
                              year_id     = unique(clean_data$year_id)))
  
  square <- CJ(location_id  = unique(clean_data$location_id),
               year_id      = year_start:year_end,
               age_group_id = ages_except_1to4,
               sex_id       = 3)
  clean_data_238  <- copy(clean_data) %>% .[, age_group_id := 238]
  clean_data_34   <- copy(clean_data) %>% .[, age_group_id := 34]
  clean_data_both <- rbind(clean_data_238, clean_data_34)
  data <-
    merge(
      square,
      clean_data_both,
      by = c("location_id", "year_id", "age_group_id", "sex_id"),
      all.x = TRUE
    )

  
  ages_to_fill      <- c(    6:20, 30:32, 235) 
  ages_to_fill_from <- c(34, 6:20, 30:32)      
  
  assertthat::assert_that(assertthat::are_equal(length(ages_to_fill),
                                                 length(ages_to_fill_from)))

  for (i in 1:length(ages_to_fill)) {
    for (yr in (as.numeric(year_start) + 5):year_end) {
      
        data[age_group_id == ages_to_fill[i] & year_id==yr, gpr_mean  := data[age_group_id==(ages_to_fill_from[i]) & year_id==(yr-5), gpr_mean]]
        data[age_group_id == ages_to_fill[i] & year_id==yr, gpr_lower := data[age_group_id==(ages_to_fill_from[i]) & year_id==(yr-5), gpr_lower]]
        data[age_group_id == ages_to_fill[i] & year_id==yr, gpr_upper := data[age_group_id==(ages_to_fill_from[i]) & year_id==(yr-5), gpr_upper]]
        
    }
  }

  data[is.na(gpr_mean), gpr_mean := 0]
  data[is.na(gpr_lower), gpr_lower := 0]
  data[is.na(gpr_upper), gpr_upper := 0]

  
  data[, me_name := paste0(me, "_cohort")]
  data[, measure_id := clean_data$measure_id %>% unique]
  data[, covariate_id := clean_data$covariate_id %>% unique]
  save_collapsed_general(df = data, me = me_name_write, results_root = output_root)
  

}
