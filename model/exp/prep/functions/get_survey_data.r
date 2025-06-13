#' @title Get Survey Data

#' @description Loads in tabulated survey data, drops cohorts younger than recommended



#' @param locs output of get_location_data

#' @return data.table

#' @concept prep_tabulated_survey_data
get_survey_data <- function(locs) {

  df <- lapply(list.files("FILEPATH", full.names=TRUE, pattern = "*.csv"), fread) %>% rbindlist(., fill=TRUE)
  
  df <- df[age_year >= me_cohort_schedule, ]
  setnames(df, "mean", "data")
  df[, variance := standard_error ^ 2]  
  df[, age_group_id := 22]
  df[, cv_survey := 1]
  
  
  
  
  
  
  
  if (nrow(df[data %in% c(0, 1)]) > 0) {
    df.w <- df[data %in% c(0, 1)]
    sample_size <- df.w$sample_size
    n  <- ifelse(df.w$data==0, 0, sample_size)
    ci <- binom.confint(n, sample_size, conf.level=0.95, methods="wilson")
    se <- (ci$upper - ci$lower) / 3.92
    variance <- se ^ 2 * 2.25 
    df[data %in% c(0, 1)]$variance <- variance
  }
  
  df <- df[design_effect < 1, variance := variance * 2.25 / design_effect]
  df <- df[design_effect < 1, design_effect := 2.25]

  return(df)

}
