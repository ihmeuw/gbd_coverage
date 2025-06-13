









#' @param me_ratio [chr] e.g. "vacc_mcv2_mcv1_ratio"
#' @param run_date [chr] e.g. "2024_07_02"

#' @return [none] side effect - overwrite ratio draw files to disk
#' @export

#' @examples
fhs_calculate_vaccine_ratio_task <- function(
    me_ratio,
    run_date
){
  
  
  message(paste0("-- COVID adjustment: ", me_ratio))
  mes   <- unlist(strsplit(me_ratio, "_"))[2:3]
  num   <- paste0("vacc_", mes[1])
  denom <- paste0("vacc_", mes[2])
  
  df.num_covid_free   <- read_covidfree_and_covidimputed_draws(num, run_date, type = "covid-free")
  
  df.denom_covid_free <- read_covidfree_and_covidimputed_draws(denom, run_date, type = "covid-free")
  
  
  key <- c("location_id", "year_id", "age_group_id", "sex_id")
  
  df.num_covid_free   <- melt(df.num_covid_free, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="num")
  df.denom_covid_free <- melt(df.denom_covid_free, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="denom")
  df_covid_free       <- merge(df.num_covid_free, df.denom_covid_free, by=c(key, "draw"))
  
  
  df_covid_free <- df_covid_free[, est := num / denom]
  df_covid_free <- df_covid_free[, c("num", "denom") := NULL]
  df_covid_free <- df_covid_free[est >= 1, est := 0.99]
  
  
  if (num %in% me_db$me_name) cov_id <- me_db[me_name == num, covariate_id]
  
  
  message(paste0("---- Saving draw estimates of covid-free and covid-imputed ", me_ratio, " in FILEPATH", gbd_cycle, '/'))
  df2_covid_free <- dcast.data.table(df_covid_free, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
  df2_covid_free <- df2_covid_free[, measure_id := 18]
  if (num %in% me_db$me_name) df2_covid_free[, covariate_id := cov_id]
  save_covidfree_and_covidimputed_draws(df2_covid_free, me_ratio, run_date, type = "covid-free")
  
}