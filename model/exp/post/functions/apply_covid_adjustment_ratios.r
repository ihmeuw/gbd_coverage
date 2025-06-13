#' @title Apply COVID adjustment ratios to vaccine coverage estimates

#' @description This function applies COVID adjustment ratios to vaccine coverage estimates.

#' @param me The measure to prepare
#' @param ndraws The number of draws in the data
#' @param save_collapsed A logical indicating whether to save the prepared collapsed estimates
#' @param path A path to the root directory where the prepared draws will be saved
#' @param stockout_ratios A logical indicating whether to use stockout-adjusted ratios
#' @param offset_remove A logical indicating whether to remove the offset from the vaccine coverage estimates (default is FALSE)
#' @param gbd_run_date A string indicating the date of the model run
#' @param covid_years [int] years to apply covid shocks

#' @return NULL, saves outputs

#' @concept prep_draws_composite_functions
apply_covid_adjustment_ratios <- function(me,
                                          ndraws,
                                          covid_years,
                                          save_collapsed  = TRUE,
                                          path            = save_root,
                                          stockout_ratios = TRUE,
                                          offset_remove   = FALSE,
                                          gbd_run_date    = run_date,
                                          s1_stockout_ratios,
                                          s1_cascade_run_date,
                                          to_model_dir){

  
  message(paste0("-- COVID adjustment: ", me))
  mes   <- unlist(strsplit(me, "_"))[2:3]
  num   <- paste0("vacc_", mes[1])
  denom <- paste0("vacc_", mes[2])

  
  
  covidfree_counterfactual_and_covidimputation(gbd_run_date              = gbd_run_date,
                                               calculate_counterfactuals = TRUE,
                                               copy_covid_files          = TRUE,
                                               stockout_ratios           = stockout_ratios,
                                               v                         = me,
                                               ndraws                    = ndraws,
                                               covid_years               = covid_years,
                                               stage_1_stockout_ratios   = s1_stockout_ratios,
                                               stage_1_cascade_run_date  = s1_cascade_run_date,
                                               to_model_dir              = to_model_dir)

  
  
  df.ratio_covid_free    <- read_covidfree_and_covidimputed_draws(me, gbd_run_date, type = "covid-free")
  df.ratio_covid_imputed <- read_covidfree_and_covidimputed_draws(me, gbd_run_date, type = "covid-imputed")
  df.ratio_covid_free    <- set_intro(df.ratio_covid_free, me)
  df.ratio_covid_imputed <- set_intro(df.ratio_covid_imputed, me)

  df.denom_covid_free    <- read_covidfree_and_covidimputed_draws(denom, gbd_run_date, type = "covid-free")
  df.denom_covid_imputed <- read_covidfree_and_covidimputed_draws(denom, gbd_run_date, type = "covid-imputed")

  df.ratio_covid_free    <- cap_est(df.ratio_covid_free)
  df.ratio_covid_imputed <- cap_est(df.ratio_covid_imputed)
  df.denom_covid_free    <- cap_est(df.denom_covid_free)
  df.denom_covid_imputed <- cap_est(df.denom_covid_imputed)

  
  key <- c("location_id", "year_id", "age_group_id", "sex_id")
  
  df.ratio_covid_free    <- melt(df.ratio_covid_free, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="ratio")
  df.denom_covid_free    <- melt(df.denom_covid_free, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="denom")
  df_covid_free          <- merge(df.ratio_covid_free, df.denom_covid_free, by=c(key, "draw"))

  df.ratio_covid_imputed <- melt(df.ratio_covid_imputed, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="ratio")
  df.denom_covid_imputed <- melt(df.denom_covid_imputed, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="denom")
  df_covid_imputed       <- merge(df.ratio_covid_imputed, df.denom_covid_imputed, by=c(key, "draw"))

  
  df_covid_free <- df_covid_free[, est := ratio * denom]
  df_covid_free <- df_covid_free[, c("ratio", "denom") := NULL]
  df_covid_free <- df_covid_free[est >= 1, est := 0.99]

  df_covid_imputed <- df_covid_imputed[, est := ratio * denom]
  df_covid_imputed <- df_covid_imputed[, c("ratio", "denom") := NULL]
  df_covid_imputed <- df_covid_imputed[est >= 1, est := 0.99]

  
  df_covid_free    <- aggregate_most_detailed(df_covid_free, varname="est")
  df_covid_imputed <- aggregate_most_detailed(df_covid_imputed, varname="est")

  
  if (num %in% me_db$me_name) cov_id <- me_db[me_name==num, covariate_id]

  
  message(paste0("---- Saving draw estimates of covid-free and covid-imputed ", me, " in FILEPATH",gbd_cycle,'/'))
  df2_covid_free <- dcast.data.table(df_covid_free, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
  df2_covid_free <- df2_covid_free[, measure_id := 18]
  if (num %in% me_db$me_name) df2_covid_free[, covariate_id := cov_id]
  save_covidfree_and_covidimputed_draws(df2_covid_free, num, gbd_run_date, type = "covid-free")

  df2_covid_imputed <- dcast.data.table(df_covid_imputed, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
  df2_covid_imputed <- df2_covid_imputed[, measure_id := 18]
  if (num %in% me_db$me_name) df2_covid_imputed[, covariate_id := cov_id]
  save_covidfree_and_covidimputed_draws(df2_covid_imputed, num, gbd_run_date, type = "covid-imputed")

  
  if (save_collapsed){
    message(paste0("---- Saving collapsed estimates of covid-free and covid-imputed ", num, " in ", results_root, "\n"))
    df_covid_free <- df_covid_free[, gpr_mean := mean(est), by=key]
    df_covid_free <- df_covid_free[, gpr_lower := quantile(est, 0.025), by=key]
    df_covid_free <- df_covid_free[, gpr_upper := quantile(est, 0.975), by=key]
    df_covid_free <- df_covid_free[, c(key, "gpr_mean", "gpr_lower", "gpr_upper"), with=FALSE] %>% unique
    df_covid_free <- df_covid_free[, me_name := num]
    df_covid_free <- df_covid_free[, measure_id := 18]
    if (num %in% me_db$me_name) df_covid_free[, covariate_id := cov_id]
    save_covidfree_and_covidimputed_collapsed(df_covid_free, num, type = "covid-free", results_root)

    df_covid_imputed <- df_covid_imputed[, gpr_mean := mean(est), by=key]
    df_covid_imputed <- df_covid_imputed[, gpr_lower := quantile(est, 0.025), by=key]
    df_covid_imputed <- df_covid_imputed[, gpr_upper := quantile(est, 0.975), by=key]
    df_covid_imputed <- df_covid_imputed[, c(key, "gpr_mean", "gpr_lower", "gpr_upper"), with=FALSE] %>% unique
    df_covid_imputed <- df_covid_imputed[, me_name := num]
    df_covid_imputed <- df_covid_imputed[, measure_id := 18]
    if (num %in% me_db$me_name) df_covid_imputed[, covariate_id := cov_id]
    save_covidfree_and_covidimputed_collapsed(df_covid_imputed, num, "covid-imputed", results_root)
  }
}
