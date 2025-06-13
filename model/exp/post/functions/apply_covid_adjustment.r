#' @title Apply COVID adjustment

#' @description The effects of covid are incorporated into the model as a stockout covariate in the primary





#' @param me A character string specifying the vaccine
#' @param run_date A character string specifying the model run directory
#' @param ndraws The number of draws in the data
#' @param save_collapsed A logical indicating whether to save collapsed estimates of covid-free and covid-imputed
#' @param path A character string specifying the path to the directory where the results are to be saved
#' @param results_root A character string specifying the directory where the collapsed results are to be saved
#' @param offset_remove A logical indicating whether to remove the offset from the draws
#' @param stockout_ratios A logical indicating whether to calculate stockout ratios for covid-years
#' @param covid_years [int] years to apply covid shocks

#' @return NULL (results are saved to the specified directories.)

#' @concept prep_draws_composite_functions
apply_covid_adjustment <- function(me,
                                   run_date,
                                   ndraws,
                                   covid_years,
                                   save_collapsed         = TRUE,
                                   path                   = save_root,
                                   results_root           = results_root,
                                   offset_remove          = FALSE,
                                   stockout_ratios        = FALSE, 
                                   to_model_dir) {

  message(paste0("-- COVID adjustment: ", me))

  
  
  covidfree_counterfactual_and_covidimputation(gbd_run_date              = run_date,
                                               calculate_counterfactuals = TRUE,
                                               copy_covid_files          = TRUE,
                                               stockout_ratios           = stockout_ratios,
                                               v                         = me,
                                               ndraws                    = ndraws,
                                               covid_years               = covid_years,
                                               stage_1_stockout_ratios   = FALSE,
                                               to_model_dir              = to_model_dir)
  message('made it outside covidfreeimpute function')
  
  if (me %in% me_db$me_name) cov_id <- me_db[me_name == me, covariate_id]
  df_covid_free    <- read_covidfree_and_covidimputed_draws(v = me, gbd_run_date = run_date, type = "covid-free")
  df_covid_imputed <- read_covidfree_and_covidimputed_draws(v = me, gbd_run_date = run_date, type = "covid-imputed")
  
  df_covid_free    <- cap_est(df_covid_free)
  df_covid_imputed <- cap_est(df_covid_imputed)
  
  df_covid_free    <- set_intro_epi(df_covid_free, me)
  df_covid_imputed <- set_intro_epi(df_covid_imputed, me)
  
  
  if (me %in% c("vacc_bcg")) {
    df_covid_free    <- apply_outro_year(df_covid_free, me)
    df_covid_imputed <- apply_outro_year(df_covid_imputed, me)

    df_covid_free    <- apply_outro_grl(df_covid_free, me)
    df_covid_imputed <- apply_outro_grl(df_covid_imputed, me)
  }
  
  message(paste0("---- Saving draw estimates of covid-free and covid-imputed ", me, " in FILEPATH",gbd_cycle,'/', run_date))
  df2_covid_free    <- copy(df_covid_free)
  df2_covid_imputed <- copy(df_covid_imputed)

  if (me %in% me_db$me_name) {
    df2_covid_free[, covariate_id := cov_id]
    df2_covid_imputed[, covariate_id := cov_id]
  }
  save_covidfree_and_covidimputed_draws(df = df2_covid_free, me, gbd_run_date = run_date, type = "covid-free")
  save_covidfree_and_covidimputed_draws(df = df2_covid_imputed, me, gbd_run_date = run_date, type = "covid-imputed")

  
  if (save_collapsed){
    message(paste0("---- Saving collapsed estimates of covid-free and covid-imputed ", me, " in ", results_root, "\n"))
    df_covid_free    <- collapse_draws(df_covid_free)
    df_covid_imputed <- collapse_draws(df_covid_imputed)
    df_covid_free    <- df_covid_free[, me_name := me]
    df_covid_imputed <- df_covid_imputed[, me_name := me]
    if (me %in% me_db$me_name) {
      df_covid_free[, covariate_id := cov_id]
      df_covid_imputed[, covariate_id := cov_id]
    }
    save_covidfree_and_covidimputed_collapsed(df_covid_free, me, type = "covid-free", results_root)
    save_covidfree_and_covidimputed_collapsed(df_covid_imputed, me, type = "covid-imputed", results_root)
  }
}
