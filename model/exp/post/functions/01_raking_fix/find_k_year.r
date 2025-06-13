#' @title Find k year

#' @description wrapper function to find the best value of k







#' @param target_count (int) - National level coverage count for year of interest
#' @param unraked_draws (data.table) - unraked draws and associated metadata as data table
#' @param tolerance (numeric) - max difference between raking factor and raking target
#' @param int interval for optimize() function

#' @concept logit_raking
find_k_year <- function(target_count, unraked_draws,
                        tolerance = 1e-6,
                        int = c(-10, 10)) {

  
  pops <- get_population(
    age_group_id    = 1,
    sex_id          = 3,
    release_id      = release_id,
    year_id         = unique(unraked_draws$year_id),
    location_id     = unique(unraked_draws$location_id),
    location_set_id = location_set_id,
    forecasted_pop  = fhs_run_TF
  )

  e_k <- function(k) {
    eval_k(k,
           t_count    = target_count,
           u_draws    = unraked_draws,
           population = pops)
  }

  best_k <- optimize(e_k, interval = int, tol = tolerance)$minimum

  return(best_k)

}
