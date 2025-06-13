#' @title Eval k

#' @description function to evaluate some value of k




#' @param k (int) - Raking factor
#' @param unraked_draws (data.table) - unraked draws and associated metadata as data table

#' @return absolute value of the difference between the proposed and


#' @concept logit_raking
eval_k <- function(k, t_count, u_draws, population) {

  
  raked_draws <- rake_draws_with_k(u_draws, k)

  
  raked_draws_agg_count <- agg_pop(raked_draws, population)

  
  
  return(abs(raked_draws_agg_count - t_count))
}
