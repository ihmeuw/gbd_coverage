#' @title Save collapsed draws

#' @description Save collapsed draws to best folder

#' @param df A data.table to save
#' @param me A character string specifying the vaccine of interest.
#' @param results_root File path to the save location

#' @return NULL, saves files in <results_root>/<me>.rds

#' @concept prep_draws_individual_functions
save_collapsed_general <- function(df, me, results_root) {

  path <- file.path(results_root, paste0(me, ".rds"))
  saveRDS(df, path)
  message(paste0("Summaries saved for ", me, " to ", path))

}
