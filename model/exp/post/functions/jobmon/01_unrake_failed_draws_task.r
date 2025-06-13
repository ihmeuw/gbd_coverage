#' @title Unrake failed draws task

#' @description Part 2/4 of the 01 raking task in the post pipeline.






#' @param failed_run_id Model run_id
#' @param location_id list of location ids to read. If NULL, read all
#' @param intro_year the year_id for the intro year
#' @param me_outdir directory to save outputs (typically model run date folder)

#' @return NULL

#' @concept 01_post_raking
unrake_failed_draws_task <- function(failed_run_id, location_id, intro_year, me_outdir) {
  
  unraked_draws <- read_draws(run_id = failed_run_id, location_id = location_id, raked = FALSE)
  draw_names <- paste0('draw_',0:(draws_count-1))

  
  df_zeros <- copy(unraked_draws)
  df_zeros[, (draw_names) := 0]
  df_zeros <- df_zeros[year_id < intro_year]

  unraked_draws[, ysi := year_id - year_start]
  unraked_draws[, year_id := intro_year + ysi]
  unraked_draws <- unraked_draws[year_id <= year_end]
  unraked_draws[, ysi := NULL]

  unraked_draws <- rbind(df_zeros, unraked_draws)

  
  unraked_draws_intro_applied <- apply_draw_range_check(unraked_draws)

  
  fwrite(unraked_draws_intro_applied, file = file.path(me_outdir, paste0(location_id, ".csv")))
}
