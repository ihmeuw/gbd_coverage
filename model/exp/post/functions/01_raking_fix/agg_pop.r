#' @title agg_pop

#' @description function to aggregate by populations







#' @param draws matrix, as described above
#' @param population data.table of population with location_id, year_id, and population values

#' @concept logit_raking
agg_pop <- function(draws, population) {

  
  draws <- merge(draws,
                 population[, .(location_id, year_id, population)],
                 all.x = TRUE, by = c("location_id", "year_id"))

  
  draw_columns     <- names(draws)[grepl("draw", names(draws))]
  metadata_columns <- names(draws)[!grepl("draw", names(draws))]

  draw_matrix   <- as.matrix(draws[, (draw_columns), with = FALSE])
  draw_metadata <- draws[, (metadata_columns), with = FALSE]

  
  collapsed_draws     <- rowMeans(draw_matrix)
  raked_draw_coverage <- cbind(draw_metadata, data.table(cov = collapsed_draws))

  
  raked_draw_coverage[, count := cov * population]

  
  agg_draws <- sum(raked_draw_coverage$count)

  
  return(agg_draws)

}
