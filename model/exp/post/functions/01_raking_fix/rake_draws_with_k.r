#' @title Rake draws with k
#' @description Given a data table of unraked draws of all subnationals for




#' @param unraked_draws unraked draws of all subnationals for

#' @param k raking factor to apply to draws

#' @concept logit_raking
rake_draws_with_k <- function(unraked_draws, k) {

  draw_columns     <- names(unraked_draws)[grepl("draw", names(unraked_draws))]
  metadata_columns <- names(unraked_draws)[!grepl("draw", names(unraked_draws))]

  unraked_draw_matrix <- as.matrix(unraked_draws[, (draw_columns), with = FALSE])
  draw_metadata       <- unraked_draws[, (metadata_columns), with = FALSE]

  if (any(unraked_draw_matrix < 0 | unraked_draw_matrix > 1 )) {
    stop("Unraked values outside of [0-1] range - double check prior to attempting logit raking")
  }

  one_idx  <- which(unraked_draw_matrix == 1)
  zero_idx <- which(unraked_draw_matrix == 0)

  non_one_zero_idx <- 1:length(unraked_draw_matrix)
  non_one_zero_idx <- non_one_zero_idx[!(non_one_zero_idx %in% c(one_idx, zero_idx))]

  raked_draw_matrix <- matrix(nrow = nrow(unraked_draw_matrix),
                              ncol = ncol(unraked_draw_matrix))

  
  raked_draw_matrix[non_one_zero_idx] <- ilogit(logit(unraked_draw_matrix[non_one_zero_idx]) + k)


  if (length(one_idx) > 0) {
    raked_draw_matrix[one_idx] <- 1
  }

  if (length(zero_idx) > 0) {
    raked_draw_matrix[zero_idx] <- 0
  }

  
  raked_draws <- as.data.table(raked_draw_matrix)
  names(raked_draws) <- draw_columns
  raked_draws <- cbind(draw_metadata, raked_draws)

  return(raked_draws)
}
