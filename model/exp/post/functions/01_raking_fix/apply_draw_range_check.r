#' @title Apply draw range check

#' @description Flag draws less than 0 or more than 1 and force to


#' @param draws Data table of draws

#' @return modified `draws`
apply_draw_range_check <- function(draws) {

  
  draw_columns     <- names(draws)[grepl("draw", names(draws))]
  metadata_columns <- names(draws)[!grepl("draw", names(draws))]

  draw_matrix   <- as.matrix(draws[, (draw_columns), with = FALSE])
  draw_metadata <- draws[, (metadata_columns), with = FALSE]

  
  if(any(draw_matrix < 0)) draw_matrix[which(draw_matrix < 0)] <- 0
  if(any(draw_matrix > 1)) draw_matrix[which(draw_matrix > 1)] <- 1

  
  draws        <- as.data.table(draw_matrix)
  names(draws) <- draw_columns
  raked_draws  <- cbind(draw_metadata, draws)
}
