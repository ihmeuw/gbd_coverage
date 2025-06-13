#' @title Save draws



#' @param df A data.table to save, with a location_id column
#' @param me A character string specifying the vaccine of interest.
#' @param root File path to the save location

#' @return NULL, saves files in root/me by location_id

#' @concept prep_draws_individual_functions
save_draws_general <- function(df, me, root) {

  path <- file.path(root, me)
  message("Unlinking ", path, " before saving draws.")
  unlink(path, recursive=TRUE)
  dir.create(path, showWarnings=FALSE)
  location_ids <- unique(df$location_id)
  ncores <- Sys.getenv("OMP_NUM_THREADS")
  if (is.null(ncores) || ncores==""){
    ncores <- 2
  }
  invisible(mclapply(location_ids, function(x) {
    fwrite(df[location_id==x], paste0(path, "/", x, ".csv"), row.names=FALSE)
  }, mc.cores=ncores))

}
