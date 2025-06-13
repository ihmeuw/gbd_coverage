#' @title Check STGPR model success

#' @description Return TRUE if stgpr run id finished successfully, else FALSE
#' @param run_id STGPR model run_id
check_stgpr_model_success <- function(run_id) {
  model_complete_file_path <- file.path("FILEPATH", run_id, "model_complete.csv")
  return(file.exists(model_complete_file_path))
}
