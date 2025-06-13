#' @title Wait on ST-GPR

#' @description takes a list of run_ids (from register_stgpr_model) and holds until they are


#' @param run_ids List of run_ids (RUNS)

#' @return NULL

#' @concept utilities
wait_on_stgpr <- function(run_ids) {

  
  message("Checking for jobs still running...")
  lapply(run_ids, function(x) {
    job_hold(job_pattern = x)
  })
  message("All models supposedly done!")

  
  
  any_failures <- F
  lapply(run_ids, function(x) {
    if (get_model_status(x) != 1) {
      message(paste0('run_id ', x, ' broke! Find error before continuing!'))
      any_failures <- T
    }
  })
  if (any_failures) stop()

  print('Runs finished!')

}
