









#' @param workflow [env] created by jobmon tool
#' @param start_time [POSIXct] format: "2023-12-18 15:25:11"
#' @param tasks [list] created by `jobmonr::task`
#' @param log_dir [path] datetime versioned output folder
#' @param status [chr] single letter Jobmon workflow stats (success = "D")
#' @param jobmon_env_root [path] folder containing jobmon temporary environment to remove
#' @param combine_logs [lgl] combine std_err logs into a single fullrun.out output log
#' @param delete_empty_logs [lgl] if logs are size 0kb, delete?
#' @param delete_temp_env [lgl] if you created an temporary environment for you jombon process, delete it?
#' @param verbose [lgl] messages about task resource consumption (Dec 2023 buggy)

#' @return [std_err] user messages, and file deletion/creation
tidy_after_jobmon <-
  function(
    workflow,
    start_time,
    tasks,
    log_dir,
    status,
    jobmon_env_root   = NULL,
    combine_logs      = TRUE,
    delete_empty_logs = TRUE,
    delete_temp_env   = TRUE,
    verbose           = FALSE
  ) {
    
    
    if(verbose){
      for(task in tasks){
        message(paste0("task: ", task$name, " used: ", get_maxrss(task$name, run_time), " and took: ", get_elapsed_time(task$name, run_time), "\n"))
      }
    }
    
    
    if(delete_empty_logs)  {
      deleted_files <- system2(SYSTEM_COMMAND), "-size", "0", "-delete", '-print'), stdout=TRUE)
    }
    
    error_files <- system2(SYSTEM_COMMAND), "|", "sed", "'s|\\(.*\\)\\..*|\\1|'"), stdout=TRUE)
    error_files <- unique(error_files)
    if(!is.null(error_files) && length(error_files) > 0) {
      if (status == "D"){
        message(paste0("The following tasks might have failed silently, check the error logs! \n"))
      } else{
        message(paste0("The following tasks have failed, check the error logs! \n"))
      }
      message(paste0(error_files, "\n"))
    }
    
    
    if(combine_logs){
      task_log_files <- list()
      for(task in rev(tasks)){
        
        if(task$name %in% error_files) break
        task_log_files <- c(task_log_files, file.path(log_dir, "output", paste0(task$name, ".out")))
      }
      
      path_combined_logs <- file.path(log_dir, "output", "fullrun.out")
      system2(SYSTEM_COMMAND)), stdout = path_combined_logs, stderr = NULL)
      message("Concatenated all the log files, find at ", path_combined_logs)
      message("||---- Use for debugging - includes all user-defined messages from modeling scripts.\n")
    }
    
    
    if(delete_temp_env){
      if(is.null(jobmon_env_root)) stop("Must provide `jobmon_env_root` to delete temp_env folder - this step deletes the entire `jobmon_env_root` directory.")
      unlink(jobmon_env_root, recursive = TRUE)
      message("Deleted temp_env from ", jobmon_env_root, "\n")
    }
    
    message(paste0("\nWorkflow id: ", workflow$workflow_id))
    runtime <- round(Sys.time() - start_time, 2)
    runtime_units <- attr(runtime, "units")
    message(paste("Total runtime =", runtime, runtime_units), "\n")
    
  }
