#' @title Write Message to Workflow

#' @description Creates a task that only writes a message to preserve order of messages in the final log.

#' @param message The message to write

#' @return The task object




#' @concept general_tasks
write_message_to_workflow <- function(message="") {
  
  task_name <- "send_message_"
  for(i in 1:8) task_name <- paste0(task_name, sample(9, size=1))
  return(jobmonr::task(
            task_template     = template_send_message,
            cluster_name      = "slurm",
            compute_resources = resources_task_send_message,
            name              = task_name,
            nthreads          = format(resources_task_send_message$cores, nsmall=0),
            r_shell           = r_shell,
            scriptname        = paste0(code_root, "FILEPATH/execute_general_tasks.R"),
            code_root         = code_root,
            results_root      = results_root,
            message           = paste0('"', "'", message, "'", '"'),
            output_file       = file.path(log_dir, "output", paste0(task_name, ".out"))
        ))
}
