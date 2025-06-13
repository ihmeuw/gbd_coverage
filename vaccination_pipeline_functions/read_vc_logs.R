

#' @param root [path] path to the to_model directory
#' @param log_type [chr] type of log file to read

#' @return
#' @export

#' @examples
read_vc_logs <- function(root = to_model_dir, log_type){
   
   valid_log_types <- c(
      "stgpr_runs"
      , "stgpr_quotas"
   )
   
   
   stopifnot(dir.exists(root))
   
   if(!log_type %in% valid_log_types){
      stop("Enter a valid log type, options:\n", toString(valid_log_types))
   }
   
   
   message("Reading log: ", log_type, " from ", root)
   log <- switch(
      log_type
      , "stgpr_runs" = data.table::fread(file.path(root, "log_stgpr_run_ids.csv"), fill = TRUE)
      , "stgpr_quotas" = data.table::fread(file.path(root, "log_stgpr_quotas.csv"), fill = TRUE)
   )
   
   if(nrow(log) == 0) {
      stop("No rows found in ", log_type, " log in ", file.path(root))
   }
   
   return(log)
}


