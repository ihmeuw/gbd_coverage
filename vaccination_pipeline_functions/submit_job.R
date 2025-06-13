CLUSTER_NAME




#' @param script_path [path] full path to submitted script
#' @param threads [chr] cluster resource requirement
#' @param mem_G [chr] cluster resource requirement
#' @param runtime_min [chr] cluster resource requirement
#' @param archiveTF [lgl] (default FALSE) do you need an archive node?
#' @param job_name [chr] Will be name of script if NULL
#' @param partition [chr] a.k.a. 'queue' - cluster resource requirement
#' @param Account [chr] a.k.a. 'project' - cluster resource requirement
#' @param language [chr] coding language for job (see valid_langs validation)
#' @param shell_script_path [path] path to shell script (language-specific)
CLUSTER_NAME
CLUSTER_NAME
#' @param console_style_log_tf [lgl] if TRUE, combine std_err and std_out into one log in the std_out_root
#' @param r_image [chr] (default "latest.img") 
#' @param args_list [list, chr] optional list() of arguments, e.g. list(arg1 = arg1, arg2 = arg2)
#' @param verbose [lgl] print submission command and job_id
#' @param v_verbose [lgl] print log paths
#' @param dry_runTF [lgl] (default FALSE) if TRUE, only print submission command, no job submission, return 0L

#' @return [int] cluster job_id for submitted job, or 0L if dry_runTF == TRUE
#' @export
submit_job <- function(
    script_path          = NULL, 
    threads              = "1", 
    mem_G                = "10G", 
    runtime_min          = "20", 
    archiveTF            = TRUE,  
    job_name             = NULL, 
    partition            = "all.q", 
    Account              = NULL, 
    language             = "R",
    shell_script_path    = NULL, 
    std_err_root         = file.path("FILEPATH", Sys.getenv()["USER"], "error"),
    std_out_root         = file.path("FILEPATH", Sys.getenv()["USER"], "output"),
    console_style_log_tf = FALSE,
    r_image              = NULL,  
    args_list            = NULL,
    verbose              = TRUE,
    v_verbose            = FALSE,
    dry_runTF            = FALSE
) {
  
  
  
  valid_langs     <- c("r")
  valid_langs_msg <- paste0(valid_langs, collapse = ", ")
  if(is.null(language)) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  language        <- tolower(language)
  if(!language %in% valid_langs) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  
  if(is.null(script_path)) stop("Please define a valid script path to submit")
  if(is.null(Account))     stop("Please define a Slurm Account e.g. ")
  if(is.null(partition))   stop("Please define a Slurm partition e.g. all.q")
  if(is.null(threads))     stop("Please define a number of threads")
  if(is.null(mem_G))       stop("Please define a memory requirement e.g. '30G' or '300M'")
  if(is.null(runtime_min)) stop("Please define a runtime requirement")
  stopifnot(is.logical(console_style_log_tf))
  stopifnot(is.logical(archiveTF))
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(v_verbose))
  stopifnot(is.logical(dry_runTF))
  
  dir.create(std_err_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(std_out_root, recursive = TRUE, showWarnings = FALSE)
  
  
  if (is.null(job_name)) {
    script_path_decon <- unlist(strsplit(script_path, "[/.]"))
    job_name          <- script_path_decon[length(script_path_decon) - 1]
  }
  
  
  if(language == "r") {
    
    if(is.null(r_image)) {
      r_image_cmd <- "-i FILEPATH/latest.img"
    } else {
      r_image_cmd <- paste0("-i ", r_image)
    }
    
    if(is.null(shell_script_path)) {
      shell_script_path <- "FILEPATH/execRscript.sh"
    }
    
  } 
  
  
  if(console_style_log_tf){
    std_err_path <- file.path(std_out_root, "%x_%j_console.log")
    std_out_path <- file.path(std_out_root, "%x_%j_console.log")    
  } else{
    std_err_path <- file.path(std_err_root, "%x_e%j.log")
    std_out_path <- file.path(std_out_root, "%x_o%j.log")
  }
  archive_cmd  <- ifelse(archiveTF, " -C archive", "")
  
  
  if(!is.null(args_list)){
    if(!is.list(args_list)) stop("args_list must be a named list")
    if(is.null(names(args_list))) stop("args_list must be a named list")
    if(any(nchar(names(args_list)) == 0)) stop("args_list must be a named list")
    
    names(args_list) <- gsub("^--", "", names(args_list))
    
    names(args_list) <- paste0("--", names(args_list))
  }
  
  
  command <- paste0(
    "sbatch",
    " -J ",    job_name,
    "",        archive_cmd,
    " --mem=", mem_G,
    " -c ",    threads,
    " -t ",    runtime_min,
    " -p ",    partition,
    " -A ",    Account,
    " -e ",    std_err_path,
    " -o ",    std_out_path, 
    " "   ,    shell_script_path,
    " "   ,    r_image_cmd,
    " -s ",    script_path
  )
  
  
  for (arg_name in names(args_list)) {
    command <- paste(command, arg_name, args_list[arg_name])
  }
  
  if(dry_runTF) {
    message(command, "\n")
    return(0L)
  }
  
  submission_return <- system(SYSTEM_COMMAND)
  job_id <- regmatches(submission_return, gregexpr("\\d+$", submission_return))
  
  if(length(job_id) > 1) warning("job_id from submitted job '",  job_name ,"' is longer than 1, inspect before use.")
  job_id <- as.integer(unlist(job_id))
  
  if(verbose) message(paste("\n", submission_return, " : ", job_name, "\n"))
  if(v_verbose) message("Logs saved to: \n", paste0(unique(c(std_out_path, std_err_path)), collapse = "\n"), "\n")
  
  return(job_id)
  
}


CLUSTER_NAME

















CLUSTER_NAME


















CLUSTER_NAME



CLUSTER_NAME
#' @param initial_sleep_sec [int] how long to sleep before initial check for jobs on cluster
#' @param cycle_sleep_sec [int] how long to wait between checks
#' @param filter_by [chr] vector of sacct fields to search e.g. `c("User", "JobName")` (case insensitive)
#' @param filter_regex [regex] required if `filter_by %in% c("JobName", "Account")`
#' @param break_on_failure [lgl] if _any_ of your jobs fail, should this function break?
#' @param dryrun [lgl] return a list of commands built by this function, but do not wait on jobs

#' @return [std_out/std_err] std_out for sleep cycle duration & successful ending, std_err printing failed job ids
wait_on_slurm_job_id <- function(
    job_id,
    initial_sleep_sec = 30,
    cycle_sleep_sec   = 30,
    filter_by         = c("jobidraw"), 
    filter_regex      = NULL, 
    break_on_failure  = FALSE,
    dryrun            = FALSE
) {
  
  
  format_list <- list(
    state      = "State%16"
    , jobid    = "JobID%20"
    , jobidraw = "JobIDRaw%20"
    , user     = "User%12"
    , jobname  = "JobName%50"
    , account  = "Account%20"
  )
  names(format_list) <- tolower(names(format_list))
  formatting_str    <- paste0("--format=", paste(format_list, collapse = ","))
  if(!names(format_list)[[1]] == "state") stop("state must be first `format_list` object element")
  
  irrelevant_fields <- c("state", "jobid")
  regex_req_fields  <- c("jobname", "account")
  
  filter_by     <- unique(tolower(filter_by))
  valid_filters <- setdiff(names(format_list), irrelevant_fields)
  
  
  if(!all(filter_by %in% valid_filters)) stop("filter_by must be one of: ", paste(c(valid_filters, "NULL"), collapse = ", "))
  if(is.null(filter_regex) && any(regex_req_fields %in% filter_by)) stop("must define filter_regex for: ",
                                                                         paste(regex_req_fields, collapse = ", "))
  regex_user_fields <- filter_by[filter_by %in% regex_req_fields]
  if(length(regex_user_fields) > 1) warning("More than one field depends on filter_regex - this may affect  results: ",
                                            paste(regex_user_fields, collapse = ", "))
  
  filter_by <- c("state", unique(tolower(filter_by)))
  
  start.time <- proc.time()
  Sys.sleep(initial_sleep_sec)
  
  job_id_regex_raw          <- paste(job_id, collapse = "|")
  job_id_regex_or_quoted    <- paste0("'", job_id_regex_raw, "'")
  job_id_regex_comma_quoted <- paste0("'", paste(job_id, collapse = ","), "'")
  
  
  filter_regex_list <- lapply(filter_by, function(flt){
    switch(
      flt
      , "state"    = "'.*'" 
      
      , "jobidraw" = job_id_regex_or_quoted
      , "user"     = Sys.info()[["user"]]
      , "jobname"  = filter_regex
      , "account"  = filter_regex
    )
  })
  names(filter_regex_list) <- filter_by
  filter_idx               <- which(names(format_list) %in% filter_by)
  filter_idx_str           <- paste(filter_idx, collapse = ",")
  
  
  filter_regex <- character()
  for(rgx in filter_regex_list){
    filter_regex <- paste0(filter_regex, " | grep -P ", rgx)
  }
  
  
  
  
  
  
  
  
  filter_str <- paste0(
    "| tail -n +3 ",
    "| grep -vP 'batch|extern' ",
    "| sed 'FILEPATH' ", 
    "| cut -d' ' -f", filter_idx_str, 
    filter_regex
  )
  
  
  cmd_base <- paste("sacct -j", job_id_regex_comma_quoted, formatting_str, filter_str)
  cmd_pass <- paste0(cmd_base, " | grep -P  'RUNNING|PENDING'")
  cmd_fail <- paste0(cmd_base, " | grep -vP 'RUNNING|PENDING|COMPLETED'")
  
  if(dryrun) return(list(cmd_base = cmd_base, cmd_pass = cmd_pass, cmd_fail = cmd_fail))
  
  if(!length(suppressWarnings(system(SYSTEM_COMMAND)))) stop ("No jobs found: ", cmd_base)
  
  
  if(break_on_failure){
    fail_chk    <- suppressWarnings(system(SYSTEM_COMMAND))
    if(length(fail_chk)){
      failed_jobs <- paste(unique(regmatches(fail_chk, regexpr(job_id_regex_raw, fail_chk))), collapse = ", ")
      if(length(fail_chk)) stop('there is a failure among the launched jobs, investigate:\n', failed_jobs)
    }
  }
  
  message("Waiting on submitted jobs - seconds elapsed:")
  
  
  while(length(suppressWarnings(system(SYSTEM_COMMAND))) > 0 ) {
    Sys.sleep(cycle_sleep_sec)
    message(round((proc.time() - start.time)[[3]],0))
    
    
    if(break_on_failure){
      fail_chk    <- suppressWarnings(system(SYSTEM_COMMAND))
      if(length(fail_chk)){
        failed_jobs <- paste(unique(regmatches(fail_chk, regexpr(job_id_regex_raw, fail_chk))), collapse = ", ")
        if(length(fail_chk)) stop('there is a failure among the launched jobs, investigate:\n', failed_jobs)
      }
    }
  }
  
  
  job.runtime <- proc.time() - start.time
  job.runtime <- round(job.runtime[3], 0)
  
  
  job_id_msg <- paste(job_id, collapse = ", ")
  message(paste0("Job(s) ", job_id_msg, " no longer PENDING, RUNNING, or FAILED. Time elapsed: ", job.runtime, " seconds"))
}

