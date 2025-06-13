






#' @param config_path [path] where does the config file live? Defined in vaccination_pipeline.r

#' @return (invisible) list of objects _retained_ after this functions runs
clean_pipeline_environment <- function(.config_path = config_path, verbose = FALSE) {
  
  validate_config_file(step = "pipeline", config_path = .config_path)
  config               <- yaml::read_yaml(.config_path)
  config_pipeline_step <- config[["pipeline"]]
  obj_to_keep_config   <- names(config_pipeline_step)
  
  
  pipeline_functions_directory <- file.path(DescTools::SplitPath(config_path)$dirname, "vaccination_pipeline_functions")
  pipeline_function_filenames  <- list.files(pipeline_functions_directory, full.names = TRUE)
  
  pipeline_function_filenames  <- grep("[rR]$", pipeline_function_filenames, value = TRUE)
  
  pipeline_functions           <- unlist(lapply(pipeline_function_filenames, get_functions_from_file))
  pipeline_function_names      <- names(pipeline_functions)
  
  
  list_to_keep <- list(
    
    pipeline_constants = obj_to_keep_config
    
    , paths = c(
      "code_root"
      , "config_path"
    )
    
    , functions = c(
      pipeline_function_names
      
      
    )
    
  )
  
  objects_to_keep   <- unlist(list_to_keep)
  objects_to_remove <- setdiff(ls(envir = globalenv()), objects_to_keep)
  rm(list = c(objects_to_remove), envir = globalenv())

  if(verbose) {
    message("Retaining these objects:\n")
    return(list_to_keep)
  }
  
  invisible(list_to_keep)
  
}






#' @param filename [path] path to a .r file that contains functions

#' @return [list] list of parsed functions, available for use
get_functions_from_file <- function(filename){
  function_name = function (expr) {
    as.character(expr[[2L]])
  }
  
  is_assign = function (expr) {
    is.call(expr) && as.character(expr[[1L]]) %in% c('=', '<-', 'assign')
  }
  
  is_function = function (expr) {
    if (! is_assign(expr)) return(FALSE)
    value = expr[[3L]]
    is.call(value) && as.character(value[[1L]]) == 'function'
  }
  
  file_parsed = parse(filename)
  functions = Filter(is_function, file_parsed)
  function_names = unlist(Map(function_name, functions))
  
  
  tmp_env <- new.env()
  sys.source(file = filename
             , envir = tmp_env)
  fun_list <- lapply(function_names, env = tmp_env, function(fname, env){
    get(fname, envir = tmp_env)
  })
  
  names(fun_list) <- function_names
  
  return(fun_list)
}
