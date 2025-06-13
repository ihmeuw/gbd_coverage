version_register_update <- function(
    root
    , run_date
    , register_name = "version_register.yaml"
    , datestamp = gsub("-", "_", Sys.Date())
) {
  
  
  
  
  
  
  
  
  
  
  
  #' @param root [path] root to folder where the version register will be placed
  #' @param run_date [chr] config param e.g. "2023-11-02" or "2023-11-02-run_comment"
  #' @param register_name [chr] a `.yaml` file name
  #' @param datestamp [chr] what date was an update made?
  
  
  
  #' @return [std_err] user message
  
  fname <- file.path(root, register_name)
  if(!grepl("\\.yaml", fname)) stop("Only .yaml files supported.")
  
  if(!file.exists(fname)){
    file.create(fname)
    Sys.chmod(fname, mode = "0777")
    cat("best_updates:\n", file = fname)
  }
  
  version_register <- yaml::read_yaml(fname)
  item_name        <- paste0("update_on_", datestamp)
  
  version_register$best_updates[[item_name]] <- run_date
  
  yaml::write_yaml(version_register, fname)
  
  message("Updated version register:
            ", fname)
  
}