# started: 2024 Jun 10 13:19:17
# purpose: launch the entire pipeline as a submitted job

r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})

if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
  code_root <- Sys.getenv()[['CODE_ROOT']] 
} else {
  
  code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
}

config_path <- file.path(code_root, "pipeline_config.yaml")
message("code_root: ", code_root)

PARAMS <- list(
  copy_pipeline_config = TRUE  
  , use_prep_config    = TRUE  
  , use_post_config    = FALSE 
)

if (all(PARAMS$copy_pipeline_config == TRUE,
        PARAMS$use_post_config      == TRUE,
        PARAMS$copy_pipeline_config == TRUE)) {
  stop("Choose ONE of either the pre or post-processing config options.")
}


if (PARAMS$copy_pipeline_config == TRUE) {
   
  source(file.path(code_root, "init.r"))
  R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), recursive = FALSE, modifiedOnly = FALSE)
  config <- se$read_file(config_path)
  
  
  if (PARAMS$use_prep_config == TRUE){
     
    to_model_config_dir  <- file.path(to_model_root, config$prep_exp$date, "config")
    config_path_to_model <- file.path(to_model_config_dir, "pipeline_config.yaml")
    if(file.exists(config_path_to_model)) archive_single_file(config_path_to_model)
    se$make_directory(to_model_config_dir, recursive = TRUE)
    file.copy(config_path, config_path_to_model, overwrite = TRUE)
    
    config_path          <- config_path_to_model
    
    metadata <- list(
      metadata      = se$build_metadata_shell(code_root)
      , init_list   = init_list
      , config_path = config_path
    )
    fpath_metadata <- file.path(to_model_config_dir, "metadata_pipeline_launch.yaml")
    if(file.exists(fpath_metadata)) archive_single_file(fpath_metadata)
    se$save_file(metadata, fpath_metadata)
    
    
  } else if (PARAMS$use_post_config == TRUE){
     
    modeled_config_dir <- file.path(modeled_root, config$prep_exp$date, "config")
    config_path_modeled <- file.path(modeled_config_dir, "pipeline_config.yaml")
    if(file.exists(config_path_modeled)) archive_single_file(config_path_modeled)
    se$make_directory(modeled_config_dir, recursive = TRUE)
    file.copy(config_path, config_path_modeled, overwrite = TRUE)
    
    config_path          <- config_path_modeled
    
  } else {
    stop("Choose ONE of either the pre or post-processing config options.")
  }
}

message("config_path: ", config_path)

se$submit_job(script_path            = file.path(code_root, "vaccination_pipeline.r")
              , threads              = 1L
              , mem                  = "1G"
              , runtime_min          = 60 * 72
              , partition            = "all.q,long.q"
              , account              = "proj_cov_vpd"
              , send_email           = TRUE
              , console_style_log_tf = TRUE
              , args_list            = list(code_root     = code_root
                                            , config_path = config_path))
