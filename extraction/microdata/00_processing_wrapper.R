






















username        <- Sys.info()[["user"]]
vaccines_repo   <- paste0("FILEPATH", username, "FILEPATH")
reference_repo  <- "FILEPATH"
extraction_root <- "FILEPATH"


R.utils::sourceDirectory(file.path(vaccines_repo, "vaccination_pipeline_functions/"))


packages_to_load_lbd <- c("proto", "findpython", "argparse", "data.table", "magrittr", "survey", "parallel", "plyr", "dplyr",
                          "rgeos", "raster", "rgdal", "dismo", "gbm", "foreign", "doParallel", "grid", "gridExtra", "gtools", "ggplot2",
                          "assertthat", "INLA", "seegSDM", "seegMBG", "pacman", "glmnet", "RMySQL", "tictoc", "binom", "sf", "fasterize") 
suppressMessages(invisible(lapply(packages_to_load_lbd, function(x) {
  message(x)
  library(x, character.only = TRUE)
})))


source(paste0(vaccines_repo, "FILEPATH/01_process.R"))






if(interactive()) {
  code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines")
  R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"))

  
  if (!exists("config_path")) config_path <- file.path(code_root, "pipeline_config.yaml")

  load_pipeline_config(step = "survey_processing", config_path = config_path)

  
  
  action <- "launch"

} else {
  
  parser <- ArgumentParser()
  parser$add_argument("--action",   help="'launch' to launch a job to process each NID listed; 'process' to actually run processing", default="launch", type="character")
  parser$add_argument("--nid",      help="NID(s) to process", type="character")
  parser$add_argument("--project",  help="prod cluster project", default="proj_geo_nodes", type="character")
  parser$add_argument("--cores",    help="how many cores?", default=4, type="integer")
  parser$add_argument("--date",     help="leave missing", default=as.character(Sys.Date()), type="character")
  parser$add_argument("--lbd",      help="logical; process for LBD?", default="FALSE", type="character")
  parser$add_argument("--gbd",      help="logical; process for GBD?", default="FALSE", type="character")

  
  args      <- parser$parse_args()
  list2env(args, environment())
  lbd       <- as.logical(lbd)
  gbd       <- as.logical(gbd)

  cat("\n"); print(as.data.table(args)); cat("\n"); rm(args)

  if(nid != "all") {
    nids      <- as.numeric(strsplit(gsub(",", " ", nid), split=" +")[[1]])
  }
}


if (nid=="all") {
  nids      <- gsub(".csv", "", gsub(".*_\\s*|_.*", "", list.files(file.path(extraction_root, "00_raw")))) %>% unique %>% as.numeric %>% sort
}






if (action == "launch") {

  start_time <- as.POSIXct(format(Sys.time()))

  
  for (nid in nids) {

    
    date <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")

    CLUSTER_NAME
    if(!dir.exists(paste0("FILEPATH", username, "FILEPATH"))) {
      dir.create(paste0("FILEPATH", username, "FILEPATH"))
    }
    if(!dir.exists(paste0("FILEPATH", username, "FILEPATH")))  {
      dir.create(paste0("FILEPATH", username, "FILEPATH"))
    }

    CLUSTER_NAME
    job_name <- paste0("CHILD_", nid, "_vax_processing")
    shell    <- "FILEPATH/execRscript.sh"
    job      <- paste0("sbatch -J ", job_name,
                       " --mem=20G",
                       " -c ", cores,
                       " -A ", project,
                       " -p long.q", 
                       
                       " -o FILEPATH", username, "FILEPATH",
                       " -e FILEPATH", username, "FILEPATH",
                       " ", shell,
                       " -s ", vaccines_repo, "FILEPATH/00_processing_wrapper.R",
                       " --action process --nid ", nid, " --project ", project,
                       " --date ", date, " --gbd ", gbd, " --lbd ", lbd)
    system(SYSTEM_COMMAND); cat(job)
  }

  
  source("FILEPATH/ubcov_tools.r")

  
  cat("\nBeginning job_hold()")
  job_hold(paste0("CHILD_", nids, "_vax*"))

  
  cat("\nBeginning lapply()")
  invisible(lapply(nids, check_nids, gbd = gbd, lbd = lbd, start_time = start_time))

} else if (action == "process") {

  
  process_successful <- process(nid)

  
  if (process_successful) {

    
    if (gbd) {
      source(paste0(vaccines_repo, "FILEPATH/02_tabulate_gbd.R"))
      tabulate_gbd(nid)
    }

    
    if (lbd) {

      
      source(paste0(vaccines_repo, "FILEPATH/02_tabulate_lbd.R"))
      tabulate_lbd(nid)

      
      source(paste0(vaccines_repo, "FILEPATH/03_disaggregate_lbd.R"))

      stems <- get_vaccine_stems(nid)
      if (any(c("dpt", "mcv", "bcg", "polio") %in% stems)) {
        disaggregate_and_resample(nid, resample_polys=TRUE, vaccine_stems=c("dpt", "mcv", "bcg", "polio"))
      }

      if (any(c("pcv", "rota", "hepb", "hib", "mcv") %in% stems)) {
        disaggregate_and_resample(nid, resample_polys=FALSE, vaccine_stems=c("dpt", "mcv", "bcg", "polio"))
      }
    }
  }
}


