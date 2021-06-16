#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    October 2017
# Purpose: Run ST-GPR for vaccinations
# Run:     source("FILEPATH/launch_gpr.r", echo=TRUE)
#***********************************************************************************************************************


#----CONFIG-------------------------------------------------------------------------------------------------------------
### clear memory
#rm(list=ls())
username <- Sys.info()[["user"]]

### clean workspace and load special functions
model_root <- code_root  
setwd(model_root)
source("init.r")
central_root <- "FILEPATH/"
setwd(central_root)
source("FILEPATH/register.R")
source('FILEPATH/sendoff.R')

### set date
if (!exists("date"))         date         <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")
if (!exists("to_model_dir")) to_model_dir <- paste0(data_root, "/FILEPATH/", date)

### load config
config_path <- file.path(ref_data_repo, "FILEPATH/vaccination_model_db.csv")
config_file <- fread(config_path)[is_best==1 & round==gbd_cycle & gbd_round_id==gbd_round & (is.na(bundle_id) | bundle_id==""), ]
if (config_file[duplicated(me_name), ] %>% nrow > 0 ) stop(paste0("BREAK | You marked best multiple models for the same me_name: ", 
                                                                  toString(config_file[duplicated(me_name), me_name])))
RUNS <- NULL
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### prep for data upload
all_me_name     <- mes_to_launch 
all_my_model_id <- NULL
all_my_model_id <- lapply( 1:length(all_me_name), function(x) c(all_my_model_id, config_file[me_name==all_me_name[x], model_index_id]) )
all_data_notes  <- rep(c(NOTES), length(all_me_name))
#***********************************************************************************************************************


#----LAUNCH-------------------------------------------------------------------------------------------------------------
### settings
holdouts        <- 0 
ko_pattern      <- "country"
cluster_project <- "proj_cov_vpd"
draws           <- 1000                                     
nparallel       <- 50                                       
slots           <- 5
master_slots    <- 5
logs            <- file.path("FILEPATH/", username)   

### batch launch vaccination models
for (xx in 1:length(all_me_name)) {  
  
me_name     <- all_me_name[xx]
my_model_id <- all_my_model_id[[xx]]
data_notes  <- all_data_notes[xx]  
mark_best   <- 0
data_path   <- file.path(to_model_dir, paste0(me_name, ".csv"))
#***********************************************************************************************************************


#----RUN MODEL----------------------------------------------------------------------------------------------------------
### register data
message("|||"); message("|||"); message(paste0("Starting launch for ", me_name))
  run_id <- register_stgpr_model(path_to_config = config_path,
                                 model_index_id = my_model_id)
  stgpr_sendoff(run_id=run_id, project = cluster_project, nparallel=nparallel)
#***********************************************************************************************************************


#----SAVE LOG-----------------------------------------------------------------------------------------------------------
### save log to gpr log file
logs_path <- file.path(ref_data_repo, "FILEPATH/vaccination_run_log.csv")
  
logs_df   <- data.frame("date"         = format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M"), 
                        "me_name"      = me_name, 
                        "my_model_id"  = my_model_id, 
                        "run_id"       = run_id[[1]],  
                        "data_id"      = "",  
		                "model_id"     = "", 
		                "data_path"    = data_path, 
		                "is_best"      = 0,
		                "notes"        = data_notes,
		                "status"       = "")

logs_file <- fread(logs_path) %>% rbind(., logs_df, fill=TRUE)
fwrite(logs_file, logs_path, row.names=FALSE)
message(paste0("Log file saved for ", me_name, " under run_id (", run_id[[1]], ")")); message("|||"); message("|||")  
#***********************************************************************************************************************

RUNS <- c(RUNS, run_id)

}
