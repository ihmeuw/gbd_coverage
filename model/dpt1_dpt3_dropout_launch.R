#----HEADER-------------------------------------------------------------------------------------------------------------
#' Author:  USERNAME
#' Date:    July 2020
#' Path:    file.path(code_root, "FILEPATH/dpt1_dpt3_dropout_launch.R")
#' Purpose: Step 2/5: Launch ST-GPR model of prepped DTP1/DPT3 dropout
#'          Product of script: Launched ST-GPR model from main config and saved run_ids to separate dropout run_id tracking csv
#' Notes:   This script picks up in the middle of modeling "prep_exp.r" script before formal dpt12_cond indicator calculation
#***********************************************************************************************************************



#----CONFIG-------------------------------------------------------------------------------------------------------------

message("Launching vacc_dpt1_dpt3_admin_dropout")

### clean workspace and load special functions
model_root <- code_root  
setwd(model_root)
source("init.r")
central_root <- "FILEPATH/stgpr/"
setwd(central_root)
source("FILEPATH/register.R")
source('FILEPATH/sendoff.R')

### set user
username <- Sys.info()[["user"]]

### load config
config_path <- file.path(ref_data_repo, "FILEPATH/vaccination_model_db.csv")
  
config_file <- fread(config_path)[is_best==1 & round==gbd_cycle & gbd_round_id==gbd_round & (is.na(bundle_id) | bundle_id==""), ]

if (config_file[duplicated(me_name), ] %>% nrow > 0 ) stop(paste0("BREAK | You marked best multiple models for the same me_name: ", 
                                                                  toString(config_file[duplicated(me_name), me_name])))
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### prep for data upload
data_notes  <- NOTES
mark_best   <- 0
RUNS        <- NULL
#***********************************************************************************************************************


#----LAUNCH-------------------------------------------------------------------------------------------------------------
### settings
cluster_project <- "proj_cov_vpd"
nparallel       <- 50                                       

### launch dropout model
me_name     <- "vacc_dpt1_dpt3_admin_dropout"
data_path   <- file.path(to_model_dropout_dir, "vacc_dpt1_dpt3_admin_dropout.csv")
my_me_name  <- copy(me_name)
my_model_id <- config_file[me_name==my_me_name & is_best==1, model_index_id]
#***********************************************************************************************************************


#----RUN MODEL----------------------------------------------------------------------------------------------------------
### launch model
run_id <- register_stgpr_model(path_to_config = config_path,
                               model_index_id = my_model_id)

stgpr_sendoff(run_id=run_id, project=cluster_project, nparallel=nparallel)  
#***********************************************************************************************************************


#----SAVE LOG-----------------------------------------------------------------------------------------------------------
### save log to gpr log file
logs_path <- file.path(ref_data_repo, "FILEPATH/dropout_run_log.csv")
print_date <- gsub("-", "_", date) %>% as.character
logs_df   <- data.frame("date"         = format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%m_%d_%y_%H%M"),
                        "me_name"      = me_name, 
                        "my_model_id"  = my_model_id, 
                        "run_id"       = run_id, 
                        "data_path"    = data_path, 
                        "is_best"      = mark_best,
                        "notes"        = data_notes,
                        "status"       = "", 
                        "date_version" = print_date)  
logs_file <- fread(logs_path) %>% rbind(., logs_df, fill=TRUE)
fwrite(logs_file, logs_path, row.names=FALSE)
print(paste0("Log file saved for ", me_name, " under run_id ", run_id[[1]]))
#***********************************************************************************************************************

RUNS <- c(RUNS, run_id)
  
