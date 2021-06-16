#----HEADER-------------------------------------------------------------------------------------------------------------
#' Author:  USERNAME
#' Date:    July 2020
#' Path:    file.path(code_root, "FILEPATH/dpt1_dpt3_dropout_integrate.R")
#' Purpose: Step 3/5: Use the ST-GPR results to impute DTP1 coverage in early years while propagating uncertainty (prior to any bias adjustment)
#'          Product of script: adds rows of fake DTP1 admin data pre-bias adj. to working input data df
#' Notes:   This script picks up in the middle of modeling "prep_exp.r" script before formal dpt12_cond indicator calculation
#***********************************************************************************************************************



#----TASK---------------------------------------------------------------------------------------------------------------

integrate.dpt.dropout <- function(df, locations=locs, runs=RUNS) {
  
  # pull raked estimates based on RUNS run_id
  source("FILEPATH/utility.r")
  dropout <- rbindlist(lapply(runs, function(x) model_load(x, obj="raked") %>% data.table %>% .[, `:=` (run_id=x)])) 
  message("Just pulled dropout adjustment model results!")
  
  # merge on location information 
  dropout <- merge(dropout, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
  
  # pull out corresponding dpt3 rows from df to apply adjustment to
  dpt3_df <- df[me_name=="vacc_dpt3" & cv_admin==1]
  dpt3_df <- merge(dpt3_df, dropout, by=c("ihme_loc_id", "year_id", "age_group_id", "sex_id"), all.x=T)
  
  # make new 'data' column by adding dropout percent to dpt3 data value
  fake_dpt1 <- dpt3_df[, fake_data := data + gpr_mean][,.(ihme_loc_id, year_id, me_name, nid, survey_name, cv_admin, fake_data)]
  fake_dpt1 <- fake_dpt1[, me_name := "vacc_dpt1"]
  
  # merge in fake dpt1 admin data, matching to existing vacc_dpt1 admin rows where possible, or else adding new rows
  df <- merge(df, fake_dpt1, by=c("ihme_loc_id", "year_id", "me_name", "nid", "survey_name", "cv_admin"), all.x=T, all.y=T)
  
  # if me_name vacc_dpt1 exists and has a value in either 'data' or 'cv_outlier', don't use fake data
  df <- df[(is.na(data) & is.na(cv_outlier)) &   
             me_name=="vacc_dpt1" & cv_admin==1 & !is.na(fake_data) & year_id <= 2000, data := fake_data]
  
  # get rid of fake_data column entries where not using that value
  df <- df[(data != fake_data | is.na(data)) &   
             me_name=="vacc_dpt1" & cv_admin==1 & !is.na(fake_data), fake_data := NA] 
  
  
  # assign fake data a NID
  df[me_name=="vacc_dpt1" & data==fake_data & cv_admin==1 & !is.na(fake_data) & year_id <= 2000, `:=` (nid=203321, cv_admin=1)]  
  
  # all done!
  message("All done integrating dropout model results and imputing DPT1 admin pre-2000")
  return(df)    
  
}


