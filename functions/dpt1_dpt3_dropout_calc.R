#----HEADER-------------------------------------------------------------------------------------------------------------
#' Author:  USERNAME
#' Date:    July 2020
#' Path:    file.path(code_root, "FILEPATH/dpt1_dpt3_dropout_calc.R")
#' Purpose: Step 1/5: Calculate dropout from admin data using paired DTP1/DTP3 observations (prior to any bias adjustment)
#'          Product of script: input data file of absolute DTP1-DPT3 dropout differences for use in ST-GPR model
#' Notes:   This script picks up in the middle of modeling "prep_exp.r" script before formal dpt12_cond indicator calculation
#***********************************************************************************************************************



#----TASK---------------------------------------------------------------------------------------------------------------

prep.dpt.dropout <- function(df, locations=locs, me="vacc_dpt1_dpt3_admin_dropout") {
  
  message("Prepping vacc_dpt1_dpt3_admin_dropout")
  
  # 1. Calculate absolute dropout from admin data using paired DTP1/DTP3 observations (prior to any bias adjustment)
  
  # Subset main df down to actual frame of interest
  dpt <- copy(df)[me_name %in% c("vacc_dpt1", "vacc_dpt3") & cv_admin==1 & !is.na(data)][,.(me_name, nid, ihme_loc_id, year_id, survey_name, data)]
  
  dpt_wide <- data.table::dcast(dpt, ... ~ me_name, value.var = "data", fun.aggregate = length)  
  # only keep rows where there is paired dpt1 and dpt3 
  dpt_wide <- dpt_wide[vacc_dpt1 != 0 & vacc_dpt3 != 0][, match := paste0(nid, ihme_loc_id, year_id)]
  
  # make same match column in 'dpt'
  dpt_data <- dpt[, match := paste0(nid, ihme_loc_id, year_id)][match %in% unique(dpt_wide$match)]
  
  # cast paired data to be used as input data in ST-GPR
  dpt_data <- data.table::dcast(dpt_data, ... ~ me_name, value.var = "data")

  # Calcuate the dropout difference, which will actually get modeled
  dpt_data[, dropout := vacc_dpt1-vacc_dpt3]
  
  # Add / clean up columns needed for ST-GPR
  dpt_data[, `:=` (vacc_dpt1=NULL, vacc_dpt3=NULL, survey_name=NULL, me_name=me, measure_id=19, age_group_id=22, sex_id=3, is_outlier=0, sample_size=NA)]  
  dpt_data <- merge(dpt_data, locations[,.(ihme_loc_id, location_id)], by="ihme_loc_id", all.x=T)[, ihme_loc_id := NULL]
  setnames(dpt_data, "dropout", "val")
  
  dpt_data[, variance := val * (1 - val) / 50]  
  # if val==0 so binomial variance calculation==0, change so that variance for these rows is median of non-0 variances in dataset
  dpt_data[variance==0, variance := quantile(dpt_data[variance != 0]$variance, 0.5)]
  
  # Return cleaned dpt_data dataset ready for ST-GPR other than variance calculation
  return(dpt_data)
  
}

#***********************************************************************************************************************

