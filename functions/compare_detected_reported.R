# HEADER ------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    2019-05-31
# Project: GBD Vaccines: Compare detected stockouts (leave-one-out model) to reported stockouts (JRF indicator sheet)
# Purpose: Which model-detected stockouts are actual reported stockouts/disruptions?
# Details: 
#********************************************************************************************

### Empty the environment
# rm(list = ls())

### Set os flexibility
os <- .Platform$OS.type
if (os == "windows") {
  j_root <- "J:"
} else {
  j_root <- "FILEPATH"
}

### Load functions and helper objects
library(ggplot2)
library(magrittr)
source(paste0("FILEPATH/init.r"))
source(db_tools)
source("FILEPATH/cluster_tools.r")
source("FILEPATH/read_excel.R")
source("FILEPATH/get_location_metadata.R")
'%!in%' <- function(x,y)!('%in%'(x,y))

me.db <- file.path(ref_data_repo, "me_db.csv") %>% fread  

temp_root <- "FILEPATH/"


compare_detected_reported <- function(ratios=FALSE) {

if (!ratios) {
  
  modeling  <- c("vacc_bcg", "vacc_polio", "vacc_dpt", "vacc_hepb", "vacc_hib", "vacc_mcv", "vacc_pcv", "vacc_rotac", "vacc_rcv")
  
  if (!exists("date")) { date <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d") }
  
  #############################################################################################
  ###                       PREP STOCKOUT FILES TO COMPARE                                  ###
  #############################################################################################
  ## Read in prepped files
  predicted <- readRDS(paste0(data_root, "FILEPATH/detected_stockouts.rds")) 
  reported <- readRDS(file.path(ref_data_repo, "who_reported_stockouts.rds"))  
  
  ## Further cleaning of 'predicted' sheet to match format of 'reported'
  # copy vaccine with dose into new column
  predicted[, cv_stockout_dose := me_name]
  # drop dose number from me_name
  predicted$me_name <- gsub("[0-9]+", "", predicted$me_name)
  # setnames from me to me_name
  setnames(predicted, "me_name", "me")
  # drop columns to match 'reported'
  predicted <- predicted[, c("ihme_loc_id", "year_id", "me", "data", "pred_fitted", "cv_stockout_dose", "diff")]  
  # drop duplicates now that dose is dropped
  predicted <- predicted[!duplicated(predicted)]
  
  ## drop vaccines we aren't modeling
  predicted <- predicted[me %in% modeling]
  reported  <- reported[me %in% modeling]
  
  ## compute the ratio of actual original admin data / predicted loess coverage to use for stockout covariate in prep_exp
  predicted[, cv_stockout_ratio := diff]
  
  #############################################################################################
  ###                           REVIEW AND COMPARE                                          ###
  #############################################################################################
  fwrite(predicted, file = paste0(temp_root, "detected_stockouts_cp_full_", date, ".csv"))
  
  ## Now only comparing years as in reported, only mes in reported etc. 
  reported_years <- unique(reported$year_id)
  predicted <- predicted[year_id %in% reported_years]

  ## Clean reported to compare
  reported$year_id <- as.numeric(as.character(reported$year_id))



  stockout.match <- function(me) {
    pred_me   <- predicted[me==me]
    report_me <- reported[me==me]
    exact_match <- merge(pred_me, report_me, by = c("ihme_loc_id", "year_id", "me"))
    return(exact_match)
  }
  
  for (me in unique(predicted$me)) {
    exact_match <- stockout.match()
  }
  
  # assign JRF nid
  exact_match[, nid := 203321]
  
  # change me to modeling me_name
  setnames(exact_match, "me", "me_name")
  # switch me_name back to specific dose detected comparing original admin and loess data
  exact_match[, me_name := cv_stockout_dose][, cv_stockout_dose := NULL]


  # Not including reported stockouts within 3 years of introduction (intro_year:intro_year+2) 
  vacc.intro <- file.path(ref_data_repo, "vaccine_intro.rds")
  intros <- readRDS(vacc.intro)
  exact_match <- merge(exact_match, intros, by=c("ihme_loc_id", "year_id", "me_name"), all.x=T)
  exact_match <- exact_match[cv_intro_years>2 | is.na(cv_intro_years)][, c("cv_intro", "cv_intro_years", "cv_outro", "location_id") := NULL]  
  
  
  fwrite(exact_match, file = paste0(data_root, "FILEPATH/exact_matches_", date, ".csv"), row.names = FALSE)
  exact_match <- exact_match[, `:=` (data=NULL, pred_fitted=NULL, diff=NULL)]  
  
  fwrite(exact_match, file = paste0(data_root, "FILEPATH/exact_matches.csv"), row.names = FALSE)
  
  print("All done -- 'exact_matches' file made and saved to FILEPATH/stockouts")

} else if (ratios) {
  ### Negative stockouts
  # read in predicted negative deflection ratio stockouts
  predicted_ratios <- readRDS(paste0(data_root, "FILEPATH/detected_ratio_stockouts.rds"))
  # read in already matched dose-/antigen-specific stockouts (with any outliers applied to who.admin antigen directly)
  antigen_match <- fread(paste0(data_root, "FILEPATH/exact_matches.csv"))
  
  # split up ratio me_name into numerator and denominator cols (num, denom)
  predicted_ratios[me_name=="vacc_pcv3_dpt3_ratio", `:=` (num="vacc_pcv3", denom="vacc_dpt3")]
  predicted_ratios[me_name=="vacc_rotac_dpt3_ratio", `:=` (num="vacc_rotac", denom="vacc_dpt3")]
  predicted_ratios[me_name=="vacc_hib3_dpt3_ratio", `:=` (num="vacc_hib3", denom="vacc_dpt3")]
  predicted_ratios[me_name=="vacc_hepb3_dpt3_ratio", `:=` (num="vacc_hepb3", denom="vacc_dpt3")]
  predicted_ratios[me_name=="vacc_mcv2_mcv1_ratio", `:=` (num="vacc_mcv2", denom="vacc_mcv1")]
  predicted_ratios[me_name=="vacc_rcv1_mcv1_ratio", `:=` (num="vacc_rcv1", denom="vacc_mcv1")]
  
  # compare numerators with antigen_match to determine if a negative ratio stockout
  antigen_match <- antigen_match[me_name %in% unique(predicted_ratios$num)][, `:=` (stockout=1, cv_stockout_ratio=NULL)] %>% setnames(., "me_name", "num")
  predicted_ratios <- merge(predicted_ratios, antigen_match, by=c("ihme_loc_id", "year_id", "num", "nid"), all.x=T, all.y=T)[!is.na(me_name) & stockout==1]
  
  # stockout difference for modeling is the 'diff' column, and clean cols to match 'antigen_match' for future rbind
  ratio_exact_match <- predicted_ratios[, cv_stockout_ratio := diff][,.(ihme_loc_id, year_id, me_name, cv_stockout_ratio, nid)]
  
  # Not including reported stockouts within 3 years of introduction (intro_year:intro_year+2) 
  vacc.intro <- file.path(ref_data_repo, "vaccine_intro.rds")
  intros <- readRDS(vacc.intro)
  setnames(intros, "me_name", "num")
  ratio_exact_match[grepl("mcv2", me_name), num := "vacc_mcv2"]
  ratio_exact_match[grepl("rcv1", me_name), num := "vacc_rcv1"]
  ratio_exact_match[grepl("hepb3", me_name), num := "vacc_hepb3"]
  ratio_exact_match[grepl("hib3", me_name), num := "vacc_hib3"]
  ratio_exact_match[grepl("rotac", me_name), num := "vacc_rotac"]
  ratio_exact_match[grepl("pcv3", me_name), num := "vacc_pcv3"]
  ratio_exact_match <- merge(ratio_exact_match, intros, by=c("ihme_loc_id", "year_id", "num"), all.x=T)
  ratio_exact_match <- ratio_exact_match[cv_intro_years>2][, c("num", "cv_intro", "cv_intro_years", "cv_outro", "location_id") := NULL]
  
  
  # save
  fwrite(ratio_exact_match, file = paste0(data_root, "FILEPATH/exact_ratio_matches_", date, ".csv"), row.names = FALSE)
  fwrite(ratio_exact_match, file = paste0(data_root, "FILEPATH/exact_ratio_matches.csv"), row.names = FALSE)
  
  
  ### Positive stockouts: save subset location-years for post-processing where a stockout in denominator and stockout deflection is positive
  # read in predicted positive deflection ratio stockouts
  predicted_pos_ratios <- readRDS(paste0(data_root, "FILEPATH/detected_ratio_pos_stockouts.rds"))
  # read in already matched dose-/antigen-specific stockouts
  antigen_match <- fread(paste0(data_root, "FILEPATH/exact_matches.csv"))
  
  # split up ratio me_name into numerator and denominator cols (num, denom)
  predicted_pos_ratios[me_name=="vacc_pcv3_dpt3_ratio", `:=` (num="vacc_pcv3", denom="vacc_dpt3")]
  predicted_pos_ratios[me_name=="vacc_rotac_dpt3_ratio", `:=` (num="vacc_rotac", denom="vacc_dpt3")]
  predicted_pos_ratios[me_name=="vacc_hib3_dpt3_ratio", `:=` (num="vacc_hib3", denom="vacc_dpt3")]
  predicted_pos_ratios[me_name=="vacc_hepb3_dpt3_ratio", `:=` (num="vacc_hepb3", denom="vacc_dpt3")]
  predicted_pos_ratios[me_name=="vacc_mcv2_mcv1_ratio", `:=` (num="vacc_mcv2", denom="vacc_mcv1")]
  predicted_pos_ratios[me_name=="vacc_rcv1_mcv1_ratio", `:=` (num="vacc_rcv1", denom="vacc_mcv1")]
  
  # compare denominators with antigen_match to determine if a positive ratio stockout
  antigen_match_denom <- antigen_match[me_name %in% unique(predicted_pos_ratios$denom)][, `:=` (stockout=1, cv_stockout_ratio=NULL)] %>% setnames(., "me_name", "denom")
  predicted_pos_ratios <- merge(predicted_pos_ratios, antigen_match_denom, by=c("ihme_loc_id", "year_id", "denom", "nid"), all.x=T, all.y=T)[!is.na(me_name) & stockout==1]
  
  # Then ALSO need to compare to numerators, because if "denominator correct" an admin value that's not actually  modeled as a stockout, will get a spike
  antigen_match_num <- antigen_match[me_name %in% unique(predicted_pos_ratios$num)][, `:=` (stockout=1, cv_stockout_ratio=NULL)] %>% setnames(., "me_name", "num")
  predicted_pos_ratios <- merge(predicted_pos_ratios, antigen_match_num, by=c("ihme_loc_id", "year_id", "num", "nid"), all.x=T, all.y=T)[!is.na(me_name) & stockout.x==1 & stockout.y==1]
  
  
  # non-transformed stockout adjustment post-modeling and multiplying out is [actual ratio data]/[expected ratio data]; then clean cols to match 'antigen_match' for future rbind
  ratio_pos_exact_match <- predicted_pos_ratios[, cv_stockout_ratio := data/pred_fitted][,.(ihme_loc_id, year_id, me_name, cv_stockout_ratio, nid)]
  
  # save
  fwrite(ratio_pos_exact_match, file = paste0(data_root, "FILEPATH/exact_ratio_pos_matches_", date, ".csv"), row.names = FALSE)
  fwrite(ratio_pos_exact_match, file = paste0(data_root, "FILEPATH/exact_ratio_pos_matches.csv"), row.names = FALSE)

  # all done!
  print("All done -- 'exact_ratio_matches' files (positive and negative deflections) made and saved to FILEPATH/stockouts")
}

}
