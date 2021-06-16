#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    October 2017, May 2019
# Purpose: Prep vaccination for ST-GPR (reference data)
#***********************************************************************************************************************


#----SETUP--------------------------------------------------------------------------------------------------------------
### clear memory
rm(list=ls())

### load packages
library(data.table)
library(dplyr)
library(parallel)
library(readxl)
library(ggplot2)
library(boot)
library(lme4)
library(purrr)
library(splines)
library(stringr)
library(magrittr)

### set os flexibility
username <- Sys.info()[["user"]]
if (.Platform$OS.type=="windows") {
  j <- "J:/"
} else {
  j <- "FILEPATH/"
}

### path locals
source(paste0("FILEPATH/init.r"))

### source functions
source(db_tools)
source("FILEPATH/read_excel.R")
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/cluster_tools.r")
'%!in%'  <- function(x,y)!('%in%'(x,y))
decomp_step <- "iterative"

locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step=decomp_step)
locs      <- locations[level >= 3, ]
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### set paths for vaccine introduction reference data and admin coverage reports
vacc.intro          <- paste0(data_root, "FILEPATH/year_vaccine_introduction.xls")
vacc.intro.supp     <- paste0(data_root, "FILEPATH/vacc_intro_supplement.csv")
vacc.bcg.outro      <- paste0(data_root, "FILEPATH/BCG_WORLD_ATLAS_2017_2ND_ED_Y2019M05D22.XLS")
vacc.bcg.prepped    <- paste0(data_root, "FILEPATH/vacc_bcg_outro_prepped.csv")
vacc.outro.supp     <- paste0(data_root, "FILEPATH/bcg_outro_supplement.xlsx") 
vacc.official       <- paste0(data_root, "FILEPATH/coverage_series.xls")  # 
vacc.admin.subnat   <- paste0(j, "FILEPATH/Data_request_20171201.xlsx")
vacc.whosurvey      <- paste0(data_root, "FILEPATH/Coverage_survey_data.xls")  
vacc.whosurveyclean <- paste0(data_root, "FILEPATH/who_coverage_survey_cleaned_with_NIDs.xlsx")
vacc.schedule       <- paste0(data_root, "FILEPATH/schedule_data.xls")  
vacc.target         <- paste0(data_root, "FILEPATH/special_country_mmr_schedules.csv") 
vacc.stock.ind      <- paste0(data_root, "FILEPATH/indicator_series.xls")    
vacc.admin          <- paste0(data_root, "FILEPATH/administrative_coverage.xlsx")
vacc.epi            <- paste0(data_root, "FILEPATH/later_epi_start.csv")
stockout.add        <- paste0(data_root, "FILEPATH/supplemental_unreported_stockouts.csv")  
ind.unicef          <- paste0(data_root, "FILEPATH/ind_unicef_reported.csv")

### prep modelable entity db for reference
me.db <- file.path(ref_data_repo, "me_db.csv") %>% fread  
date  <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")
ref_data_repo <- "FILEPATH/reference"

### set objects
unadj.stockout <- FALSE        
LONG           <- TRUE         
gam_stockouts  <- TRUE         
year.est.start <- year_start
year.est.end   <- year_end
#***********************************************************************************************************************


########################################################################################################################
# SECTION 1: Prep WHO Survey Data
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
prep.who_survey <- function() {
  df <- read_excel(vacc.whosurvey, skip=1) %>% data.table
  old <- c("ISO3", "cohortYear", "vaccine", "coverage", "surveyNameProduction")
  new <- c("ihme_loc_id", "year_id", "who_name", "data", "survey_name")
  setnames(df, old, new)
  ## Subset to card or history
  df <- df[evidence == "Card or History"]
  ## Subset to crude
  df <- df[validity == "crude"]
  ## Clean up who_name (to be merged to me_name)
  ## Compare against names from me.db
  setdiff(unique(df$who_name), me.db$who_name)
  ## Clean
  df <- df[who_name == "PcV1", who_name := "PCV1"]
  df <- df[who_name == "PcV3", who_name := "PCV3"]
  df <- df[who_name == "RotaC", who_name := "rotac"]
  ## Check again
  setdiff(unique(df$who_name), me.db$who_name)
  ## Merge on to me.db
  df <- merge(df, me.db[,.(me_name, who_name)], by='who_name', all.x=TRUE)
  ## Drop superfulous me's
  extra.mes <- df[is.na(me_name)][['who_name']] %>% unique
  print("LIST OF VACC NOT USED:")
  print(extra.mes)
  df <- df[!is.na(me_name)]
  ## Drop surveys for which we have microdata
  drop.surveys <- c("Multiple Indicator Cluster Survey",
                    "Demographic and Health Survey",
                    "D?mographique et de Sant?",
                    "MICS",
                    "Demogr?fica y de Salud Familiar")
  for (drop in drop.surveys) df <- df[!grepl(drop, survey_name)]
  ## Adjust data
  df <- df[, data := data/100]
  setnames(df, "Sample_Size", "sample_size")
  df <- df[, variance := data*(1-data)/sample_size]
  ## Keep
  keep <- c("survey_name", "ihme_loc_id", "year_id", "data", "variance", "sample_size", "me_name")
  df <- df[, (keep),  with=FALSE]
  ## Clean
  df <- df[, cv_whosurvey := 1]
  df <- df[, nid := 210618]
  ## Drop unmapped locations
  drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id)]$ihme_loc_id %>% unique
  if (length(drop.locs)>0 ) {
    print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
    df <- df[!(ihme_loc_id %in% drop.locs)]
  }
  return(df)
}

prep.who_survey_clean <- function() {
  df <- read_excel(vacc.whosurveyclean) %>% data.table
  old <- c("ISO3", "cohortYear", "vaccine", "coverage", "surveyNameProduction", "NID", "Sample_Size")
  new <- c("ihme_loc_id", "year_id", "who_name", "data", "survey_name", "nid", "sample_size")
  setnames(df, old, new)
  
  ## Subset to card or history
  df <- df[evidence=="Card or History"]
  ## Subset to crude
  df <- df[validity=="crude"]
  ## Keep only surveys we don't already have microdata for, as well as surveys we don't have NIDs for (i.e. aren't catalogued)
  df[is.na(ignore), ignore := 2]
  df <- df[(ignore==0 | (Ownership=="Report only" & ignore==2)) & !is.na(nid)]
  ## Clean up who_name (to be merged to me_name)
  ## Compare against names from me.db
  setdiff(unique(df$who_name), me.db$who_name)
  ## Clean
  df[who_name == "PcV1", who_name := "PCV1"]
  df[who_name == "PcV3", who_name := "PCV3"]
  df[who_name == "HepBB", who_name := "HepB3"]
  df[who_name %in% c("TT2+", "TT2"), who_name := "TT2plus"]
  ## Check again
  setdiff(unique(df$who_name), me.db$who_name)
  ## Merge on to me.db
  df <- merge(df, me.db[, .(me_name, who_name)], by='who_name', all.x=TRUE)
  ## Drop superfulous me's
  extra.mes <- df[is.na(me_name)][['who_name']] %>% unique
  print(paste0("Vaccines dropped: ", paste(extra.mes, collapse=", ")))
  df <- df[!is.na(me_name)]
  ## Adjust data
  df[, data := data / 100]
  df[, variance := data * (1 - data) / sample_size]
  ## Keep
   df <- df[, c("nid", "survey_name", "ihme_loc_id", "year_id", "data", "variance", "sample_size", "me_name"),  with=FALSE]
  ## Clean
  df[, cv_whosurvey := 1]
  ## Drop unmapped locations
  drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id)]$ihme_loc_id %>% unique
  if (length(drop.locs)>0 ) {
    print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
    df <- df[!(ihme_loc_id %in% drop.locs)]
  }
  return(df)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 2: Prep WHO official country-reported data
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
if (LONG) {
  prep.who_official <- function(me) {
    message(paste0("Processing ", me))
    who <- me.db[me_name==me]$who_name
    df <- read_excel(vacc.official, sheet="world_coverage") %>% data.table
    df <- df[Vaccine==who]
    if (me=="vacc_rotac") {
      df.2 <- read_excel(vacc.official, sheet="world_coverage") %>% data.table
      df.2 <- df.2[Vaccine=="RotaC"]
      df   <- rbind(df, df.2)
    }
    drop <- c("WHO_REGION", "Cname", "Continent", "Asterisc")
    df <- df[, (drop) := NULL]
    old_names <- c("Vaccine", "Year", "Percent_covrage", "ISO_code")
    new_names <- c("me_name", "year_id", "data", "ihme_loc_id")
    setnames(df, old_names, new_names)
    df <- df[, me_name := me]
    df <- df[, data := data / 100]
    df <- df[!is.na(data)]
    df <- df[, cv_admin := 1]
    df <- df[, nid := 203321]
    df <- df[, survey_name := "WHO/UNICEF Official Country-Reported Data"]
    ## Duplicate Demark for Greenland
    df.grl <- df[ihme_loc_id=="DNK"] %>% copy
    df.grl <- df.grl[, ihme_loc_id := "GRL"]
    df <- rbind(df, df.grl)
    df <- df[ihme_loc_id=="DEU", year_id := year_id-5]
    ## Drop unmapped locations
    drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id)]$ihme_loc_id %>% unique
    if (length(drop.locs) > 0 ) {
      print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
      df <- df[!(ihme_loc_id %in% drop.locs)]
    }
    return(df)
  }
} else {
  prep.who_official <- function(me) {
    who <- me.db[me_name==me]$who_name
    df <- read_excel(vacc.official, sheet=who) %>% data.table
    drop <- c("WHO_REGION", "Cname", "Vaccine")
    df <- df[, (drop) := NULL]
    df <- melt(df, id="ISO_code", variable.name="year_id", value.name="data")
    df <- df[, me_name := me]
    df <- df[, data := data / 100]
    df <- df[!is.na(data)]
    df <- df[, cv_admin := 1]
    setnames(df, "ISO_code", "ihme_loc_id")
    df <- df[, nid := 203321]
    df <- df[, survey_name := "WHO/UNICEF Official Country-Reported Data"]
    ## Duplicate Demark for Greenland
    df.grl <- df[ihme_loc_id=="DNK"] %>% copy
    df.grl <- df.grl[, ihme_loc_id := "GRL"]
    df <- rbind(df, df.grl)
    df <- df[ihme_loc_id=="DEU", year_id := year_id-5]
    ## Drop unmapped locations
    drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id)]$ihme_loc_id %>% unique
    if (length(drop.locs) > 0 ) {
      print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
      df <- df[!(ihme_loc_id %in% drop.locs)]
    }
    return(df)
  }
}


prep.who.admin.subnat <- function(me) {
  ### read in dataset
  data <- read_excel(vacc.admin.subnat, sheet="Data_request") %>% data.table %>%
    setnames(., "Vaccine Type", "who_name")
  ### grab GBD me_names for modeling
  data <- merge(data, me.db[, .(who_name, me_name)], by="who_name", all.x=TRUE)
  data[is.na(me_name) & who_name %in% c("RotaC", "rotac"), me_name := "vacc_rotac"]
  ### prep
  setnames(data, c("Year", "Iso Code", "Country Name", "Coverage"), c("year_id", "ihme_loc_id", "location_name", "data"))
  ### just keep rows with either both numerator/denominator or coverage
  data <- data[!is.na(data) | !is.na(Numerator & Denominator), ]
  data[is.na(data), data := Numerator / Denominator]
  ### keep only countries where we estimate subnationals
  parents <- locations[location_id %in% locations[level > 3, parent_id] & level==3, ihme_loc_id]
  data <- data[ihme_loc_id %in% parents]
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 3: Prep JRF administrative data
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
prep.who_admin <- function(me, map=me.db) {
  message(paste0("Pulling JRF admin data for ", me))
  who <- me.db[me_name==me]$who_name
  df <- read_excel(vacc.admin, sheet="Data") %>% data.table
  df <- df[VaccineCode==who]
  if (me=="vacc_rotac") {
    df.2 <- read_excel(vacc.admin, sheet="Data") %>% data.table
    df.2 <- df.2[VaccineCode=="RotaC"]
    df   <- rbind(df, df.2)
  }
  drop <- c("NamePublicationEnglish", "WHOregionCode")
  df <- df[, (drop) := NULL]
  old_names <- c("ISO3countryCode", "VaccineCode", "Year", "PerCentCoverage")
  new_names <- c("ihme_loc_id", "me_name", "year_id", "data")
  setnames(df, old_names, new_names)
  df <- df[, me_name := me]
  df <- df[!is.na(data)]
  df <- df[, data := data / 100]
  df <- df[, cv_admin := 1]
  df <- df[, nid := 203321]  
  df <- df[, survey_name := "WHO/UNICEF Admin Data"]
  ## Duplicate Demark for Greenland
  df.grl <- df[ihme_loc_id=="DNK"] %>% copy
  df.grl <- df.grl[, ihme_loc_id := "GRL"]
  df <- rbind(df, df.grl)
  ## Drop unmapped locations
  drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id)]$ihme_loc_id %>% unique
  if (length(drop.locs) > 0 ) {
    print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
    df <- df[!(ihme_loc_id %in% drop.locs)]
  }
  return(df)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 4: Prep vaccine intro years
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
## For recently introduced vaccinations, set year of introduction.
make.intro_frame <- function(me) {
  ## Ratios
  ## Get WHO ME
  if (grepl("ratio", me))  {
    temp   <- unlist(strsplit(me, "_"))[1:2] %>% paste0(., collapse="_")
    who_me <- me.db[me_name==temp]$who_name_agg
  } else if (grepl("correlation", me)) {
    temp   <- paste0("vacc_", gsub(".*correlation_\\s*|_.*", "", me))
    who_me <- me.db[me_name==temp]$who_name_agg
  } else if (grepl("dpt3_polio3_mcv1", me) & me != "vacc_dpt3_polio3_mcv1") {
    temp   <- gsub("_dpt3_polio3_mcv1", "", me)
    who_me <- me.db[me_name==temp]$who_name_agg
  } else {
    who_me <- me.db[me_name==me]$who_name_agg
  }
  message(paste0(me, ": ", who_me))
  ## Load intro
  df <- read_excel(vacc.intro, sheet=who_me, na=c("n/a", "n/d")) %>% data.table
  ## Subset intro years
  old <- c("ISO 3 code", "Year of introduction in entire country", "Year of introduction in part of the country")
  new <- c("ihme_loc_id", "intro", "intro_partial")
  setnames(df, old, new)
  df <- df[, new, with=FALSE]
  df <- df[, intro_partial := gsub("prior to ", "", intro_partial) %>% as.character %>% as.numeric]
  df <- df[, cv_intro := gsub("prior to ", "", intro) %>% as.character %>% as.numeric]
  df <- df[, intro_partial := gsub("prior or= to ", "", intro_partial) %>% as.character %>% as.numeric]  
  df <- df[, cv_intro := gsub("prior or= to ", "", intro) %>% as.character %>% as.numeric]
  df <- df[!is.na(ihme_loc_id)]
  ## Replace intro if partial intro in country
  df <- df[!is.na(intro_partial), cv_intro := intro_partial]
  df <- df[, .(ihme_loc_id, cv_intro)]
  ## Append using vacc intro supplement
  intro.supp <- fread(vacc.intro.supp)[who_name_agg==who_me & (is.na(ignore) | ignore != 1)] 
  ## Drop locations in main if added in supplement
  drop.locs <- intersect(unique(intro.supp$ihme_loc_id), unique(df$ihme_loc_id))
  df <- df[!(ihme_loc_id %in% drop.locs)]
  if (nrow(intro.supp) > 0) {
    intro.supp <- intro.supp[, .(ihme_loc_id, cv_intro)]
    df <- rbind(df, intro.supp)
  }
  ## Merge onto hierarchy
  df <- merge(locs[, .(ihme_loc_id, location_id, parent_id, level)], df, by='ihme_loc_id', all.x=TRUE)
  ## For each level >3, see if parent has a intro date, and replace with that if not present
  for (lvl in unique(df[level > 3]$level) %>% sort) {
  intro.parent <- df[, .(location_id, cv_intro)]
  setnames(intro.parent, c('cv_intro', 'location_id'), c('intro_parent', 'parent_id'))
  df <- merge(df, intro.parent, by='parent_id', all.x=TRUE)
  df <- df[level==lvl & is.na(cv_intro), cv_intro := intro_parent]
  df <- df[, intro_parent := NULL]
  }
  df <- df[, c("parent_id", "level") := NULL]

  df <- df[is.na(cv_intro), cv_intro := 9999]
  ## Create square frame
  square <- expand.grid(ihme_loc_id=locs$ihme_loc_id, year_id=as.numeric(year.est.start:year.est.end)) %>% data.table
  df <- merge(square, df, by='ihme_loc_id', all.x=TRUE)
  df <- df[, me_name := me]
  df <- df[order(ihme_loc_id, year_id)]
  ## Set years since introduction (where year of introduction counts as 1st year)
  df <- df[, cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]
  return(df)
}

## Adjust intro year of straight antigen models when know EPI program started after 1980
straight.intro_frame <- function() {
  straight_models <- c("vacc_mcv1", "vacc_dpt3", "vacc_bcg", "vacc_polio3", "vacc_dpt1")  
  # make square dataframe 
  df <- CJ(location_id = unique(locs$location_id),
           year_id = year_start:year.est.end,
           age_group_id = 22,
           sex_id = 3,
           me_name = straight_models)
  df <- df[, cv_intro := 1950]
  # read in supp'l sheet of later epi years
  late_epi <- fread(vacc.epi)[(is.na(ignore) | ignore != 1)] %>% data.table
  late_epi <- merge(late_epi, locs[, c("location_name", "ihme_loc_id", "location_id", "level")], by = c("location_name", "ihme_loc_id"), all.x = TRUE)
  # change EPI year for locs in late_epi
  df <- merge(df, late_epi, by = c("location_id", "me_name"), all.x = TRUE)
  df <- df[!is.na(cv_intro.y), cv_intro.x := cv_intro.y]
  df <- df[, `:=` (location_name=NULL, ihme_loc_id=NULL, cv_intro.y=NULL)]
  setnames(df, "cv_intro.x", "cv_intro")
  df <- df[, level := NULL]
  df <- df[, cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]
  return(df)
}

#***********************************************************************************************************************


########################################################################################################################
# SECTION 5: Prep vaccine schedule
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
## Preps vaccination schedule to get the number of doses in the schedule (for rota)
prep.schedule <- function(me) {
  ## Get WHO ME
  who_me <- me.db[me_name==me]$who_name_agg
  ## Load schedule
  df <- read_excel(vacc.schedule, sheet="schedule") %>% data.table
  ## Subset
  df <- df[, c(2, 4, 6)]
  setnames(df, names(df), c("ihme_loc_id", "vacc", "schedule"))
  ## Keep
  df <- df[vacc == who_me]
  df <- df[, doses := (gsub(";", ",", schedule) %>% gsub(".$", "", .) %>% gsub(" months", "", .) %>% 
                         gsub(" weeks", "", .) %>% gsub(" month", "", .) %>% gsub(" week", "", .) %>% 
                         str_count(., "[ ]")) + 1]
  df <- df[is.na(doses), doses := 0]
  ## Push into locs
  df <- merge(locs[, .(ihme_loc_id, location_id, parent_id, level)], df, by="ihme_loc_id", all.x=TRUE)
  ## For each level>3, set schedule based on parent
  for (lvl in unique(df[level > 3]$level) %>% sort) {
  parent <- df[, .(location_id, doses)]
  setnames(parent, c("location_id", "doses"), c("parent_id", "parent_doses"))
  df <- merge(df, parent, by='parent_id', all.x=TRUE)
  df <- df[level==lvl & is.na(doses), doses := parent_doses]
  df <- df[, parent_doses := NULL]
  }
  df <- df[, c("parent_id", "level", "vacc", "schedule") := NULL]
  df <- df[, me_name := me]
  df <- df[!is.na(doses)]
  return(df)
}

## Preps vaccination target age cohort to apply country-specific definitions of coverage
## Match admin data denominators
## MMR components (MCV and RCV)
prep.target <- function(me) {
  ## Load schedule
  df <- fread(vacc.target)[me_name==me]
  ## For each level>3, set schedule based on parent
  df <- merge(locs[, .(ihme_loc_id, location_id, parent_id, level)], df[, .(ihme_loc_id, age_cohort)], by="ihme_loc_id", all.x=TRUE)
  for (lvl in unique(df[level > 3]$level) %>% sort) {
    parent <- df[, .(location_id, age_cohort)]
    setnames(parent, c("location_id", "age_cohort"), c("parent_id", "parent_age_cohort"))
    df <- merge(df, parent, by='parent_id', all.x=TRUE)
    df <- df[level==lvl & is.na(age_cohort), age_cohort := parent_age_cohort]
    df <- df[, parent_age_cohort := NULL]
  }
  ## Subset
  df <- df[, .(ihme_loc_id, age_cohort)]
  df[, me_name := me]
  df <- df[!is.na(age_cohort)]
  return(df)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 6: Prep vaccine removal (BCG)
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
## Clean raw BCG World Atlas, keeping relevant columns for outro.year function, delayed epi program start function
# return list object of dfs relevant for both delayed introduction and making outro frame function
bcg.outro.clean <- function(me, prepped_outro_path=vacc.bcg.prepped) {
  # read in raw data 
  data <- read_excel(vacc.bcg.outro) %>% as.data.table
  
  # 1. clean data for make.outro_frame function
  # subset to columns of interest
  df <- data[,.(iso3, Country, `Current BCG Vaccination?`, `BCG Policy Type`, `Which year was the vaccination introduced?`, 
                `Which year was the policy discontinued? (Group B only)`, `Year of Changes to BCG Policy`, 
                `Details of Changes to BCG Policy`, `Are there targeted/'special' groups which receive BCG?`, 
                `Details of Targeted/'Special' Groups`)]  # added last two cols 3/6/20
  df <- df[!is.na(`Current BCG Vaccination?`)]
  
  # clean column names
  old <- c("iso3", "Country", "Current BCG Vaccination?", "BCG Policy Type", "Which year was the vaccination introduced?", 
           "Which year was the policy discontinued? (Group B only)", "Year of Changes to BCG Policy", "Details of Changes to BCG Policy", 
           "Are there targeted/'special' groups which receive BCG?", 
           "Details of Targeted/'Special' Groups")
  new <- c("ihme_loc_id", "location_name", "current_bcg", "policy_type", "intro_year", 
           "outro_year", "policy_change_year", "policy_change_details", "special_pops", "special_pops_details")
  setnames(df, old, new)
  
  df <- df[current_bcg != "Yes"]
  
  df[is.na(outro_year) & !is.na(policy_change_year), `:=` (outro_year=policy_change_year, note="Using policy change year as outro_year")]
  df[is.na(outro_year) & ihme_loc_id=="ECU", `:=` (current_bcg="Yes", note="Consistent admin data and WUENIC estimates also")]
  df[is.na(outro_year) & ihme_loc_id=="AND", `:=` (outro_year="1979", note="Ambiguous reporting and no admin data nor WUENIC estimate")]
  df[is.na(outro_year) & ihme_loc_id=="LUX", `:=` (outro_year="1979", note="Ambiguous reporting and no admin data nor WUENIC estimate")]
  df <- df[current_bcg != "Yes"]
  df[grepl("specific groups or none", policy_type) & (grepl("[:space:]", outro_year) | outro_year == "N/A"), 
     `:=` (outro_year="1979", note="Ambiguous reporting and no admin data nor WUENIC estimate")]
  
  df[outro_year=="N/A" & ihme_loc_id=="NZL", `:=` (outro_year="1979", note="Phase outs between 1960s and 90s, so do not estimate")]
  df[grepl("[:space:]", outro_year) & ihme_loc_id=="AUS", `:=` (outro_year="1979", note="Phase outs between 1960s and 90s, so do not estimate")]
  
  # clean up df object for making outro frame function
  df.outro <- df[,.(ihme_loc_id, location_name, outro_year, note)]
  # drop years where outro year applies to latest year available for special pops, but not fully representative
  df.outro <- df.outro[!ihme_loc_id %in% c("DEU", "NOR", "GBR")]
  
  special.pops <- data.table(ihme_loc_id=c("DEU", "NOR", "GBR"), 
                    location_name=c("Germany", "Norway", "United Kingdom"), 
                    outro_year=c("1975", "1979", "1979"), 
                    note="Special populations adjustment")
  
  # bind df.outro with supplement with special pop custom adjustments
  df.outro <- rbind(df.outro, special.pops)
  
  # last column and df cleaning
  df.outro[, `:=` (me_name=me, nid=407800)]  
  df.outro$outro_year <- as.numeric(df.outro$outro_year)
  
  # append and replace supplement outro years
  suppl <- read_excel(vacc.outro.supp, sheet="clean_and_append") %>% data.table
  suppl[source=="WORLD ATLAS", nid := 407800]
  suppl[, c("source", "source2", "note2") := NULL]
  suppl[, me_name := "vacc_bcg"]
  suppl <- merge(suppl, locations[,.(ihme_loc_id, location_name)], by="ihme_loc_id", all.x=T)
  df.outro <- df.outro[!ihme_loc_id %in% unique(suppl$ihme_loc_id)]  
  df.outro <- rbind(df.outro, suppl)
  
  df.outro[!ihme_loc_id %in% c("AUT", "CZE", "MLT", "SVN"), outro_year := outro_year + 1]
  
  # save prepped outro file
  fwrite(df.outro, file=prepped_outro_path)
  
  
  # 2. clean data for delayed EPI supplement
  df <- data[,.(iso3, Country, `Current BCG Vaccination?`, `BCG Policy Type`, `Which year was the vaccination introduced?`, 
                `Which year was the policy discontinued? (Group B only)`, `Year of Changes to BCG Policy`, `Details of Changes to BCG Policy`)]
  df <- df[!is.na(`Current BCG Vaccination?`)]
  # clean column names
  old <- c("iso3", "Country", "Current BCG Vaccination?", "BCG Policy Type", "Which year was the vaccination introduced?", 
           "Which year was the policy discontinued? (Group B only)", "Year of Changes to BCG Policy", "Details of Changes to BCG Policy")
  new <- c("ihme_loc_id", "location_name", "current_bcg", "policy_type", "intro_year", 
           "outro_year", "policy_change_year", "policy_change_details")
  setnames(df, old, new)
  
  df <- df[!grepl("[:space:]", intro_year) & !is.na(intro_year) & !intro_year %in% c("Unknown", "UNK", "N/A")]
  df <- df[ihme_loc_id != "JPN"]
  
  # select and save the rows with EPI intro years to adjust!
  df$intro_year <- as.numeric(df$intro_year)
  df.epi <- df[,.(ihme_loc_id, location_name, intro_year)]
  df.epi <- df.epi[intro_year >= 1980]
  df.epi[, `:=` (me_name=me, nid=407800)]
  
  # save!
  fwrite(df.epi, file=paste0(data_root, "FILEPATH/bcg_world_atlas_delayed_intro.csv"))
  
  # make object to return to keep both dfs in working space
  all <- list(df.outro, df.epi)
  
  return(all)
    
}



## Preps vaccination frame for when vaccines were removed from schedule (mostly BCG)
make.outro_frame <- function(me) {
  ## Load intro
  df <- fread(vacc.bcg.prepped)[me_name==me]  
  setnames(df, "outro_year", "cv_outro")
  ## Merge onto hierarchy
  df <- merge(locs[, .(ihme_loc_id, location_id, parent_id, level)], df, by='ihme_loc_id', all.x=TRUE)
  ## For each level >3, see if parent has a outro date, and replace with that if not present
  for (lvl in unique(df[level>3]$level) %>% sort) {
    outro.parent <- df[, .(location_id, cv_outro)]
    setnames(outro.parent, c('cv_outro', 'location_id'), c('outro_parent', 'parent_id'))
    df <- merge(df, outro.parent, by='parent_id', all.x=TRUE)
    df <- df[level==lvl & is.na(cv_outro), cv_outro := outro_parent]
    df <- df[, outro_parent := NULL]
  }
  df <- df[, c("parent_id", "level") := NULL]
  ## Set outro year to 9999 if no data
  df <- df[is.na(cv_outro), cv_outro := 9999]
  ## Create square frame
  square <- expand.grid(ihme_loc_id=locs$ihme_loc_id, year_id=as.numeric(year.est.start:year.est.end)) %>% data.table
  df <- merge(square, df, by='ihme_loc_id', all.x=TRUE)
  df <- df[, me_name := me]
  df <- df[order(ihme_loc_id, year_id)]
  df <- df[, cv_intro_years := ifelse((cv_outro - year_id)>0, year_id - 1980 + 1, 0)]  
  df <- df[, .(ihme_loc_id, year_id, location_id, me_name, cv_outro, cv_intro_years)]
  return(df)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 7: New vaccine denominator scaleup adjustment
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
## Check denominators of antigens on DPT schedule
admin.dpt.denom <- function(df) {
  vacc_to_test <- list(c(paste0("vacc_pcv", 1:3), "vacc_rotac", paste0("vacc_hib", 1:3), paste0("vacc_hepb", 1:3)),
                       c("vacc_rcv1", "vacc_mcv2"),
                       c("vacc_dpt3", "vacc_mcv1"))
  drop <- c("WHOregionCode", "NamePublicationEnglish")
  df   <- df[, (drop) := NULL]
  df <- df[!is.na(TargetGroup) & TargetGroup != 0 & !is.na(DosesAdministered) & DosesAdministered != 0]
  orig <- copy(df)
  df <- df[, denom_change := ""]
  df <- df[, orig_TargetGroup := TargetGroup]
  # compare DTP3 schedule pairs
  for (me in vacc_to_test[[1]]) {
    # pull out WHO name using me.db
    who   <- me.db[me_name==me]$who_name
    denom <- me.db[me_name==vacc_to_test[[3]][1]]$who_name
    message(paste("Confirming correct admin data denominator for", who, "vs", denom))
    # subset to rows with numerators and denominators
    working <- df[VaccineCode==who | VaccineCode==denom]
    for (loc in unique(df$ISO3countryCode)) {  
      cp  <- working[ISO3countryCode==loc]
      yrs <- cp[, .N, by = "Year"][N == 2, Year]  
      cp  <- cp[Year %in% yrs]
      # compare denominators
      for (yr in yrs) {
        tg_who   <- cp[VaccineCode==who & Year==yr]
        tg_denom <- cp[VaccineCode==denom & Year==yr]
        if (tg_denom$TargetGroup > tg_who$TargetGroup) {  
          df <- df[ISO3countryCode==loc & VaccineCode==tg_who$VaccineCode & Year==yr, `:=` (TargetGroup=tg_denom$TargetGroup, denom_change=1)]
        }
      }
      df <- df[, new_PerCentCoverage := round(DosesAdministered/TargetGroup * 100)]
      df <- df[is.na(new_PerCentCoverage), new_PerCentCoverage := 0]
    }
  }
  mismatch <- df[denom_change==1]
  mismatch <- mismatch[, change := new_PerCentCoverage - PerCentCoverage]
  return(mismatch)
}


## Adjust coverage values in who.admin for loc-antigen-years with denominator changes
denom.adjust <- function(adjust_file) {
  df        <- read_excel(vacc.admin, sheet="Data") %>% data.table
  # make rotac naming consistent across 'rotac' and 'RotaC' for complete matching and adjustment
  df[VaccineCode=="RotaC", VaccineCode := "rotac"]
  ## change denominators where don't match appropriate comparable schedule
  dpt.denom <- admin.dpt.denom(df)
  both      <- copy(dpt.denom)        
  wow       <- both[change < -50]
  path  <- file.path("FILEPATH/scaleup_denominators", date); if (!dir.exists(path)) dir.create(path, recursive=TRUE)  
  fwrite(both, file=file.path(path, "denom_mismatch.csv"))         
  fwrite(wow, file=file.path(path, "mismatch_change_over_50.csv"))  
  ## now clean 'both' 
  # clean column names
  old <- c("ISO3countryCode", "VaccineCode", "Year", "new_PerCentCoverage")
  new <- c("ihme_loc_id", "who_name", "year_id", "data_replace")
  setnames(both, old, new)
  # drop cols don't need anymore
  replace <- both[, new, with=FALSE]
  # change "who_name" col values to "me_name" values
  setdiff(unique(df$who_name), me.db$who_name)
  replace <- merge(replace, me.db[, .(me_name, who_name)], by='who_name', all.x=TRUE)
  replace <- replace[, who_name := NULL]
  # divide data col by 100 so between 0-1
  replace <- replace[, data_replace := data_replace / 100]
  replace[ihme_loc_id=="DEU", year_id := year_id-5]
  data <- readRDS(paste0(data_root, "FILEPATH/", adjust_file, ".rds"))  
  saveRDS(data, paste0(data_root, "FILEPATH/", adjust_file, "_denom_unadjusted.rds"))  
  message("Updating coverage where denominators changed")
  if (!LONG) {
    replace$year_id <- as.factor(replace$year_id)
  }
  dt <- merge(data, replace, by = c("ihme_loc_id", "year_id", "me_name"), all.x = TRUE)
  dt <- dt[!is.na(data_replace), data := data_replace]  
  # last cleaning
  dt <- dt[, data_replace := NULL]
  # save adjusted file
  saveRDS(dt, paste0(data_root, "FILEPATH/", adjust_file, ".rds")) 
  message(paste0("Saved updated ", adjust_file, " input dataset in ", paste0(data_root, "/exp/reference")))
  return(dt)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 8: Stockout detection
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
## Clean WHO/JRF reported stockout indicator sheet
prep.reported.stockouts <- function(df) {
  # immediate cleaning
  drop <- c("WHO_REGION", "IVBCode", "Cname", "Category", "ExpectedAnswer", "CategorySort")
  df <- df[, (drop) := NULL]
  # subset to stockout indicators of interest
  df <- df[grepl("stockout", Indicator) | grepl("stock-out", Indicator) | grepl("interrupted", Indicator)]
  df <- melt(df, id=c("ISO_code", "Indicator", "IndicatorCode"), variable.name="year_id", value.name="response")
  df <- df[grepl("Is there a stock-out at national level for", Indicator)]
  df <- df[response=="Yes" | response=="yes"]
  df <- df[, count := seq(1:nrow(df))]
  for (row in seq(1, nrow(df), 1)) {
    temp   <- unlist(strsplit(df[count==row]$Indicator, "Is there a stock-out at national level for "))[2]
    df     <- df[count==row, me := temp]
  }
  # column cleaning
  df <- df[, `:=` (Indicator=NULL, IndicatorCode=NULL, count=NULL, response=NULL)]
  df <- df[me=="Pneumococcal conjugate vaccine", me := "vacc_pcv"]
  df <- df[me=="OPV", me := "vacc_polio"]
  df <- df[me=="MenA conj.", me := "vacc_mena"]
  df <- df[me=="DTP", me := "vacc_dpt"]
  df <- df[me=="IPV", me := "vacc_polio"]
  df <- df[me=="HPV", me := "vacc_hpv"]
  df <- df[me=="BCG", me := "vacc_bcg"]
  df <- df[me=="Hib", me := "vacc_hib"]
  df <- df[me=="Rotavirus", me := "vacc_rotac"]
  df <- df[me=="Rubella", me := "vacc_rcv"]
  df <- df[me=="Measles", me := "vacc_mcv"]
  df <- df[me=="Yellow fever", me := "vacc_yfv"]
  df <- df[me=="Hepatitis B", me := "vacc_hepb"]
  df <- df[me=="Tetanus toxoid", me := "vacc_tt2"]
  setnames(df, "ISO_code", "ihme_loc_id")
  return(df)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 9: Combine JRF administrative data and official country-reported data
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
combine.jrf.data <- function(admin_data=who.admin, official_data=who.official) {
  setnames(official_data, "data", "data_official", skip_absent = T)
  setnames(admin_data, "data", "data_admin", skip_absent = T)
  
  both <- merge(admin_data, official_data, by=c("ihme_loc_id", "me_name", "year_id", "cv_admin"), all=T)
  both[, c("nid.x", "nid.y", "survey_name.x", "survey_name.y") := NULL]  
  
  both[!is.na(data_admin), `:=` (data=data_admin, survey_name="WHO/UNICEF Admin Data")]
  both[is.na(data_admin) & !is.na(data_official), `:=` (data=data_official, survey_name="WHO/UNICEF Official Country-Reported Data")]
  both[, nid := 203321]  
  
  both[, c("TargetGroup", "DosesAdministered", "data_admin", "data_official") := NULL]
  
  return(both)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 9: Combine JRF administrative data and official country-reported data
########################################################################################################################

#----FUNCTION-----------------------------------------------------------------------------------------------------------
use.alternate.admin <- function(new_data, change_df) {

  # read in new data to use instead of direct JRF download
  alt_data <- fread(new_data)
  # convert extracted percentages into proportions
  alt_data[, data := data/100]
  # change_df is the data to be used in modeling where want to replace location-antigen-years
  # drop years in change_df that going to instead use from 'data' object
  alt_data[, id := paste0(ihme_loc_id, me_name, year_id)]
  change_df[, id := paste0(ihme_loc_id, me_name, year_id)]
  change_df <- change_df[!id %in% unique(alt_data$id)]
  first_yr <- min(alt_data[ihme_loc_id=="IND", year_id])
  change_df[ihme_loc_id=="IND" & year_id <= first_yr, data := NA]
  change_df <- change_df[!is.na(data)]
  
  # rbind alt_data rows to use instead
  done <- rbind(change_df, alt_data)
  done[, id := NULL]
  
  return(done)
}

#***********************************************************************************************************************




#----RUN!---------------------------------------------------------------------------------------------------------------
#----CLEAN BCG WORLD ATLAS DATA-----------------------------------------------------------------------------------------
bcg_data <- bcg.outro.clean(me="vacc_bcg")
bcg.outro.df <- bcg_data[[1]]
bcg.epi.df <- bcg_data[[2]]


#----MAKE INTRO YEAR RDS (INCLUDES BCG OUTRO YEAR)----------------------------------------------------------------------
intro.vacc <- c(paste0("vacc_hepb", 1:3),
                paste0("vacc_hib", 1:3),
                paste0("vacc_pcv", 1:3),
                paste0("vacc_rota", 1:3),
                "vacc_rotac",
                "vacc_mcv2",
                "vacc_rcv1",
                "vacc_yfv",
                "vacc_hib3_dpt3_ratio", 
                "vacc_hepb3_dpt3_ratio",
                "vacc_pcv3_dpt3_ratio", 
                "vacc_rotac_dpt3_ratio",
                "vacc_mcv2_mcv1_ratio",
                "vacc_rcv1_mcv1_ratio",
                paste0("correlation_",
                       c("mcv2_mcv1", "hepb3_dpt3", "hib3_dpt3", "pcv3_dpt3", "rotac_dpt3",
                         "mcv2_polio3_mcv1_dpt3", "hepb3_polio3_mcv1_dpt3", "hib3_polio3_mcv1_dpt3", "pcv3_polio3_mcv1_dpt3", "rotac_polio3_mcv1_dpt3")),
                "vacc_rotac_dpt3_polio3_mcv1", "vacc_hib3_dpt3_polio3_mcv1", "vacc_hepb3_dpt3_polio3_mcv1", "vacc_pcv3_dpt3_polio3_mcv1"
)
intro.frame <- lapply(intro.vacc, make.intro_frame) %>% rbindlist
outro.frame <- make.outro_frame(me="vacc_bcg")  
intro.frame <- rbind(intro.frame, outro.frame, fill=TRUE, use.names=TRUE)
saveRDS(intro.frame, file = file.path(ref_data_repo, "vaccine_intro.rds"))

## Make Intro frame for countries with post-1980 national immunization program start
intro.epi.frame <- straight.intro_frame()
saveRDS(intro.epi.frame, file.path(ref_data_repo, "vaccine_intro_epi.rds"))


#----MAKE SCHEDULE RDS----------------------------------------------------------------------------------------------
## Make doses frame
schedule.vacc <- lapply("vacc_rotac", prep.schedule) %>% rbindlist
saveRDS(schedule.vacc, file.path(ref_data_repo, "vaccine_schedule.rds"))

## Make doses frame for MCV-schedule cohorts
target.vacc <- lapply(c("vacc_mcv1", "vacc_mcv2", "vacc_rcv1"), prep.target) %>% rbindlist
saveRDS(target.vacc, file.path(ref_data_repo, "vaccine_target.rds"))


#----CLEAN & COMPARE WHO SURVEY DATA (GBD 2017)---------------------------------------------------------------------
## Prep who survey data
who.survey <- prep.who_survey_clean()
saveRDS(who.survey, paste0(data_root, "FILEPATH/who_survey.rds"))


#----PREP JRF OFFICIAL COUNTRY ESTIMATES (what was previously calling admin data) & ACTUAL ADMIN DATA---------------
## Prep WHO official country-reported estimates                                                     
admin.vacc <- c("vacc_bcg", "vacc_polio3", "vacc_dpt1", "vacc_dpt3", "vacc_hepb3", "vacc_hib3", 
                "vacc_mcv1", "vacc_mcv2", "vacc_pcv1",  "vacc_pcv3", "vacc_rotac", "vacc_yfv", "vacc_rcv1")  
who.official <- lapply(admin.vacc, prep.who_official) %>% rbindlist
saveRDS(who.official, paste0(data_root, "FILEPATH/who_official.rds"))

## Prep WHO admininstrative data
who.admin <- lapply(admin.vacc, prep.who_admin) %>% rbindlist
saveRDS(who.admin, paste0(data_root, "FILEPATH/who_admin.rds"))


#----COMBINE JRF ADMIN DATA WITH COUNTRY-REPORTED ESTIMATES TO USE IN MODELING--------------------------------------
## Create continuous time series of admin data, using official country-estimate where admin data is missing
combined <- combine.jrf.data()
saveRDS(combined, paste0(data_root, "FILEPATH/who_combined.rds"))


#----USE DATA REPORTED TO UNICEF VS WHO FOR INDIA 1980-1999---------------------------------------------------------
## Swap in alternative JRF admin data (e.g. IND UNICEF-reported vs WHO-reported)
who.official <- readRDS(paste0(data_root, "FILEPATH/who_official.rds"))  
replaced <- use.alternate.admin(new_data=ind.unicef, change_df=who.official)
saveRDS(replaced, paste0(data_root, "FILEPATH/who_official.rds"))


#----ADMIN DATA DENOMINATOR ADJUSTMENT------------------------------------------------------------------------------
## Apply new vaccine scale-up denominator adjustment                                                      
who.admin <- denom.adjust(adjust_file="who_official")                     


#----PREP JRF-REPORTED STOCKOUTS------------------------------------------------------------------------------------
## Prep WHO reported stockout years file to use when making covariate in modeling prep_exp
reported     <- read_excel(vacc.stock.ind, sheet="Indicator") %>% data.table
who.reported <- prep.reported.stockouts(reported) 
# append on unreported but known stockouts
add.stockouts <- fread(stockout.add)[keep==1][, keep := NULL]
setnames(add.stockouts, "me_name", "me")
# drop the last number of 'me'
add.stockouts$me <- gsub("[0-9]+", "", add.stockouts$me)
add.stockouts <- add.stockouts[, c("ihme_loc_id", "year_id", "me"), with = FALSE]
who.reported <- rbind(who.reported, add.stockouts)

rotac_ratio <- who.reported[me=="vacc_rotac"]
rotac_ratio <- rotac_ratio[, me := "vacc_rotac_dpt_ratio"]
mcv_ratio <- who.reported[me=="vacc_mcv"]
mcv_ratio <- mcv_ratio[, me := "vacc_mcv_mcv_ratio"]
hepb_ratio <- who.reported[me=="vacc_hepb"]
hepb_ratio <- hepb_ratio[, me := "vacc_hepb_dpt_ratio"]
hib_ratio <- who.reported[me=="vacc_hib"]
hib_ratio <- hib_ratio[, me := "vacc_hib_dpt_ratio"]
pcv_ratio <- who.reported[me=="vacc_pcv"]
pcv_ratio <- pcv_ratio[, me := "vacc_pcv_dpt_ratio"]
rcv_ratio <- who.reported[me=="vacc_rcv"]
rcv_ratio <- rcv_ratio[, me := "vacc_rcv_mcv_ratio"]
who.reported <- rbind(who.reported, rotac_ratio, mcv_ratio, hepb_ratio, hib_ratio, pcv_ratio, rcv_ratio)

saveRDS(who.reported, file.path(ref_data_repo, "who_reported_stockouts.rds"))
saveRDS(who.reported, paste0(data_root, "FILEPATH/", "who_reported_stockouts_", date, ".rds"))  
