# HEADER ------------------------------------------------------------------
# Author:  USERNAME
# Date:    2018-04-10
# Project: GBD Vaccines: DPT ordinal regression model
# Purpose: Making DPT1 coverage draws from DPT12 conditional and DPT3 draws (from new ordinal regression)
# source("FILEPATH/dpt1_draw_math.R")
#**************************************************************************


dpt1.draw.math <- function(dpt_cond_id, dpt_3_id, gbdcycle=gbd_cycle, run_date=date, me="vacc_dpt1", 
                           covid_disruption=FALSE, covid_version=NULL) {

### using ST-GPR draw-level output:
run_id_12 <- dpt_cond_id  
run_id_3  <- dpt_3_id  

# Modified to use GBD read.draws() 
pacman::p_load(data.table, magrittr)
read.draws <- function(me) {
  path <- paste0("FILEPATH/", me)
  files <- list.files(path, full.names=TRUE)
  if (length(files)==0) stop(paste0("No draws for this run_id (", me, ")"))
  df <- mclapply(files, fread, mc.cores=10) %>% rbindlist(., use.names=TRUE)
  key <- c("location_id", "year_id", "age_group_id", "sex_id")
  setkeyv(df, cols=key)
  df <- unique(df)
  return(df)
}

message("Reading dpt12_conditional draws \n")
dpt12_test <- read.draws("vacc_dpt12_cond")
dpt12_test <- dpt12_test[year_id %in% c(year_start:year_end)]  
dpt12_info_test <- dpt12_test[,c("location_id", "year_id", "age_group_id", "sex_id")]
dpt12_draws_test <- dpt12_test[,-c("location_id", "year_id", "age_group_id", "sex_id")]

message("Reading dpt3 draws \n")
dpt3_test  <- read.draws("vacc_dpt3")

if (covid_disruption) {
  ### need to take away the disruption in dpt3 by dividing it away!
  # read in the covid adjustments for dtp3
  antigen <- "vaccine_dtp"
  covid_disrupt <- fread(file.path("FILEPATH/covid_interruption_covariates",  
                                   paste0("cumulative_", covid_version, "_new_data_prep"), paste0("mrbrt_", antigen, "_results_annual_FHS.csv")))[scenario_id==0][,.(location_id, year_id, value)]
  
  # for subnationals, apply parent disruption to subnational location
  sq <- CJ(location_id=unique(locations[level>=3, location_id]),
           year_id=2020:max(covid_disrupt$year_id))
  covid_disrupt <- merge(sq, covid_disrupt, by=c("location_id", "year_id"), all.x=T)
  covid_disrupt <- merge(covid_disrupt, locations[,.(location_id, level, parent_id)], by="location_id", all.x=T)
  for (lvl in unique(covid_disrupt[level > 3]$level) %>% sort) {
    covid.parent <- covid_disrupt[, .(location_id, year_id, value)]
    setnames(covid.parent, c('value', 'location_id'), c('value_parent', 'parent_id'))
    covid_disrupt <- merge(covid_disrupt, covid.parent, by=c('parent_id', 'year_id'), all.x=TRUE)
    covid_disrupt <- covid_disrupt[level==lvl & is.na(value), value := value_parent]
    covid_disrupt <- covid_disrupt[, value_parent := NULL]
  }
  covid_disrupt <- covid_disrupt[, c("parent_id", "level") := NULL]
  
  # merge to df
  dpt3_test <- merge(dpt3_test, covid_disrupt, by=c("location_id", "year_id"), all.x=T)
  
  # .SDcols to divide away
  cols <- grep("draw_", names(dpt3_test), value=TRUE)
  dpt3_test <- dpt3_test[!is.na(value), (cols) := .SD/value, .SDcols=cols]
  # get rid of cols that don't need anymore
  dpt3_test[, value := NULL]

}

dpt3_info_test <- dpt3_test[,c("location_id", "year_id", "age_group_id", "sex_id")]
dpt3_draws_test <- dpt3_test[,-c("location_id", "year_id", "age_group_id", "sex_id")]



##### dpt1_cov = dpt12cond(1 - dpt3_cov) + dpt3_cov
one_matrix_test <- matrix(1, dim(dpt3_draws_test)[1], dim(dpt3_draws_test)[2])
one_minus_dpt3_draws_test <- one_matrix_test - dpt3_draws_test
left_draws_test <- dpt12_draws_test * one_minus_dpt3_draws_test

dpt1_draws_test <- left_draws_test + dpt3_draws_test


diff_test_test <- dpt1_draws_test - dpt3_draws_test

df <- data.frame(dpt12_info_test, dpt1_draws_test)

# blah <- copy(df)
df <- as.data.table(df)


if (covid_disruption) {
  ### need to add the calculated dtp3 covid disruption to dpt1!
  # read in the covid adjustments for dtp3
  antigen <- "vaccine_dtp"
  covid_disrupt <- fread(file.path("FILEPATH/covid_interruption_covariates",  
                                   paste0("cumulative_", covid_version, "_new_data_prep"), paste0("mrbrt_", antigen, "_results_annual_FHS.csv")))[scenario_id==0][,.(location_id, year_id, value)]
  
  # for subnationals, apply parent disruption to subnational location
  sq <- CJ(location_id=unique(locations[level>=3, location_id]),
           year_id=2020:2021)
  covid_disrupt <- merge(sq, covid_disrupt, by=c("location_id", "year_id"), all.x=T)
  covid_disrupt <- merge(covid_disrupt, locations[,.(location_id, level, parent_id)], by="location_id", all.x=T)
  for (lvl in unique(covid_disrupt[level > 3]$level) %>% sort) {
    covid.parent <- covid_disrupt[, .(location_id, year_id, value)]
    setnames(covid.parent, c('value', 'location_id'), c('value_parent', 'parent_id'))
    covid_disrupt <- merge(covid_disrupt, covid.parent, by=c('parent_id', 'year_id'), all.x=TRUE)
    covid_disrupt <- covid_disrupt[level==lvl & is.na(value), value := value_parent]
    covid_disrupt <- covid_disrupt[, value_parent := NULL]
  }
  covid_disrupt <- covid_disrupt[, c("parent_id", "level") := NULL]
  
  # merge to df
  df <- merge(df, covid_disrupt, by=c("location_id", "year_id"), all.x=T)
  # .SD cols to multiply out
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[!is.na(value), (cols) := .SD*value, .SDcols=cols]
  # get rid of cols that don't need anymore
  df[, value := NULL]

}


### save DPT1 draw file as have for all other antigens (i.e. by {loc_id}.csv)
draw_dir <- paste0("FILEPATH/")
ifelse(!dir.exists(draw_dir), dir.create(draw_dir), FALSE)

message("Saving calculated dpt1 draws and means \n")
location_ids <- unique(df$location_id)
invisible(mclapply(location_ids, function(x) {
  fwrite(df[location_id==x], paste0(draw_dir, "/", x, ".csv"), row.names=FALSE)
}, mc.cores=10))


### Apply delayed EPI to these DTP1 draws 
source(file.path(code_root, "FILEPATH/prep_save_results.r"))
df <- df[, me_name := me]
df <- set.intro.epi(df, me)


### aggregate, etc. to SAVE as .rds file
df <- collapse.draws(df)
df[, covariate_id := NA]  
save.collapsed(df, me)
print(paste0("Saved collapsed ", me, " in ", results.root))

}

##*************************************************************************************************************************************
