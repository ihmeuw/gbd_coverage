#----HEADER-------------------------------------------------------------------------------------------------------------
# Authors: USERNAME
# Date:    October 2017 with rolling annual updates
# Purpose: Prep vaccination modelable entities for ST-GPR modeling; modifying for dpt1_cond ordinal regression modeling
#***********************************************************************************************************************


#----SETUP--------------------------------------------------------------------------------------------------------------
### clear workspace
rm(list=ls())

### set os flexibility
username <- Sys.info()[["user"]]
os <- .Platform$OS.type
if (os == "windows") {
  j_root <- "FILEPATH/"
} else {
  j_root <- "FILEPATH/"
  h_root <- "~/"
}


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
library(binom)
library(stringr)
library(magrittr)
library(mgcv)
library(forecast)


### load functions
source(paste0("FILEPATH/init.r"))
decomp_step <- "iterative"
source(db_tools)
source("FILEPATH/read_excel.R")
source("FILEPATH/collapse_point.R")
source("FILEPATH/get_population.R")
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_covariate_estimates.R")
source("FILEPATH/utility.r")  
'%!in%'  <- function(x,y)!('%in%'(x,y))
locs <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step=decomp_step)[level >= 3, ]

### source central functions to interact with Epi uploader for ST-GPR, and also custom wrapper
source('FILEPATH/save_bundle_version.R')
source('FILEPATH/get_bundle_version.R')
source('FILEPATH/save_crosswalk_version.R')
source('FILEPATH/get_crosswalk_version.R')
source("FILEPATH/get_bundle_data.R")
source("FILEPATH/upload_bundle_data.R")
# bundle upload wrapper
source("FILEPATH/bundle_wrapper.R")


### make directory
date            <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")
to_model_dir    <- file.path(data_root, "exp/to_model", gbd_cycle, date); if (!dir.exists(to_model_dir)) dir.create(to_model_dir)
share_model_dir <- file.path("FILEPATH/coverage")
NOTES           <- paste0("NOTES; ", date)

### Data options
run_via_bundle  <- FALSE  
beta_version    <- TRUE   
full_coverage   <- FALSE  
lit_global_fix  <- TRUE   

### MICS options
hybrid_mics     <- TRUE   
OLD_MICS        <- FALSE 

### Bias correction
# ST-GPR bias
launch_bias_correction <- TRUE   
  alt_bias_cycle <- "gbd2020"; alt_bias_date <- "2021-03-11"  
manual_offset_remove   <- FALSE  
# MR-BRT bias
proj            <- "proj_cov_vpd"  
queue           <- "long"  
plot_mrbrt_bias <- FALSE

### Stockout options
run.stockout    <- FALSE   
stkout_method   <- "gam"  
ratio_stockouts <- FALSE  

### DPT adjustment options
dpt_conditional    <- TRUE    
launch_dpt_dropout <- FALSE   
  alt_dropout_run_id <- 168605  
dpt_timeliness     <- FALSE   
  
### Launch options
just_dpt_cond       <- FALSE  
just_dpt3           <- FALSE
straight_only       <- FALSE  
dpt_timeliness_only <- FALSE  
polio3_only         <- FALSE
ratios_only         <- FALSE
just_mcv1           <- FALSE
just_bcg            <- FALSE
just_rotac_ratio    <- FALSE
just_hib3_ratio     <- FALSE
just_hepb3_ratio    <- FALSE
mcv_rcv_ratios_only <- FALSE
# age-specific prep only launch option (set bias_only to TRUE)
bias_only           <- FALSE
  ifelse(bias_only, on_time_cohort_only <- TRUE, on_time_cohort_only <- FALSE)  
  if (bias_only) run.stockout <- TRUE  

cause_decomp_step <- "iterative" 

### list models to launch
straight_models    <- c("vacc_mcv1", "vacc_dpt3", "vacc_bcg", "vacc_polio3")
ratios             <- c("vacc_hib3_dpt3_ratio", "vacc_pcv3_dpt3_ratio", "vacc_rotac_dpt3_ratio",
                        "vacc_rcv1_mcv1_ratio", "vacc_hepb3_dpt3_ratio", "vacc_mcv2_mcv1_ratio")
mes_to_launch      <- c(straight_models, ratios) 
correlation_models <- c(paste0("correlation_", c("hepb3_polio3_mcv1_dpt3", "hib3_polio3_mcv1_dpt3", "pcv3_polio3_mcv1_dpt3", "rotac_polio3_mcv1_dpt3")), "vacc_dpt3_polio3_mcv1")
admin.vacc <- c("vacc_bcg", "vacc_polio3", "vacc_dpt1", "vacc_dpt3", "vacc_hepb3", "vacc_hib3",
                "vacc_mcv1", "vacc_mcv2", "vacc_pcv1",  "vacc_pcv3", "vacc_rotac", "vacc_yfv", "vacc_rcv1")
if (full_coverage)
  mes_to_launch    <- c(mes_to_launch, correlation_models)
if (dpt_conditional)
  mes_to_launch    <- c(mes_to_launch, "vacc_dpt12_cond")  
if (dpt_timeliness)
  mes_to_launch    <- c(mes_to_launch, "vacc_dpt3_timeliness_ratio")  

### version notes
if (!exists("NOTES")) NOTES <- paste0("Whoops, you forgot to make an informative note so... ", date)
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### load vaccine introduction and admin coverage paths
vacc.intro     <- file.path(ref_data_repo, "vaccine_intro.rds")
vacc.admin     <- paste0(data_root, "FILEPATH/who_official.rds")  
vacc.whosurvey <- paste0(data_root, "FILEPATH/who_survey.rds")
vacc.survey    <- paste0(data_root, "FILEPATH/")
vacc.outliers  <- file.path(ref_data_repo, "vaccination.csv")
vacc.lit       <- file.path(ref_data_repo, "vaccine_lit.rds")  
vacc.schedule  <- file.path(ref_data_repo, "vaccine_schedule.rds")
vacc.epi       <- file.path(ref_data_repo, "vaccine_intro_epi.rds")  
vacc.stockouts <- paste0(data_root, "FILEPATH/exact_matches.csv")  
vacc.ratio.stockouts <- paste0(data_root, "FILEPATH/exact_ratio_matches.csv")
stockout.cov.out <- paste0(ref_data_repo, "/FILEPATH/stockout_cov.csv")  
age.specific.stockout.out <- file.path(ref_data_repo, "age_specific_stockouts_outliers.csv")
mics.tabulations <- paste0(data_root, "FILEPATH/mics_tabulations.rds")  
vacc.timeliness  <- paste0(data_root, "FILEPATH/mics_timeliness_tabulations.rds")

### load other references
me.db <- file.path(ref_data_repo, "me_db.csv") %>% fread  
bundle.version.dir <- file.path(ref_data_repo, "gbd_covariate_bundle_versions.csv")

### set objects
year.est.start <- year_start %>% as.numeric
year.est.end   <- year_end %>% as.numeric

### age_bins 
max_bin   <- 7  
age_bins  <- fread(file.path(ref_data_repo, "vax_age_lookup.csv"))[age_bin <= max_bin][,.(age_bin, definition, gbd_ids, age_month_start, age_month_end, age_year_start, age_year_end)]  
age_bins[, `:=` (age_month_start=as.numeric(age_month_start), age_month_end=as.numeric(age_month_end))]
age_bins[, month_range := Map(":", age_month_start, age_month_end)]
#***********************************************************************************************************************


########################################################################################################################
# SECTION 1: Prep tabulated survey data
########################################################################################################################


#----SURVEY FUNCTIONS---------------------------------------------------------------------------------------------------
### get tabulations
get.survey.data <- function() {
  
  df <- lapply(list.files("FILEPATH", full.names=TRUE), fread) %>% rbindlist(., fill=TRUE)
  setnames(df, "mean", "data")
  df[, variance := standard_error ^ 2]  
  df[, age_group_id := 22]
  df[, cv_survey := 1]
  drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id), ihme_loc_id] %>% unique
  if (length(drop.locs) > 0 ) {
    print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
    df <- df[!(ihme_loc_id %in% drop.locs)]
  }
  if (nrow(df[data %in% c(0, 1)]) > 0) {
    df.w <- df[data %in% c(0, 1)]
    sample_size <- df.w$sample_size
    n  <- ifelse(df.w$data==0, 0, sample_size)
    ci <- binom.confint(n, sample_size, conf.level=0.95, methods="wilson")
    se <- (ci$upper - ci$lower) / 3.92
    variance <- se ^ 2 * 2.25 
    df[data %in% c(0, 1)]$variance <- variance
  }
  df <- df[design_effect < 1, variance := variance * 2.25 / design_effect]
  df <- df[design_effect < 1, design_effect := 2.25]
  
  return(df)
  
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 2: Outlier data
########################################################################################################################


### outlier data from outlier database
outlier.data <- function(df, gbd=TRUE) {
  
  ## Get outlier frame
  df.o <- fread(vacc.outliers) %>% unique
  if (dpt_timeliness) {
    df.time <- df.o[me_name=="vacc_dpt3"]
    df.time <- df.time[, me_name := "vacc_dpt3_timeliness_ratio"]
    df.o <- rbind(df.o, df.time)
  }
  if (gbd) df.o <- df.o[gbd==1, ]
  df.o <- df.o[, outlier := 1] %>% .[, .(me_name, nid, batch_outlier, ihme_loc_id, year_id)]
  df.o[, year_id := as.integer(year_id)]
  ## Set conditions
  cond <- list(
    list('me_name==""', 
         c("nid")),
    list('batch_outlier==1 & me_name != ""', 
         c("me_name", "nid")),
    list('batch_outlier==0', 
         c("me_name", "nid", "ihme_loc_id", "year_id"))
  )
  ## Loop through and outlier by condition
  for (i in cond) {
    # print(i)
    condition <- i[[1]]
    vars <- i[[2]]
    ## Subset
    o.sub <- df.o[eval(parse(text=condition)), (vars), with=FALSE]
    o.sub <- o.sub[, outlier := 1]
    ## Merge
    df <- merge(df, unique(o.sub), by=vars, all.x=TRUE)  
    ## Set outlier
    df <- df[outlier==1 & !is.na(data), cv_outlier := data]
    ## Clean
    df <- df[outlier==1 & !is.na(data), data := NA]
    df <- df[, outlier := NULL]
  }
  return(df)
  
}

### temp outlier certain surveys 
temp.outlier <- function(df) {
  
  df <- df[which(grepl("WHO_WHS", survey_name) & !(nid %in% c(21535)) & !is.na(data)), cv_outlier := data]
  df <- df[which(grepl("WHO_WHS", survey_name) & !(nid %in% c(21535)) & !is.na(data)), data := NA]
  
  df <- df[which(grepl("SUSENAS", survey_name) & !is.na(data)), cv_outlier := data]
  df <- df[which(grepl("SUSENAS", survey_name) & !is.na(data)), data := NA]
  
  df <- df[which(grepl("IND/HMIS", survey_name) & !is.na(data)), cv_outlier := data]
  df <- df[which(grepl("IND/HMIS", survey_name) & !is.na(data)), data := NA]
  if (dpt_timeliness) {
    df <- df[which(grepl("IND/HMIS", survey_name) & me_name=="vacc_dpt3_timeliness_ratio" & !is.na(data)), cv_outlier := data]
    df <- df[which(grepl("IND/HMIS", survey_name) & me_name=="vacc_dpt3_timeliness_ratio" & !is.na(data)), data := NA]
  }
  df <- df[which(grepl("IND/HMIS", survey_name) & ihme_loc_id=="IND" & !is.na(data)), cv_outlier := data]
  df <- df[which(grepl("IND/HMIS", survey_name) & ihme_loc_id=="IND" & !is.na(data)), data := NA]
  df <- df[grepl("IND/HMIS", survey_name), cv_admin := 1]
  df <- df[grepl("BRA/NATIONAL_IMMUNIZATION_PROGRAM_PNI", survey_name), cv_admin := 1]
  
  df <- df[which(grepl("MEX/INEGI", survey_name) & me_name=="vacc_dpt3" & !is.na(data)), cv_outlier := data]
  df <- df[which(grepl("MEX/INEGI", survey_name) & me_name=="vacc_dpt3" & !is.na(data)), data := NA]
  if (dpt_timeliness) {
    df <- df[which(grepl("MEX/INEGI", survey_name) & me_name=="vacc_dpt3_timeliness_ratio" & !is.na(data)), cv_outlier := data]
    df <- df[which(grepl("MEX/INEGI", survey_name) & me_name=="vacc_dpt3_timeliness_ratio" & !is.na(data)), data := NA]
  }
  
  df <- df[which(grepl("RUS/LONGITUDINAL", survey_name) & age_year != 2 & !is.na(data)), cv_outlier := data]
  df <- df[which(grepl("RUS/LONGITUDINAL", survey_name) & age_year != 2 & !is.na(data)), data := NA]
  
}


dpt.outlier <- function(df) {

  dpt <- copy(df)[me_name %in% c("vacc_dpt1", "vacc_dpt3") & !is.na(data) & cv_admin==1][,.(me_name, nid, ihme_loc_id, year_id, age_year, age_length, survey_name, data)]  
  gbr_subs <- locs[grep("GBR_", ihme_loc_id), ihme_loc_id]
  dpt <- dpt[!ihme_loc_id %in% gbr_subs] 
  
  dpt_wide <- data.table::dcast(dpt, ... ~ me_name, value.var = "data")
  dpt_wide <- dpt_wide[!is.na(vacc_dpt1) & !is.na(vacc_dpt3)]  

  dpt_wide[, dropout := vacc_dpt1-vacc_dpt3]
  
  dpt_outlier <- dpt_wide[dropout < 0, outlier_dpt1 := 1][outlier_dpt1==1]
  dpt_outlier[, id := paste0("vacc_dpt1", nid, ihme_loc_id, year_id, age_year, age_length)]
  
  # Match dpt_outlier rows to rows in df
  df[, id := paste0(me_name, nid, ihme_loc_id, year_id, age_year, age_length)]
  
  # Where ids match, move dpt1 admin data value to cv_outlier and NA in data
  df[id %in% unique(dpt_outlier$id), `:=` (cv_outlier=data, data=NA)]
  
  # Null 'id' column
  df[, id := NULL]
  
  return(df)

}


### Pre-intro year ratio data cleaning after add in intro years for new vaccines 
pre.intro.outliers <- function(df) {
  
  # Outlier admin data pre-intro 
  df[year_id < cv_intro & is.na(cv_outlier) & !is.na(data), `:=` (cv_outlier=data, data=NA)] 
  
  return(df)
  
}



#***********************************************************************************************************************


########################################################################################################################
# SECTION 3: Clean data
########################################################################################################################


#----BASIC CLEANING FUNCTIONS-------------------------------------------------------------------------------------------
### cleanup dataset
clean.data <- function(df) {
  
  ## Get list of cvs
  cvs <- grep("cv_", names(df), value=TRUE)
  ## Subset to ones that are binary
  i <- lapply(cvs, function(x) length(setdiff(unique(df[!is.na(get(x))][[x]]), c(0, 1)))==0) %>% unlist
  cvs <- cvs[i]
  ## Harmonize cv_* terms that are binary
  for (var in cvs) df[is.na(get(var)), (var) := 0]
  ## Clean year_id
  df <- df[, year_id := as.numeric(as.character(year_id))]
  ## Clean sample_size
  df <- df[, sample_size := as.numeric(sample_size)]
  ## Offset coverage
  df <- df[data <= 0, data := 0.001]
  df <- df[data >= 1, data := 0.999]
  ## Age group and sex
  df <- df[, `:=` (age_group_id=22, sex_id=3)]
  ## Bundle
  df <- df[, bundle := "vaccination"]
  ## Order
  df <- df[order(me_name, ihme_loc_id, year_id)]
  return(df)
  
}

### create unique identifier by survey
create.id <- function(df) {
  
  ## Harmonize the survey_name of WHO admin data so can accurately make ids 
  df[, survey_name_orig := survey_name]
  df[nid==203321, survey_name := "WHO/UNICEF Official Country-Reported Data"]
  
  ## Create id for cleanliness
  cols.id <- c('nid', 'ihme_loc_id', 'year_start', 'year_end', 'year_id', 'survey_name', 'survey_module', 'file_path', 'age_end')  
  cols.id <- cols.id[cols.id %in% names(df)]
  
  df <- df[, cv_id := .GRP, by=cols.id] 

  ## Look for duplicates
  df <- df[, dupe := lapply(.SD, function(x) length(x) - 1), .SDcols='cv_id', by=c('cv_id', 'me_name')]
  
  df[, cv_id_2 := paste0(me_name, cv_id)]  
  df <- df[!duplicated(df$cv_id_2, fromLast=TRUE), ]  
  df <- df[, dupe := lapply(.SD, function(x) length(x) - 1), .SDcols='cv_id', by=c('cv_id', 'me_name')]
  
  ndupe <- nrow(df[which(dupe > 0)])
  if (ndupe > 0) {
    print(paste0("Dropping duplicates by cv_id: ", ndupe, " rows"))
    df <- df[!which(dupe > 0)]
  }
  ## Check for remaining duplicates
  if (max(df$dupe) > 0) stop("Duplicates in meta")
  df$dupe <- NULL
  
  ## Change survey_name of WHO admin data back to original 
  df[nid==203321, survey_name := survey_name_orig]
  df <- df[, survey_name_orig := NULL]
  
  
  return(df)
  
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 4: Calculate out administrative bias
########################################################################################################################


#----ADMIN BIAS CORRECTION FUNCTIONS------------------------------------------------------------------------------------
### function to use ST-GPR to smooth administrative bias using SDI as a predictor
est.time_varying_bias <- function(df, launch=launch_bias_correction, NOTES, straight_models, 
                                  alt_bias_cycle=alt_bias_cycle, alt_bias_date=alt_bias_date, 
                                  on_time_cohort_only=on_time_cohort_only, locations=locs, bins=age_bins, 
                                  cycle=gbd_cycle, rn=date) {
  
  to_model_bias_dir <- file.path(to_model_dir, "admin_bias")
  ifelse(!dir.exists(to_model_bias_dir), dir.create(to_model_bias_dir), FALSE)
  
  if (launch) {
    
    ### prep dataset to include in bias adjustment 
    print("Prepping dataset")
    df.include <- df[!is.na(data) & me_name %in% straight_models, ]
    
    ## Only keep the survey data corresponding to the on-time scheduled cohorts
    if (on_time_cohort_only) {
      
      # subset out the admin data
      df.include_admin <- df.include[cv_admin==1]
    
      df.include_1 <- df.include[cv_admin != 1 & !me_name %in% "vacc_mcv1" & age_year==2]
       df.age <- readRDS(file.path("FILEPATH/03_data_post_outliering.rds")) %>%
       df.age <- df.age[age_bin==4 & me_name=="vacc_mcv1"]  # just keep 12-23mo data

      # rbind in admin data
      df.include <- rbind(df.age, df.include_1, df.include_admin, fill=T)
     
    }
    
    df.include <- df.include[cv_lit==1 & !is.na(sample_size), variance := data * (1 - data) / sample_size]
    df.include <- df.include[cv_lit==1 & is.na(variance), variance := data * (1 - data) / 100]
    df.include[is.na(standard_error) & !is.na(variance), standard_error := sqrt(variance)]
    
    ### calculate difference between admin data and survey
    ifelse(on_time_cohort_only, merge_cols <- c("me_name", "ihme_loc_id", "year_id", "sex_id"), 
           merge_cols <- c("me_name", "ihme_loc_id", "year_id", "age_group_id", "sex_id"))  
    df.adjust  <- merge(df.include[cv_admin==1, c(merge_cols, "nid", "data", "standard_error", "variance"), with=FALSE],
                        df.include[cv_admin==0, c(merge_cols, "data", "standard_error", "variance", "survey_name", "year_start"), with=FALSE],
                        by=merge_cols, all=TRUE)
    df.adjust  <- df.adjust[!is.na(data.x) & !is.na(data.y), ]
    df.adjust[, data := data.y / data.x]  # data.y is survey data; data.x is admin data
    df.adjust[, variance := data.y ^ 2 / data.x ^ 2 * (variance.y / data.y ^ 2 + 0 / data.x ^ 2)]
    df.adjust <- merge(df.adjust, locs[, .(location_id, ihme_loc_id)], by="ihme_loc_id", all.x=TRUE)
    df.adjust[, sample_size := NA_integer_]  
    
    if (on_time_cohort_only) df.adjust <- df.adjust[, age_group_id := 22]
    
    saveRDS(df.adjust, file = file.path(to_model_bias_dir, "bias_correction_data_pairs_pre_outlier.rds"))
    
    mrbrt_launch <- paste0("qsub -q ", queue, ".q -l h_rt=01:00:00 -l m_mem_free=10G -l fthread=2 -l archive -P ", proj, " -N mrbrt_cascade",  
                           " -o FILEPATH",
                           " FILEPATH/", 
                           " -s FILEPATH/mrbrt_cascade.R",
                           " --run_date ", date,
                           " --plot_mrbrt_bias ", plot_mrbrt_bias)
    system(mrbrt_launch)

    # job hold
    file_names <- c(file.path("FILEPATH/mrbrt_cascade", date, "done.csv"))
    sum_files <- 0
    while (sum_files < 1) {
      sum_files <- sum(sapply(file_names, file.exists))
      print('still waiting on the MR-BRT admin bias adj model to finish')
      Sys.sleep(60)
    }
    message('MR-BRT bias model complete -- moving on to prep for ST-GPR')
      
    # script to prep MR-BRT outputs as custom_stage_1 inputs to ST-GPR
    source(file.path(code_root, "FILEPATH/custom_stage_1_prep.R"))
    
    
    ### outlier
    outlier <- df.adjust[(data > 2) |  
                           (ihme_loc_id=="GNB" & data > 1.4) |  
                           (ihme_loc_id=="TGO" & year_id==1996) |  
                           (ihme_loc_id=="VUT" & year_id %in% 2003:2005) |  
                           (ihme_loc_id=="MHL" & year_id==2004), ] 

    fwrite(outlier, file.path(to_model_bias_dir, "outliers.csv"), row.names=FALSE)
    df.adjust <- fsetdiff(df.adjust, outlier, all = TRUE)
    df.adjust <- df.adjust[, .(me_name, nid, location_id, year_id, age_group_id, sex_id, data, variance, sample_size)]
    
    ### add new columns for new ST-GPR
    df.adjust <- df.adjust[, measure_id := 18]  
    df.adjust <- df.adjust[, is_outlier := 0]
    
    saveRDS(df.adjust, file = file.path(to_model_bias_dir, "bias_correction_data_pairs.rds"))
    
    ### save for upload
    for (i in straight_models) {
      df.sub <- df.adjust[me_name==i, ] %>% copy
      setnames(df.sub, "data", "val") 
      df.sub[, me_name := paste0("bias_", me_name)]
      fwrite(df.sub, file.path(to_model_bias_dir, paste0(i, ".csv")), row.names=FALSE, na="")
      fwrite(df.sub, file.path(share_model_dir, "bias", paste0(i, ".csv")), row.names=FALSE, na="")
    }
    
    if (run_via_bundle) {
      for (i in straight_models) {
        print(paste0("Preparing to upload bias_", i, " bundle"))
        upload_model_xw(data=df.adjust, me=i, data_date=date, bundle_map=me.db, path_to_version_dir=bundle.version.dir,
                        decomp="iterative", round=gbd_round, note=NOTES, bias=TRUE)
      }
    }  

    
    ### launch ST-GPR models
    print("Calculating administrative bias across vaccinations")
    
    # make sure no residual RUNS object exists from dpt dropout model
    if (launch_dpt_dropout | exists("RUNS")) rm(RUNS)  

    source(file.path(code_root, "FILEPATH/launch.r"), local=TRUE)
    
    message("Sleeping for 5 min while models run before checking to see if complete")
    Sys.sleep(300)  
    message("Checking for jobs still running...")
    message("RUNS[1]...")
    while(length(system(paste0("qstat -r | grep ", RUNS[1]), intern = T)) > 0) { Sys.sleep(60) }
    message("RUNS[2]...")
    while(length(system(paste0("qstat -r | grep ", RUNS[2]), intern = T)) > 0) { Sys.sleep(30) }
    message("RUNS[3]...")
    while(length(system(paste0("qstat -r | grep ", RUNS[3]), intern = T)) > 0) { Sys.sleep(30) }
    message("RUNS[4]...")
    while(length(system(paste0("qstat -r | grep ", RUNS[4]), intern = T)) > 0) { Sys.sleep(30) }
    message("All models supposedly done!")
    
    # check that run actually finished and didn't break 
    if (check_run(RUNS[1]) != 1) stop(paste0('run_id ', RUNS[1], ' broke! Find error before continuing!'))
    if (check_run(RUNS[2]) != 1) stop(paste0('run_id ', RUNS[2], ' broke! Find error before continuing!'))
    if (check_run(RUNS[3]) != 1) stop(paste0('run_id ', RUNS[3], ' broke! Find error before continuing!'))
    if (check_run(RUNS[4]) != 1) stop(paste0('run_id ', RUNS[4], ' broke! Find error before continuing!'))
    print('Runs finished!')
    
    
    
  # pull model results
  if (manual_offset_remove) {
    
    message("Need to pull draws, remove offset, then collapse, then assign run_id")
    ### read in draws
    read.draws <- function(run_id) {
      path <- paste0("FILEPATH/draws_temp_0")  
      files <- list.files(path, full.names=TRUE)
      if (length(files)==0) stop(paste0("No draws for this run_id (", run_id, ")"))
      df <- mclapply(files, fread, mc.cores=10) %>% rbindlist(., use.names=TRUE)
      df[, run_id := run_id]
      key <- c("location_id", "year_id", "age_group_id", "sex_id")
      setkeyv(df, cols=key)
      df <- unique(df)
      cols <- grep("draw_", names(df), value=TRUE)
      df <- df[, (cols) := .SD-0.01, .SDcols=cols]
      return(df)
    }
    bias <- rbindlist(lapply(RUNS, read.draws)) %>% collapse_point
    old <- c("mean", "lower", "upper")
    new <- c("gpr_mean", "gpr_lower", "gpr_upper")
    setnames(bias, old, new)

  } else {
    
    bias <- rbindlist(lapply(RUNS, function(x) model_load(x, obj="raked") %>% data.table %>% .[, run_id := x])) 
    message("Just pulled admin adjustment model results!")
  }
  
  # attach me names
  logs_path <- file.path(ref_data_repo, "gbd_model/bias_run_log.csv")
  logs_file <- fread(logs_path)
  logs_file <- logs_file[, .(run_id, me_name)]
  logs_file$run_id <- as.numeric(logs_file$run_id)         
  bias <- merge(bias, logs_file, by="run_id", all.x=TRUE)  
  setnames(bias, "gpr_mean", "cv_admin_bias_ratio")
  bias <- merge(bias, locs[, .(location_id, ihme_loc_id)], by="location_id", all.x=TRUE)
  bias <- bias[, .(me_name, ihme_loc_id, year_id, age_group_id, sex_id, cv_admin_bias_ratio)]
  
  data_locs <- unique(df.adjust$location_id)
  has_subs <- locs[level==3 & most_detailed != 1, location_id]
  sub_ids1 <- locs[level >3 & parent_id %in% has_subs & parent_id %in% data_locs, location_id]
  sub_ids2 <- locs[level >3 & parent_id %in% sub_ids1, location_id]  
  sub_ids3 <- locs[level >3 & parent_id %in% sub_ids2, location_id]
  sub_ids4 <- locs[level >3 & parent_id %in% sub_ids3, location_id]
  data_locs <- c(data_locs, sub_ids1, sub_ids2, sub_ids3, sub_ids4) %>% unique
  
  global_locs <- setdiff(locs[level >= 3, location_id], data_locs)  
  # read in mrbrt cascade results
  work_dir <- paste0(j_root, "/FILEPATH/")
  mrbrt_results <- lapply(c("vacc_dpt3", "vacc_mcv1", "vacc_polio3", "vacc_bcg"), function(x) {
    mrbrt <- fread(file.path(work_dir, x, paste0("mrbrt_", x, "_results.csv")))
  }) %>% rbindlist
  # subset down to global_locs
  mrbrt_results <- mrbrt_results[location_id %in% global_locs]
  setnames(mrbrt_results, "pred", "cv_admin_bias_ratio")
  mrbrt_results <- merge(mrbrt_results, locs[, .(location_id, ihme_loc_id)], by="location_id", all.x=TRUE)[, `:=` (location_id=NULL, mean_value=NULL, age_group_id=22, sex_id=3)]
  # remove global_locs from existing bias object from stgpr
  bias <- bias[!ihme_loc_id %in% unique(mrbrt_results$ihme_loc_id)]
  # rbind global_locs bias from mrbrt
  bias <- rbind(bias, mrbrt_results)
  # save a file of bias effect to use in plotting code
  saveRDS(bias, file.path(to_model_bias_dir, "hybridized_bias.rds"))
  
  } else {
    print(paste0("Did NOT re-launch admin bias models and adjusting current data based on ", alt_bias_cycle, " ", alt_bias_date, " saved estimates."))
    # if not re-launching the admin bias model, pull in a previous set of hybridized bias estimates as specified
    alt_bias_path <- file.path(data_root, "FILEPATH/hybridized_bias.rds")
    bias <- readRDS(alt_bias_path)
    # save a file of bias effect to use in plotting code
    saveRDS(bias, file.path(to_model_bias_dir, "hybridized_bias.rds"))
    
  }
  
    ### merge on bias ratio
  bias[, cv_admin := 1]
  df <- merge(df, bias, by=c("me_name", "ihme_loc_id", "year_id", "age_group_id", "sex_id", "cv_admin"), all.x=TRUE)
  
  ### shift administrative data and save preshifted value
  print("Shifting administrative estimates")
  df <- df[cv_admin==1 & !is.na(cv_admin_bias_ratio), cv_admin_orig := data]
  df <- df[cv_admin==1 & !is.na(cv_admin_bias_ratio), data := data * cv_admin_bias_ratio]
  
  return(df)
  
}

### function to calculate uncertainty in administrative data
set.admin_variance <- function(df) {
  
  ### by country, year, vaccination, count how many non-admin points
  df <- df[, n_survey := length(data[!is.na(data) & cv_survey==1]), by=c("ihme_loc_id", "year_id", "me_name")]
  df <- df[n_survey > 0 & cv_admin==1, variance := data * (1 - data) / 10]
  df <- df[n_survey==0 & cv_admin==1, variance := data * (1 - data) / 50]
  
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 5: Adjust introduction frame on if data is present prior to official introduction date
########################################################################################################################


#----INTRO YEAR FUNCTIONS-----------------------------------------------------------------------------------------------
adjust.intro_frame <- function(df) {
  
  df <- df[, cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]
  return(df)
  
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 7: Create ratios for modeling
########################################################################################################################
make.ratios <- function(df, me) {
  
  ## Setup
  vaccs <- unlist(strsplit(me, "_"))[2:3]
  num <- paste0("vacc_", vaccs[1])
  denom <- paste0("vacc_", vaccs[2])
  ## Combine frames
  cols <- c("ihme_loc_id", "year_id", "age_group_id", "sex_id", "nid", "cv_id", "cv_admin_orig", "data", "variance", "sample_size", "cv_admin")  
  df.num <- df[me_name==num, cols, with=F]; df.num$x <- "num"
  df.denom <- df[me_name==denom, cols, with=F]; df.denom$x <- "denom"
  ## Make sure that ratio calculated between admin data is based on pre-bias shift
  df.num <- df.num[!is.na(cv_admin_orig), data := cv_admin_orig]
  df.num$cv_admin_orig <- NULL
  df.denom <- df.denom[!is.na(cv_admin_orig), data := cv_admin_orig]
  df.denom$cv_admin_orig <- NULL
  cols <- setdiff(cols, c("cv_admin_orig"))  
  df.r <- rbind(df.num, df.denom) %>% dcast.data.table(., ihme_loc_id + year_id + age_group_id + sex_id + nid + cv_id + cv_admin ~ x, value.var=c("data", "variance", "sample_size"))  
  ## Calculate ratios, variance
  df.r <- df.r[, data := data_num / data_denom]
  df.r <- df.r[!is.na(data)]
  df.r <- df.r[, variance := data_num ^ 2 / data_denom ^ 2 * (variance_num / data_num ^ 2 + variance_denom / data_denom ^ 2)] 
  df.r <- df.r[, sample_size := data * (1 - data) / variance] 
  ## Clean
  df.r <- df.r[, cols, with=F]
  df.r <- df.r[, me_name := me]
  return(df.r)
}
#***********************************************************************************************************************


########################################################################################################################
# SECTION 8: Split metadata
########################################################################################################################


#----SPLIT AND SAVE FUNCTIONS-------------------------------------------------------------------------------------------
### split dataset
split.meta <- function(df) {
  
  ### create id for cleanliness
  cols.id <- c('nid', 'ihme_loc_id', 'year_start', 'year_end', 'year_id', 'survey_name', 'survey_module', 'file_path')
  cols.meta <- c(cols.id[cols.id %in% names(df)], grep("cv_", names(df), value=TRUE), "stockout") 
  cols.data <- c('me_name', 'data', 'variance', 'sample_size')
  
  ### split sets
  meta <- df[, cols.meta, with=FALSE] %>% unique
  df.mod <- df[, c("ihme_loc_id", "year_id", "age_group_id", "sex_id", "nid",  "cv_id", "cv_intro", "cv_intro_years", "stockout", "cv_stockout",  "cv_stockout_ratio", cols.data), with=FALSE]
  
  return(list(meta=meta, df.mod=df.mod, df=df))
}


### account for border changes 
split.loc.data <- function(all_data, locations=locs) {

  ap_loc_id <- "IND_4841"
  t_loc_id  <- "IND_4871"
  ap_to_split <- all_data[ihme_loc_id==ap_loc_id & year_id <= 2014]  
  ap_nids <- unique(ap_to_split$nid)
  already_split <- unique(all_data[nid %in% ap_nids & ihme_loc_id %in% t_loc_id]$nid)
  # subset to just mes_to_launch here to reduce size of dataset that don't otherwise need
  ap_to_split <- ap_to_split[me_name %in% admin.vacc & !nid %in% already_split]  
  t_to_split <- copy(ap_to_split)
  t_to_split[, ihme_loc_id := t_loc_id]
  # merge together to add on population info to split sample_size
  both_to_split <- rbind(ap_to_split, t_to_split)
  
  # subset df of AP vs T population proportions (only need years before state divide)
  mini_pop <- get_population(age_group_id = 22, location_id = c(4841, 4871), sex_id = 3, year_id = 1980:2014,
                             gbd_round_id = gbd_round, decomp_step = cause_decomp_step)
  mini_pop_combined <- mini_pop[, lapply(.SD,sum),by = .(year_id, age_group_id, sex_id, run_id), .SDcols = "population"]
  setnames(mini_pop_combined, "population", "population_combined")
  mini_pop <- merge(mini_pop, mini_pop_combined[,.(year_id, population_combined)], by="year_id", all.x=T)
  # calculate proportion for AP vs T per year!
  mini_pop[, pop_proportion := population / population_combined]
  mini_pop <- merge(mini_pop, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
  
  # add pop_proportion col into both_to_split
  both_to_split <- merge(both_to_split, mini_pop[,.(year_id, ihme_loc_id, pop_proportion)], by=c("year_id", "ihme_loc_id"), all.x=T)
  
  # split sample_size
  both_to_split[, sample_size := sample_size * pop_proportion]
  
  # re-compute variance, standard_error
  both_to_split[, variance := data * (1 - data) / sample_size]
  both_to_split[, standard_error := NA]
  both_to_split$standard_error <- as.numeric(both_to_split$standard_error)
  
  # clean
  both_to_split[, pop_proportion := NULL]
  
  # remove the AP data getting split from all_data, and rbind in the split data (both_to_split)
  all_data <- all_data[!(ihme_loc_id==ap_loc_id & me_name %in% admin.vacc & nid %in% unique(ap_to_split$nid))]  
  all_data <- rbind(all_data, both_to_split)
  
  # all done!
  return(all_data)
  
}
#***********************************************************************************************************************




#----RUN---------------------------------------------------------------------------------------------------------------
df.survey    <- get.survey.data()                                                   
df.survey    <- df.survey[nid %!in% c(20798, 18865)]  
df.survey <- df.survey[!(nid==18979 & me_name=="vacc_mcv1")]

df.admin     <- readRDS(vacc.admin)[, year_id := as.numeric(as.character(year_id))] 

if (lit_global_fix) {
  
  df.survey2 <- copy(df.survey)
  df.survey2 <- df.survey2[, match_id := paste0(nid, ihme_loc_id, me_name)]

  df.lit2 <- readRDS(vacc.lit) %>%
    .[cv_admin != 1 | is.na(cv_admin), cv_lit := 1] %>%  
    .[, sample_size := as.numeric(gsub(",", "", sample_size))] %>%
    .[, match_id := paste0(nid, ihme_loc_id, me_name)] %>%
    .[!(match_id %in% unique(df.survey2$match_id)), ] %>%  
    .[, match_id := NULL]
  
  df.lit <- copy(df.lit2)
  
} else {
  
  df.lit       <- readRDS(vacc.lit) %>%                                               
    .[, cv_lit := 1] %>%
    .[, sample_size := as.numeric(gsub(",", "", sample_size))] %>%
    .[!nid %in% unique(df.survey$nid), ]                                            
  
}


df.lit.decider <- readRDS(vacc.lit) %>%                                         
  .[, cv_lit := 1] %>%
  .[, sample_size := as.numeric(gsub(",", "", sample_size))]


df <- rbind(df.survey, df.admin, df.lit, fill=TRUE)

saveRDS(df, file = file.path(to_model_dir, "01_rbind_survey_lit_admin.rds"))


if (hybrid_mics) {
  j <- fread(file.path(ref_data_repo, "mics_comparison_decisions_by_me_nid.csv"))  
  
  a <- fread(file.path(ref_data_repo, "mics_for_review_with_subnationals.csv"))
  
tabs_survey_name <- df.lit.decider[grep("UNICEF_MICS", survey_name),.(nid, file_path, ihme_loc_id, age_start, age_end, sample_size, me_name, data, age_length, age_year, year_id, survey_name)]
  tabs_file_path   <- df.lit.decider[grep("UNICEF_MICS", file_path),.(nid, file_path, ihme_loc_id, age_start, age_end, sample_size, me_name, data, age_length, age_year, year_id, survey_name)]
  tabs <- rbind(tabs_survey_name, tabs_file_path) %>% unique
  tabs[is.na(survey_name) | survey_name=="", survey_name := "UNICEF_MICS"]  
  
  decisions <- merge(a, j, by=c("me_name", "nid"), all.x=TRUE, all.y=TRUE)  
  decisions <- decisions[me_name=="ratio_dpt1_dpt3", me_name := "vacc_dpt3_dpt1_ratio"]
  decisions <- decisions[me_name=="ratio_hib3_dpt3", me_name :="vacc_hib3_dpt3_ratio"]
  decisions <- decisions[me_name=="ratio_pcv3_dpt3", me_name :="vacc_pcv3_dpt3_ratio"]
  decisions <- decisions[me_name=="ratio_rotac_dpt3", me_name :="vacc_rotac_dpt3_ratio"]
  decisions <- decisions[me_name=="ratio_rcv1_mcv1", me_name :="vacc_rcv1_mcv1_ratio"]
  decisions <- decisions[me_name=="ratio_hepb3_dpt3", me_name :="vacc_hepb3_dpt3_ratio"]
  decisions <- decisions[me_name=="ratio_mcv2_mcv1", me_name :="vacc_mcv2_mcv1_ratio"]
  if (dpt_timeliness) {
    dpt3_decisions <- decisions[me_name=="vacc_dpt3"]
    dpt3_timeliness_decisions <- dpt3_decisions[, me_name := "vacc_dpt3_timeliness_ratio"]
    decisions <- rbind(decisions, dpt3_timeliness_decisions)
  }
  fwrite(decisions, file = paste0(to_model_dir, "/mics_decider_decisions.csv"), row.names = F)  
  need_decision <- decisions[abs(difference) > 0.10 & is.na(decision)]
  if (nrow(need_decision) > 1) {
    fwrite(need_decision, file = paste0(to_model_dir, "/mics_decider_need_decisions.csv")) 
  }
  

  to_swap <- function(each_nid, calculating_ratios=FALSE, top=NULL, bottom=NULL, ant=NULL) {
    checking <- decisions[nid==each_nid]  
    checking <- checking[decision != "use_microdata"]
    checking <- checking[decision != "drop"]  
    if (calculating_ratios) {  
      checking <- checking[me_name==ant]  
      checking <- checking[!duplicated(checking)]  
    }
    mini.df     <- data.table()
    if (calculating_ratios) {   
      for (vax in unique(checking$me_name)) {
        rm.df     <- df[nid==each_nid & (me_name==top | me_name==bottom) & cv_lit != 1]  #
        mini.df   <- rbind(mini.df, rm.df)
      }
    } else {
      for (me in unique(checking$me_name)) {
        rm.df     <- df[nid==each_nid & me_name==me]  
        mini.df   <- rbind(mini.df, rm.df)
      }
    }
    return(mini.df)
  }
  take_out <- lapply(unique(decisions$nid), to_swap) %>% rbindlist()
  
  make_swap <- function(calculating_ratios=FALSE, num.sub=NULL, denom.sub=NULL, top=NULL, bottom=NULL) {  
    if (!calculating_ratios) {  
      
      take_out[, match := paste(nid, me_name, age_year, sep="_")]
      df <- df[, match := paste(nid, me_name, age_year, sep="_")]
      df <- df[match %!in% unique(take_out$match)]
      tabs <- tabs[, match := paste(nid, me_name, sep="_")]
      tabs <- tabs[, cv_lit := 1]
      take_out[, match := paste(nid, me_name, sep="_")]
      df <- rbind(df, tabs[match %in% unique(take_out$match)], fill=TRUE)  
      df <- df[, match := NULL]   
      add.df <- decisions[decision=="use_report_extractions" & !is.na(report) & is.na(tabulation)]
      add.df <- add.df[, match := paste(nid, me_name, report, sep="_")]
      df <- df[survey_name=="UNICEF_MICS", match := paste(nid, me_name, data, sep="_")]  
      add.df <- add.df[!match %in% unique(df$match)]
      df <- df[, match := NULL] 
      tabs   <- tabs[, match := paste(nid, me_name, data, sep="_")]  
      df     <- rbind(df, tabs[match %in% unique(add.df$match)], fill=TRUE)
      df     <- df[, match := NULL]
    } else {  
      take_out[, match := paste(nid, me_name, age_year, sep="_")]
      df.num   <- num.sub[, match := paste(nid, top, age_year, sep="_")]
      df.denom <- denom.sub[, match := paste(nid, bottom, age_year, sep="_")]
      df.num   <- df.num[match %!in% unique(take_out$match)]
      df.denom <- df.denom[match %!in% unique(take_out$match)]
      tabs <- tabs[, match := paste(nid, me_name, sep="_")]
      tabs.r <- tabs[match %like% num | match %like% denom]  
      tabs.r <- tabs.r[, cv_lit := 1]
      take_out[, match := paste(nid, me_name, sep="_")]
      df.num   <- rbind(df.num, tabs.r[match %in% unique(take_out$match) & me_name==num], fill=TRUE)
      df.denom <- rbind(df.denom, tabs.r[match %in% unique(take_out$match) & me_name==denom], fill=TRUE)
      df.num   <- df.num[, c("match", "me_name") := NULL]
      df.denom <- df.denom[, c("match", "me_name") := NULL]
      df.num$x   <- "num"
      df.denom$x <- "denom"
      df.return <- rbind(df.num, df.denom)
    }
    if (!calculating_ratios) return(df) else return(df.return)
  }
  df <- make_swap()
}

saveRDS(df, file = file.path(to_model_dir, "02_after_decider_swaps.rds"))

df.whosurvey <- readRDS(vacc.whosurvey) %>%                     
  .[, nid := nid %>% as.numeric] %>%
  .[!nid %in% unique(df$nid), ]                                 
df  <- rbind(df, df.whosurvey, fill=TRUE)


df <- split.loc.data(all_data=df)


## 2.) Apply outliers
# cleanup for full coverage indicators
# using database
df <- outlier.data(df)
# batch exclude other outlier series
df <- temp.outlier(df)
df <- dpt.outlier(df)
print("Done with all outliering")


df <- clean.data(df)
df <- df[-which(sample_size < 20)]

## 4.) Clean and remove duplicates
df <- clean.data(df)

saveRDS(df, file = file.path(to_model_dir, "02.5_pre_dpt1_imputation.rds"))


if (launch_dpt_dropout) {
  source(file.path(code_root, "FILEPATH/dpt1_dpt3_dropout_calc.R"))
  dpt_dropout <- prep.dpt.dropout(df)
  to_model_dropout_dir <- file.path(to_model_dir, "dpt_dropout")
  ifelse(!dir.exists(to_model_dropout_dir), dir.create(to_model_dropout_dir), FALSE)
  fwrite(dpt_dropout, file.path(to_model_dropout_dir, "vacc_dpt1_dpt3_admin_dropout.csv"))
  fwrite(dpt_dropout, file.path(share_model_dir, "dpt_dropout", "vacc_dpt1_dpt3_admin_dropout.csv"))
  
  source(file.path(code_root, "FILEPATH/dpt1_dpt3_dropout_launch.R"))
  # job hold
  file_names <- c(file.path("FILEPATH", RUNS[1], "model_complete.csv"))
  sum_files <- 0
  while (sum_files < 1) {
    sum_files <- sum(sapply(file_names, file.exists))
    print('still waiting on these dropout files')
    Sys.sleep(60)
  }
  print('All done with ST-GPR dropout model -- moving along')
  
} else {
  print(paste0("Did NOT re-launch dpt dropout model and adjusting current data based on run_id ", alt_dropout_run_id, " estimates."))
  RUNS <- alt_dropout_run_id 
  
}

# 3. Use the ST-GPR results to impute DTP1 coverage in early years while propagating uncertainty
source(file.path(code_root, "FILEPATH/dpt1_dpt3_dropout_integrate.R"))
df <- integrate.dpt.dropout(df)

## 4 Clean and remove duplicates
df <- clean.data(df)
df <- create.id(df)  


if (dpt_conditional) {
  source(file.path(code_root, "FILEPATH/dpt_math.R")) 
}



## 5.) Set variance where missing
# Set admin variance based on survey data availability by country-year-vaccine
df <- set.admin_variance(df)
# Set lit variance where we have sample_size
df[!is.na(sample_size) & is.na(variance), variance := data * (1 - data) / sample_size]
# Set lit variance where we dont have sample_size
df[(cv_lit==1 | cv_whosurvey==1) & is.na(variance), variance := data * (1 - data) / 100]
# Set sample_size based off of variance
df[!is.na(data) & is.na(sample_size), sample_size := data * (1 - data) / variance]
# offset the variance when dpt1-dpt3=0 in conditional calcuation
if (dpt_conditional){
  df[me_name=="vacc_dpt12_cond" & data==0 | variance==0, variance := 0.001]  
}

saveRDS(df, file = file.path(to_model_dir, "03_pre_bias_adj.rds"))


## 6.) Estimate and apply administrative bias adjustment
df <- clean.data(df)
df <- est.time_varying_bias(df, launch=launch_bias_correction, straight_models=straight_models, NOTES=NOTES, 
                            alt_bias_cycle=alt_bias_cycle, alt_bias_date=alt_bias_date,
                            on_time_cohort_only=on_time_cohort_only, locations=locs, rn=date)
df <- clean.data(df)


saveRDS(df, file = file.path(to_model_dir, "04_post_bias_adj.rds"))

if (run.stockout){
  print("Starting stockout detection on bias-adjusted JRF data")
  source("FILEPATH/gam_stockout_detection.R")
  who.admin <- readRDS(vacc.admin)  
  do_table <- data.table(expand.grid(vaccine = admin.vacc,
                                     iso_code = unique(who.admin$ihme_loc_id),
                                     stringsAsFactors = F))
  test_data <- parallel::mclapply(1:nrow(do_table),
                                  function(i) {
                                    v <- do_table[i, vaccine]
                                    is <- do_table[i, iso_code]
                                    return(stockout_eval(v, is, methods = stkout_method, logit_transform = TRUE, adjusted_admin = TRUE))
                                  },
                                  mc.cores = 25)
  predicted <- rbindlist(test_data, fill = T, use.names = T)
  fwrite(predicted, paste0(to_model_dir, "/", stkout_method, "_full_detected_stockouts_", date, ".csv"))
  
  old <- c(paste0("pred_fitted_", stkout_method), paste0("se_", stkout_method), paste0("diff_", stkout_method), paste0("diff_", stkout_method, "_normal"), paste0("rmse_", stkout_method))
  new <- c("pred_fitted", "pred_se", "diff", "diff_normal", "rmse")
  setnames(predicted, old, new)
  predicted <- predicted[, c("count", "rmse") := NULL]
  predicted <- predicted[diff_normal < 0]  
  
  # Save detected admin stockouts
  saveRDS(predicted, paste0(data_root, "FILEPATH/detected_stockouts.rds")) 
  saveRDS(predicted, paste0(to_model_dir, "/", stkout_method, "_detected_stockouts_", date, ".rds")) 
  
  ## Compare detected and reported to make 'exact_matches' file
  source("FILEPATH/compare_detected_reported.R")  
  compare_detected_reported(ratios=FALSE)
} else {
  # If 'else', then NOT re-running the stockout detection and will use the last saved detected stockouts .rds to produce 'exact_matches'
  source("FILEPATH/compare_detected_reported.R")
  compare_detected_reported(ratios=FALSE)
}



## 7.) Make ratios to model out
df <- df[me_name=="vacc_full_sub", me_name := "vacc_fullsub"]
df <- df[me_name=="vacc_dpt3_on_time", me_name := "vacc_dpt3time"]
if (hybrid_mics) {
  tabs <- a[,.(nid, ihme_loc_id, me_name, tabulation)]
  
  df.ratios <- lapply(ratios, function(x) make.ratios(df, x)) %>% rbindlist
} else {
  df.ratios <- lapply(ratios, function(x) make.ratios(df, x)) %>% rbindlist
}
df <- rbind(df, df.ratios, fill=TRUE, use.names=TRUE)

df <- clean.data(df)


if (ratio_stockouts) {
if (run.stockout) {
  print("Starting stockout detection on bias-adjusted JRF ratios")
  source("FILEPATH/gam_stockout_detection.R")
  who.admin <- readRDS(vacc.admin)  
  do_table <- data.table(expand.grid(vaccine = c(ratios),  
                                     iso_code = unique(who.admin$ihme_loc_id),
                                     stringsAsFactors = F))
  tictoc::tic()
  test_data <- parallel::mclapply(1:nrow(do_table),
                                  function(i) {
                                    v <- do_table[i, vaccine]
                                    is <- do_table[i, iso_code]
                                    return(stockout_eval(v, is, methods = stkout_method, logit_transform = TRUE, adjusted_admin = TRUE))
                                  },
                                  mc.cores = 25)
  tictoc::toc()
  predicted <- rbindlist(test_data, fill = T, use.names = T)
  fwrite(predicted, paste0(to_model_dir, "/", stkout_method, "_ratio_detected_stockouts_", date, ".csv"))

  old <- c(paste0("pred_fitted_", stkout_method), paste0("se_", stkout_method), paste0("diff_", stkout_method), paste0("diff_", stkout_method, "_normal"), paste0("rmse_", stkout_method))
  new <- c("pred_fitted", "pred_se", "diff", "diff_normal", "rmse")
  setnames(predicted, old, new)
  predicted <- predicted[, c("count", "rmse") := NULL]
  
  predicted_pos <- predicted[diff_normal > 0]
  predicted <- predicted[diff_normal < 0] 
  
  # Save detected admin stockouts
  saveRDS(predicted, paste0(data_root, "FILEPATH/detected_ratio_stockouts.rds")) 
  saveRDS(predicted_pos, paste0(data_root, "FILEPATH/detected_ratio_pos_stockouts.rds")) 
  saveRDS(predicted, paste0(to_model_dir, "/", stkout_method, "_detected_ratio_stockouts_", date, ".rds")) 
  saveRDS(predicted_pos, paste0(to_model_dir, "/", stkout_method, "_detected_ratio_pos_stockouts_", date, ".rds")) 
  
  source("FILEPATH/compare_detected_reported.R")  
  compare_detected_reported(ratios=TRUE)
} else {
  source("FILEPATH/compare_detected_reported.R")
  compare_detected_reported(ratios=TRUE)
}
}


## 8.) Set introduction frame
df.intro <- readRDS(vacc.intro)
delayed.epi <- readRDS(vacc.epi)[, c("age_group_id", "sex_id", "source", "notes", "notes2", "ignore") := NULL]
delayed.epi <- merge(delayed.epi, locs[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
df.intro <- df.intro[!me_name %in% unique(delayed.epi$me_name)]  
df.intro <- rbind(df.intro, delayed.epi, fill=T) 

df <- merge(df, df.intro[ihme_loc_id %in% locs$ihme_loc_id], by=c("ihme_loc_id", "year_id", "me_name"), all=TRUE)  
df <- adjust.intro_frame(df)  
df <- clean.data(df)


## Outlier surveys/cohorts and lit data pre-intro year
df <- df[year_id < cv_intro & !is.na(data) & !is.na(cv_intro) & (cv_survey==1 | cv_lit==1), `:=` (cv_outlier=data, data=NA)]  

saveRDS(df, file = file.path(to_model_dir, "05_ratios_and_intros_applied.rds"))

df <- pre.intro.outliers(df)  


# prep files
exact_match <- fread(vacc.stockouts) %>% unique
# make id for merging and stockout indicator
exact_match[, id := paste0(ihme_loc_id, year_id, me_name)]
df[, id := paste0(ihme_loc_id, year_id, me_name)]
exact_match[, stockout := 1]

stockout_outliers <- fread(stockout.cov.out)  
stockout_outliers[outlier==1, id := paste0(ihme_loc_id, year_id, me_name)]  
exact_match <- exact_match[id %!in% stockout_outliers$id]

exact_match_cond <- exact_match[me_name=="vacc_dpt1"][, me_name := "vacc_dpt12_cond"]
exact_match <- rbind(exact_match, exact_match_cond)

# subset exact_match to just straight and conditional
exact_match_straight <- exact_match[me_name %in% c(straight_models, "vacc_dpt12_cond")]

# merge files to add stockout identifying column for straight vaccines (i.e. those with exact match me_names)
df <- merge(df, exact_match_straight, by = c("ihme_loc_id", "nid", "year_id", "me_name", "id"), all.x = T)  

if (!ratio_stockouts) {
  ### Apply stockout deflection to ratios based on numerator-only detected deflections
  exact_match_ratios <- copy(exact_match) 
  exact_match_ratios[me_name=="vacc_hepb3", me_name := "vacc_hepb3_dpt3_ratio"]
  exact_match_ratios[me_name=="vacc_mcv2", me_name := "vacc_mcv2_mcv1_ratio"]
  exact_match_ratios[me_name=="vacc_pcv3", me_name := "vacc_pcv3_dpt3_ratio"]
  exact_match_ratios[me_name=="vacc_dpt1", me_name := "vacc_dpt12_cond"]
  exact_match_ratios[me_name=="vacc_rcv1", me_name := "vacc_rcv1_mcv1_ratio"]
  exact_match_ratios[me_name=="vacc_hib3", me_name := "vacc_hib3_dpt3_ratio"]
  exact_match_ratios[me_name=="vacc_rotac", me_name := "vacc_rotac_dpt3_ratio"]
  
  exact_match_ratios <- exact_match_ratios[me_name %in% ratios | me_name=="vacc_dpt12_cond"]
  
  # make merge id
  exact_match_ratios[, id := paste0(ihme_loc_id, year_id, me_name)][, nid := NULL]
  
  df <- merge(df, exact_match_ratios, by = c("ihme_loc_id", "year_id", "me_name", "id"), all.x = T)[, id := NULL]
  df[is.na(stockout.x) & !is.na(stockout.y), stockout.x := stockout.y]
  setnames(df, "stockout.x", "stockout")
  df[, stockout.y := NULL]
  df[is.na(cv_stockout_ratio.x) & !is.na(cv_stockout_ratio.y), cv_stockout_ratio.x := cv_stockout_ratio.y]
  setnames(df, "cv_stockout_ratio.x", "cv_stockout_ratio")
  df[, cv_stockout_ratio.y := NULL]
  
} else {
  ### Apply stockout deflection to ratios based on ratio-detected deflections
  exact_match_ratios <- fread(vacc.ratio.stockouts) %>% unique
  # make id for merging and stockout indicator
  exact_match_ratios[, id := paste0(ihme_loc_id, year_id, me_name)]
  df[, id := paste0(ihme_loc_id, year_id, me_name)]
  exact_match_ratios[, stockout := 1]
  
  stockout_outliers <- fread(stockout.cov.out)   
  stockout_outliers[outlier==1, id := paste0(ihme_loc_id, year_id, me_name)]
  exact_match_ratios <- exact_match_ratios[id %!in% stockout_outliers$id]  
  
  exact_match_ratios <- exact_match_ratios[me_name %in% ratios]
  
  df <- merge(df, exact_match_ratios, by = c("ihme_loc_id", "nid", "year_id", "me_name", "id"), all.x = T)  
  
  df[me_name %in% ratios & is.na(cv_stockout_ratio.x) & !is.na(cv_stockout_ratio.y), cv_stockout_ratio.x := cv_stockout_ratio.y]
  df[me_name %in% ratios & is.na(stockout.x) & !is.na(stockout.y), stockout.x := stockout.y]
  df[, c("cv_stockout_ratio.y", "stockout.y") := NULL] %>% setnames(., c("cv_stockout_ratio.x", "stockout.x"), c("cv_stockout_ratio", "stockout"))
  
}


df[is.na(stockout), stockout := 0]
# add stockout covariate column
df[stockout==1, cv_stockout := cv_stockout_ratio]  
df[stockout==0, cv_stockout := 0.5]  

df[!is.na(fake_data), `:=` (nid=451398, underlying_survey_name=survey_name, survey_name="IHME Imputed DPT1 Administrative Coverage Data")]  


## 9.) Split metadata out for documentation of model runs
split <- split.meta(df)
df <- split$df.mod   
df.full <- split$df  
meta <- split$meta   
#***********************************************************************************************************************


#----SAVE---------------------------------------------------------------------------------------------------------------
### save full dataset
saveRDS(df.full, file.path(to_model_dir, "vaccination.rds"))

### save metadata
saveRDS(meta, file.path(to_model_dir, "vaccination_meta.rds"))

### keep model mes
df <- df[me_name %in% mes_to_launch]

### map location_id
df <- merge(df, locs[, .(ihme_loc_id, location_id)], by=c('ihme_loc_id'), all.x=TRUE)  
if (nrow(df[is.na(location_id)]) > 0) stop("STOP | Unmapped locations \n Maybe there are rows of mis-tagged or non-GBD subnationals accidentally in the data df?")
df <- df[!is.na(location_id)]

### add new columns for new ST-GPR
df <- df[, measure_id := 18]  
df[, is_outlier := ifelse(is.na(data), 1, 0)]

df <- df[!is.na(data)]

### make and save square dataset of custom_covs to specify in path_to_custom_inputs in config
cv_covs <- CJ(location_id = unique(locs$location_id),
              year_id = year_start:year.est.end,
              age_group_id = 22,
              sex_id = 3)


# Grab cv_customs, location_id, year_id columns from df to add to cv_covs 
custom_covs <- df.full[nid==203321]
custom_covs <- custom_covs[, c("ihme_loc_id", "year_id", "me_name", "cv_intro", "cv_intro_years", "stockout", "cv_stockout")]
custom_covs <- merge(custom_covs, locs[, c("ihme_loc_id", "location_id")], by = "ihme_loc_id", all.x = TRUE)[, ihme_loc_id := NULL]
custom_covs <- custom_covs[me_name %in% ratios & is.na(cv_intro), cv_intro := 9999]
custom_covs <- custom_covs[me_name %in% ratios & is.na(cv_intro_years), cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]  

for (me in straight_models) {
  me_covs <- custom_covs[me_name==me]
  # Merge on cv_intro_years and cv_stockout, with associated builder columns (cv_intro and stockout)
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  me_full <- me_full[is.na(cv_stockout) & (is.na(stockout) | stockout==0), cv_stockout := 0.5]
  # drop unnecessary cols
  cv_me <- me_full[, `:=` (me_name=NULL, cv_intro=NULL, cv_intro_years=NULL, stockout=NULL)]  
  cv_me <- cv_me[!duplicated(cv_me), ]
  # save each cv_cov file
  fwrite(cv_me, file = file.path(share_model_dir, "FILEPATH/covariates", paste0(me, "_cv_covariates.csv")))
  if (!dir.exists(file.path(to_model_dir, "covariates"))) dir.create(file.path(to_model_dir, "covariates")); fwrite(cv_me, file = file.path(to_model_dir, "covariates", paste0(me, "_cv_covariates.csv")))
}

for (me in "vacc_dpt12_cond") {
  me_covs <- custom_covs[me_name==me]
  # Merge on cv_intro_years and cv_stockout, with associated builder columns (cv_intro and stockout)
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  me_full <- me_full[is.na(cv_stockout) & (is.na(stockout) | stockout==0), cv_stockout := 0.5]
  # drop unnecessary cols
  cv_me <- me_full[, `:=` (me_name=NULL, cv_intro=NULL, cv_intro_years=NULL, stockout=NULL)]  
  cv_me <- cv_me[!duplicated(cv_me), ]
  # save each cv_cov file
  fwrite(cv_me, file = file.path(share_model_dir, "FILEPATH/covariates", paste0(me, "_cv_covariates.csv")))
  if (!dir.exists(file.path(to_model_dir, "covariates"))) dir.create(file.path(to_model_dir, "covariates")); fwrite(cv_me, file = file.path(to_model_dir, "covariates", paste0(me, "_cv_covariates.csv")))
}

for (me in "vacc_dpt3_timeliness_ratio") {
  me_covs <- custom_covs[me_name=="vacc_dpt3"]  
  me_covs <- me_covs[, `:=` (me_name=me, cv_stockout=0.5)]  
  # Merge on cv_intro_years and cv_stockout, with associated builder columns (cv_intro and stockout)
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  me_full <- me_full[is.na(cv_stockout) & (is.na(stockout) | stockout==0), cv_stockout := 0.5]
  # drop unnecessary cols
  cv_me <- me_full[, `:=` (me_name=NULL, cv_intro=NULL, cv_intro_years=NULL, stockout=NULL)]  
  cv_me <- cv_me[!duplicated(cv_me), ]
  # save each cv_cov file
  fwrite(cv_me, file = file.path(share_model_dir, "FILEPATH/covariates", paste0(me, "_cv_covariates.csv")))
  if (!dir.exists(file.path(to_model_dir, "covariates"))) dir.create(file.path(to_model_dir, "covariates")); fwrite(cv_me, file = file.path(to_model_dir, "covariates", paste0(me, "_cv_covariates.csv")))
}


intros <- readRDS(vacc.intro)[, c("cv_outro", "ihme_loc_id") := NULL][me_name %in% ratios]
custom_covs <- df.full[me_name %in% ratios, c("ihme_loc_id", "year_id", "me_name", "stockout", "cv_stockout", "nid")]
custom_covs <- merge(custom_covs, locs[, c("ihme_loc_id", "location_id")], by = "ihme_loc_id", all.x = TRUE)[, ihme_loc_id := NULL]
custom_covs <- unique(custom_covs)
multiple_data <- custom_covs[me_name %in% ratios, .N, by=c("location_id", "year_id", "me_name")][N>=2][, mult_id := paste0(location_id, year_id, me_name)]
custom_covs[, mult_id := paste0(location_id, year_id, me_name)]
data_1 <- custom_covs[!mult_id %in% unique(multiple_data$mult_id)]
data_2 <- custom_covs[mult_id %in% unique(multiple_data$mult_id) & nid==203321]
custom_covs <- rbind(data_1, data_2) 
custom_covs <- custom_covs[, c("nid", "mult_id") := NULL] %>% unique
# merge available stockouts to intro square
custom_covs <- merge(custom_covs, intros, by=c("year_id", "location_id", "me_name"), all.x=T, all.y=T)[year_id >= year.est.start]

for (me in ratios) {
  me_covs <- custom_covs[me_name==me]
  me_full <- copy(me_covs)
  me_full <- me_full[is.na(cv_intro_years), cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]
  me_full <- me_full[is.na(cv_stockout) & (is.na(stockout) | stockout==0), cv_stockout := 0.5]
  cv_me <- me_full[, `:=` (me_name=NULL, cv_intro=NULL, stockout=NULL)]
  # get rid of duplicates rows!
  cv_me <- cv_me[!duplicated(cv_me), ]
  # add st-gpr rows
  cv_me[, `:=` (age_group_id=22, sex_id=3)]
  # save each cv_cov file
  fwrite(cv_me, file = file.path(share_model_dir, "FILEPATH/covariates", paste0(me, "_cv_covariates.csv")))
  if (!dir.exists(file.path(to_model_dir, "covariates"))) dir.create(file.path(to_model_dir, "covariates")); fwrite(cv_me, file = file.path(to_model_dir, "covariates", paste0(me, "_cv_covariates.csv")))
}


### Drop covariate columns on df since have saved as separate covariates file
df <- df[, `:=` (cv_intro = NULL, cv_intro_years = NULL, stockout = NULL, cv_stockout = NULL)]

### save for upload
for (i in mes_to_launch) {
  df.sub <- df[me_name==i, ] %>% copy
  setnames(df.sub, "data", "val")  
  df.sub[!is.na(variance), sample_size := 0]  
  fwrite(df.sub, file.path(to_model_dir, paste0(i, ".csv")), row.names=FALSE, na="")
  fwrite(df.sub, file.path(share_model_dir, "to_model", paste0(i, ".csv")), row.names=FALSE, na="")
}

if (just_dpt_cond) mes_to_launch <- "vacc_dpt12_cond"
if (straight_only) mes_to_launch <- c("vacc_dpt3", "vacc_mcv1", "vacc_bcg", "vacc_polio3")
if (just_dpt3) mes_to_launch <- "vacc_dpt3"
if (dpt_timeliness_only) mes_to_launch <- "vacc_dpt3_timeliness_ratio"
if (polio3_only) mes_to_launch <- "vacc_polio3"
if (ratios_only) mes_to_launch <- ratios
if (just_mcv1) mes_to_launch <- "vacc_mcv1"
if (just_bcg) mes_to_launch <- "vacc_bcg"
if (just_rotac_ratio) mes_to_launch <- "vacc_rotac_dpt3_ratio"
if (just_hib3_ratio) mes_to_launch <- "vacc_hib3_dpt3_ratio"
if (just_hepb3_ratio) mes_to_launch <- "vacc_hepb3_dpt3_ratio"
if (bias_only) mes_to_launch <- NULL
if (mcv_rcv_ratios_only) mes_to_launch <- c("vacc_mcv1", "vacc_mcv2_mcv1_ratio", "vacc_rcv1_mcv1_ratio")

if (is.null(mes_to_launch)) stop("Specified to only run through prep code to produce new bias results - no ST-GPR models launched.")

if (run_via_bundle) {
  df[, c("cv_id", "cv_stockout_ratio") := NULL]
  df[!is.na(variance), sample_size := 0]  
  for (i in mes_to_launch) {
    print(paste0("Preparing to upload ", i, " bundle"))
    upload_model_xw(data=df, me=i, data_date=date, bundle_map=me.db, path_to_version_dir=bundle.version.dir, 
                    decomp="iterative", round=gbd_round, note=NOTES, bias=FALSE)
  }
}  
#***********************************************************************************************************************


#----LAUNCH-------------------------------------------------------------------------------------------------------------
### launch ST-GPR models
code_root <- file.path("FILEPATH/vaccines", username, "vaccines")
source(file.path(code_root, "FILEPATH/launch_gpr.r"))
file.exists(file.path("FILEPATH/output", RUNS, "model_complete.csv"))
#***********************************************************************************************************************
