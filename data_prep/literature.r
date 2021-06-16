#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    Modified for GBDs 2016 - 2020
# Purpose: Prep literature data on vaccine coverage
# Run:     source("FILEPATH/literature.r")
#***********************************************************************************************************************


#----SETUP--------------------------------------------------------------------------------------------------------------
### clear workspace
rm(list=ls())

### load packages
library(data.table)
library(dplyr)
library(parallel)
library(readxl)

### set os flexibility
username <- Sys.info()[["user"]]
os <- .Platform$OS.type
if (os=="windows") {
  j <- "J:/"
} else {
  j <- "FILEPATH/"
}

### settings
dpt_timeliness <- TRUE  


### source functions
decomp_step = "iterative"
source(paste0("FILEPATH/init.r"))
source(db_tools)
source("FILEPATH/get_location_metadata.R")
locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step=decomp_step)[level >= 3]
'%!in%'  <- function(x,y)!('%in%'(x,y))
cohorts <- readRDS(file.path(ref_data_repo, "vaccine_target.rds"))

### get literature data
key         <- "ADDRESS" 
sheet_name  <- "ADDRESS"                                    
gs_download <- function(key, sheet_name=NULL) {
  if (!is.null(sheet_name)) sheet <- paste0("&gid=", sheet_name) else sheet <- ""
  return(fread(paste0("ADDRESS", sheet)))
}
df <- gs_download(key, sheet_name=sheet_name)

### country-specific RotaC doses
rotac_schedule <- readRDS(file.path(ref_data_repo, "vaccine_schedule.rds"))
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### if have higher combination vaccine coverage, swap out component vaccines
duples <- list(c("vacc_dpt1",  "vacc_tetra1"),
               c("vacc_dpt1",  "vacc_pent1"),
               c("vacc_dpt2",  "vacc_pent2"),
               c("vacc_dpt2",  "vacc_tetra2"),
               c("vacc_dpt3",  "vacc_tetra3"),
               c("vacc_dpt3",  "vacc_pent3"),
               c("dpt3_timeliness",  "pent3_timeliness"),  
               c("vacc_hib3",  "vacc_pent3"),
               c("vacc_hib3",  "vacc_tetra3"),
               c("vacc_hepb3", "vacc_pent3"),
               c("vacc_mcv1",  "vacc_mmr1"),
               c("vacc_mcv2",  "vacc_mmr2"),
               c("vacc_rcv1",  "vacc_mmr1"),
               c("vacc_rcv2",  "vacc_mmr2"))
duples_alt_1 <- list(c("vacc_dpt1",  "vacc_tetra1"),
                     c("vacc_dpt1",  "vacc_pent1"),
                     c("vacc_dpt2",  "vacc_pent2"),
                     c("vacc_dpt2",  "vacc_tetra2"),
                     c("vacc_dpt3",  "vacc_tetra3"),
                     c("vacc_dpt3",  "vacc_pent3"),
                     c("dpt3_timeliness",  "pent3_timeliness"),  
                     c("vacc_hib3",  "vacc_pent3"),
                     c("vacc_hib3",  "vacc_tetra3"),
                     c("vacc_polio3", "vacc_pent3"),  
                     c("vacc_mcv1",  "vacc_mmr1"),
                     c("vacc_mcv2",  "vacc_mmr2"),
                     c("vacc_rcv1",  "vacc_mmr1"),
                     c("vacc_rcv2",  "vacc_mmr2"))
duples_alt_2 <- list(c("vacc_dpt1",  "vacc_tetra1"),
                     c("vacc_dpt1",  "vacc_pent1"),
                     c("vacc_dpt2",  "vacc_pent2"),
                     c("vacc_dpt2",  "vacc_tetra2"),
                     c("vacc_dpt3",  "vacc_tetra3"),
                     c("vacc_dpt3",  "vacc_pent3"),
                     c("dpt3_timeliness",  "pent3_timeliness"),  
                     c("vacc_polio3",  "vacc_pent3"),  
                     c("vacc_hib3",  "vacc_tetra3"),
                     c("vacc_hepb3", "vacc_pent3"),
                     c("vacc_mcv1",  "vacc_mmr1"),
                     c("vacc_mcv2",  "vacc_mmr2"),
                     c("vacc_rcv1",  "vacc_mmr1"),
                     c("vacc_rcv2",  "vacc_mmr2"))

### alternative pentavalent in select countries/nids
unconventional_penta_filepath <- "FILEPATH/penta_unconventional_component.csv"
alt_penta <- fread(unconventional_penta_filepath)
## what countries/nids should be processed with duples_alt_1?
alt_penta_1 <- alt_penta[DTAP==1 & HepB==0 & HiB==1 & IPV==1]
## what countries/nids should be processed with duples_alt_2?
alt_penta_2 <- alt_penta[DTAP==1 & HepB==1 & HiB==0 & IPV==1]

for (i in duples_alt_1) {
  ## e.g. if !is.na(penta) & penta > dpt | !is.na(penta) & is.na(dpt), replace dpt with penta
  df <- df[nid %in% unique(alt_penta_1$survey_id), (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
}

for (i in duples_alt_2) {
  ## e.g. if !is.na(penta) & penta > dpt | !is.na(penta) & is.na(dpt), replace dpt with penta
  df <- df[nid %in% unique(alt_penta_2$survey_id), (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
}

for (i in duples) {
  ## e.g. if !is.na(penta) & penta > dpt | !is.na(penta) & is.na(dpt), replace dpt with penta
  df <- df[!nid %in% unique(alt_penta$survey_id), (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
}


### location
df[, ihme_loc_id := tstrsplit(location_name_short_ihme_loc_id, "[|]")[[2]] ]

### save wide df for dtp3_timeliness prep 
if (dpt_timeliness) {
  setnames(df, "dpt3_timeliness", "vacc_dpt3_timeliness_ratio")  
  dpt_df <- copy(df)
  
}


### reshape
id <- c("nid", "file_path", "ihme_loc_id", "year_start", "year_end", "age_start", "age_end", "sample_size", "cv_admin")  
vacc <-  grep("vacc", names(df), value=TRUE)
df <- melt(df, id=id, measure=patterns("^vacc|^maternal"), value.name="data", variable.name="me_name")  
#***********************************************************************************************************************


#----CLEANUP------------------------------------------------------------------------------------------------------------
### drop rows without antigen-specific data
df <- df[!is.na(data)]

### divide coverage by 100
df[me_name != "vacc_dpt3_timeliness_ratio", data := as.numeric(data) / 100]  
# cap coverage at 1
df[data > 1, data := 1]

### again, drop rows without antigen-specific data that were blank and are now NA
df <- df[!is.na(data)]

### drop anything without mapped ihme_loc_id
df <- df[!is.na(ihme_loc_id)]

### convert 'data' col class to numeric 
df$data <- as.numeric(df$data)

### make new country schedule-specific vacc_rotac rows
setnames(rotac_schedule, "me_name", "rota_complete")
rotac_schedule[, me_name := paste0("vacc_rota", doses)]
rotac <- merge(df, rotac_schedule, by=c("ihme_loc_id", "me_name"), all.x=T, all.y=T)[!is.na(rota_complete) & !is.na(data)]
rotac[, `:=` (me_name=rota_complete, location_id=NULL, doses=NULL, rota_complete=NULL)]
df <- rbind(df, rotac)

### remove sample size if > 5000, usually refers to target population 
df[, sample_size := gsub(",", "", sample_size) %>% as.numeric]
df[sample_size > 5000, sample_size := NA]

### SAVE the df at this point
df_months <- copy(df)
df_months <- df_months[is.na(age_start) & is.na(age_end) & is.na(cv_admin), `:=` (age_start=12, age_end=23)]  
saveRDS(df_months, file.path(ref_data_repo, "vaccine_lit_months.rds"))

### carry on as before
df[, `:=` (age_start = round(age_start / 12), 
           age_end   = round(age_end / 12))]
df[, age_length := age_end - age_start]
df[age_length %in% c(0, 1), age_year := age_start]


df[is.na(age_length), age_year := 1]

    df <- df[age_length > 1, age_year := round((age_start + age_end)/2)]

    df[, year_id := ifelse(nid %in% c(249054, 208022, 208019) | cv_admin==1, year_start, year_start-age_year)]  
    df[!nid %in% c(249054, 208022, 208019) & is.na(cv_admin), year_id := year_start-age_year]
    df[cv_admin==1, year_id := year_start] 
    df <- df[(age_year > 0 & age_year < 5) | 
               nid %in% c(394214, 156685, 56428, 56430, 56432, 56528, 56534, 56540, 174458, 
                          174460, 264845, 285974, 209946, 400427, 400428, 400429, 400452, 
                          95514, 386777, 139023, 139024, 139027, 150886, 117508, 386779, 
                          386781, 249054) |
               cv_admin==1]   


    agg <- unique(df[age_length > 1]$nid)
    
    review <- df[nid %in% agg]
    review <- review[, count := .N, by=list(nid, me_name, ihme_loc_id, year_start, year_end)]
    take_out <- review[count != 1]
    take_out <- take_out[!nid %in% c(286662, 286738, 286753, 341003, 189463, 423252, 402691, 402689)]
    

    for (n in unique(take_out$nid)) {
      working <- take_out[nid==n]
      for (me in unique(working$me_name)) {
        for (loc in unique(working$ihme_loc_id)) {  
          work  <- working[me_name==me & ihme_loc_id==loc]
          if ((work[age_length > 1]$age_end %!in% unique(work[age_length==1]$age_end)) & (work[age_length > 1]$age_start %!in% unique(work[age_length==1]$age_start))) {
            take_out[nid==n & me_name==me & ihme_loc_id==loc & age_length > 1, actually_keep := 1]
            } else { print(n)
          }  
        }
      }
    }
    take_out <- take_out[is.na(actually_keep)]
    
    duplicative <- take_out[age_length > 1][, c("actually_keep", "count") := NULL]
    duplicative <- duplicative[, im_duplicative := 1]
    df <- merge(df, duplicative, by=names(df), all.x=T)
    df <- df[is.na(im_duplicative)][, im_duplicative := NULL]


df[, c("year_start", "year_end") := NULL]


### create survey name
parsed <- df[, tstrsplit(file_path, "/", fixed=TRUE)]
parsed <- parsed[, survey_name := ifelse(nchar(V3)==3, paste0(V3, "/", V4), V3)]
df <- cbind(df, parsed$survey_name)
setnames(df, "V2", "survey_name")
#***********************************************************************************************************************


### prepare USA NIS Kindergarten report 2009-10 through 2016-17
prep.nisreport <- function() {
  
  ### NIS coverage data
  length <- year.est.end - 2009
  path <- paste0(data_root, "FILEPATH/dataView2011_3_MMRonly.xls")  
  nis <- read_excel(path, sheet="SVV Coverage Trend 2016-17 Data", skip=2, 
                    col_types=c("text", rep(c("text", "skip", "text", "text", "text", "text"), length))) %>% data.table
  # remove unnecessary rows
  nis <- nis[!Names %in% c("HP 2020 Target", "Median")]
  # remove unnecessary columns
  nis <- nis[, colnames(nis)[grep("SURVEY TYPE|TARGET|TOTAL KINDERGARTEN POPULATION|PERCENT SURVEYED", colnames(nis))] := NULL]
  # add years to colnames
  yrs <- 2009:(year.est.end - 1)
  names <- c("name", paste0("x_", yrs))
  colnames(nis) <- names
  # reshape long
  nis[, (paste0("coverage_", yrs)) := lapply(paste0("x_", yrs), function(cols) as.numeric(get(cols)))]
  nis <- melt(nis[, c("name", paste0("coverage_", yrs)), with=FALSE], value.name="data")
  # add in year of fall of academic year
  nis[, year_start := NA_integer_]
  nis[, year_end := NA_integer_]
  nis[, year_start := substring(variable, 10, 13) %>% as.integer]
  nis[, year_end := year_start + 1]
  nis[, year_id := floor((year_start + year_end) / 2)]
  nis[, variable := NULL]
  setnames(nis, "name", "location_name")
  nis <- merge(nis[!grep("Median", location_name)], locations[parent_id==102, .(ihme_loc_id, location_name)], by="location_name", all.x=TRUE) %>%  
    .[, location_name := NULL]
  # calculate population included
  nis[, data := data / 100]
  # add vars
  nis[, nid := 334866]
  nis[, file_path := path]
  nis[, variance := data * (1 - data) / 50]
  nis[, cv_survey := 0]
  nis[, age_group_id := 22]
  nis[, sex_id := 3]
  nis[, survey_name := "School Vaccination Assessment Program"]
  nis <- nis[!is.na(data), ]
  nis_mmr <- copy(nis)[, me_name := "vacc_mmr2"]
  nis_mcv <- copy(nis)[, me_name := "vacc_mcv2"]
  nis <- rbind(nis_mmr, nis_mcv)
  
  return(nis)
  
}
year.est.end <- 2017  
nis <- prep.nisreport()
  
df  <- rbind(df, nis, fill=TRUE)

#***********************************************************************************************************************


#----SAVE---------------------------------------------------------------------------------------------------------------
### save
saveRDS(df, file.path(ref_data_repo, "vaccine_lit.rds"))  
#***********************************************************************************************************************
