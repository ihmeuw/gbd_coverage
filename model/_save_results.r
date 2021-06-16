#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME 
# Date:    May 2018
# Purpose: save results for all vaccination formulations
# Run:     source("FILEPATH/_save_results.r", echo=TRUE)
#***********************************************************************************************************************


#----SET UP-------------------------------------------------------------------------------------------------------------
### clean work space
rm(list=ls())
# set os flexibility
os <- .Platform$OS.type
if (os == "windows") {
  j_root <- "J:"
} else {
  j_root <- "/home/j"
  username <- Sys.info()[["user"]]
}

### load libraries
library(data.table)  
library(dplyr)
library(parallel)
library(readxl)
library(DBI)
library(magrittr)

# source paths and functions
source(paste0("FILEPATH/init.r"))  
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_age_metadata.R")
source("FILEPATH/utility.r")  
'%!in%'  <- function(x,y)!('%in%'(x,y))
#***********************************************************************************************************************


#----FUNCTIONS----------------------------------------------------------------------------------------------------------
### source save_results functions
source(file.path(code_root, "FILEPATH/prep_save_results.r"))

### get locations
locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step="iterative")

### set paths
# date         <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")
date         <- "2021-02-11"  
run_date     <- date  # set run_date as the date of modeled data to use for pcv3/rotac CHN adjustments
save.root    <- file.path(draws_root, "exp", gbd_cycle, date); ifelse(!dir.exists(save.root), dir.create(save.root, recursive=TRUE), FALSE)
results.root <- file.path(data_root, "exp/modeled", gbd_cycle, date); ifelse(!dir.exists(results.root), dir.create(results.root, recursive=TRUE), FALSE)
input.root   <- file.path(data_root, "exp/to_model", gbd_cycle, date)

### database
me_db <- fread(file.path(ref_data_repo, "me_db.csv"))  
year.est.start <- year_start %>% as.numeric
year.est.end   <- year_end %>% as.numeric
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### prep with draws?
save_draws <- TRUE
save_summaries  <- TRUE
manual_offset_remove <- FALSE 

### prep region, super region, and global aggregates?
save_regions <- FALSE  

### specify decomp_step
decomp_step <- "iterative"  # "step4"

### include COVID-19 disruption?
covid <- FALSE
covid_date <- "2021-04-04"  

stockout_ratios <- FALSE
#***********************************************************************************************************************


#----SAVE MEANS AND DRAWS-----------------------------------------------------------------------------------------------
### prep and save summary versions for vetting, draws for save_results
invisible(lapply(c("vacc_dpt3", "vacc_mcv1", "vacc_bcg", "vacc_polio3", "vacc_dpt12_cond"),   
                 function(x) prep.draws(x, draws=save_draws, quantiles=save_summaries, 
                                        agg_regions=save_regions, offset_remove=manual_offset_remove, 
                                        covid_disruption=covid, covid_version=covid_date)))

### compute dpt1_coverage from dpt12_conditional draws
conditional_id <- best.run_id("vacc_dpt12_cond")  
dpt3_id        <- best.run_id("vacc_dpt3")  
source(file.path(code_root, "FILEPATH/dpt1_draw_math.R"))
dpt1.draw.math(dpt_cond_id = conditional_id, dpt_3_id = dpt3_id, me="vacc_dpt1", 
               covid_disruption=covid, covid_version=covid_date)

### prep ratios
source(file.path(code_root, "FILEPATH/prep_save_results.r"))  
ratios <- c("vacc_mcv2_mcv1_ratio", 
            "vacc_hib3_dpt3_ratio", 
            "vacc_rotac_dpt3_ratio", 
            "vacc_hepb3_dpt3_ratio", 
            "vacc_pcv3_dpt3_ratio", 
            "vacc_rcv1_mcv1_ratio")
invisible(lapply(ratios, function(x) prep.ratio(x, head=("vacc_"), draws=save_draws, ratio_draws=save_draws, quantiles=save_summaries, 
                                                agg_regions=save_regions, offset_remove=manual_offset_remove, 
                                                covid_disruption=covid, covid_version=covid_date, ratio_stockouts=stockout_ratios)))

#***********************************************************************************************************************


#----CUSTOM POST-PROCESSING ON PREPARED OUTPUTS-------------------------------------------------------------------------
source("FILEPATH/chn_cap.R")

vaccines <- c("rotac", "pcv3", "hib3")
for (vaccine in vaccines) { 
  message(paste0("Adjusting China national **", vaccine, "** distribution"))
  loc_id    <- 6
  output_antigen <- stock_limit(gbd_cycle = gbd_cycle,  
                                vaccine = vaccine, 
                                run_date = run_date,
                                loc_name = "China",
                                quantile_max = 0.975, 
                                last_data_yr = 2019, 
                                last_est_yr = year.est.end)
  

  # re-save ST-GPR modeled CHN_vacc draws as "_original"
  vax_draws <- fread(paste0("FILEPATH/", loc_id, ".csv"))  
  fwrite(vax_draws, file=paste0("FILEPATH/", loc_id, "_orig.csv"))

  vax_trans <- output_antigen[[2]]
  fwrite(vax_trans, file=paste0("FILEPATH/", loc_id, ".csv"))
  # re-collapse CHN rotac draws
  #1. pull in existing collapsed means
  collapsed <- readRDS(paste0(results.root, "/vacc_", vaccine, ".rds"))
  #1.5 Resave with diff name
  saveRDS(collapsed, file=paste0(results.root, "/vacc_", vaccine, "_orig.rds"))
  #2. merge on transformed collapsed means only for CHN
  collapsed_loc <- collapsed[location_id==loc_id]
  collapsed_loc <- merge(collapsed_loc, output_antigen[[1]][, c("location_id", "year_id", "mean_trans", "lower_trans", "upper_trans")], 
                         by = c("location_id", "year_id"))
  #3. replace old with transformed
  collapsed_loc <- collapsed_loc[, `:=` (gpr_mean=NULL, gpr_lower=NULL, gpr_upper=NULL)]
  old <- c("mean_trans", "lower_trans", "upper_trans")
  new <- c("gpr_mean", "gpr_lower", "gpr_upper")
  setnames(collapsed_loc, old, new)
  #4. fill in NAs with 0s
  collapsed_loc[is.na(gpr_mean), gpr_mean := 0]
  collapsed_loc[is.na(gpr_lower), gpr_lower := 0]
  collapsed_loc[is.na(gpr_upper), gpr_upper := 0]
  #5. merge back to full collapsed file
  collapsed <- collapsed[location_id != loc_id]
  collapsed <- rbind(collapsed, collapsed_loc)
  #6. resave with original name
  saveRDS(collapsed, paste0(results.root, "/vacc_", vaccine, ".rds"))
  #7. save collapsed_loc pcv3 collapsed files to just CHN 
  saveRDS(collapsed_loc, paste0(results.root, "/vacc_", vaccine, "_china.rds"))
}


### Pull additional CHN PCV and Rotac raking files 
## LEVEL 4s COME FROM SAVED COLLAPSED MEANS OF PCV3
pcv3 <- readRDS(paste0(results.root, "/vacc_pcv3.rds"))
pcv3 <- pcv3[location_id==354 | location_id==361 | location_id==44533]
fwrite(pcv3, "FILEPATH/pcv3_level4_prepped.csv")

## LEVEL 4s COME FROM SAVED COLLAPSED MEANS OF ROTAC
rotac <- readRDS(paste0(results.root, "/vacc_rotac.rds"))
rotac <- rotac[location_id==354 | location_id==361 | location_id==44533]
fwrite(rotac, "FILEPATH/rotac_level4_prepped.csv")

## LEVEL 4s COME FROM SAVED COLLAPSED MEANS OF Hib3
hib3 <- readRDS(paste0(results.root, "/vacc_hib3.rds"))
hib3 <- hib3[location_id==354 | location_id==361 | location_id==44533]
fwrite(hib3, "FILEPATH/hib3_level4_prepped.csv")


## LEVEL 4s ALSO FROM SAVED DRAWS OF PCV3
china_4s <- locations[location_id==354 | location_id==361 | location_id==44533, location_id]
df <- data.table()
read.files <- function(chn_sub) {
  path <- paste0("FILEPATH/", chn_sub, ".csv")
  file <- fread(path)
  return(file)
}
df <- lapply(china_4s, read.files) %>% rbindlist
fwrite(df, "FILEPATH/pcv3_level4_draws_prepped.csv")

## LEVEL 4s ALSO FROM SAVED DRAWS OF ROTAC
df <- data.table()
read.files <- function(chn_sub) {
  path <- paste0("FILEPATH/", chn_sub, ".csv")
  file <- fread(path)
  return(file)
}
df <- lapply(china_4s, read.files) %>% rbindlist
fwrite(df, "FILEPATH/rotac_level4_draws_prepped.csv")

## LEVEL 4s ALSO FROM SAVED DRAWS OF Hib3
df <- data.table()
read.files <- function(chn_sub) {
  path <- paste0("FILEPATH/", chn_sub, ".csv")
  file <- fread(path)
  return(file)
}
df <- lapply(china_4s, read.files) %>% rbindlist
fwrite(df, "FILEPATH/hib3_level4_draws_prepped.csv")


## LEVEL 5s COME FROM SAVED DRAWS OF PCV3
china_5s <- locations[parent_id==44533, location_id]
df <- data.table()
read.files <- function(chn_sub) {
  path <- paste0("FILEPATH/", chn_sub, ".csv") 
  file <- fread(path)
  return(file)
}
df <- lapply(china_5s, read.files) %>% rbindlist
fwrite(df, "FILEPATH/pcv3_level5_draws_prepped.csv")

## LEVEL 5s COME FROM SAVED DRAWS OF ROTAC
df <- data.table()
read.files <- function(chn_sub) {
  path <- paste0("FILEPATH/", chn_sub, ".csv")  
  file <- fread(path)
  return(file)
}
df <- lapply(china_5s, read.files) %>% rbindlist
fwrite(df, "FILEPATH/rotac_level5_draws_prepped.csv")

## LEVEL 5s COME FROM SAVED DRAWS OF Hib3
df <- data.table()
read.files <- function(chn_sub) {
  path <- paste0("FILEPATH/", chn_sub, ".csv")  
  file <- fread(path)
  return(file)
}
df <- lapply(china_5s, read.files) %>% rbindlist
fwrite(df, "FILEPATH/hib3_level5_draws_prepped.csv")


source("FILEPATH/chn_gbd_raking.R")

source("FILEPATH/replace_raked_subnat_draws.R")
#***********************************************************************************************************************


#----PLOT!--------------------------------------------------------------------------------------------------------------
### national time series plot
system(paste0("qsub -N plot_gpr_output_vax -l m_mem_free=10G -l fthread=3 -l archive -l h_rt=2:30:00 -P proj_cov_vpd -q long.q -o FILEPATH/", username,
              " -e FILEPATH/", username, 
              " FILEPATH/health_fin_forecasting_shell_singularity.sh ",
              file.path(code_root, "FILEPATH/_plot_all_vaccines.r --args --date_str "), date))  
#***********************************************************************************************************************
