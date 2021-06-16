# 1. Prep

# clear workspace ----------------------------
rm(list=ls())

# set os flexibility & packages --------------
username <- Sys.info()[["user"]]
os <- .Platform$OS.type
if (os == "windows") {
  j_root <- "J:"
} else {
  j_root <- "/home/j"
}

library(data.table)
library(magrittr)
library(ggplot2)
library(openxlsx)
library(ggrepel)
library(dplyr)
library(reticulate)

source(paste0("FILEPATH/init.r"))
year_end        <- 2019
location_set_id <- 22
date            <- "2021-02-11" 
covid_free      <- "2021-02-11-covidfree"  
under_1s        <- 28  
decomp_step     <- "iterative"

# shared functions ---------------------
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_covariate_estimates.R")
source("FILEPATH/get_population.R")
source("FILEPATH/collapse_point.R")
'%!in%'   <- function(x,y)!('%in%'(x,y))

# core objects & directories -----------
temp.root    <- "FILEPATH/paper"
save.root    <- file.path(draws_root, "exp", gbd_cycle, date)
results.root <- file.path(data_root, "exp/modeled", gbd_cycle, date)
numbers.root <- file.path(temp.root, date, "number_plugging")  
ifelse (!dir.exists(numbers.root), dir.create(numbers.root, recursive = TRUE), FALSE)

# get locations
locations  <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step=decomp_step)
locs       <- locations[level >= 3, ]

# get populations
population <- get_population(age_group_id = under_1s, location_id = unique(locations$location_id),
                             year_id = c(1980:year_end), gbd_round_id = gbd_round, decomp_step=decomp_step, sex_id = 3, single_year_age = T)
cat(paste0("One-year-old population - model run ", unique(population$run_id)),
    file=file.path(numbers.root, paste0(date, "_input_model_version_ids.txt")), sep="\n", append=TRUE)

vaccines       <- paste0("vacc_", c("bcg", "dpt1", "dpt3", "polio3", "mcv1", "mcv2", "rcv1", "hepb3", "hib3", "rotac", "pcv3")) 
paper_vaccines <- copy(vaccines)  
new_vax        <- paste0("vacc_", c("mcv2", "rcv1", "hepb3", "hib3", "rotac", "pcv3"))
me_names   <- c("vacc_dpt1", "vacc_dpt3", "vacc_polio3", "vacc_mcv1", "vacc_mcv2", "vacc_rcv1",
                "vacc_hepb3", "vacc_hib3", "vacc_rotac", "vacc_pcv3", "vacc_bcg")
disp_names <- c("DTP1", "DTP3", "Polio3", "MCV1", "MCV2", "RCV1", "HepB3", "Hib3",  "RotaC",  "PCV3", "BCG")
colors     <- c("gold2", "#225ea8", "#1d91c0", "#4c9a2a", "#54278f", "#dd3497", "#fd8d3c", "#fc4e2a", "#980043", "#cb181d")
draw_vax   <- paste0("vacc_", c("dpt1", "dpt3", "mcv1", "polio3", "pcv3", "rotac", "hib3")) 
sdi_vax    <- c("vacc_dpt3", "vacc_mcv1", "vacc_polio3")
gvap_vax   <- c("vacc_dpt3", "vacc_mcv1", "vacc_polio3")

# introduction years
intros <- readRDS(file.path(ref_data_repo, "vaccine_intro.rds"))[me_name %in% vaccines]  

#********************************************************************************************************
#********************************************************************************************************


# 2. Read in the estimates

# reading in national means -------------
national  <- lapply(paste0(data_root, "/FILEPATH/", vaccines, ".rds"),
                    readRDS) %>% rbindlist(., fill=TRUE) %>% setnames(., c("gpr_mean", "gpr_lower", "gpr_upper"), c("mean", "lower", "upper")) %>% .[location_id %in% locations[level==3, location_id]]
nat_means <- merge(national, locations[, .(location_id, location_name_short, level, ihme_loc_id, super_region_name)], all.x=TRUE)
national  <- merge(national, locations[, .(location_id, location_name, super_region_name, region_name, lancet_label)], by="location_id", all.x=TRUE)
national  <- national[, c("age_group_id", "sex_id", "run_id", "covariate_id", "measure_id") := NULL]
national[, ihme_loc_id := NULL]
national <- merge(national, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)

# reading in aggregate estimates --------
agg_summaries <- lapply(paste0(data_root, "/FILEPATH/", vaccines, ".csv"),
                        fread) %>% rbindlist(., fill=TRUE) %>% unique
global <- agg_summaries[level=="global", .(me_name, year_id, mean, lower, upper)][, location_id := 1]
region <- agg_summaries[level=="region", .(me_name, year_id, mean, lower, upper, region_name)]
super_region <- agg_summaries[level=="super_region", .(me_name, year_id, mean, lower, upper, super_region_name)]

# read in national draws ----------------
all <- data.table()
for (vax in vaccines) {  
  message(paste0("Reading in the draws for ", vax))
  vax_draws <- lapply(paste0(draws_root, "/exp/", gbd_cycle, "/", date, "/", vax, "/", locations[level==3, location_id], ".csv"),
                      fread) %>% rbindlist(., fill=TRUE) 
  vax_draws[, me_name := vax]
  all <- rbind(all, vax_draws, fill=TRUE)
  all <- all[, c("run_id", "covariate_id", "ihme_loc_id") := NULL]
}
nat_draws <- copy(all)

#********************************************************************************************************
#********************************************************************************************************


# 3. Functions for number plugging

print_ <- function(dataset="national", type="exp", me, year, loc, loc_column="location_id", just_UI=FALSE) {
  col <- c("mean", "lower", "upper")
  if (type=="exp" & loc=="NA") df <- get(dataset)[me_name==me & year_id==year, col, with=FALSE]
  if (type=="exp" & loc != "NA") df <- get(dataset)[me_name==me & get(loc_column)==loc & year_id==year, col, with=FALSE]
  df[, (col) := lapply(.SD, function(x) format(round(x*100, 1), nsmall=1)), .SDcols=col]
  str <- paste0(df$mean, "% (", df$lower, "–", df$upper, "%)")
  if (just_UI) str <- paste0(df$lower, "-", df$upper, "%")
  return(str)
}
# Example output: "99.4% (98.4–99.8%)"


print_sq_brackets_ <- function(dataset="national", type="exp", me, year, loc, loc_column="location_id") {
  col <- c("mean", "lower", "upper")
  if (type=="exp") df <- get(dataset)[me_name==me & get(loc_column)==loc & year_id==year, col, with=FALSE]
  df[, (col) := lapply(.SD, function(x) format(round(x*100, 1), nsmall=1)), .SDcols=col]
  str <- paste0(df$mean, "% [95% UI: ", df$lower, "-", df$upper, "%]")
  return(str)
}
# Example output: "99.4% [95% UI: 98.4-99.8%]"


print_list_oxford_comma_ <- function(vector) {
  n_ <- length(vector)
  if (n_ > 2) {
    string_list <- paste(c(paste(vector[1:(n_ - 1)], collapse=", "), vector[n_]), collapse=", and ")
  } else {
    string_list <- paste(vector, collapse=" and ")
  }
  return(string_list)
}
# Example output: "1, 2, 3, 4, 5, 6, 7, 8, 9, and 10"

simple_pop_print <- function(pop_row) {
  m <- round(pop_row$mean/1e6,1)
  l <- round(pop_row$lower/1e6,1)
  u <- round(pop_row$upper/1e6,1)
  str <- paste0(m, " (", l, "-", u, ") million")
  return(str)
  
}

simple_print <- function(cov_row) {
  m <- round(cov_row$mean*100,1)
  l <- round(cov_row$lower*100,1)
  u <- round(cov_row$upper*100,1)
  str <- paste0(m, "% (", l, "-", u, "%)")
  return(str)
  
}


#********************************************************************************************************
#********************************************************************************************************

run_date    <- copy(date)  
vaccination <- readRDS(file.path("FILEPATH/", gbd_cycle, run_date, "vaccination.rds"))

v <- copy(vaccination)
used <- v[me_name %in% vaccines & !is.na(data) & ihme_loc_id %in% unique(locs[level==3]$ihme_loc_id) & nid != 451398]  
# total number of sources
length(unique(used$nid))

# total number of points
nrow(unique(used))
#********************************************************************************************************
#********************************************************************************************************


# 6. Number plugging - Results


## bcg outro:
intro_levs <- merge(intros, locations[,.(ihme_loc_id, level)], by="ihme_loc_id", all.x=T)
length(unique(intro_levs[me_name=="vacc_bcg" & level==3 & cv_outro < 9999]$ihme_loc_id))


# GLOBAL EPI ANTIGEN TRENDS ------------

# mcv
print_(dataset="global", me="vacc_mcv1", year=1980, loc=1)
print_(dataset="global", me="vacc_mcv1", year=2019, loc=1)

# dpt3
print_(dataset="global", me="vacc_dpt3", year=1980, loc=1)
print_(dataset="global", me="vacc_dpt3", year=2019, loc=1)

# polio3
print_(dataset="global", me="vacc_polio3", year=1980, loc=1)
print_(dataset="global", me="vacc_polio3", year=2019, loc=1)

## Increases between 1980-1989:
print_(dataset="global", me="vacc_dpt3", year=1980, loc=1)
print_(dataset="global", me="vacc_dpt3", year=1989, loc=1)


## which SRs increased between 1980-1989
super_region[year_id %in% c(1980,1989) & me_name=="vacc_dpt3"]


# universal country-level gains:
nat_dpt_1 <- national[year_id==1980 & me_name=="vacc_dpt3"]
nat_dpt_2 <- national[year_id==1989 & me_name=="vacc_dpt3"]
nat_dpt_decade1 <- merge(nat_dpt_1, nat_dpt_2, by=c("location_id", "me_name", "location_name", "super_region_name", 
                                                    "region_name", "lancet_label", "ihme_loc_id"))

nat_dpt_decade1 <- nat_dpt_decade1[, sr_N := .N, by=c("super_region_name")]

nat_dpt_decade1[, mean_increase := ifelse(mean.y > mean.x, 1, 0)]
nat_dpt_decade1[, num_mean_increase_sr := sum(mean_increase), by="super_region_name"]

nat_sr_increases <- nat_dpt_decade1[,.(super_region_name, sr_N, num_mean_increase_sr)] %>% unique
nat_sr_increases[, prop_increase := num_mean_increase_sr/sr_N][order(-prop_increase)]


# decade diffs
me <- "vacc_dpt3"
full_diffs <- fread(file.path(numbers.root, paste0(me, "_decade_mean_diffs_stagnation_full.csv")))

# first decade: 
nrow(full_diffs[vax_mean_1980 < 0.6])
nrow(full_diffs[vax_mean_1980 < 0.6 & decade_1_mean_diff > 0.1])
nrow(full_diffs[vax_mean_1980 < 0.6 & decade_1_mean_diff > 0.1]) / nrow(full_diffs[vax_mean_1980 < 0.6])

# second decade: 
nrow(full_diffs[vax_mean_1990 < 0.6])
nrow(full_diffs[vax_mean_1990 < 0.6 & decade_2_mean_diff > 0.1])
nrow(full_diffs[vax_mean_1990 < 0.6 & decade_2_mean_diff > 0.1]) / nrow(full_diffs[vax_mean_1990 < 0.6])

# third decade: 
nrow(full_diffs[vax_mean_2000 < 0.6])
nrow(full_diffs[vax_mean_2000 < 0.6 & decade_3_mean_diff > 0.1])
nrow(full_diffs[vax_mean_2000 < 0.6 & decade_3_mean_diff > 0.1]) / nrow(full_diffs[vax_mean_2000 < 0.6])

# last decade declines:
nrow(full_diffs[vax_mean_2010 >= 0.9 & vax_mean_2019 < 0.9])
nrow(full_diffs[vax_mean_2010 > 0.6 & vax_mean_2010 < 0.9 & decade_4_mean_diff < -0.05])
nrow(full_diffs[decade_4_mean_diff < 0])

# Last decade SRs:
head(full_diffs[order(decade_4_mean_diff)][,.(location_name, ihme_loc_id, super_region_name)])



# GLOBAL NEW VAX COVERAGE --------------

print_(dataset="global", me="vacc_hepb3", year=2019, loc=1)
print_(dataset="global", me="vacc_hib3", year=2019, loc=1)
print_(dataset="global", me="vacc_rcv1", year=2019, loc=1)
print_(dataset="global", me="vacc_mcv2", year=2019, loc=1)

print_(dataset="global", me="vacc_pcv3", year=2019, loc=1)
print_(dataset="global", me="vacc_rotac", year=2019, loc=1)


### Look at PCV and Rota vs DTP3
# country-level scale-up: 
pcv_rotac_nats <- national[me_name %in% c("vacc_pcv3", "vacc_rotac")]
pcv_rotac_nats <- merge(pcv_rotac_nats, intros[,.(ihme_loc_id, year_id, cv_intro, me_name)], by=c("ihme_loc_id", "year_id", "me_name"), all.x=T)

dpt_nats <- national[me_name=="vacc_dpt3"][,.(ihme_loc_id, year_id, mean, lower, upper)] %>% setnames(., c("mean", "lower", "upper"), c("dpt_mean", "dpt_lower", "dpt_upper"))
pcv_rotac_nats <-  merge(pcv_rotac_nats, dpt_nats, by=c("ihme_loc_id", "year_id"), all.x=T)
pcv_rotac_nats[, abs_diff_vs_dpt := abs(dpt_mean-mean)]

length(unique(pcv_rotac_nats[me_name=="vacc_pcv3" & cv_intro < 2015]$ihme_loc_id))
length(unique(pcv_rotac_nats[me_name=="vacc_pcv3" & year_id==2019 & cv_intro < 2015 & abs_diff_vs_dpt <= 0.05]$ihme_loc_id))
length(unique(pcv_rotac_nats[me_name=="vacc_pcv3" & year_id==2019 & cv_intro < 2015 & abs_diff_vs_dpt <= 0.05]$ihme_loc_id)) / length(unique(pcv_rotac_nats[me_name=="vacc_pcv3" & cv_intro < 2015]$ihme_loc_id))

length(unique(pcv_rotac_nats[me_name=="vacc_rotac" & cv_intro < 2015]$ihme_loc_id))
length(unique(pcv_rotac_nats[me_name=="vacc_rotac" & year_id==2019 & cv_intro < 2015 & abs_diff_vs_dpt <= 0.05]$ihme_loc_id))
length(unique(pcv_rotac_nats[me_name=="vacc_rotac" & year_id==2019 & cv_intro < 2015 & abs_diff_vs_dpt <= 0.05]$ihme_loc_id)) / length(unique(pcv_rotac_nats[me_name=="vacc_pcv3" & cv_intro < 2015]$ihme_loc_id))



### GVAP threshold based on MEANS only, nothing with UI or high certainty ---------------------------
# 2019:
dpt3_nats <- national[me_name=="vacc_dpt3"]
nrow(dpt3_nats[mean >= 0.9 & year_id==2019])
nrow(dpt3_nats[mean >= 0.9 & year_id==2019]) / 204

nrow(national[mean >= 0.9 & year_id==2019 & me_name=="vacc_mcv1"])
nrow(national[mean >= 0.9 & year_id==2019 & me_name=="vacc_mcv1"]) / 204

nrow(national[mean >= 0.9 & year_id==2019 & me_name=="vacc_polio3"])
nrow(national[mean >= 0.9 & year_id==2019 & me_name=="vacc_polio3"]) / 204

# 2010: 
nrow(dpt3_nats[mean >= 0.9 & year_id==2010])
nrow(dpt3_nats[mean >= 0.9 & year_id==2010]) / 204

nrow(national[mean >= 0.9 & year_id==2010 & me_name=="vacc_mcv1"])
nrow(national[mean >= 0.9 & year_id==2010 & me_name=="vacc_mcv1"]) / 204

nrow(national[mean >= 0.9 & year_id==2010 & me_name=="vacc_polio3"])
nrow(national[mean >= 0.9 & year_id==2010 & me_name=="vacc_polio3"]) / 204


# SR trends: 
tf <- fread(file.path(numbers.root, "gvap_heatmap_means_sr.csv"))[order(super_region_name)]
tf_2010 <- tf[year_id==2010] %>% setnames(., "p", "p_2010")
tf_2019 <- tf[year_id==2019] %>% setnames(., "p", "p_2019")
tf_wide <- merge(tf_2010, tf_2019, by=c("me_name", "super_region_name"), all.x=T, all.y=T)
tf_wide[!is.na(p_2010), change_in_p := p_2019-p_2010]
tf_wide[order(change_in_p)][!me_name %in% c("vacc_all", "vacc_any")]

tf_wide[!me_name %in% c("vacc_dpt3", "vacc_mcv1", "vacc_polio3", "vacc_any", "vacc_all")][order(-change_in_p)]
super_region[year_id==2019 & !me_name %in% c("vacc_dpt3", "vacc_mcv1", "vacc_polio3", "vacc_bcg", "vacc_dpt1")] %>%
  .[order(-mean)]


## SR trends (cont) and lowest proportion SR: 
sr_gvap_spread <- copy(national[year_id==2019 & !me_name %in% c("vacc_bcg", "vacc_dpt1")])
sr_gvap_spread[, flag := ifelse(mean>=0.9, 1, 0)]
sr_gvap_spread[, n := sum(flag), by=.(year_id, super_region_name, me_name)]
sr_gvap_spread[, tot := .N, by=.(year_id, super_region_name, me_name)]
sr_gvap_spread[, p := n/tot]

sr_gvap_spread <- sr_gvap_spread[, .(me_name, year_id, super_region_name, p, n, tot)] %>% unique
sr_gvap_spread[order(p)][order(super_region_name)][year_id==2019]  

length(unique(national[grepl("Sub-Saharan", super_region_name) & mean >= 0.9 & year_id==2019 & !me_name %in% c("vacc_bcg", "vacc_dpt1")]$ihme_loc_id)) / length(unique(national[grepl("Sub-Saharan", super_region_name) & year_id==2019]$ihme_loc_id))


# Globally 2019: 
all_gvap_vax <- national[!me_name %in% c("vacc_bcg", "vacc_dpt1") & mean >= 0.9 & year_id==2019]
all_gvap_vax <- all_gvap_vax[, .N, by="ihme_loc_id"]

length(unique(all_gvap_vax[N>=9]$ihme_loc_id))  
length(unique(all_gvap_vax[N>=9]$ihme_loc_id)) / 204

print_list_oxford_comma_(locations[ihme_loc_id %in% unique(all_gvap_vax[N>=9]$ihme_loc_id)]$lancet_label)




###### EXPECTED GIVEN SDI
ant <- "vacc_dpt3"  
expected <- fread(file.path(temp.root, "sdi", covid_free, paste0(ant, ".csv")))[,.(sdi, mean)] %>% setnames(., "mean", "expected")

## changes in expected coverage at diff sdi cutoffs
expected1 <- fread(file.path(temp.root, "sdi", covid_free, paste0(ant, ".csv")))[,.(sdi, mean, lower, upper)] %>% setnames(., "mean", "expected")
expected1[sdi==0.25]
expected1[sdi==0.35]

expected1[sdi==0.75]
expected1[sdi==0.85]


# combining national and expected ~~~
sdi <- get_covariate_estimates(covariate_id=881, gbd_round_id = gbd_round, decomp_step = decomp_step,
                               location_id = unique(ant_draws$location_id))[year_id==2019, .(location_id, year_id, mean_value)] %>% setnames(., "mean_value", "sdi")
national_expected <- merge(national, sdi, by=c("location_id", "year_id"), all.x=T)[year_id==2019]
national_expected[, sdi := round(sdi, 2)]

expected <- fread(file.path(temp.root, "sdi", covid_free, paste0(ant, ".csv")))[,.(sdi, mean, lower, upper)] %>% 
  setnames(., c("mean", "lower", "upper"), c("expected", "expected_lower", "expected_upper"))
national_expected <- merge(national_expected, expected, by="sdi", all.x=T)

# BFA actual vs expected:
national[ihme_loc_id=="BFA" & year_id==2019 & me_name=="vacc_dpt3", c(mean, lower, upper)]
national_expected[ihme_loc_id=="BFA" & year_id==2019 & me_name=="vacc_dpt3", c(expected, expected_lower, expected_upper)]

# AGO actual vs expected: 
national[ihme_loc_id=="AGO" & year_id==2019 & me_name=="vacc_dpt3", c(mean, lower, upper)]
national_expected[ihme_loc_id=="AGO" & year_id==2019 & me_name=="vacc_dpt3", c(expected, expected_lower, expected_upper)]



###### ZERO-DOSE
## Mean DPT0 counts at the GLOBAL level --

# read in DTP1 counts, which already contains an aggregated population column
global_draws <- fread(paste0(results.root, "FILEPATH/vacc_dpt1_global_draws.csv"))

# start doing the math
draw_cols <- names(global_draws)[grepl("draw_[0-9]*", names(global_draws))]
global_draws[, (draw_cols) := (population-.SD), .SDcols = draw_cols]
global_mean <- collapse_point(global_draws)

# save for zero_dose_stacked_bar_plots.R
fwrite(global_mean, file.path(numbers.root, "zero_dose_global_counts.csv"), row.names=FALSE)

# grab the numbers
simple_pop_print(global_mean[year_id==1980])
simple_pop_print(global_mean[year_id==2019])


# DPT0 GLOBAL decline (since 1980) -----
start_year <- 1980 

# read in DTP1 counts, which already contains an aggregated population column
region_draws <- fread(paste0(results.root, "/FILEPATH/vacc_dpt1_global_draws.csv"))

# DTP0 math and prep draw proportions
draw_cols <- names(region_draws)[grepl("draw_[0-9]*", names(region_draws))]
region_draws[, (draw_cols) := (population-.SD), .SDcols = draw_cols]
historical_draws <- region_draws[year_id==start_year] %>% setnames(., draw_cols, paste0("historical_", draw_cols)) %>% .[, year_id := NULL]
current_draws <- region_draws[year_id==2019] %>% setnames(., draw_cols, paste0("current_", draw_cols)) %>% .[, year_id := NULL]

# make wide by appending columns
current_draws <- current_draws[, c(paste0("current_", draw_cols)), with=F]
both <- cbind(historical_draws, current_draws)

# current / historical, and smallest proportion indicates greatest DTP0 proportional decrease
for (x in 0:999) {
  both[, paste0("prop_col_", x) := get(paste0("current_draw_", x)) / get(paste0("historical_draw_", x))]
}

region_mean <- collapse_point(both, draws_name = "prop_col")
region_mean[, reduction_prop := 1-mean]
region_mean[, reduction_prop_lower := 1-lower]
region_mean[, reduction_prop_upper := 1-upper]

region_mean[order(mean)]  



## Mean DPT0 counts at the SR level --

# read in DTP1 counts, which already contains an aggregated population column
sr_draws <- fread(paste0(results.root, "/FILEPATH/vacc_dpt1_super_region_draws.csv"))

# start doing the math
draw_cols <- names(sr_draws)[grepl("draw_[0-9]*", names(sr_draws))]
sr_draws[, (draw_cols) := (population-.SD), .SDcols = draw_cols]
sr_mean <- collapse_point(sr_draws)

# save for zero_dose_stacked_bar_plots.R
fwrite(sr_mean, file.path(numbers.root, "zero_dose_sr_counts.csv"), row.names=FALSE)
sr_mean <- fread(file.path(numbers.root, "zero_dose_sr_counts.csv"))


# DPT0 REGIONAL decline (since 1980) -----
start_year <- 2000 ## 1980 or 2000 based on sentence

# read in DTP1 counts, which already contains an aggregated population column
region_draws <- fread(paste0(results.root, "/FILEPATH/vacc_dpt1_super_region_draws.csv"))

# DTP0 math and prep draw proportions
draw_cols <- names(region_draws)[grepl("draw_[0-9]*", names(region_draws))]
region_draws[, (draw_cols) := (population-.SD), .SDcols = draw_cols]
historical_draws <- region_draws[year_id==start_year] %>% setnames(., draw_cols, paste0("historical_", draw_cols)) %>% .[order(super_region_name)] %>% .[, year_id := NULL]
current_draws <- region_draws[year_id==2019] %>% setnames(., draw_cols, paste0("current_", draw_cols)) %>% .[order(super_region_name)] %>% .[, year_id := NULL]

# make wide by appending columns
current_draws <- current_draws[, c(paste0("current_", draw_cols)), with=F]
both <- cbind(historical_draws, current_draws)

# current / historical, and smallest proportion indicates greatest DTP0 proportional decrease
for (x in 0:999) {
  both[, paste0("prop_col_", x) := get(paste0("current_draw_", x)) / get(paste0("historical_draw_", x))]
}

region_mean <- collapse_point(both, draws_name = "prop_col")
region_mean[, reduction_prop := 1-mean]
region_mean[, reduction_prop_lower := 1-lower]
region_mean[, reduction_prop_upper := 1-upper]

region_mean[order(mean)]


## between 1980-2019:   
region_mean[super_region_name=="South Asia", c(reduction_prop, reduction_prop_lower, reduction_prop_upper)]
region_mean[grepl("Southeast Asia", super_region_name), c(reduction_prop, reduction_prop_lower, reduction_prop_upper)]
region_mean[super_region_name=="Sub-Saharan Africa", c(reduction_prop, reduction_prop_lower, reduction_prop_upper)]
      
# num zero-dose in SSA
## SSA:
simple_pop_print(sr_mean[grepl("Sub-Saharan", super_region_name) & year_id==2019])
sr_mean[grepl("Sub-Saharan", super_region_name) & year_id==2019]

      
      
# percentage of 0-dose in SSA reduction
sr_cov_draws <- fread(paste0(results.root, "/FILEPATH/vacc_dpt1_super_region_cov_draws.csv"))
draw_cols <- names(sr_cov_draws)[grepl("draw_[0-9]*", names(sr_cov_draws))]
sr_cov_draws[, (draw_cols) := (1-.SD), .SDcols = draw_cols]
sr_cov_mean <- collapse_point(sr_cov_draws)
# save for zero_dose_stacked_bar_plots.R
fwrite(sr_cov_mean, file.path(numbers.root, "zero_dose_sr_proportion_w_ui.csv"), row.names=FALSE)

# 1980 to 2019 SSA percentage:
sr_cov_mean[grepl("Sub-Saharan", super_region_name) & year_id==1980]
sr_cov_mean[grepl("Sub-Saharan", super_region_name) & year_id==2019]


## between 2000-2019 (repeat of above for LatinAm)
simple_pop_print(sr_mean[grepl("Latin Am", super_region_name) & year_id==2000])
simple_pop_print(sr_mean[grepl("Latin Am", super_region_name) & year_id==2019])
sr_mean[grepl("Latin Am", super_region_name) & year_id==2000]
sr_mean[grepl("Latin Am", super_region_name) & year_id==2019]

# 2000 to 2019 SSA percentage:
sr_cov_mean[grepl("Latin Am", super_region_name) & year_id==2000]
sr_cov_mean[grepl("Latin Am", super_region_name) & year_id==2019]

# Mean DPT0 counts at COUNTRY level -----
dpt_draws <- nat_draws[me_name=="vacc_dpt1"]

# subset to 2000 and 2019
dpt_draws <- dpt_draws[year_id %in% c(2000, 2019)]

# add in 2000 and 2019 population
unvax <- merge(dpt_draws, population[,.(location_id, year_id, population)], by=c("location_id", "year_id"), all.x=T)

# start doing the math
draw_cols <- names(unvax)[grepl("draw_[0-9]*", names(unvax))]
unvax[, (draw_cols) := (1-.SD) * population, .SDcols = draw_cols]
zerodose <- collapse_point(unvax)
setnames(zerodose, "mean", "zerodose")

# how many countries contribute 75% unvaccinated kids
newdata <- zerodose[order(-zerodose),]
newdata$cumsum <- cumsum(newdata$zerodose)
total0 <- sum(newdata$zerodose, na.rm=TRUE)
newdata$percent <- newdata$cumsum / total0
newdata[year_id==2019 & percent >= 0 & percent <= 0.1, cat := 10]
newdata[year_id==2019 & percent >= 0.10 & percent <= 0.25, cat := 25]
newdata[year_id==2019 & percent >= 0.25 & percent <= 0.5, cat := 50]
newdata[year_id==2019 & percent >= 0.5 & percent <= 0.75, cat := 75]
newdata[year_id==2019 & percent >= 0.75 & percent <= 0.9, cat := 90]
newdata[year_id==2019 & percent >= 0.9, cat := 100]
newdata$cat <- as.factor(newdata$cat)
ftable(newdata$cat)

# merge on location_name
newdata <- merge(newdata, locations[,.(location_id, lancet_label)], by="location_id", all.x=T)
try <- newdata[order(lancet_label)]
try[cat %in% c(50, 75)]
#********************************************************************************************************
#********************************************************************************************************
