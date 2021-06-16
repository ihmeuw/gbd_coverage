#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    Februrary 2017, modified December 2017 for GBD 2017
# Purpose: Vaccinations - prep model results for save_results (covariates)
#***********************************************************************************************************************


#----CONFIG-------------------------------------------------------------------------------------------------------------
### set paths
run_log <- file.path(ref_data_repo, "FILEPATH/vaccination_run_log.csv")
me_db   <- fread(paste0(code_root, "/FILEPATH/me_db.csv"))

### load functions
source("FILEPATH/get_population.R")  
#***********************************************************************************************************************


#----FUNCTIONS----------------------------------------------------------------------------------------------------------
### get the best run_id from vaccination run log
best.run_id <- function(me, age_specific=FALSE) {
  
  df <- fread(run_log)
  if (age_specific) {
    run_id <- df[me_name==me & is_best=="age_specific"]$run_id
  } else {
    run_id <- df[me_name==me & is_best==1]$run_id
  }
  if (length(run_id) > 1) stop("More than 1 run_id")
  return(run_id)
  
}

### read in draws
read.draws <- function(run_id, offset=FALSE) {
  
  path <- paste0("FILEPATH/draws_temp_0")  
  files <- list.files(path, full.names=TRUE)
  if (length(files)==0) stop(paste0("No draws for this run_id (", run_id, ")"))
  df <- mclapply(files, fread, mc.cores=10) %>% rbindlist(., use.names=TRUE)
  key <- c("location_id", "year_id", "age_group_id", "sex_id")
  setkeyv(df, cols=key)
  df <- unique(df)
  
  if (offset) {
    cols <- grep("draw_", names(df), value=TRUE)
    df <- df[, (cols) := .SD-0.01, .SDcols=cols]
  }
  
  return(df)
  
}

### duplicate draws
duplicate.draws <- function(df, age_group_ids=c(2:20, 30:32, 235), sex_ids=c(1, 2)) { 
  
  demo <- expand.grid(age_group_id=age_group_ids, sex_id=sex_ids) %>% data.table
  df.i <- df %>% copy
  df <- lapply(1:nrow(demo), function(x) {
    age <- demo[x]$age_group_id
    sex <- demo[x]$sex_id
    cf <- df.i[, `:=` (age_group_id=age, sex_id=sex)] %>% copy
    return(cf)
  }) %>% rbindlist
  return(df)
  
}

### apply introduction dates
set.intro <- function(df, me) {
  
  vacc.intro <- data.table(readRDS(file.path(ref_data_repo, "vaccine_intro.rds")))[me_name==me]
  vacc.intro <- vacc.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.intro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[cv_intro_years < 1, (cols) := 0]
  df$cv_intro_years <- NULL
  return(df)
  
}

### apply delayed introduction dates for EPI vaccines
set.intro.epi <- function(df, me) {
  
  vacc.epi.intro <- data.table(readRDS(file.path(ref_data_repo, "vaccine_intro_epi.rds")))[me_name==me]
  vacc.epi.intro <- vacc.epi.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.epi.intro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[cv_intro_years < 1, (cols) := 0]
  df$cv_intro_years <- NULL
  return(df)
  
}

### cap estimates to 99% coverage
cap.est <- function(df) {
  
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[, (cols) := lapply(.SD, function(x) ifelse(x >= 1, 0.99, x)), .SDcols=cols]
  return(df)
  
}

### collapse draws into mean, lower, and upper
collapse.draws <- function(df) {
  
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[, gpr_mean := rowMeans(.SD), .SDcols=cols]
  df$gpr_lower <- apply(df[, (cols), with=F], 1, quantile, probs=0.025)
  df$gpr_upper <- apply(df[, (cols), with=F], 1, quantile, probs=0.975)
  df <- df[, -(cols), with=FALSE]
  return(df)
  
}

### save draws to /share/ folder
save.draws <- function(df, me, root) {
  
  path <- file.path(root, me)
  unlink(path, recursive=TRUE)
  dir.create(path, showWarnings=FALSE)
  location_ids <- unique(df$location_id)
  invisible(mclapply(location_ids, function(x) {
    fwrite(df[location_id==x], paste0(path, "/", x, ".csv"), row.names=FALSE)
  }, mc.cores=10))
  
}

### save collapsed draws to best folder
save.collapsed <- function(df, me) {
  
  path <- paste0(results.root, "/", me, ".rds")
  saveRDS(df, path)
  
}


### apply BCG outro_year force to 0 at the draw level 
apply.outro.year <- function(df, me) {
  
  vacc.intro <- data.table(readRDS(file.path(ref_data_repo, "vaccine_intro.rds")))[me_name==me]
  vacc.outro <- vacc.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.outro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[cv_intro_years == 0, (cols) := 0]  
  df$cv_intro_years <- NULL
  return(df)
  
}

### apply custom BCG outro and re-intro year for GRL
apply.outro.grl <- function(df, me) {
  
  vacc.intro <- data.table(readRDS(file.path(ref_data_repo, "vaccine_intro.rds")))[me_name==me]
  vacc.outro <- vacc.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.outro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df[location_id==349 & year_id %in% c(1991:1995), (cols) := 0]  
  df$cv_intro_years <- NULL
  return(df)
  
}



### prep draws from the model output folder
prep.draws <- function(me, draws=FALSE, path=save.root, dupe=FALSE, quantiles=TRUE, agg_regions=FALSE, offset_remove=FALSE, 
                       covid_disruption=TRUE, covid_version=NULL) {
  
  # read in model estimates
  if (me %in% me_db$me_name) cov_id <- me_db[me_name==me, covariate_id]
  id <- best.run_id(me)
  if (me == "vacc_hib3") {
    df <- read.draws(best.run_id("vacc_dpt3"))
  } else if (me == "vacc_rotac") {
    df <- read.draws(best.run_id("vacc_rota1"))
  } else {
    df <- read.draws(id, offset=offset_remove)
  }
  df <- cap.est(df)
  df <- df[, run_id := id]
  
  # set intro
  if (me %in% c("vacc_hib3", "vacc_pcv3", "vacc_rotac")) df <- set.intro(df, me)
  if (grepl("ratio", me)) df <- set.intro(df, me)
  
  # set intro epi
  if (me %in% c("vacc_bcg", "vacc_dpt3", "vacc_mcv1", "vacc_polio3")) df <- set.intro.epi(df, me)
  
  # set outro bcg  
  if (me %in% c("vacc_bcg")) {
    df <- apply.outro.year(df, me)
    df <- apply.outro.grl(df, me)
  } 
  if (me %in% "vacc_polio3") {  
    message("Custom outro of CUB Polio3")
    cols <- grep("draw_", names(df), value=TRUE)
    df <- merge(df, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df <- df[grepl("CUB", ihme_loc_id), (cols) := 0]
  }
  
  # aggregate up
  if (me %in% c("vacc_hib3", "vacc_pcv3", "vacc_rotac", "vacc_hepb3", "vacc_mcv2", "vacc_rcv1") | grepl("ratio", me)) {
    key <- c("location_id", "year_id", "age_group_id", "sex_id")
    df <- melt(df, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="est")
    df <- aggregate_most_detailed(df, varname="est")
    df <- dcast.data.table(df, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
  }
  
  # apply COVID-disruption
  if (me %in% c("vacc_dpt3", "vacc_mcv1", "vacc_polio3", "vacc_bcg") & covid_disruption) {
    # read in covid_disruption (reference scenario)
    if (me=="vacc_mcv1") antigen <- "vaccine_mcv" else antigen <- "vaccine_dtp"
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
    df <- merge(df, covid_disrupt, by=c("location_id", "year_id"), all.x=T)
    # .SD cols to multiply out
    cols <- grep("draw_", names(df), value=TRUE)
    df <- df[!is.na(value), (cols) := .SD*value, .SDcols=cols]
    # get rid of cols that don't need anymore
    df[, value := NULL]
  }
  
  # save draws
  if (draws) {
    if (dupe) df2 <- duplicate.draws(df) else df2 <- copy(df)
    if (me %in% me_db$me_name) df2[, covariate_id := cov_id]
    save.draws(df2, me, path)
    print(paste0("Saved draws of ", me, " under run_id ", id, " in ", path))
  } 
  
  # if want to aggregate region, super region, and global population-weighted coverage
  if (agg_regions) {
    if (!draws) {
      if (dupe) df2 <- duplicate.draws(df) else df2 <- copy(df)
    }
    aggregate_regions(df=df2, me=me)
  }
  
  # save summaries
  if (quantiles) {
    df <- collapse.draws(df)
    df <- df[, me_name := me]
    if (me %in% me_db$me_name) df[, covariate_id := cov_id]
    save.collapsed(df, me)
    print(paste0("Saved collapsed ", me, " under run_id ", id, " in ", results.root))
  }
  
}

### prep draws/summary for mes calculated as ratios
prep.ratio <- function(me, head="vacc_", draws=FALSE, ratio_draws=TRUE, path=save.root, dupe=FALSE, flip=FALSE, quantiles=TRUE, 
                       agg_regions=FALSE, offset_remove=FALSE, covid_disruption=TRUE, covid_version=NULL, ratio_stockouts=FALSE) {
  
  # setup and load
  key   <- c("location_id", "year_id", "age_group_id", "sex_id")
  mes   <- unlist(strsplit(me, "_"))[2:3]
  num   <- paste0(head, mes[1])
  denom <- paste0(head, mes[2])
  
  # flip if denom / num instead of num / denom (i.e. DTP1)
  if (flip) {
    num   <- paste0(head, mes[2])
    denom <- paste0(head, mes[1])
  } 
  id.ratio <- best.run_id(me)
  id.denom <- best.run_id(denom)
  df.ratio <- read.draws(id.ratio, offset=offset_remove)
  df.ratio <- set.intro(df.ratio, me)
  df.denom <- read.draws(id.denom, offset=offset_remove)
  df.ratio <- cap.est(df.ratio)
  df.denom <- cap.est(df.denom)
  
  # apply COVID-disruption to denominator
  if (covid_disruption) {
    # read in covid_disruption (reference scenario)
    if (me %in% c("vacc_mcv2_mcv1_ratio", "vacc_rcv1_mcv1_ratio")) antigen <- "vaccine_mcv" else antigen <- "vaccine_dtp"
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
    df.denom <- merge(df.denom, covid_disrupt, by=c("location_id", "year_id"), all.x=T)
    # .SD cols to multiply out
    cols <- grep("draw_", names(df.denom), value=TRUE)
    df.denom <- df.denom[!is.na(value), (cols) := .SD*value, .SDcols=cols]
    # get rid of cols that don't need anymore
    df.denom[, value := NULL]
  }
  
  # add in a new function for forcing specific pcv3, rotac, etc. to 0 in PHL for example
  if (me %in% "vacc_rotac_dpt3_ratio") {
    message("Custom outro of PHL RotaC after 2015")
    cols <- grep("draw_", names(df.ratio), value=TRUE)
    df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df.ratio <- df.ratio[grepl("PHL", ihme_loc_id) & year_id > 2015, (cols) := 0][, ihme_loc_id := NULL]
    message("Done")
    message("Custom outro of VEN RotaC after 2017")
    cols <- grep("draw_", names(df.ratio), value=TRUE)
    df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df.ratio <- df.ratio[grepl("VEN", ihme_loc_id) & year_id > 2017, (cols) := 0][, ihme_loc_id := NULL]
    message("Done")
  }
  if (me %in% "vacc_hib3_dpt3_ratio") {
    message("Custom outro of TUN Hib3 2006-2010")
    cols <- grep("draw_", names(df.ratio), value=TRUE)
    df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df.ratio <- df.ratio[grepl("TUN", ihme_loc_id) & year_id %in% 2006:2010, (cols) := 0]
    message("Done")
  }
  if (me %in% "vacc_rcv1_mcv1_ratio") {
    message("Custom outro of MAR RCV1 2008-2013")
    cols <- grep("draw_", names(df.ratio), value=TRUE)
    df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df.ratio <- df.ratio[grepl("MAR", ihme_loc_id) & year_id %in% 2008:2013, (cols) := 0]
    message("Done")
  }
  if (me %in% "vacc_mcv2_mcv1_ratio") {
    message("Custom outro of MAR MCV2 2008-2013")
    cols <- grep("draw_", names(df.ratio), value=TRUE)
    df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df.ratio <- df.ratio[grepl("MAR", ihme_loc_id) & year_id %in% 2008:2013, (cols) := 0]
    message("Done")
  }
  if (me %in% "vacc_pcv3_dpt3_ratio") {
    message("Custom outro of VEN PCV3 starting 2017")
    cols <- grep("draw_", names(df.ratio), value=TRUE)
    df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df.ratio <- df.ratio[grepl("VEN", ihme_loc_id) & year_id > 2016, (cols) := 0][, ihme_loc_id := NULL]
    message("Done")
  }
  
  
  # save draws for future use
  if (ratio_draws) { 
    save.draws(df.ratio, me, path)
    print(paste0("Saved ratio draws of ", me, " under run_id ", id.ratio, " in ", path))
  }
  
  # reshape long, merge
  df.ratio <- melt(df.ratio, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="ratio")
  df.denom <- melt(df.denom, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="denom")
  df <- merge(df.ratio, df.denom, by=c(key, "draw"))
  
  # multiply out ratio
  if (flip) df <- df[, ratio := 1 / ratio]
  df <- df[, est := ratio * denom]
  df <- df[, c("ratio", "denom") := NULL]
  df <- df[est >= 1, est := 0.99]
  
  if (ratio_stockouts) {
    # read in exact_matches of positive ratio stockouts and subset to current me_name
    pos_stockouts <- fread(paste0(data_root, "FILEPATH/exact_ratio_pos_matches.csv"))[me_name==me] %>% unique
    pos_stockouts <- merge(pos_stockouts, locations[,.(ihme_loc_id, location_id)], all.x=T)
    pos_stockouts <- pos_stockouts[,.(location_id, year_id, cv_stockout_ratio)]
    
    # merge to df
    df <- merge(df, pos_stockouts, by=c("location_id", "year_id"), all.x=T, all.y=T)
    
    df <- df[!is.na(cv_stockout_ratio), est := est*cv_stockout_ratio][, cv_stockout_ratio := NULL]
    
    df[est > 1, est := 0.99]
  }
  
  
  # aggregate up
  df <- aggregate_most_detailed(df, varname="est")
  
  # save covariate id
  if (num %in% me_db$me_name) cov_id <- me_db[me_name==num, covariate_id]
  
  # if want to save by draws, reshape and write files
  if (draws) {
    df2 <- dcast.data.table(df, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
    df2 <- df2[, measure_id := 18]
    if (num %in% me_db$me_name) df2[, covariate_id := cov_id]
    if (dupe) df2 <- duplicate.draws(df2)
    save.draws(df2, num, path)
    print(paste0("Saved draws of ", num, " under run_id ", id.ratio, " in ", path))
  } 
  
  # if want to aggregate region, super region, and global population-weighted coverage
  if (agg_regions) {
    if (!draws) {
      df2 <- dcast.data.table(df, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
      if (dupe) df2 <- duplicate.draws(df2)
    }
    aggregate_regions(df=df2, me=num)
  }
  
  # if want to save mean-lower-upper, compute and write files
  if (quantiles) {
    # calculate and save summaries
    df <- df[, gpr_mean := mean(est), by=key]
    df <- df[, gpr_lower := quantile(est, 0.025), by=key]
    df <- df[, gpr_upper := quantile(est, 0.975), by=key]
    df <- df[, c(key, "gpr_mean", "gpr_lower", "gpr_upper"), with=FALSE] %>% unique
    df <- df[, me_name := num]
    df <- df[, measure_id := 18]
    if (num %in% me_db$me_name) df[, covariate_id := cov_id]
    save.collapsed(df, num)
    print(paste0("Saved collapsed ", num, " under run_id ", id.ratio, " in ", results.root))
  }
  
}



### aggregate estimates by super region
aggregate_estimates <- function(df, vars, by="super_region_name") {
  
  key <- c("year_id", by)
  agg <- df %>% copy
  # aggregate estimates to the parent_id [ sum(var * pop) /sum_pop ]
  agg <- agg[, sum_pop := sum(population), by=key]
  agg <- agg[, (vars) := lapply(.SD, function(x) x * agg[['population']]), .SDcols=vars]
  agg <- agg[, (vars) := lapply(.SD, sum), .SDcols=vars, by=key]
  # de-duplicate so get one set of estimates
  agg <- unique(agg[, c(by, "year_id", "age_group_id", "sex_id", "sum_pop", vars), with=FALSE])
  # divide by sum_pop
  agg <- agg[, (vars) := lapply(.SD, function(x) x / agg[['sum_pop']]), .SDcols=vars]
  agg <- agg[, sum_pop := NULL]
  # rename parent_id -> location_id
  return(agg)
  
}

### lagged vaccine coverage
make_lags <- function(me, lag_years) {
  
  new <- paste0(me, "_lag_", lag_years)
  clean_data <- readRDS(file.path(results.root, paste0(me, ".rds")))
  square <- CJ(location_id=unique(clean_data$location_id), year_id=year_start:year_end, age_group_id=22, sex_id=3)
  data_lag <- merge(square, copy(clean_data)[, year_id := year_id + lag_years], by=c("location_id", "year_id", "age_group_id", "sex_id"), all.x=TRUE)
  data_lag[is.na(gpr_mean), c("gpr_mean", "gpr_lower", "gpr_upper") := 0]
  data_lag[, me_name := new]
  data_lag[, measure_id := clean_data$measure_id %>% unique]
  save.collapsed(data_lag, new)
  print(paste0("Summaries saved for ", new))
  
}


### discard all but most detailed location, aggregate up the location hierarchy
### ensures internally consistent estimates even in geographies with staggered roll-out over time and space
aggregate_most_detailed <- function(df, varname="est") {
  
  ### separate most detailed from parent geographies
  df_all <- merge(df[, c("location_id", "year_id", "age_group_id", "sex_id", "draw", varname), with=FALSE],
                  locations[, c("location_id", "most_detailed"), with=FALSE], 
                  by="location_id", all.x=TRUE)
  
  ### get populations
  population <- get_population(gbd_round_id=gbd_round, 
                               age_group_id=unique(df_all$age_group_id), 
                               sex_id=unique(df_all$sex_id), 
                               location_id=unique(df_all$location_id), 
                               year_id=unique(df_all$year_id), 
                               decomp_step = decomp_step)
  
  ### set variables for aggregation
  by_vars  <- c("year_id", "age_group_id", "sex_id", "draw", "parent_id")
  loc_cols <- c("parent_id", "level")
  
  ### start with smallest level and work up
  df_most_detailed <- df_all[most_detailed==1, c("location_id", by_vars[!by_vars %in% "parent_id"], varname), with=FALSE]
  for (location_level in c(6, 5, 4)) {
    
    # merge missing vars
    df_agg <- merge(df_most_detailed, locations[, c("location_id", loc_cols[!loc_cols %in% colnames(df_most_detailed)]), with=FALSE], by="location_id", all.x=TRUE)
    df_agg <- df_agg[level==location_level, c("location_id", by_vars, varname), with=FALSE]
    df_agg <- merge(df_agg, population[, .(location_id, year_id, age_group_id, sex_id, population)], by=c("location_id", "year_id", "age_group_id", "sex_id"), all.x=TRUE)
    
    # aggregate
    df_agg[, sum_pop := sum(population), by=by_vars]
    df_agg[, mu      := get(varname) * (population / sum_pop)]
    df_agg[, agg     := sum(mu), by=by_vars]
    df_aggregated <- unique(df_agg[, c(by_vars, "agg"), with=FALSE])
    
    # keep and clean
    setnames(df_aggregated, c("parent_id", "agg"), c("location_id", varname))
    df_most_detailed <- rbind(df_most_detailed, df_aggregated, fill=TRUE)
    
  }
  
  return (df_most_detailed)
  
}
#***********************************************************************************************************************