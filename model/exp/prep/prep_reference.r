









username <- Sys.info()[["user"]]


library(data.table)
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(magrittr)


code_root <- file.path("FILEPATH", username, "vaccines")
source(file.path(code_root, "init.r"))
source(db_tools)
source("FILEPATH/read_excel.R")
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/cluster_tools.r")


'%!in%'  <- function(x,y)!('%in%'(x,y))


decomp_step <- "iterative"
locations   <- get_location_metadata(location_set_id = location_set_id,
                                     release_id = release_id)
locs        <- locations[level >= 3, ]





vacc.intro          <- paste0(data_root, "FILEPATH/vaccine-introduction--2023.xlsx")
vacc.intro.supp     <- paste0(data_root, "FILEPATH/vacc_intro_supplement.csv")

vacc.bcg.outro      <- paste0(data_root, "FILEPATH/BCG_WORLD_ATLAS_2017_2ND_ED_Y2019M05D22.XLS")
vacc.bcg.prepped    <- paste0(data_root, "FILEPATH/vacc_bcg_outro_prepped.csv")
vacc.outro.supp     <- paste0(data_root, "FILEPATH/bcg_outro_supplement.xlsx")

prev.vacc.jrf.coverage   <- file.path(data_root, "FILEPATH/coverage--2021.xlsx")
vacc.jrf.coverage   <- file.path(data_root, "FILEPATH/coverage--2023.xlsx")
ind.unicef          <- paste0(data_root, "FILEPATH/ind_unicef_reported.csv")

vacc.schedule       <- paste0(data_root, "FILEPATH/vaccine-schedule--2023.xlsx")
vacc_target         <- paste0(data_root, "FILEPATH/special_country_mmr_schedules.csv") 
vacc.stock.ind      <- paste0(data_root, "/raw/stockouts/Vaccine supply and commodities-2023.xlsx")
vacc.epi            <- paste0(data_root, "FILEPATH/later_epi_start.csv")
stockout.add        <- paste0(data_root, "FILEPATH/supplemental_unreported_stockouts.csv")


ref_data_repo <- "FILEPATH"
me.db         <- file.path(ref_data_repo, "me_db.csv") %>% fread
date          <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")


year.est.start <- year_start[[1]]
year.est.end   <- year_end[[1]]
model.mes      <- paste0("vacc_", c("bcg", "dpt1", "dpt3", "hepb3", "hib3", "mcv1", "mcv2", "pcv3", "polio3", "rcv1", "rotac"))










prep.jrf.coverage <- function(path, model_mes, me_db=me.db) {

  
  data <- read_excel(path) %>% data.table

  
  names(data)            <- tolower(names(data))
  data$coverage_category <- tolower(data$coverage_category)
  setnames(data, c("year", "code", "coverage"), c("year_id", "ihme_loc_id", "data"))
  data[grepl("DTPCV", antigen),   antigen := gsub("CV", "", antigen)]
  data[grepl("RUBELLA", antigen), antigen := gsub("RUBELLA", "RCV", antigen)]
  data[, data := data/100]

  me_db$who_name <- toupper(me_db$who_name)
  setnames(me_db, "who_name", "antigen")

  
  admin     <- data[coverage_category == "admin" & ihme_loc_id %in% unique(locations$ihme_loc_id) &
                   !grepl("booster|birth|FLU", antigen_description)]

  official  <- data[coverage_category=="official" & ihme_loc_id %in% unique(locations$ihme_loc_id) &
                      !grepl("booster|birth|FLU", antigen_description)]

  wuenic    <- data[coverage_category=="wuenic" &  
                   !grepl("booster|birth|FLU", antigen_description)]

  
  admin    <- merge(admin, me_db[,.(antigen, me_name)], by="antigen", all.x=TRUE)[!is.na(me_name)]
  official <- merge(official, me_db[,.(antigen, me_name)], by="antigen", all.x=TRUE)[!is.na(me_name)]
  wuenic   <- merge(wuenic, me_db[,.(antigen, me_name)], by="antigen", all.x=TRUE)[!is.na(me_name)]

  admin    <- admin[me_name %in% model.mes]
  official <- official[me_name %in% model.mes]
  wuenic   <- wuenic[me_name %in% model.mes]

  
  admin <- admin[,.(ihme_loc_id, me_name, year_id, target_number, doses, data)]
  admin[, `:=` (cv_admin    = 1,
                nid         = 203321,
                survey_name = "WHO/UNICEF Admin Data")]
  setnames(admin, c("target_number", "doses"), c("TargetGroup", "DosesAdministered"))

  official <- official[,.(ihme_loc_id, year_id, me_name, data)]
  official[, `:=` (cv_admin    = 1,
                   nid         = 203321,
                   survey_name = "WHO/UNICEF Official Country-Reported Data")]

  wuenic <- wuenic[,.(group, ihme_loc_id, year_id, me_name, data, coverage_category_description)]

  
  ad.grl   <- admin[ihme_loc_id == "DNK"]
  ad.grl[, ihme_loc_id := "GRL"]
  admin    <- rbind(admin, ad.grl)
  of.grl   <- official[ihme_loc_id == "DNK"]
  of.grl[, ihme_loc_id := "GRL"]
  official <- rbind(official, of.grl)

  
  admin    <- admin[ihme_loc_id == "DEU", year_id := year_id - 5]
  official <- official[ihme_loc_id == "DEU", year_id := year_id - 5]

  
  done          <- list()
  done$admin    <- admin
  done$official <- official
  done$wuenic   <- wuenic

  
  return(done)
}








make.intro_frame <- function(path=vacc.intro, me_db=me.db) {

  
  intros_df        <- read_excel(path) %>% data.table %>% unique
  names(intros_df) <- tolower(names(intros_df))

  
  me_db.new <- me_db[!is.na(who_description) & who_description != "", .(who_description, me_name)]
  setnames(me_db.new, "who_description", "description")
  message("Preparing introduction year df for only new vaccines: ", paste0("\n-- ", c(unique(me_db.new$me_name)), " and ratio"))
  intros_df <- merge(intros_df, me_db.new, by = "description", all.x = TRUE)
  intros_df <- intros_df[!is.na(me_name)][, c("description", "countryname") := NULL]

  
  setnames(intros_df, c("year", "iso_3_code"), c("year_id", "ihme_loc_id"))

  
  intros_df <- intros_df[ihme_loc_id %in% unique(locs$ihme_loc_id)]

  
  intros_df <- intros_df[intro %in% c("Yes", "Yes (P)")]
  intros_df[, cv_intro := .SD[!is.na(intro), min(year_id, na.rm = TRUE)],
            by = c("me_name", "ihme_loc_id"),
            .SDcols = c("ihme_loc_id", "me_name", "year_id", "intro")]
  intros_df <- unique(intros_df[, .(ihme_loc_id, cv_intro, me_name)])

  
  supp_intros <- fread(vacc.intro.supp)[(is.na(ignore) | ignore != 1)][,.(me_name, ihme_loc_id, cv_intro)]
  setnames(supp_intros, "cv_intro", "alt_cv_intro")
  intros_df <- merge(intros_df, supp_intros, by=c("me_name", "ihme_loc_id"), all.x=TRUE, all.y=TRUE)
  intros_df[!is.na(alt_cv_intro), cv_intro := alt_cv_intro][, alt_cv_intro := NULL]

  
  all_intros <- CJ(ihme_loc_id = unique(locs$ihme_loc_id),
                   year_id     = as.numeric(year_start):as.numeric(year_end),
                   me_name     = c(unique(intros_df$me_name)))
  all_intros <- merge(all_intros, intros_df, by=c("ihme_loc_id", "me_name"),
                      all.x=TRUE, all.y=TRUE)

  
  all_intros <- merge(all_intros, locs[,.(ihme_loc_id, location_id, parent_id, level)], by="ihme_loc_id", all.x=TRUE)
  all_intros[is.na(cv_intro), cv_intro := 9999]

  for (lvl in unique(all_intros[level > 3]$level) %>% sort) {
    intro.parent <- all_intros[, .(location_id, year_id, cv_intro, me_name)]
    setnames(intro.parent, c('cv_intro', 'location_id'), c('intro_parent', 'parent_id'))
    all_intros <- merge(all_intros, intro.parent, by=c("parent_id", "year_id", "me_name"), all.x=TRUE)
    all_intros <- all_intros[level==lvl & cv_intro == 9999, cv_intro := intro_parent]
    all_intros <- all_intros[, intro_parent := NULL]
  }

  
  all_intros[, country := substr(ihme_loc_id, 1, 3)]
  all_intros[, min_intro_subnational := min(cv_intro, na.rm = TRUE), by = c("country", "me_name")]
  all_intros[ihme_loc_id == country, cv_intro := min_intro_subnational]
  all_intros <- all_intros[, c("parent_id", "level", "min_intro_subnational", "country") := NULL]

  
  all_intros[is.na(cv_intro), cv_intro := 9999]

  
  for (me in unique(all_intros$me_name)) {

    df_me_ratio <- all_intros[me_name==me]

    if (me %in% paste0("vacc_", c("hepb3", "hib3", "pcv3", "rotac"))) {
      df_me_ratio[, me_name := paste0(me_name, "_dpt3_ratio")]
    } else if (me %in% paste0("vacc_", c("mcv2", "rcv1"))) {
      df_me_ratio[, me_name := paste0(me_name, "_mcv1_ratio")]
    }
    all_intros <- rbind(all_intros, df_me_ratio)
  }

  
  all_intros <- all_intros[order(me_name, ihme_loc_id, year_id)]

  
  all_intros[, cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]

  
  message("All done")
  return(all_intros)
}



straight.intro_frame <- function() {

  
  straight_models <- c("vacc_mcv1", "vacc_dpt3", "vacc_bcg", "vacc_polio3", "vacc_dpt1")
  df <- CJ(location_id  = unique(locs$location_id),
           year_id      = year_start:year.est.end,
           age_group_id = 22,
           sex_id       = 3,
           me_name      = straight_models)

  df <- df[, cv_intro := 1950]

  
  late_epi <- fread(vacc.epi)[(is.na(ignore) | ignore != 1)] %>% data.table
  late_epi <- merge(late_epi, locs[, c("location_name", "ihme_loc_id", "location_id", "level")], by = c("location_name", "ihme_loc_id"), all.x = TRUE)

  
  df <- merge(df, late_epi, by = c("location_id", "me_name"), all.x = TRUE)
  df <- df[!is.na(cv_intro.y), cv_intro.x := cv_intro.y]
  df <- df[, `:=` (location_name=NULL, ihme_loc_id=NULL, cv_intro.y=NULL)]
  setnames(df, "cv_intro.x", "cv_intro")

  
  df <- df[, level := NULL]

  
  df <- df[, cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]
  return(df)
}








prep.schedule.rotac <- function() {

  
  df <- read_excel(vacc.schedule, sheet="Data") %>% data.table
  names(df) <- tolower(names(df))

  
  df <- df[grepl("Rota", vaccine_description)]

  
  df <- df[is.na(targetpop) | targetpop != "RISKGROUPS"]

  
  df <- df[is.na(ageadministered) | ageadministered != "-4444"]

  
  setnames(df, "iso_3_code", "ihme_loc_id")
  setnames(df, "schedulerounds", "dose_number")

  
  df_schedule <- data.table()
  new_col <- lapply(unique(df$ihme_loc_id), function(x) {

    df_sched <- df[ihme_loc_id==x]
    df_sched <- df_sched[, doses := max(df_sched$dose_number)]

  }) %>% rbindlist
  df_schedule <- rbind(df_schedule, new_col)
  df <- copy(df_schedule)

  
  df <- df[,.(ihme_loc_id, doses)] %>% unique

  
  df <- df[ihme_loc_id %in% unique(locations$ihme_loc_id)]

  
  df <- merge(df, locs[,.(ihme_loc_id, location_id, parent_id, level)], by="ihme_loc_id", all.y=TRUE)

  
  for (lvl in unique(df[level > 3]$level) %>% sort) {
    parent <- df[, .(location_id, doses)]
    setnames(parent, c("location_id", "doses"), c("parent_id", "parent_doses"))
    df <- merge(df, parent, by='parent_id', all.x=TRUE)
    df <- df[level==lvl & is.na(doses), doses := parent_doses]
    df <- df[, parent_doses := NULL]
  }
  df[, c("parent_id", "level") := NULL]

  
  df[, me_name := "vacc_rotac"]

  
  df <- df[!is.na(doses)]

  
  return(df)

}





prep.target <- function(me, vacc_target) {
  
  df <- fread(vacc_target)[me_name==me]
  
  df <- merge(locs[, .(ihme_loc_id, location_id, parent_id, level)], df[, .(ihme_loc_id, age_cohort)], by="ihme_loc_id", all.x=TRUE)
  for (lvl in unique(df[level > 3]$level) %>% sort) {
    parent <- df[, .(location_id, age_cohort)]
    setnames(parent, c("location_id", "age_cohort"), c("parent_id", "parent_age_cohort"))
    df <- merge(df, parent, by='parent_id', all.x=TRUE)
    df <- df[level==lvl & is.na(age_cohort), age_cohort := parent_age_cohort]
    df <- df[, parent_age_cohort := NULL]
  }
  
  df <- df[, .(ihme_loc_id, age_cohort)]
  df[, me_name := me]
  df <- df[!is.na(age_cohort)]
  return(df)
}









bcg.outro.clean <- function(me, prepped_outro_path = vacc.bcg.prepped) {

  
  column_rename_table <- c("iso3"                                                   = "ihme_loc_id",
                           "Country"                                                = "location_name",
                           "Current BCG Vaccination?"                               = "current_bcg",
                           "BCG Policy Type"                                        = "policy_type",
                           "Which year was the vaccination introduced?"             = "intro_year",
                           "Which year was the policy discontinued? (Group B only)" = "outro_year",
                           "Year of Changes to BCG Policy"                          = "policy_change_year",
                           "Details of Changes to BCG Policy"                       = "policy_change_details",
                           "Are there targeted/'special' groups which receive BCG?" = "special_pops",
                           "Details of Targeted/'Special' Groups"                   = "special_pops_details")
  old_column_names <- names(column_rename_table)
  new_column_names <- unname(column_rename_table)

  
  data <- read_excel(vacc.bcg.outro) %>% as.data.table

  
  
  df <- subset(data, select = old_column_names)
  setnames(df, names(column_rename_table), unname(column_rename_table))
  df <- df[!is.na(current_bcg)]

  
  df <- df[current_bcg != "Yes"]

  
  df[is.na(outro_year) & !is.na(policy_change_year), `:=` (outro_year = policy_change_year,
                                                           note       = "Using policy change year as outro_year")]

  
  df[is.na(outro_year) & ihme_loc_id=="ECU", `:=` (current_bcg = "Yes",  note = "Consistent admin data and WUENIC estimates also")]
  df[is.na(outro_year) & ihme_loc_id=="AND", `:=` (outro_year  = "1979", note = "Ambiguous reporting and no admin data nor WUENIC estimate")]
  df[is.na(outro_year) & ihme_loc_id=="LUX", `:=` (outro_year  = "1979", note = "Ambiguous reporting and no admin data nor WUENIC estimate")]
  df <- df[current_bcg != "Yes"]

  
  df[grepl("specific groups or none", policy_type) & (grepl("[:space:]", outro_year) | outro_year == "N/A"),
     `:=` (outro_year="1979", note="Ambiguous reporting and no admin data nor WUENIC estimate")]

  
  
  df[outro_year == "N/A" & ihme_loc_id == "NZL", `:=` (outro_year = "1979", note = "Phase outs between 1960s and 90s, so do not estimate")]
  df[grepl("[:space:]", outro_year) & ihme_loc_id=="AUS", `:=` (outro_year = "1979", note = "Phase outs between 1960s and 90s, so do not estimate")]

  
  df.outro <- df[, .(ihme_loc_id, location_name, outro_year, note)]

  
  df.outro <- df.outro[!ihme_loc_id %in% c("DEU", "NOR", "GBR")]

  
  special.pops <- data.table(ihme_loc_id   = c("DEU", "NOR", "GBR"),
                             location_name = c("Germany", "Norway", "United Kingdom"),
                             outro_year    = c("1975", "1979", "1979"),
                             note          = "Special populations adjustment")

  
  df.outro <- rbind(df.outro, special.pops)

  
  df.outro[, `:=` (me_name = me, nid = 407800)]  
  df.outro$outro_year <- as.numeric(df.outro$outro_year)

  
  suppl <- read_excel(vacc.outro.supp, sheet="clean_and_append") %>% data.table
  suppl[source=="WORLD ATLAS", nid := 407800]
  suppl[, c("source", "source2", "note2") := NULL]
  suppl[, me_name := "vacc_bcg"]
  suppl    <- merge(suppl, locations[,.(ihme_loc_id, location_name)], by="ihme_loc_id", all.x = TRUE)
  df.outro <- df.outro[!ihme_loc_id %in% unique(suppl$ihme_loc_id)]  
  df.outro <- rbind(df.outro, suppl)

  
  df.outro[!ihme_loc_id %in% c("AUT", "CZE", "MLT", "SVN"), outro_year := outro_year + 1]

  
  fwrite(df.outro, file = prepped_outro_path)


  
  df <- data[,.(iso3, Country,
                `Current BCG Vaccination?`,
                `BCG Policy Type`,
                `Which year was the vaccination introduced?`,
                `Which year was the policy discontinued? (Group B only)`,
                `Year of Changes to BCG Policy`,
                `Details of Changes to BCG Policy`)]
  df <- df[!is.na(`Current BCG Vaccination?`)]

  
  setnames(df, names(column_rename_table), unname(column_rename_table), skip_absent = TRUE)

  
  df <- df[!grepl("[:space:]", intro_year) & !is.na(intro_year) & !intro_year %in% c("Unknown", "UNK", "N/A")]
  
  df <- df[ihme_loc_id != "JPN"]

  
  df$intro_year <- as.numeric(df$intro_year)
  df.epi <- df[,.(ihme_loc_id, location_name, intro_year)]
  df.epi <- df.epi[intro_year >= 1980]
  df.epi[, `:=` (me_name=me, nid=407800)]

  
  fwrite(df.epi, file=paste0(data_root, "FILEPATH/bcg_world_atlas_delayed_intro.csv"))

  
  all <- list(df.outro, df.epi)
  return(all)
}



make.outro_frame <- function(me) {

  
  df <- fread(vacc.bcg.prepped)[me_name==me]  
  setnames(df, "outro_year", "cv_outro")

  
  df <- merge(locs[, .(ihme_loc_id, location_id, parent_id, level)], df, by='ihme_loc_id', all.x=TRUE)

  
  for (lvl in unique(df[level>3]$level) %>% sort) {
    outro.parent <- df[, .(location_id, cv_outro)]
    setnames(outro.parent, c('cv_outro', 'location_id'), c('outro_parent', 'parent_id'))
    df <- merge(df, outro.parent, by='parent_id', all.x=TRUE)
    df <- df[level==lvl & is.na(cv_outro), cv_outro := outro_parent]
    df <- df[, outro_parent := NULL]
  }
  df <- df[, c("parent_id", "level") := NULL]

  
  df <- df[is.na(cv_outro), cv_outro := 9999]

  
  square <- expand.grid(ihme_loc_id=locs$ihme_loc_id, year_id=as.numeric(year.est.start:year.est.end)) %>% data.table
  df <- merge(square, df, by='ihme_loc_id', all.x=TRUE)
  df <- df[, me_name := me]
  df <- df[order(ihme_loc_id, year_id)]

  df <- df[, cv_intro_years := ifelse((cv_outro - year_id)>0, year_id - 1980 + 1, 0)]  
  df <- df[, .(ihme_loc_id, year_id, location_id, me_name, cv_outro, cv_intro_years)]
  return(df)
}








admin.dpt.denom <- function(df) {

  vacc_to_test <- list(c("vacc_pcv3", "vacc_rotac", "vacc_hib3", "vacc_hepb3"),
                       c("vacc_rcv1", "vacc_mcv2"),
                       c("vacc_dpt3", "vacc_mcv1"))
  df   <- df[!is.na(TargetGroup) & TargetGroup != 0 & !is.na(DosesAdministered) & DosesAdministered != 0]
  orig <- copy(df)
  df   <- df[, denom_change := ""]
  df   <- df[, orig_TargetGroup := TargetGroup]

  
  for (me in vacc_to_test[[1]]) {

    
    who   <- copy(me)
    denom <- vacc_to_test[[3]][1]
    message(paste("Confirming correct admin data denominator for", who, "vs", denom))

    
    working <- df[me_name==who | me_name==denom]

    
    for (loc in unique(df$ihme_loc_id)) {
      
      
      if(loc == 'BIH') next

      cp  <- working[ihme_loc_id==loc]
      yrs <- cp[, .N, by = "year_id"][N == 2, year_id]
      cp  <- cp[year_id %in% yrs]

      
      for (yr in yrs) {
        tg_who   <- cp[me_name==who & year_id==yr]
        tg_denom <- cp[me_name==denom & year_id==yr]
        if (tg_denom$TargetGroup > tg_who$TargetGroup) {  

          
          df <- df[ihme_loc_id==loc & me_name==tg_who$me_name & year_id==yr, `:=` (TargetGroup=tg_denom$TargetGroup, denom_change=1)]
        }
      }
      
      df <- df[, new_PerCentCoverage := round(DosesAdministered/TargetGroup, 2)]
      df <- df[is.na(new_PerCentCoverage), new_PerCentCoverage := 0]
    }
  }
  mismatch <- df[denom_change==1]
  mismatch <- mismatch[, change := new_PerCentCoverage - data]

  return(mismatch)

}


denom.adjust <- function(adjust_file="who_official") {

  
  df <- readRDS(paste0(data_root, "FILEPATH/who_admin.rds"))

  
  message("Making the necessary swaps")
  dpt.denom <- admin.dpt.denom(df)
  both      <- copy(dpt.denom)
  
  setnames(both, "new_PerCentCoverage", "data_replace")
  
  replace <- both[, c("ihme_loc_id", "me_name", "year_id", "data_replace")]
  
  replace[ihme_loc_id=="DEU", year_id := year_id-5]
  
  data <- readRDS(paste0(data_root, "FILEPATH", adjust_file, ".rds"))
  
  saveRDS(data, paste0(data_root, "FILEPATH", adjust_file, "_denom_unadjusted.rds"))
  
  message("Updating coverage where denominators changed")
  dt <- merge(data, replace, by = c("ihme_loc_id", "year_id", "me_name"), all.x = TRUE)
  dt <- dt[!is.na(data_replace), data := data_replace]  

  
  dt <- dt[, data_replace := NULL]

  message("All done")
  return(dt)
}








prep.reported.stockouts <- function(vacc.stock.ind) {

  
  message("-- Reading in the data")
  df <- read_excel(vacc.stock.ind) %>% data.table
  names(df) <- tolower(names(df))

  
  df <- df[grepl("stockout", description) | grepl("stock-out", description) | grepl("interrupted", description)]

  
  
  df <- df[grepl("Was there a stock-out at the national level", description)]

  
  df <- df[value=="Yes"]

  
  message("-- Determining vaccine-specific stockouts")
  df <- df[, count := seq(1:nrow(df))]
  for (row in seq(1, nrow(df), 1)) {
    temp   <- unlist(strsplit(df[count==row]$indcode, "SUPPLY_NATIONAL_"))[2]
    df     <- df[count==row, me := temp]
  }

  
  message("-- Cleaning")
  setnames(df, c("iso_3_code", "year"), c("ihme_loc_id", "year_id"))
  df <- df[,.(ihme_loc_id, year_id, indcode, me)]

  
  df$me <- tolower(df$me)
  df[grepl("^pcv", me), me := "vacc_pcv"]
  df[grepl("^hepb", me), me := "vacc_hepb"]
  df[grepl("^yfv", me), me := "vacc_yfv"]
  df[grepl("^bcg", me), me := "vacc_bcg"]
  df[grepl("^measles", me) | grepl("mm", me), me := "vacc_mcv"]
  df[grepl("^polio", me) | grepl("ipv", me) | grepl("opv", me), me := "vacc_polio"]
  df[grepl("^rota", me), me := "vacc_rotac"]
  df[grepl("^dtp", me), me := "vacc_dpt"]
  df[grepl("^tt", me), me := "vacc_tt"]
  df[grepl("^hpv", me), me := "vacc_hpv"]
  df[grepl("^hib", me), me := "vacc_hib"]
  df[grepl("^typhoid", me), me := "vacc_typhoid"]

  
  df <- unique(df)
  df[, indcode := NULL]

  
  df <- df[ihme_loc_id %in% locations$ihme_loc_id]

  
  message("-- DONE prepping reported stockouts")
  return(df)

}








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







use.alternate.admin <- function(new_data, change_df) {
  
  

  
  alt_data <- fread(new_data)
  alt_data <- alt_data[, .(ihme_loc_id, me_name, year_id, data, cv_admin, nid, survey_name)]

  
  alt_data[, data := data/100]
  
  
  alt_data[, id := paste0(ihme_loc_id, me_name, year_id)]
  change_df[, id := paste0(ihme_loc_id, me_name, year_id)]
  change_df <- change_df[!id %in% unique(alt_data$id)]
  
  first_yr <- min(alt_data[ihme_loc_id=="IND", year_id])
  change_df[ihme_loc_id=="IND" & year_id <= first_yr, data := NA]
  change_df <- change_df[!is.na(data)]

  
  done <- rbind(change_df, alt_data)
  done[, id := NULL]

  return(done)
}




fill_missing_official_with_admin <- function(prepped_official_path, prepped_admin_path) {

  message("Where official coverage data is missing, replacing with administrative data")

  
  official <- readRDS(prepped_official_path)[, .(ihme_loc_id, year_id, me_name, official = data)]
  admin    <- readRDS(prepped_admin_path)[, .(ihme_loc_id, year_id, me_name, admin = data)]

  
  admin <- admin[!(ihme_loc_id %in% c('BGD','MHL') & year_id == 2022)]
  admin <- admin[!(ihme_loc_id == 'ROU' & year_id %in% c(2019, 2020) & me_name %in% c('vacc_dpt1', 'vacc_pcv3'))]

  
  admin <- admin[!is.na(admin), ]
  official_with_admin_fill <- merge(official, admin, by = c("ihme_loc_id", "year_id", "me_name"), all = TRUE)
  official_with_admin_fill[is.na(official) & !is.na(admin), official := admin]

  
  official_with_admin_fill <- official_with_admin_fill[, .(ihme_loc_id, year_id, me_name, data = official,
                                                           cv_admin = 1,
                                                           nid = 203321,
                                                           survey_name = "WHO/UNICEF Official Country-Reported Data")]
  return(official_with_admin_fill)

}



make_hotfixes <- function(who_official, who_admin) {

  
  admin_GNB_2021 <- who_admin[ihme_loc_id == "GNB" & year_id %in% 2020:2022 & !is.na(data) &
                                me_name %in% paste0("vacc_", c("bcg", "dpt1", "dpt3", "mcv1", "polio3", "rotac")),
                              .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "GNB" & year_id %in% 2020:2022 &
                                     me_name %in% paste0("vacc_", c("bcg", "dpt1", "dpt3", "mcv1", "polio3", "rotac"))), ]
  who_official   <- rbind(who_official, admin_GNB_2021, fill = TRUE)

  
  admin_MRT <- who_admin[ihme_loc_id == "MRT" & year_id %in% 2020:2022 & !is.na(data) &
                           me_name %in% paste0("vacc_", c("bcg", "dpt1", "dpt3", "mcv1", "polio3", "rotac")),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "MRT" & year_id %in% 2020:2022 &
                                     me_name %in% paste0("vacc_", c("bcg", "dpt1", "dpt3", "mcv1", "polio3", "rotac"))), ]
  who_official   <- rbind(who_official, admin_MRT, fill = TRUE)

  
  admin_BFA_2021_PCV3 <- who_admin[ihme_loc_id == "BFA" & year_id == 2021 & me_name == "vacc_pcv3",
                                   .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "BFA" & year_id == 2021 & me_name == "vacc_pcv3"), ]
  who_official   <- rbind(who_official, admin_BFA_2021_PCV3, fill = TRUE)

  
  admin_UGA <- who_admin[ihme_loc_id == "UGA" & year_id %in% 2018:2019 & !is.na(data),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "UGA" & year_id %in% 2018:2019), ]
  who_official   <- rbind(who_official, admin_UGA, fill = TRUE)

  
  admin_ARM <- who_admin[ihme_loc_id == "ARM" & year_id ==2010 & me_name=='vacc_hib3',
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "ARM" & year_id ==2010 & me_name=='vacc_hib3'), ]
  who_official   <- rbind(who_official, admin_ARM, fill = TRUE)

  
  
  admin_BRA_2020_dpt3 <- who_admin[ihme_loc_id == "BRA" & year_id == 2020 & me_name == "vacc_dpt3",
                                   .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "BRA" & year_id == 2020 & me_name == "vacc_dpt3"), ]
  who_official   <- rbind(who_official, admin_BRA_2020_dpt3, fill = TRUE)

  
  
  who_official[ihme_loc_id == "LBR" & me_name == "vacc_mcv2" & year_id == 2019, data := data * .25]


  
  admin_CAF <- who_admin[ihme_loc_id == "CAF" & year_id %in% c(2019,2022) & !is.na(data),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "CAF" & year_id %in% c(2019,2022)), ]
  who_official   <- rbind(who_official, admin_CAF, fill = TRUE)

  
  admin_KEN <- who_admin[ihme_loc_id == "KEN" & year_id ==2022 & me_name=='vacc_rotac',
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "KEN" & year_id ==2022 & me_name=='vacc_rotac'), ]
  who_official   <- rbind(who_official, admin_KEN, fill = TRUE)

  admin_RWA <- who_admin[ihme_loc_id == "RWA" & year_id == 2020 & !is.na(data),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "RWA" & year_id == 2020), ]
  who_official   <- rbind(who_official, admin_RWA, fill = TRUE)

  admin_FSM <- who_admin[ihme_loc_id == "FSM" & year_id == 2022 & !is.na(data),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "FSM" & year_id == 2022), ]
  who_official   <- rbind(who_official, admin_FSM, fill = TRUE)
  
  admin_GNB <- who_admin[ihme_loc_id == "GNB" & year_id == 2023,
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "GNB" & year_id == 2023), ]
  who_official   <- rbind(who_official, admin_GNB, fill = TRUE)
  
  admin_ATG <- who_admin[ihme_loc_id == "ATG" & year_id == 2012 & me_name=='vacc_dpt1',
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "ATG" & year_id == 2012 & me_name=='vacc_dpt1'), ]
  who_official   <- rbind(who_official, admin_ATG, fill = TRUE)
  
  admin_COM <- who_admin[ihme_loc_id == "COM" & year_id %in% 2016:2022 & !is.na(data),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "COM" & year_id %in% 2016:2022), ]
  who_official   <- rbind(who_official, admin_COM, fill = TRUE)
  
  admin_KEN <- who_admin[ihme_loc_id == "KEN" & year_id == 2004 & me_name=='vacc_dpt1',
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "KEN" & year_id == 2004 & me_name=='vacc_dpt1'), ]
  who_official   <- rbind(who_official, admin_KEN, fill = TRUE)
  
  admin_PLW <- who_admin[ihme_loc_id == "PLW" & year_id == 2022 & me_name %in% c('vacc_rotac','vacc_pcv3'),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "PLW" & year_id == 2022 & me_name %in% c('vacc_rotac','vacc_pcv3')), ]
  who_official   <- rbind(who_official, admin_PLW, fill = TRUE)
  
  admin_PRY <- who_admin[ihme_loc_id == "PRY" & year_id == 2023,
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "PRY" & year_id == 2023), ]
  who_official   <- rbind(who_official, admin_PRY, fill = TRUE)
  
  admin_SLB <- who_admin[ihme_loc_id == "SLB" & year_id == 2001 & me_name=='vacc_dpt1',
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "SLB" & year_id == 2001 & me_name=='vacc_dpt1'), ]
  who_official   <- rbind(who_official, admin_SLB, fill = TRUE)
  
  admin_UGA <- who_admin[ihme_loc_id == "UGA" & year_id == 2023 & me_name %in% c('vacc_dpt3','vacc_polio3','vacc_hib3','vacc_mcv1','vacc_pcv3', 'vacc_rcv1'),
                         .(ihme_loc_id, year_id, me_name, data, cv_admin, nid, survey_name)]
  who_official   <- who_official[!(ihme_loc_id == "UGA" & year_id == 2023 & me_name %in% c('vacc_dpt3','vacc_polio3','vacc_hib3','vacc_mcv1','vacc_pcv3', 'vacc_rcv1')), ]
  who_official   <- rbind(who_official, admin_UGA, fill = TRUE)
  
  return(who_official)
}








 bcg_data     <- bcg.outro.clean(me = "vacc_bcg")
 bcg.outro.df <- bcg_data[[1]]
 bcg.epi.df   <- bcg_data[[2]]






intro.frame <- make.intro_frame(path = vacc.intro, me_db = me.db)
outro.frame <- make.outro_frame(me = "vacc_bcg")
intro.frame <- rbind(intro.frame, outro.frame, fill=TRUE, use.names=TRUE)
saveRDS(intro.frame, file = file.path(ref_data_repo, "vaccine_intro.rds"))



intro.epi.frame <- straight.intro_frame()
saveRDS(intro.epi.frame, file.path(ref_data_repo, "vaccine_intro_epi.rds"))





schedule.vacc <- prep.schedule.rotac()
saveRDS(schedule.vacc, file.path(ref_data_repo, "vaccine_schedule.rds"))


target.vacc <- lapply(c("vacc_mcv1", "vacc_mcv2", "vacc_rcv1"), prep.target, vacc_target) %>% rbindlist
saveRDS(target.vacc, file.path(ref_data_repo, "vaccine_target.rds"))





jrf.data <- prep.jrf.coverage(path=vacc.jrf.coverage, model_mes=model.mes)
old.jrf.data <- prep.jrf.coverage(path=prev.vacc.jrf.coverage, model_mes=model.mes)



for(x in 1:length(jrf.data)){
  for(a in unique(jrf.data[[x]]$ihme_loc_id)){
    if(a == 'DEU'){
      if(nrow(jrf.data[[x]][ihme_loc_id == a & year_id < 1995]) == 0)
        jrf.data[[x]] <- rbind(jrf.data[[x]], old.jrf.data[[x]][ihme_loc_id==a & year_id < 1995])
    } else if(a != 'DEU' & nrow(jrf.data[[x]][ihme_loc_id==a & year_id < 2000]) == 0){
      jrf.data[[x]] <- rbind(jrf.data[[x]], old.jrf.data[[x]][ihme_loc_id==a & year_id < 2000])
    }

  }
}


saveRDS(jrf.data$offic, paste0(data_root, "FILEPATH/who_official.rds"))

saveRDS(jrf.data$admin, paste0(data_root, "FILEPATH/who_admin.rds"))

saveRDS(jrf.data$wuenic, file.path(ref_data_repo, "wuenic_estimates.rds"))












who.official <- readRDS(paste0(data_root, "FILEPATH/who_official.rds"))  
replaced     <- use.alternate.admin(new_data = ind.unicef, change_df = who.official)

saveRDS(replaced, paste0(data_root, "FILEPATH/who_official.rds"))



who_official <- readRDS(paste0(data_root, "FILEPATH/who_official.rds"))
who_admin    <- readRDS(paste0(data_root, "FILEPATH/who_admin.rds"))

who_official_with_hotfixes <- make_hotfixes(who_official, who_admin)
saveRDS(who_official_with_hotfixes, paste0(data_root, "FILEPATH/who_official.rds"))





official_with_admin_fill <- fill_missing_official_with_admin(prepped_official_path = paste0(data_root, "FILEPATH/who_official.rds"),
                                                             prepped_admin_path    = paste0(data_root, "FILEPATH/who_admin.rds"))


saveRDS(official_with_admin_fill, paste0(data_root, "FILEPATH/who_official.rds"))






who_official <- denom.adjust(adjust_file="who_official")   


saveRDS(who_official, paste0(data_root, "FILEPATH/who_official.rds"))





who.reported <- prep.reported.stockouts(vacc.stock.ind)

add.stockouts <- fread(stockout.add)[keep==1][, keep := NULL]
setnames(add.stockouts, "me_name", "me")

add.stockouts$me <- gsub("[0-9]+", "", add.stockouts$me)
add.stockouts <- add.stockouts[, c("ihme_loc_id", "year_id", "me"), with = FALSE]
who.reported <- rbind(who.reported, add.stockouts) %>% unique

saveRDS(who.reported, file.path(ref_data_repo, "who_reported_stockouts.rds"))




wd_copy <- getwd()
setwd(ref_data_repo)

system(SYSTEM_COMMAND)
system(SYSTEM_COMMAND)
system(SYSTEM_COMMAND)

setwd(wd_copy)
