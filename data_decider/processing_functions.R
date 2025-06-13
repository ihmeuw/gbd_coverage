



vet <- function(data, vaccine_stems_in_dataset, vaccines_all_in_dataset, age_drops) {
  
  
  
  cf_dataset <- data.table(ihme_loc_id=unique(data$ihme_loc_id))
  for (vax in vaccine_stems_in_dataset) {
    if (all(c(paste0(vax, "_card"), paste0(vax, "_dose")) %in% names(data))) {
      assign(paste0(vax, "_cf"), nrow(data[get(paste0(vax, "_card"))==1 & !is.na(get(paste0(vax, "_dose")))]) / nrow(data[!is.na(get(paste0(vax, "_dose")))]))
      cf_dataset[, (paste0(vax, "_card_fraction")) := get(paste0(vax, "_cf"))]
    }
  }
  
  card_stems_in_dataset <- paste0(vaccine_stems_in_dataset, "_card")[paste0(vaccine_stems_in_dataset, "_card") %in% names(data)]
  data[, cf_numerator   := rowSums(.SD == 1, na.rm=TRUE), .SDcols=card_stems_in_dataset]
  data[, cf_denominator := rowSums(.SD == "0" | .SD == "At least 1" | .SD == "1" | .SD == "2" | .SD == "3" | .SD == "4" | .SD == "5" |
                                     .SD == 0 | .SD == 1 | .SD == 2 | .SD == 3 | .SD == 4 | .SD == 5, na.rm=TRUE),
       .SDcols=paste0(vaccine_stems_in_dataset, "_dose")[paste0(vaccine_stems_in_dataset, "_dose") %in% names(data)]]
  cf_overall <- nrow(data[!is.na(cf_numerator) & cf_numerator > 0]) / nrow(data[!is.na(cf_denominator) & cf_denominator > 0])
  data[, c("cf_numerator", "cf_denominator") := NULL]
  
  
  
  cr_dataset <- data.table(ihme_loc_id=unique(data$ihme_loc_id))
  for (vax in vaccine_stems_in_dataset) {
    if (all(c(paste0(vax, "_dose_from_card"), paste0(vax, "_dose_from_recall")) %in% names(data))) {
      assign(paste0(vax, "_cr"),
             nrow(data[get(paste0(vax, "_dose_from_card"))==get(paste0(vax, "_dose")) & !is.na(get(paste0(vax, "_dose")))]) /
               nrow(data[!is.na(get(paste0(vax, "_dose")))]))
      cr_dataset[, (paste0(vax, "_proportion_from_card")) := get(paste0(vax, "_cr"))]
    }
  }
  
  
  
  missingness_dataset <- data.table(ihme_loc_id = unique(data$ihme_loc_id))
  for (vax in vaccine_stems_in_dataset) {
    if (paste0(vax, "_dose") %in% names(data)) {
      assign(paste0("missingness_", vax), nrow(data[is.na(get(paste0(vax, "_dose")))]) / nrow(data))
      missingness_dataset[, (paste0("missingness_", vax)) := get(paste0("missingness_", vax))]
    }
  }
  
  data[, missingness := rowMeans(.SD == "0" | .SD == "At least 1" | .SD == "1" | .SD == "2" | .SD == "3" | .SD == "4" | .SD == "5" |
                                   .SD == 0 | .SD == 1 | .SD == 2 | .SD == 3 | .SD == 4 | .SD == 5, na.rm=TRUE),
       .SDcols=paste0(vaccine_stems_in_dataset, "_dose")[paste0(vaccine_stems_in_dataset, "_dose") %in% names(data)]]
  missingness_overall <- nrow(data[is.na(missingness) | is.nan(missingness)]) / nrow(data)
  data[, missingness := NULL]
  
  data[!is.na(age_year), missingness_noage := rowMeans(.SD == "0" | .SD == "At least 1" | .SD == "1" | .SD == "2" | .SD == "3" | .SD == "4" | .SD == "5" |
                                                         .SD == 0 | .SD == 1 | .SD == 2 | .SD == 3 | .SD == 4 | .SD == 5, na.rm=TRUE),
       .SDcols=paste0(vaccine_stems_in_dataset, "_dose")[paste0(vaccine_stems_in_dataset, "_dose") %in% names(data)]]
  missingness_overall_noage <- nrow(data[is.na(missingness_noage) | is.nan(missingness_noage)]) / nrow(data)
  data[, missingness_noage := NULL]
  
  if ("latitude" %in% names(data)) missingness_lat <- nrow(data[is.na(latitude)]) / nrow(data) else missingness_lat <- 1
  if ("longitude" %in% names(data)) missingness_long <- nrow(data[is.na(longitude)]) / nrow(data) else missingness_long <- 1
  if ("geospatial_id" %in% names(data)) missingness_geo <- nrow(data[is.na(geospatial_id)]) / nrow(data)
  if ("geo_id" %in% names(data)) missingness_geo <- nrow(data[is.na(geo_id)]) / nrow(data)
  if ((!"geo_id" %in% names(data) & (!"geospatial_id" %in% names(data)))) missingness_geo <- 1
  
  if ("psu" %in% names(data)) missingness_psu <- nrow(data[is.na(psu)]) / nrow(data) else missingness_psu <- 1
  if ("strata" %in% names(data)) missingness_strata <- nrow(data[is.na(strata)]) / nrow(data) else missingness_strata <- 1
  if ("pweight" %in% names(data)) missingness_pweight <- nrow(data[is.na(pweight)]) / nrow(data) else missingness_pweight <- 1
  if ("pweight_admin_1" %in% names(data)) missingness_pweight_admin_1 <- nrow(data[is.na(pweight_admin_1)]) / nrow(data) else missingness_pweight_admin_1 <- NA_integer_
  if ("pweight_admin_2" %in% names(data)) missingness_pweight_admin_2 <- nrow(data[is.na(pweight_admin_2)]) / nrow(data) else missingness_pweight_admin_2 <- NA_integer_
  if ("pweight_admin_3" %in% names(data)) missingness_pweight_admin_3 <- nrow(data[is.na(pweight_admin_3)]) / nrow(data) else missingness_pweight_admin_3 <- NA_integer_
  
  
  vet_log   <- data.table("nid"                     = unique(data$nid),
                          "datestamp"               = format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d %H:%M"),
                          "ihme_loc_id"             = unique(data$ihme_loc_id),
                          "survey_name"             = unique(data$survey_name),
                          "year_start"              = unique(data$year_start),
                          "year_end"                = unique(data$year_end),
                          
                          "sample_size"             = nrow(data),
                          "included_vaccines"       = paste(vaccines_all_in_dataset, collapse=", "),
                          "latitude"                = ifelse("latitude" %in% names(data), 1, 0),
                          "longitude"               = ifelse("longitude" %in% names(data), 1, 0),
                          "card_fraction"           = cf_overall,
                          "missingness_vax"         = missingness_overall,
                          "missingness_vax_noage"   = missingness_overall_noage,
                          "missingness_age"         = nrow(data[is.na(age_year)]) / nrow(data),
                          "exclusions_age"          = age_drops,
                          "missingness_lat"         = missingness_lat,
                          "missingness_long"        = missingness_long,
                          "missingness_strata"      = missingness_strata,
                          "missingness_psu"         = missingness_psu,
                          "missingness_pweight"     = missingness_pweight,
                          "missingness_pweight_ad1" = missingness_pweight_admin_1,
                          "missingness_pweight_ad2" = missingness_pweight_admin_2,
                          "missingness_pweight_ad3" = missingness_pweight_admin_3
  )
  vet_log   <- merge(vet_log, cf_dataset, by="ihme_loc_id")
  vet_log   <- merge(vet_log, cr_dataset, by="ihme_loc_id")
  vet_log   <- merge(vet_log, missingness_dataset, by="ihme_loc_id")
  
  
  write.csv(vet_log, file.path(extraction_root, "log/details", paste0(nid, ".csv")), row.names=FALSE)
  
}


datestamp <- function(date) {
  
  
  date_extraction_root <- file.path(extraction_root, "log/datestamp", date)
  if (!dir.exists(date_extraction_root)) dir.create(date_extraction_root, recursive=TRUE)
  cat(paste0("\n", nid, "\n"), file=file.path(date_extraction_root, paste0(nid, ".txt")), append=TRUE)
  
}


load_nid <- function(nid, folder) {
  
  
  if (folder %in% c("raw")) {
    folder_path <- file.path(extraction_root, "00_raw")
  } else if (folder %in% "processed") {
    folder_path <- file.path(extraction_root, "01_processed")
  } else if (folder %in% c("tabulated")) {
    folder_path <- file.path(extraction_root, "02_tabulated/lbd")
  } else {
    stop("load_nid(nid, folder), requires 'folder' argument of 'raw', 'processed', or 'tabulated'")
  }
  
  
  if (folder == "raw") {
    raw_files  <- list.files(folder_path, full.names = TRUE)
    file       <- raw_files[grepl(paste0("_", nid, ".csv"), raw_files)]
  } else if (folder %in% c("processed", "tabulated")) {
    file       <- paste0(folder_path, "/", nid, ".csv")
  } 
  
  
  if (length(file) < 1) {
    message("||---- File not found")
    cat("\nnidnotexist", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    stop(paste0("No files for NID '", nid, "' exist in '", folder_path, "'"))
  }
  
  
  message(paste0("||---- Reading: ", paste(file, collapse=", \n                ")))
  data <- lapply(file, fread) %>% rbindlist(., fill=TRUE)
  
  
  return(data)
}



check_completeness <- function(data, cols, date, nid, team) {
  
  
  if (all(cols %in% names(data))) {
    if (length(vaccines[vaccines %in% names(data)]) > 0) {
      check <- TRUE
    } else {
      cat("\nnovaxind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
      message("Missing at least one vaccine indicator; not moving on to tabulation")
      check <- FALSE
    }
  } else {
    cat("\nmissingind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    if (team=="lbd" & !"geo_id" %in% names(data) & !"geospatial_id" %in% names(data)) cat("\nmissinggeoind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    if (!"year_id" %in% names(data)) cat("\nmissingyearid", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    message(paste0("Missing ", paste(cols[!cols %in% names(data)], collapse=", "), " from dataset; not moving on to tabulation"))
    check <- FALSE
  }
  
  
  if ("age_year" %in% names(data)) { if (nrow(data[!is.na(age_year)]) == 0) {
    cat("\nnoageind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    message(paste0("Missing age data from all observations in dataset; not moving on to tabulation"))
    check <- FALSE
  } }
  
  
  return(check)
  
}


check_and_rename_col <- function(data, old_name, new_name, quiet=TRUE) {
  
  if (old_name %in% names(data)) {
    setnames(data, old_name, new_name)
    if (!quiet) message(paste0("Renamed ", old_name, " to ", new_name))
  }
  else {
    if (!quiet) message(paste0("No column named ", old_name, " to rename"))
  }
  
}


make_full_coverage <- function(data, targets) {
  
  
  if (all(indicators %in% names(data))) {
    data[eval(parse(text=paste(paste0("!is.na(", indicators, ")"), collapse=" & "))),
         full := ifelse(eval(parse(text=paste(paste0(indicators, "==1"), collapse=" & "))), 1, 0)]
    data[eval(parse(text=paste(paste0("!is.na(", indicators, ")"), collapse=" & "))),
         paste(indicators, collapse="_") := ifelse(eval(parse(text=paste(paste0(indicators, "==1"), collapse=" & "))), 1, 0)]
    min_age <- max(targets[vaccine %in% indicators, age_cohort])
    data[age_year < min_age, full := NA_integer_]
  }
  
  
  if (all(indicators_no_mcv2 %in% names(data))) {
    data[eval(parse(text=paste(paste0("!is.na(", indicators_no_mcv2, ")"), collapse=" & "))),
         paste(indicators_no_mcv2, collapse="_") := ifelse(eval(parse(text=paste(paste0(indicators_no_mcv2, "==1"), collapse=" & "))), 1, 0)]
    min_age <- max(targets[vaccine %in% indicators_no_mcv2, age_cohort])
    data[age_year < min_age, full := NA_integer_]
  }
  
  
  list_indicators <- c("three_indicators", "four_indicators_1", "four_indicators_2", "four_indicators_3", "four_indicators_4")
  for (list_ in list_indicators) {
    if (all(get(list_) %in% names(data))) {
      data[eval(parse(text=paste(paste0("!is.na(", get(list_), ")"), collapse=" & "))),
           paste(get(list_), collapse="_") := ifelse(eval(parse(text=paste(paste0(get(list_), "==1"), collapse=" & "))), 1, 0)]
      min_age <- max(targets[vaccine %in% get(list_), age_cohort])
      data[age_year < min_age, paste(get(list_), collapse="_") := NA_integer_]
    }
  }
  
  
  for (probability in probabilities_to_model) {
    
    all    <- strsplit(gsub("_", " ", probability), split=" +")[[1]]
    root   <- gsub("\\s*|_.*", "", probability)
    others <- all[all != root]
    
    if (all(all %in% names(data))) {
      data[eval(parse(text=paste(paste0("!is.na(", all, ")"), collapse=" & "))) &
             eval(parse(text=paste(paste0(others, "==1"), collapse=" & "))),
           (paste0("correlation_", probability)) := ifelse(get(root)==1, 1, 0)]
      
      min_age <- max(targets[vaccine %in% all, age_cohort])
      data[age_year < min_age, (paste0("correlation_", probability)) := NA_integer_]
    }
  }
  
  
  return(data)
  
}


make_rotac <- function(data) {
  
  
  doses <- readRDS("FILEPATH/vaccine_schedule.rds")[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), .(doses)]
  intro <- readRDS("FILEPATH/vaccine_intro.rds")[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), .(cv_intro)] %>% unique
  
  
  if (nrow(doses)==0) doses <- 0
  doses <- ifelse(doses >= 3, 3, 2)
  rota_dose <- paste0("rota", doses)
  
  
  if (nrow(intro)==0) stop(paste0("BREAK | Missing introduction year for ", unique(data$ihme_loc_id), "; need to prep introduction frame for this geography before continuing"))
  if (intro < 9999 & rota_dose %in% names(data)) data$rotac <- data[, rota_dose, with=FALSE]
  
  
  return(data)
  
}


set_target_ages <- function(data, vaccines.=vaccines) {
  
  
  schedule <- readRDS("FILEPATH/vaccine_target.rds")
  schedule_antigens <- unique(schedule$me_name)
  schedule <- schedule[ihme_loc_id %in% unique(data$ihme_loc_id)]
  
  
  vax_targets <- data.table()
  for (vax_target in paste0("vacc_", vaccines.)) {
    assign(paste0(vax_target, "_target"),
           ifelse(vax_target %in% schedule_antigens,
                  ifelse(vax_target %in% unique(schedule$me_name), unique(schedule[me_name==vax_target, age_cohort]), NA),
                  1))
    vax_targets <- rbind(vax_targets, data.table(me_name=vax_target, age_cohort=get(paste0(vax_target, "_target"))))
  }
  
  
  
  
  return(vax_targets)
  
}


prep_for_tabulation <- function(nid, team="gbd", vaccines.=vaccines, filter_table=NA) {
  
  
  data <- load_nid(nid, folder="processed")
  
  
  
  vax_targets <- set_target_ages(data=data)[, vaccine := gsub("vacc_", "", me_name)]
  if (team=="gbd") data <- make_full_coverage(data, targets=vax_targets)
  
  
  
  
  
  if (team=="gbd") {
    for (ea in c(vaccines, "rotac")) {
      
      
      if (ea %in% names(data)) data[exists(paste0(gsub("[0-9]", "", ea), "_card"))==1, (paste0(ea, "_CARD")) := get(ea)]
      if (ea %in% names(data)) data[exists(paste0(gsub("[0-9]", "", ea), "_card"))==0 & !is.na(get(ea)), (paste0(ea, "_RECALL")) := get(ea)]
    }
  }
  
  
  if (lbd_only) datestamp(date=date)
  
  
  tab_cols <- c("nid", "survey_name", "survey_module", "file_path", "ihme_loc_id", "year_start", "year_end", "age_year")
  gbd_cols <- c("strata", "psu", paste0("pweight", c("_admin_1", "_admin_2", "_admin_3", "_admin_4", "_admin_5")), paste0("admin_", 1:5, "_id"))
  lbd_cols <- c("geospatial_id", "strata", "psu", "pweight", paste0("pweight", c("_admin_1", "_admin_2", "_admin_3", "_admin_4", "_admin_5")))
  if (team=="lbd") tab_cols <- c(tab_cols, "geospatial_id") %>% unique
  if (team=="gbd") tab_cols <- c(tab_cols, "pweight") %>% unique
  check <- check_completeness(data, cols=tab_cols, date=date, nid=nid, team=team)
  
  if (check) {
    
    
    parent <- unique(data$ihme_loc_id)
    if (team=="gbd" & "admin_1_urban_id" %in% names(data) & "pweight_admin_1" %in% names(data)) {
      data_ur <- copy(data)[!is.na(admin_1_urban_id)][, ihme_loc_id := admin_1_urban_id]
      data_ur[, pweight := pweight_admin_1]
      data_ur[, c("admin_1_urban_id", "pweight_admin_1", "admin_1_id", "admin_2_id", "pweight_admin_2", "admin_3_id", "pweight_admin_3") := NULL]
      data_ur[ihme_loc_id==parent, ihme_loc_id := NA_character_]
      data <- rbind(data, data_ur[!is.na(ihme_loc_id)], fill=TRUE)
    }
    
    
    if (team=="gbd" & "admin_2_id" %in% names(data) & "pweight_admin_2" %in% names(data)) {
      data_a2 <- copy(data)[!is.na(admin_2_id)][, ihme_loc_id := admin_2_id]
      data_a2[, pweight := pweight_admin_2]
      data_a2[, c("admin_1_urban_id", "pweight_admin_1", "admin_1_id", "admin_2_id", "pweight_admin_2", "admin_3_id", "pweight_admin_3") := NULL]
      data_a2[ihme_loc_id==parent, ihme_loc_id := NA_character_]
      data <- rbind(data, data_a2[!is.na(ihme_loc_id)], fill=TRUE)
    }
    
    
    if (team=="gbd" & "admin_3_id" %in% names(data) & "pweight_admin_3" %in% names(data)) {
      data_a3 <- copy(data)[!is.na(admin_3_id)][, ihme_loc_id := admin_3_id]
      data_a3[, pweight := pweight_admin_3]
      data_a3[, c("admin_1_urban_id", "pweight_admin_1", "admin_1_id", "admin_2_id", "pweight_admin_2", "admin_3_id", "pweight_admin_3") := NULL]
      data_a3[ihme_loc_id==parent, ihme_loc_id := NA_character_]
      data <- rbind(data, data_a3[!is.na(ihme_loc_id)], fill=TRUE)
    }
    
    
    if (team=="lbd") {
      for (vax_stem_col in unique(gsub("[0-9]", "", vaccines))) {
        if (paste0(vax_stem_col, "_dose") %in% names(data) & !vax_stem_col %in% c("mcv", "mmr", "rcv")) {
          vax_dose_cols <- vaccines[grep(vax_stem_col, vaccines)]
          if (all(vax_dose_cols %in% names(data))) {
            data[eval(parse(text=paste(paste0("is.na(", vax_dose_cols, ")"), collapse=" | "))), (vax_dose_cols) := NA]
          } else {
            data[, vax_dose_cols[vax_dose_cols %in% names(data)] := NA]
          }
        }
      }
      
      
      
      
      
      
      
      
      
      
    }
    
    
    cols       <- c(tab_cols, get(paste0(team, "_cols")))[c(tab_cols, get(paste0(team, "_cols"))) %in% names(data)] %>% unique
    vax_cols   <- vaccines.[vaccines. %in% names(data)]
    ready_data <- data[, c(cols, vax_cols), with=FALSE]
    ready_data[, individual := row_number(nid)]
    ready_data <- melt.data.table(ready_data, id.vars=cols, measure.vars=vax_cols, variable.name="me_name")
    ready_data[, me_name := paste0("vacc_", me_name)]
    
    
    
    ready_data <- ready_data[!is.na(value)]
    
    
    
    vax_targets <- set_target_ages(ready_data, vaccines.=vaccines.)
    ready <- merge(ready_data, vax_targets, by="me_name", all.x=TRUE)
    ready[, cohort := ifelse(age_year >= age_cohort, age_year - age_cohort, NA_integer_)]
    ready[, year_id := floor( (year_start + year_end) / 2) - cohort - 1]
    ready <- ready[!is.na(year_id)]
    ready[, c("age_cohort", "cohort") := NULL]
    message("Birth cohort assignment complete")
    
    
    
    return(list(check, ready))
    
  } else {
    
    
    return(list(check))
    
  }
  
}


check_nids <- function(nid) {
  
  
  if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "processed")))     check_process <- "Success" else check_process <- "Failure"
  if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "tabulated/gbd"))) check_gbd     <- "Success" else check_gbd     <- "Failure"
  if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "tabulated/lbd"))) check_lbd     <- "Success" else check_lbd     <- "Failure"
  if (lbd_only) { check_process <- "Not launched (LBD only)"; check_gbd <- "Not launched (LBD only)" }
  if (gbd_only) { check_lbd <- "Not launched (GBD only)" }
  
  
  note <- readLines(file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")))
  
  
  reason <- ""
  if (check_process=="Failure") {
    if (any(grepl("abc", note)))           reason <- paste(reason, sep=ifelse(nchar(reason) >= 1, "; ", ""))
  }
  if (check_gbd=="Failure" | check_lbd=="Failure") {
    if (any(grepl("nidnotexist", note)))   reason <- paste(reason, "This NID doesn't exist FILEPATH", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("novaxind", note)))      reason <- paste(reason, "No vaccine indicators found in dataset", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("noageind", note)))      reason <- paste(reason, "All observations in dataset missing age information", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("missingind", note)))    reason <- paste(reason, "Dataset entirely missing at least one required survey design variable", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("missinggeoind", note))) reason <- paste(reason, "Dataset entirely missing geospatial_id", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("missingyearid", note))) reason <- paste(reason, "Dataset entirely missing year_id (likely because no children with non-missing age older than target age range)", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("DataPre2000", note)))   reason <- paste(reason, "Dataset pre-2000 (skipping LBD steps)", sep=ifelse(nchar(reason) >= 1, "; ", ""))
  }
  
  
  log <- fread(file.path(extraction_root, "FILEPATH/log.csv"))
  nid_log <- data.table(nid=nid, checked=format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d %H:%M"),
                        check_process=check_process, check_gbd=check_gbd, check_lbd=check_lbd, notes=reason)
  write.csv(rbind(log, nid_log, fill=TRUE), file.path(extraction_root, "FILEPATH/log.csv"), row.names=FALSE)
  
  
  message(paste0("|| NID ", nid, " ||"))
  message(paste0("|| Processing:     ", check_process, " ||"))
  message(paste0("|| GBD tabulation: ", check_gbd, " ||"))
  message(paste0("|| LBD tabulation: ", check_lbd, " ||"))
  
}


clean_up <- function(nid, lbd_only=FALSE, gbd_only=FALSE) {
  
  
  if (!lbd_only) if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "processed")))     system(paste0(SYSTEM_COMMAND))))
  if (!lbd_only) if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "tabulated/gbd"))) system(paste0(SYSTEM_COMMAND))))
  if (!gbd_only) if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "tabulated/lbd"))) system(paste0(SYSTEM_COMMAND))))
  if (!lbd_only) if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "log/details")))   system(paste0(SYSTEM_COMMAND))))
  
}




create_filter_table <- function(){
  filter_table <- data.table(survey_id = 0, total = 0, missing_indicator = FALSE, 
                             n_with_age_data = 0, n_without_age_data  = 0,
                             n_12_59_months  = 0, n_outside_age_range = 0,
                             n_with_mcv_data = 0, n_without_mcv_data  = 0,
                             survey_in_year_range = TRUE,
                             n_resampled   = 0, n_not_resampled   = 0,
                             n_after_2000  = 0, n_before_2000     = 0,
                             outlier   = FALSE, n_after_outlier   = 0,
                             gps_clusters  = 0, polygon_clusters  = 0,
                             n_gps_located = 0, n_polygon_located = 0)
  return(filter_table)
}

load_filter_table <- function(){
  all_files = list.files(filter_table_path)
  if(paste0(nid, ".csv") %in% all_files){
    filter_table <- fread(paste0(filter_table_path, nid, ".csv"))
  } else {
    message('Filter table not present in file path. Make sure the process script was run while "record_data_drops" is TRUE')
    record_data_drop <= FALSE
    return(NULL)
  }
  return(filter_table)
}



















path.clean <- function(path) {
  
  os <- .Platform$OS.type
  if (os == "windows") {
    j <- "J:/"
    h <- "H:/"
    path <- gsub("FILEPATH", j, path)
    path <- gsub(paste0("FILEPATH", Sys.info()[["user"]], "/"), h, path)
  } else {
    j <- "FILEPATH"
    h <- paste0("FILEPATH", Sys.info()[["user"]], "/")
    path <- gsub("J:/", j, path)
    path <- gsub("H:/", h, path)
  }
  return(path)
  
}


load.file <- function(path) {
  
  path <- path.clean(path)
  ext <- strsplit(basename(path), "[.]")[[1]][2] %>% tolower
  if (ext=="csv") {
    df <- fread(path)
  } else if (ext %in% c("dta", "tmp")) {
    df <- try(read_dta(path))
    if ('try-error' %in% class(df)) df <- readstata13(path)
  } else stop("Path not accepted")
  return(data.table(df))
  
}


load.settings <- function(path, topic=NA) {
  
  df <- load.file(path)
  
  if (!is.na(topic)) {
    tsub <- topic
    df <- df[topic==tsub]
    if (nrow(df) == 0 | nrow(df) > 1) stop("BREAK || ", nrow(df), " rows in config file with topic (", topic, ")")
  }
  settings <- lapply(names(df), function(n) {
    
    i <- df[[n]]
    if (n=="custom_age_cuts") {
      if (!is.na(i) & !is.null(i) & i != "") i <- unlist(strsplit(gsub(" ", "", i), ",")) %>% as.numeric
    } else if (grepl("root", n)) {
      if (grepl("J:/", i)) i <- gsub("J:/", j, i)
    } else if (n=="cond_age_cuts") {
      if (!is.na(i) & !is.null(i) & i != "") i <- eval(parse(text=gsub("\"\"", "\"", i)))
    } else if (grepl(",", i)) {
      if (!is.na(i) & !is.null(i) & i != "") i <- unlist(strsplit(gsub(" ", "", i), ","))
    } else if (length(i) == 1L) {
      if (!is.na(i) & !is.null(i) & i != ""){
        if (i=="NULL") i <- NULL
      }
    }
    return(i)
  })
  names(settings) <- names(df)
  return(settings)
  
}


no.ext <- function(path) {
  strsplit(basename(path), "[.]")[[1]][1]
}


setup.design <- function(df, var) {
  
  
  
  options(survey.lonely.psu='adjust')
  
  
  options(survey.adjust.domain.lonely=TRUE)
  
  
  check_list <- c("strata", "psu", "pweight")
  for (i in check_list) {
    
    assign(paste0(i, "_formula"),
           ifelse(i %in% names(df) & nrow(df[!is.na(i)]) > 0, paste("~", i), NULL) %>% as.formula
    )
  }
  
  
  return(svydesign(id=psu_formula, weight=pweight_formula, strat=strata_formula, data=df[!is.na(var)], nest=TRUE))
  
}



reweight_for_subnat_collapse <- function(df, by_vars) {
  
  weights_below_admin0 <- grep("pweight_admin_.", names(df), value=TRUE)
  cols_na <- sapply(df, function(x)all(is.na(x)))
  na_cols <- names(df)[cols_na]
  non_na_admin_wts <- weights_below_admin0[! weights_below_admin0 %in% na_cols]
  
  if (length(non_na_admin_wts) > 0){
    
    admin_ids <- grep(by_vars, pattern='admin_.*id', value=TRUE)
    admins_w_subnat_info <- substring(admin_ids, 1, 7)
    correct_pweight_for_collapse <- paste0('pweight_', admins_w_subnat_info)
    
    
    if (correct_pweight_for_collapse != 'pweight_') {
      df[, pweight := get(correct_pweight_for_collapse)]
    }
  }
  
  return(df)
  
}


collapse.by <- function(df, var, by_vars, calc.sd = FALSE) {
  
  
  df.c <- df[!is.na(get(var)) & !is.na(strata) & !is.na(psu) & !is.na(pweight)] %>% copy
  df <- reweight_for_subnat_collapse(df, by_vars)
  
  
  design <- setup.design(df, var)
  
  
  by_formula <- as.formula(paste0("~", paste(by_vars, collapse="+")))
  
  
  meta <- df[, list(sample_size = length(which(!is.na(get(var)))),
                    nclust      = length(unique(psu)),
                    nstrata     = length(unique(strata)),
                    var         = var
  ), by          = by_vars]
  
  
  est <- svyby(~get(var), by_formula, svymean, design=design, deff="replace", na.rm=TRUE, drop.empty.groups=TRUE, keep.names=FALSE, multicore=TRUE) %>% data.table
  old <- c("get(var)", "DEff.get(var)", "se")
  new <- c("mean", "design_effect", "standard_error")
  setnames(est, old, new)
  
  
  out <- merge(meta, est, by=by_vars)
  
  
  if (calc.sd) {
    stdev <- svyby(~get(var), by_formula, svyvar, design=design, deff="replace", na.rm=TRUE, drop.empty.groups=TRUE, keep.names=FALSE, multicore=TRUE) %>% data.table
    setnames(stdev, "get(var)", "variance")
    stdev <- stdev[, standard_deviation := sqrt(variance)]
    
    stdev <- stdev[, standard_deviation_se := 1 / (2 * standard_deviation) * se]
    stdev <- stdev[, c("se", "variance") := NULL]
    
    out <- merge(out, stdev, by=by_vars)
  }
  
  
  return(out)
  
}


nonmiss <- function(df, vars, reverse=FALSE) {
  
  vars.sub <- intersect(vars, names(df))
  i <- lapply(vars.sub, function(x) !all(is.na(df[[x]]))) %>% unlist
  if (reverse) return(setdiff(vars, vars.sub[i]))
  else return(vars.sub[i])
  
}


categ_to_bin <- function(df, var) {
  
  vals <- df[!is.na(get(var))][[var]] %>% unique
  for (x in vals) {
    col <- paste0(var, "_", x)
    df <- df[!is.na(get(var)), (col) := ifelse(get(var) == x, 1, 0)]
  }
  newcols <- paste0(var, "_", vals)
  
  
  return(list(df=df, cols=newcols))
  
}


cut.ages <- function(df, var, cut, subset=NA) {
  
  if (is.na(subset)) subset.cmd <- "1==1"
  index <- findInterval(df[eval(parse(text=subset))][[var]], cut, right=FALSE)
  df <- df[eval(parse(text=subset)), `:=` (age_start =  cut[index], age_end = cut[index+1])]
  return(df)
  
}

update_granular_pweights <- function(pweight_colname, df) {
  
  ntl_pweight_not_na <- any(!is.na(df$pweight))
  sub_pweight_na <- any(is.na(df[, get(pweight_colname)]))
  if (ntl_pweight_not_na & sub_pweight_na){
    df <- df[, (pweight_colname) := pweight]
  }
  return(df)
  
}



collapse.subset <- function(df, var, by_vars, design_vars) {
  
  
  cols <- c(var, by_vars, design_vars)
  n.before <- nrow(df)
  for (i in cols){
    if (grepl(x=i, pattern='pweight_')){
      
      df <- update_granular_pweights(pweight_colname=i, df)
    }
    df <- df[!is.na(get(i))]
  }
  n.dropped <- n.before - nrow(df)
  
  
  df <- df[, lonely := lapply(.SD, length), .SDcols=var, by=by_vars]
  df <- df[lonely != 1]
  df <- df[, lonely := NULL]
  
  
  return(list(data=df, dropped=n.dropped))
  
}


clean.subnat <- function(df, vars.subnat) {
  
  
  rename <- intersect(vars.subnat, names(df))
  if (length(rename) > 0) {
    setnames(df, rename, rep("ihme_loc_id", length(rename)))
    if ("ihme_loc_id" %in% names(df)) df <- df[, ihme_loc_id := NULL]
  }
  return(df)
  
}




collapse.run <- function(df, config, quiet=TRUE, cores=1) {
  
  
  config.default <- list(
    
    
    vars             = c("var"),        
    vars.categ       = NULL,            
    calc.sd          = FALSE,           
    
    cv.manual        = NULL,            
    cv.detect        = TRUE,            
    
    by_sex           = TRUE,            
    by_age           = TRUE,            
    gbd_age_cuts     = TRUE,            
    aggregate_under1 = TRUE,            
    custom_age_cuts  = NULL,            
    cond_age_cuts    = NULL,            
    
    sample_threshold = NA,              
    
    vars.meta        = c(               
      "nid", "survey_name",       
      "ihme_loc_id", "year_start",
      "year_end", "survey_module",
      "file_path"
    ),
    
    vars.subnat      = c(               
      "admin_1_id", 
      "admin_1_urban_id", 
      "admin_2_id",
      "admin_3_id",
      "admin_4_id",
      "admin_5_id"),  
    
    vars.sex         = c("sex_id"),     
    
    vars.age         = c("age_year"),   
    
    
    census_data      = FALSE, 
    
    vars.design      = c(               
      "strata", "psu", "pweight", "pweight_admin_1", "pweight_admin_2", "pweight_admin_3"
    )
  )
  
  
  if (!is.null(config)) lapply(names(config), function(x) assign(x, config[[x]], pos=1))
  
  
  lapply(names(config.default), function(x) if (!exists(x)) assign(x, config.default[[x]], pos=1))
  
  
  
  
  if (!is.null(vars.categ)) if (!is.na(vars.categ) & vars.categ != "") {
    for (var in vars.categ) {
      ctb <- categ_to_bin(df, var)
      df <- ctb[['df']]
      new.vars <- ctb[['cols']]
      vars <- c(vars, new.vars)
    }
  }
  
  vars.check <- nonmiss(df, vars)
  if (length(vars.check) < 1L) stop(paste0("BREAK || All vars (", toString(vars), ") are either not in dataset or completely missing"))
  vars <- vars.check
  
  
  vars.stratify <- vars.meta
  if (by_sex) vars.stratify <- c(vars.stratify, vars.sex)
  if (by_age) vars.stratify <- c(vars.stratify, "age_start", "age_end")
  
  if (cv.detect) {
    vars.cv <- grep("^cv_", names(df), value=TRUE)
    if (length(vars.cv) > 0) {
      vars.cv <- nonmiss(df, vars.cv)
      vars.stratify <- c(vars.stratify, vars.cv)
    }
  }
  
  
  if (!is.null(cv.manual)) {
    vars.cv <- nonmiss(df, cv.manual)
    vars.stratify <- c(vars.stratify, vars.cv)
  }
  
  
  vars.subnat <- intersect(vars.subnat, names(df))
  vars.subnat <- nonmiss(df, vars.subnat)
  if (length(vars.subnat) != 0L) {
    vars.stratify <- lapply(c("ihme_loc_id", vars.subnat), function(x) c(vars.stratify, x) %>% unique)
  } else {
    vars.stratify <- list(vars.stratify)
  }
  
  for (var.subnat in vars.subnat) {
    df[get(var.subnat)==get("ihme_loc_id"), (var.subnat) := NA_character_]
  }
  
  
  
  
  
  vars.design.missing <- nonmiss(df, vars.design, reverse=TRUE)
  subnat_pweights <- c("pweight_admin_1", "pweight_admin_2", "pweight_admin_3")
  if (length(vars.design.missing) != 0L) {
    df <- df[, (vars.design.missing) := 1]
    absent_subnat_pweights <- subnat_pweights[subnat_pweights %in% vars.design.missing]
    df <- df[, (absent_subnat_pweights) := NA]
  }
  
  
  for (var in vars.design) {
    if (class(df[[var]]) != "numeric") {
      df[[var]] <- as.numeric(df[[var]])
      if (!quiet) print(paste0("ubCov Collapse || Setup || Coercing ", var, " to numeric"))
    }
  }
  
  
  n <- nrow(df[pweight==0])
  if (n > 0) {
    df <- df[pweight != 0]
    if (!quiet) print(paste0("ubCov Collapse || Setup || Dropping ", n, " rows with pweight == 0"))
  }
  
  
  errors <- list(missing_design=vars.design.missing)
  
  
  
  cf.list <- expand.grid(vars=vars, vars.stratify=vars.stratify)
  
  
  cf <- mclapply(1:nrow(cf.list), function(i) {
    
    vars <- cf.list$vars[i] %>% as.character
    vars.stratify <- cf.list$vars.stratify[i] %>% unlist
    
    if (!quiet) print(paste0("ubCov Collapse || Collapsing (", vars, ") by (", toString(vars.stratify), ")"))
    
    sf <- collapse.subset(df, var=vars, by_vars=vars.stratify, design_vars=vars.design)
    missing <- sf[['missing']]
    sf <- sf[['data']]
    
    if (nrow(sf) > 0) {
      
      if (census_data) sf.out <- census.collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      if (!census_data) sf.out <- collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      
      sf.out <- clean.subnat(sf.out, vars.subnat)
      sf.out <- rbind(sf.out, missing=missing)
      return(sf.out)
    } else {
      if (!quiet) print(paste0("ubCov Collapse || SKIPPING || Variable (", vars, ") has no observations post subset with (", toString(vars.stratify), ")"))
      return(NULL)
    }
  }, mc.cores=cores) %>% rbindlist(., use.names=TRUE, fill=TRUE)
  
  
  
  if (!is.na(sample_threshold)) cf <- cf[sample_size >= sample_threshold]
  
  
  if (nrow(cf[mean %in% c(0,1)]) > 0) {
    cf.w <- cf[mean %in% c(0,1)]
    sample_size <- cf.w$sample_size
    n <- ifelse(cf.w$mean==0, 0, sample_size)
    ci <- binom.confint(n, sample_size, conf.level = 0.95, methods = "wilson")
    se <- (ci$upper - ci$lower)/3.92
    se <- se * sqrt(2.25) 
    cf[mean %in% c(0,1)]$standard_error <- se
  }
  
  
  
  if ("strata" %in% vars.design.missing & !census_data) {
    cf <- cf[design_effect < 2.25, inflate := 2.25/design_effect]
    cf <- cf[design_effect < 2.25, standard_error := standard_error * sqrt(inflate)]
    cf <- cf[, inflate := NULL]
    cf <- cf[design_effect < 2.25, design_effect := 2.25]
  }
  
  
  return(cf)
  
}




vaccines <- c(paste0("dpt", 1:3),
              paste0("pent", 1:3),
              paste0("tetra", 1:3),
              paste0("polio", 1:3),
              paste0("hepb", 1:3),
              paste0("hib", 1:3),
              paste0("pcv", 1:3),
              paste0("rota", 1:3),
              paste0("mcv", 1:2),
              paste0("mmr", 1:2),
              paste0("rcv", 1:2),
              "bcg",
              "yfv")


date <- Sys.Date()
indicators         <- c("dpt3", "mcv1", "polio3", "mcv2", "hib3", "hepb3", "pcv3", "rotac")
indicators_no_mcv2 <- c("dpt3", "mcv1", "polio3", "hib3", "hepb3", "pcv3", "rotac")
three_indicators   <- c("dpt3", "polio3", "mcv1")
four_indicators_1  <- c("hepb3", three_indicators)
four_indicators_2  <- c("hib3", three_indicators)
four_indicators_3  <- c("pcv3", three_indicators)
four_indicators_4  <- c("rotac", three_indicators)


double <- c("mcv1_dpt3", "mcv2_mcv1", "hepb3_dpt3", "hib3_dpt3", "pcv3_dpt3", "rotac_dpt3")
triple <- c("polio3_mcv1_dpt3")
new    <- c("mcv2", "hepb3", "rotac", "pcv3", "hib3")
quad   <- c(paste0(new, "_polio3_mcv1_dpt3"))
probabilities_to_model <- c(double, triple, quad,
                            "rotac_pcv3_hib3_hepb3_polio3_mcv1_dpt3",
                            "rotac_hib3_hepb3_polio3_mcv1_dpt3",
                            "pcv3_rotac_hib3_hepb3_polio3_mcv1_dpt3",
                            "hib3_rotac_pcv3_hepb3_polio3_mcv1_dpt3",
                            "hib3_hepb3_polio3_mcv1_dpt3",
                            "pcv3_hib3_hepb3_polio3_mcv1_dpt3",
                            "hepb3_rotac_pcv3_hepb3_polio3_mcv1_dpt3",
                            "mcv2_rotac_pcv3_hib3_hepb3_polio3_mcv1_dpt3",
                            "rotac_mcv2_pcv3_hib3_hepb3_polio3_mcv1_dpt3",
                            "mcv2_pcv3_hib3_hepb3_polio3_mcv1_dpt3")




process_mics <- function(nid) {
  
  
  date <- Sys.Date()

  
  data <- load_nid(nid, folder="raw")
  
  
  
  
  
  
  
  
  message("Starting vaccination dose-specific custom processing")
  
  
  
  
  
  
  
  
  current_year <- as.integer(format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y"))
  
  
  mult <- c("dpt", "pent", "tetra", "polio", "mcv", "mmr", "hepb", "hib", "pcv", "rota", "rcv")
  one  <- c("bcg", "yfv")
  
  
  vacc_dose <- fread("FILEPATH/vacc_dose.csv")
  all_stems <- unique(vacc_dose$var)
  
  
  
  vaccine_stems_in_dataset <- c()
  for (stem in all_stems) {
    max <- vacc_dose[var == stem, refdose]
    if (max > 1) possible_doses <- 1:max else possible_doses <- ""
    possible_vaccine_columns <- c(paste0("recall_", stem, "_ever"),
                                  paste0("recall_", stem, "_times"),
                                  paste0("card_", stem, possible_doses, "_dmy"),
                                  paste0("card_", stem, possible_doses, "_day"),
                                  paste0("card_", stem, possible_doses, "_month"),
                                  paste0("card_", stem, possible_doses, "_year"),
                                  paste0("card_", stem, possible_doses, "_date"),
                                  paste0("response_", stem, possible_doses))
    if (length(possible_vaccine_columns[possible_vaccine_columns %in% names(data)]) > 0) {
      vaccine_stems_in_dataset <- c(vaccine_stems_in_dataset, stem)
    }
  }
  if (length(vaccine_stems_in_dataset) == 0) {
    cat("\nnovaxind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    stop("No vaccine indicators in dataset; not moving on to data processing")
  }
  vaccines_all_in_dataset <- vaccines[grep(paste(vaccine_stems_in_dataset, collapse="|"), vaccines)]
  
  if (unique(data$survey_name) %in% c("UNICEF_MICS")){
    
    stem_list <- fread("FILEPATH/vacc_stem_lookup.csv")
    vacc_list <- c()
    for (VACC in vaccines_all_in_dataset) {
      
      stem <- subset(stem_list, vacc== VACC)
      stem <- stem$stem
      
      recall_columns <- c(paste0("recall_", stem, "_ever"),
                          paste0("recall_", stem, "_times"))
      
      card_columns <- c(paste0("card_", VACC, "_day"),
                        paste0("card_", VACC, "_date"))
      
      if (length(recall_columns[recall_columns %in% names(data)]) == 2 & length(card_columns[card_columns %in% names(data)]) > 0) {
        vacc_list <- c(vacc_list, VACC)
      }
      if(VACC %in% c("dpt1", "pcv1", "polio1", "mcv1", "bcg", "hepb1", "hib1", "rota1", "yfv", "pent1", "tetra1", "mmr1", "rcv1")){
        if (paste0("recall_", stem, "_ever") %in% names(data) & length(card_columns[card_columns %in% names(data)]) > 0) {
          vacc_list <- c(vacc_list, VACC)
        }  
      }
    }
    
    vaccines_all_in_dataset <- unique(vacc_list)
    
    if (length(vaccine_stems_in_dataset) == 0) {
      cat("\nnovaxind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
      stop("No vaccine indicators in dataset; not moving on to data processing")
    }
  }
  
  
  
  buddhist_nids <- c(12732,
                     148649,
                     296646,
                     331377)
  
  
  data[ihme_loc_id == "ALG", ihme_loc_id := "DZA"]
  data[ihme_loc_id == "PAL", ihme_loc_id := "PSE"]
  
  
  code_variables <- c(paste0("has_vacc_card_", c("seen", "not_seen", "no")),
                      paste0("recall_source_", c("card", "recall")),
                      paste0("recall_ever_", c("true", "false")),
                      "recall_times_missing",
                      paste0("card_", c("nodate", "recall", "missing", "no", "date_format")),
                      paste0("response_", c("yes_card", "yes_recall", "no", "missing")))
  
  for (code_variable in code_variables) {
    if (code_variable %in% names(data)) {
      code_variable_new_name <- paste0(code_variable, "_code")
      assign(code_variable_new_name, data[, get(code_variable)] %>% unique %>% as.character)
      if (grepl(",", as.character(data[, get(code_variable)] %>% unique)) & !grepl(", ", as.character(data[, get(code_variable)] %>% unique))) {
        assign(code_variable_new_name, strsplit(get(code_variable_new_name), split=",")[[1]]) }
      if (grepl(", ", as.character(data[, get(code_variable)] %>% unique))) assign(code_variable_new_name, strsplit(get(code_variable_new_name), split=", ")[[1]])
      if (grepl(" ", as.character(data[, get(code_variable)] %>% unique)) & !grepl(", ", as.character(data[, get(code_variable)] %>% unique))) {
        assign(code_variable_new_name, gsub(" ", "", get(code_variable_new_name))) }
      if (length(grep(">=", as.character(get(code_variable_new_name)))) > 0) {
        for (each in grep(">=", as.character(get(code_variable_new_name)))) {
          current_code <- get(code_variable_new_name)[each]
          new_code     <- gsub(">=", "", current_code) %>% gsub(" ", "", .) %>% as.integer
          new_codes    <- (new_code):999999
          assign(code_variable_new_name, get(code_variable_new_name)[-each])
          assign(code_variable_new_name, c(get(code_variable_new_name), new_codes))
        }
      }
      if (length(grep("<=", as.character(get(code_variable_new_name)))) > 0) {
        for (each in grep("<=", as.character(get(code_variable_new_name)))) {
          current_code <- get(code_variable_new_name)[each]
          new_code     <- gsub("<=", "", current_code) %>% gsub(" ", "", .) %>% as.integer
          new_codes    <- -999:(new_code)
          assign(code_variable_new_name, get(code_variable_new_name)[-each])
          assign(code_variable_new_name, c(get(code_variable_new_name), new_codes))
        }
      }
      if (length(grep(">", as.character(get(code_variable_new_name)))) > 0) {
        for (each in grep(">", as.character(get(code_variable_new_name)))) {
          current_code <- get(code_variable_new_name)[each]
          new_code     <- gsub(">", "", current_code) %>% gsub(" ", "", .) %>% as.integer
          new_codes    <- (new_code + 1):999999
          assign(code_variable_new_name, get(code_variable_new_name)[-each])
          assign(code_variable_new_name, c(get(code_variable_new_name), new_codes))
        }
      }
      if (length(grep("<", as.character(get(code_variable_new_name)))) > 0) {
        for (each in grep("<", as.character(get(code_variable_new_name)))) {
          current_code <- get(code_variable_new_name)[each]
          new_code     <- gsub("<", "", current_code) %>% gsub(" ", "", .) %>% as.integer
          new_codes    <- -999:(new_code - 1)
          assign(code_variable_new_name, get(code_variable_new_name)[-each])
          assign(code_variable_new_name, c(get(code_variable_new_name), new_codes))
        }
      }
      if (grepl("missing", code_variable)) assign(code_variable_new_name, c(get(code_variable_new_name), "NA"))
      
      assign(code_variable_new_name, as.character(get(code_variable_new_name)))
    }
  }
  
  
  if ("child_age_month" %in% names(data)) {
    if ("age_month" %in% names(data)) data[is.na(age_month) & !is.na(child_age_month), age_month := child_age_month]
    data[!is.na(child_age_month), age_month := child_age_month]
  }
  if ("child_age_year" %in% names(data)) {
    if ("age_year" %in% names(data)) data[is.na(age_year) & !is.na(child_age_year), age_year := child_age_year]
    data[!is.na(child_age_year), age_year := child_age_year]
  }
  if ("child_sex_id" %in% names(data)) {
    if ("sex_id" %in% names(data)) data[is.na(sex_id) & !is.na(child_sex_id), sex_id := child_sex_id]
    data[!is.na(child_sex_id), sex_id := child_sex_id]
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  if ("has_vacc_card" %in% names(data)) {
    
    data[, has_vacc_card := as.character(has_vacc_card)]
    for (condition in c("seen", "not_seen", "no")) {
      var <- paste0("has_vacc_card_", condition)
      if (var %in% names(data)) {
        
        data[has_vacc_card %in% get(paste0(var, "_code")), has_vacc_card := condition]
        data[, (var) := NULL]
      }
    }
    data[!has_vacc_card %in% c("seen", "not_seen", "no"), has_vacc_card := NA_character_]
  }
  
  
  for (vacc in vaccines_all_in_dataset) {
    
    
    
    
    
    
    if(unique(data$survey_name) %in% c("UNICEF_MICS")){
      
      message(paste0("|| ", vacc, " ||"))
      
      
      if (grepl(gsub("[0-9]", "", vacc), paste0(mult, collapse="|"))) {
        
        vacc_stem <- mult[grep(gsub("[0-9]", "", vacc), mult)]
        
        if (grepl("[1-9]", vacc)) {
          dose_num  <- substr(vacc, nchar(vacc), nchar(vacc)) %>% as.integer
          dose_stem <- dose_num
        }
        
      } else if (grepl(vacc, paste0(one, collapse="|"))) {
        vacc_stem <- one[grep(vacc, one)]
        dose_num  <- 1
        dose_stem <- ""
      }
      
      
      
      
      for (var in paste0(vacc_stem, c("_dose", "_card", "_ever", "_dose_from_card", "_dose_from_recall"))) {
        if(!var %in% names(data)) {
          data[, (var) := NA_integer_]
        }
      }
      
      
      
      
      
      
      card_vacc_root <- paste0("card_", vacc_stem, dose_stem)
      if(dose_num==1){data[,paste0(vacc_stem, "_dose_from_recall") := 0]}
      
      if (paste0(card_vacc_root, "_day") %in% names(data)) {
        if (dose_num == 1) {
          
          data[(get(paste0(card_vacc_root, "_day")) == 66 | get(paste0("recall_", vacc_stem, "_ever")) == 1),
               c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
        } else if (dose_num == 2) {
          
          data[get(paste0(card_vacc_root, "_day")) == 66 | get(paste0("recall_",vacc_stem, "_ever")) == 1 & get(paste0("recall_",vacc_stem, "_times")) == dose_num,
               c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
        } else if (dose_num > 2) {
          
          data[get(paste0(card_vacc_root, "_day")) == 66 | get(paste0("recall_",vacc_stem, "_ever")) == 1 & get(paste0("recall_",vacc_stem, "_times")) %in% c(3,4,5,6,7),
               c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
        }
      }
      
      
      
      if (paste0(card_vacc_root, "_date") %in% names(data)) {
        
        if (all(c(paste0(card_vacc_root, "_day")) %in% names(data))) {
          
          if (dose_num == 1) {
            data[is.na(get(paste0(card_vacc_root, "_day"))) & (get(paste0(card_vacc_root, "_date")) == 66 | get(paste0("recall_",vacc_stem, "_ever")) == 1),
                 c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
          } else if (dose_num ==2 ) {
            data[is.na(get(paste0(card_vacc_root, "_day"))) & (get(paste0(card_vacc_root, "_date")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 & paste0("recall_",vacc_stem, "_times") == dose_num)),
                 c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            
          } else if (dose_num > 2) {
            data[is.na(get(paste0(card_vacc_root, "_day"))) & (get(paste0(card_vacc_root, "_date")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 & paste0("recall_",vacc_stem, "_times") %in% c(3,4,5,6,7))),
                 c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            
          } else {
            
            if (dose_num == 1) {
              data[(get(paste0(card_vacc_root, "_date")) == 66 | get(paste0("recall_",vacc_stem, "_ever")) == 1),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            } else if (dose_num ==2) {
              data[(get(paste0(card_vacc_root, "_date")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 & paste0("recall_",vacc_stem, "_times") == dose_num)),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            }else if (dose_num > 2) {
              data[(get(paste0(card_vacc_root, "_date")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 & paste0("recall_",vacc_stem, "_times") %in% c(3,4,5,6,7))),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            }
            
            
          }
        }
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      card_vacc_root <- paste0("card_", vacc_stem, dose_stem)
      if(dose_num == 1){data[,paste0(vacc_stem, "_dose_from_card") := 0]}
      if(dose_num == 1){data[,paste0(vacc_stem, "_true_zero") := 0]}
      
      
      for (date_var in c("date")) {
        
        variable <- paste0(card_vacc_root, "_", date_var)
        
        if (variable %in% names(data)) {
          data[, (variable):= as.character(get(variable))]
          
          codes <- c("NA", ".", "")
          for (card_val in c("missing")) {
            
            if (paste0("card_", card_val) %in% names(data)) codes <- c(codes, get(paste0("card_", card_val, "_code")))
          }
          data[get(variable) %in% unique(codes), (variable) := NA_character_]
          
          for (card_val in c("no")) {
            if (paste0("card_", card_val) %in% names(data)) {
              codes <- get(paste0("card_", card_val, "_code"))
              data[get(variable) %in% c(codes), (variable) := "0"]
            }
          }
          
          
          if ("card_date_format" %in% names(data)) {
            if (card_date_format_code %in% c("FILEPATH", "FILEPATH")) date_format_string <- "%d/%m/%Y"
            if (card_date_format_code %in% c("ddmmyy")) date_format_string <- "%d%m%Y"
          } else {
            date_format_string <- ifelse(grepl("/", data[, variable, with=FALSE]), "%d/%m/%Y", "%d%B%Y")
          }
          
          
          if (paste0(card_vacc_root, "_day") %in% names(data)) {
            data[is.na(get(paste0(card_vacc_root, "_day"))) & !is.na(get(variable)), paste0(card_vacc_root, "_day") := format(as.Date(get(variable), date_format_string), "%d")]
          } else {
            data[!is.na(get(variable)), paste0(card_vacc_root, "_day") := format(as.Date(get(variable), date_format_string), "%d")]
          }
          if (paste0(card_vacc_root, "_month") %in% names(data)) {
            data[is.na(get(paste0(card_vacc_root, "_month"))) & !is.na(get(variable)), paste0(card_vacc_root, "_month") := format(as.Date(get(variable), date_format_string), "%m")]
          } else {
            data[!is.na(get(variable)), paste0(card_vacc_root, "_month") := format(as.Date(get(variable), date_format_string), "%m")]
          }
          
          if (paste0(card_vacc_root, "_year") %in% names(data)) {
            data[is.na(get(paste0(card_vacc_root, "_year"))) & !is.na(get(variable)), paste0(card_vacc_root, "_year") := format(as.Date(get(variable), date_format_string), "%Y")]
          } else {
            data[!is.na(get(variable)), paste0(card_vacc_root, "_year") := format(as.Date(get(variable), date_format_string), "%Y")]
          }
          
          data[!is.na(get(paste0(card_vacc_root, "_day"))) & !is.na(get(paste0(card_vacc_root, "_month"))) & !is.na(get(paste0(card_vacc_root, "_year"))) &
                 !is.na(get(paste0(card_vacc_root, "_date"))), paste0(card_vacc_root, "_date") := NA_character_]
        }
      }
      
      card_vacc_root <- paste0("card_", vacc_stem, dose_stem)
      for (date_var in c("day")) {
        
        
        variable <- paste0(card_vacc_root, "_", date_var)
        
        if (variable %in% names(data)) {
          data[, (variable) := as.character(get(variable))]
          
          codes <- c("NA", ".", "")
          for (card_val in c("missing")) {
            
            if (paste0("card_", card_val) %in% names(data)) codes <- c(codes, get(paste0("card_", card_val, "_code")))
          }
          data[get(variable) %in% unique(codes), (variable) := NA_character_]
          
          for (card_val in c("no")) {
            if (paste0("card_", card_val) %in% names(data)) {
              codes <- get(paste0("card_", card_val, "_code"))
              data[get(variable) %in% c(codes), (variable) := "0"]
            }
          }
          
          data[, (variable) := as.integer(get(variable))]
          
          if (unique(data$nid) %in% buddhist_nids & date_var=="year") data[, (variable) := get(variable) - 543]
          
          if (date_var=="day")   data[get(variable) > 31 & get(variable) != 44, (variable) := NA_integer_]
          
          
        }
      }
      
      
      
      
      
      
      
      if (paste0(card_vacc_root, "_day") %in% names(data)) {
        
        
        data[(!is.na(get(paste0(card_vacc_root, "_day"))) & get(paste0(card_vacc_root, "_day")) != 0) & ((get(paste0(card_vacc_root, "_day")) >= 1 & get(paste0(card_vacc_root, "_day")) <= 31) | get(paste0(card_vacc_root, "_day")) == 44),
             c(paste0(vacc_stem, "_card"), paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list(1, (dose_num), (dose_num))]
        if (dose_num == 1) {
          
          data[get(paste0(card_vacc_root, "_day")) == 0 & is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := 0]
          data[get(paste0(card_vacc_root, "_day")) == 0 , paste0(vacc_stem, "_true_zero") := 1]
        }
        
      }
      
      
      
      if (paste0(card_vacc_root, "_date") %in% names(data)) {
        
        if (all(c(paste0(card_vacc_root, "_day")) %in% names(data))) {
          
          data[is.na(get(paste0(card_vacc_root, "_day"))) & !is.na(get(paste0(card_vacc_root, "_date"))),
               paste0(vacc_stem, "_card") := 1]
          data[is.na(get(paste0(card_vacc_root, "_day"))) & get(paste0(vacc_stem, "_card")) == 1 &
                 !is.na(get(paste0(card_vacc_root, "_date"))) & get(paste0(card_vacc_root, "_date")) != 0,
               c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list((dose_num), (dose_num))]
          if (dose_num == 1) {
            
            
            data[is.na(get(paste0(vacc_stem, "_dose"))) & is.na(get(paste0(card_vacc_root, "_day"))) & get(paste0(card_vacc_root, "_date")) == 0,
                 paste0(vacc_stem, "_dose") := 0]
            data[get(paste0(card_vacc_root, "_date")) == 0, paste0(vacc_stem, "_true_zero") := 1]
          }
          
        } else {
          
          data[!is.na(get(paste0(card_vacc_root, "_date"))), paste0(vacc_stem, "_card") := 1]
          data[!is.na(get(paste0(card_vacc_root, "_date"))) & get(paste0(card_vacc_root, "_date")) != 0 & get(paste0(vacc_stem, "_card")) == 1,
               c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list((dose_num), (dose_num))]
          if (dose_num == 1) {
            
            data[get(paste0(card_vacc_root, "_date")) == 0 & is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := 0]
            data[get(paste0(card_vacc_root, "_date")) == 0, paste0(vacc_stem, "_true_zero") := 1]
          }
        }
      }
      
      
      
      
      
      
      if (all(c(paste0("card_", vacc, "_dmy"), "card_date_format") %in% names(data))) {
        
      }
      
      
      age_vars <- c(paste0(card_vacc_root, "_month"), paste0(card_vacc_root, "_year"), "birth_month", "birth_year")
      if (all(age_vars %in% names(data))) {
        
        
        for (age_var in age_vars) if (!class(data[, get(age_var)]) %in% c("integer", "numeric")) data[, (age_var) := get(age_var) %>% as.integer]
        
        
        if (sum(!is.na(data[, get(paste0(card_vacc_root, "_month"))])) > 0 &
            sum(!is.na(data[, get(paste0(card_vacc_root, "_year")) ])) > 0) {
          
          
          
          max_year <- max(data[!is.na(get(paste0(card_vacc_root, "_year"))), (paste0(card_vacc_root, "_year")), with=FALSE], na.rm=TRUE)
          if (max_year < 100) {
            
            data[year_end <  2000, paste0(card_vacc_root, "_year") := 1900 + get(paste0(card_vacc_root, "_year"))]
            
            data[year_end >= 2000, paste0(card_vacc_root, "_year") := ifelse( (2000 + get(paste0(card_vacc_root, "_year"))) <= current_year,
                                                                              2000 + get(paste0(card_vacc_root, "_year")),
                                                                              1900 + get(paste0(card_vacc_root, "_year")))]
          }
          
          
          month_range <- 1:12
          year_range  <- 1900:current_year
          data[!get(paste0(card_vacc_root, "_month")) %in% month_range, paste0(card_vacc_root, "_month") := NA_integer_]
          data[!get(paste0(card_vacc_root, "_year")) %in% year_range, paste0(card_vacc_root, "_year") := NA_integer_]
          
          
          data[, paste0(vacc_stem, "_", dose_stem, "_age_month") := (12 * (get(paste0(card_vacc_root, "_year")) - 1900) + get(paste0(card_vacc_root, "_month")))
               - (12 * (birth_year - 1900) + birth_month)]
        }
      }
      
      
      
      
    }
    
  }
  
  
  
  
  
  
  if(unique(data$survey_name) %in% c("UNICEF_MICS")){
    for (vacc_stem in all_stems) {
      
      if (paste0(vacc_stem, "_dose") %in% names(data)) {
        
        
        
        
        
        
        
        data[,paste0(vacc_stem, "_missing_dose") := 0]
        data[(get("has_vacc_card") != "seen" | is.na(get("has_vacc_card"))) & get(paste0(vacc_stem, "_true_zero")) == 0 & get(paste0(vacc_stem, "_dose_from_card")) == 0 & get(paste0(vacc_stem, "_dose_from_recall")) == 0 & (get(paste0("recall_", vacc_stem, "_ever")) %in% c("",NA, 8,9) | is.na(get(paste0("recall_", vacc_stem, "_ever")))), paste0(vacc_stem, "_missing_dose") := 1]
        
        
        
        
        
        data[,paste0(vacc_stem, "_dose") := 0]
        data[(get(paste0(vacc_stem, "_dose_from_recall")) >= get(paste0(vacc_stem, "_dose_from_card"))) & get(paste0(vacc_stem, "_missing_dose"))== 0 & get(paste0(vacc_stem, "_dose")) == 0, paste0(vacc_stem, "_dose") := get(paste0(vacc_stem, "_dose_from_recall"))]
        data[(get(paste0(vacc_stem, "_dose_from_recall")) < get(paste0(vacc_stem, "_dose_from_card"))) & get(paste0(vacc_stem, "_missing_dose"))== 0 & get(paste0(vacc_stem, "_dose")) == 0, paste0(vacc_stem, "_dose") := get(paste0(vacc_stem, "_dose_from_card"))]
        data[(get(paste0(vacc_stem, "_missing_dose")) == 1), paste0(vacc_stem, "_dose") := NA]
      }
      
    }
  }
  
  
  
  
  
  
  
  
  
  
  if("pent" %in% vaccine_stems_in_dataset){
    
    unconventional_penta_filepath <- "FILEPATH/penta_unconventional_component.csv"
    
    unconventional_penta_countries <- fread(unconventional_penta_filepath)
    if(unique(data$nid) %in% unconventional_penta_countries$survey_id){
      contains_hepb <- unconventional_penta_countries[survey_id == nid, HepB] == 1
      contains_hib  <- unconventional_penta_countries[survey_id == nid, HiB]  == 1
      contains_ipv  <- unconventional_penta_countries[survey_id == nid, IPV]  == 1
      
      if(contains_hepb & !contains_hib){
        pent_vaccines <- c("dpt", "polio", "hepb")
      } else if (contains_hib & ! contains_hepb){
        pent_vaccines <- c("dpt", "polio", "hib")
      } else {
        mesage(paste0('Dataset contains pentavalent vaccine comprised of unconventional component vaccines. Code is not currently designed
                    to account for this configuration of component vaccines. Please make sure document containing data on unconventional
                    pentavalent vaccines is up-to-date at: ', unconventional_penta_filepath, '. Stopping...'))
        stop()
      }
    } else{
      pent_vaccines <- c("dpt", "hib", "hepb")
    }
  }
  
  
  
  tetra_vaccines <- c("dpt", "hepb")
  
  mmr_vaccines <- c("mcv", "rcv")
  
  
  for (multi_vacc in c("pent", "tetra", "mmr")) {
    
    
    
    
    
    
    for (col_in_loop in c("_dose", "_dose_from_card", "_dose_from_recall")) {
      
      
      multi_vacc_column <- paste0(multi_vacc, col_in_loop)
      
      if (multi_vacc %in% vaccine_stems_in_dataset & multi_vacc_column %in% names(data)) {
        
        
        for (vacc in get(paste0(multi_vacc, "_vaccines"))) {
          
          
          component_vacc_column <- paste0(vacc, col_in_loop)
          
          if (component_vacc_column %in% names(data)) {
            
            data[!is.na(get(multi_vacc_column)) & ( (get(multi_vacc_column) > get(component_vacc_column)) | is.na(get(component_vacc_column)) ), (component_vacc_column) := get(multi_vacc_column)]
          } else {
            
            data[!is.na(get(multi_vacc_column)), (component_vacc_column) := get(multi_vacc_column)]
          }
        }
      }
    }
    
    
    
    
    
    
    
    if (multi_vacc %in% vaccine_stems_in_dataset & paste0(multi_vacc, "_card") %in% names(data)) {
      
      for (vacc in get(paste0(multi_vacc, "_vaccines"))) {
        
        if (paste0(vacc, "_card") %in% names(data)) {
          data[!is.na(paste0(multi_vacc, "_card")) & is.na(get(paste0(vacc, "_card"))), (paste0(vacc, "_card")) := get(paste0(multi_vacc, "_card"))]
        } else {
          data[!is.na(paste0(multi_vacc, "_card")), (paste0(vacc, "_card")) := get(paste0(multi_vacc, "_card"))]
        }
      }
    }
    
    
    
    
    
    
    if (multi_vacc %in% vaccine_stems_in_dataset) { 
      for (vacc in get(paste0(multi_vacc, "_vaccines"))) {
        for (dose in c("", "1", "2", "3")) { 
          if (paste0(vacc, dose) %in% vaccines) {
            if (!paste0(vacc, dose, "_age_month") %in% names(data)) data[, paste0(vacc, dose, "_age_month") := NA_integer_]
            if (paste0(multi_vacc, dose, "_age_month") %in% names(data)) {
              data[!is.na(get(paste0(multi_vacc, dose, "_age_month"))) & is.na(get(paste0(vacc, dose, "_age_month"))), paste0(vacc, dose, "_age_month") := get(paste0(multi_vacc, dose, "_age_month"))]
            }
          } 
        }
      }
      
      
      
      if (multi_vacc %in% vaccine_stems_in_dataset){
        message(paste0("Combination vaccine components tagged for || ", multi_vacc, " (", paste(get(paste0(multi_vacc, "_vaccines")), collapse=", "), ") ||"))
      }
    }
  }
  
  
  
  
  
  
  
  if ("dpt3_age_month" %in% names(data)) {
    data[, dpt3_on_time := ifelse(is.na(dpt3_age_month), NA_integer_, ifelse(dpt3_age_month < 12, 1, 0))]
  }
  
  
  
  
  
  
  
  
  
  outvacc <- c()
  for (vacc_stem in all_stems) {
    if (paste0(vacc_stem, "_dose") %in% names(data)) {
      
      
      max <- vacc_dose[var == vacc_stem, refdose]
      data[as.integer(get(paste0(vacc_stem, "_dose"))) > max & !is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := max]
      
      
      levels <- unique(data[, get(paste0(vacc_stem, "_dose"))]) %>% as.integer
      if (all(is.na(levels) | levels=="")) {
        data[, paste0(vacc_stem, "_dose") := NULL]
        data[, paste0(vacc_stem, "_card") := NULL]
        data[, paste0(vacc_stem, "_dose_from_card") := NULL]
        data[, paste0(vacc_stem, "_dose_from_recall") := NULL]
      } else {
        outvacc <- c(outvacc, vacc_stem)
      }
      
      
      if (paste0(vacc_stem, "_dose") %in% names(data)) {
        for (dose in 1:max) {
          if (max == 1) newvar <- vacc_stem else newvar <- paste0(vacc_stem, dose)
          data[, (newvar) := ifelse(get(paste0(vacc_stem, "_dose")) >= dose, 1, 0)]
          
          if (max(data[, get(newvar)], na.rm=TRUE) == 0) data[, (newvar) := NULL]
        }
      }
    }
  }
  
  
  
  message(paste0("The following vaccines have been processed: ", paste(outvacc, collapse=", ")))
  
  
  
  
  
  
  
  if ("rota" %in% vaccine_stems_in_dataset) data <- make_rotac(data)
  
  
  if ("rota" %in% vaccine_stems_in_dataset) data <- make_rotac(data)
  
  
  
  
  if (!"age_year" %in% names(data)) data[, age_year := NA_integer_]
  if (!class(data$age_year) %in% c("numeric", "integer")) data[, age_year := as.numeric(age_year)]
  if ("age_month" %in% names(data)) {
    if (!class(data$age_month) %in% c("numeric", "integer")) data[, age_month := as.numeric(age_month)]
    data[is.na(age_year) & !is.na(age_month), age_year := age_month / 12]
  }
  
  
  
  
  
  n_age_data_drop  <- data[is.na(age_year), .N]
  n_age_range_drop <- data[age_year >= 5 | age_year < 1, .N]
  non_def_ages <- (n_age_data_drop + n_age_range_drop) / nrow(data)
  
  
  
  
  
  
  
  data <- data[!(age_year >= 5 | age_year < 1 | is.na(age_year))]
  data[, age_year := floor(age_year)]
  
  
  message("Age restrictions complete")
  
  
  
  
  
  
  
  
  metadata_cols <- c("survey_name", "nid", "ihme_loc_id", "year_start", "year_end", "survey_module", "file_path",
                     "smaller_site_unit", "strata", "strata_recode", "psu", "psu_recode", "hh_id", "line_id",
                     "hhweight", paste0("pweight", c("", "_admin_1", "_admin_2", "_admin_3")),
                     paste0("admin_", 1:5), paste0("admin_", 1:5, "_mapped"), paste0("admin_", 1:5, "_id"), paste0("admin_", 1:5, "_urban_id"),
                     "geospatial_id", "latitude", "longitude", "urban",
                     "sex_id", "age_categorical", "age_day", "age_month", "age_year", "int_month", "int_year")
  vaccine_cols  <- c("ever_vaccinated", "has_vacc_card",
                     paste0(all_stems, "_ever"), paste0(all_stems, "_dose"), paste0(all_stems, "_card"),
                     paste0(all_stems, "_dose_from_card"), paste0(all_stems, "_dose_from_recall"),
                     vaccines, paste0(vaccines, "_age_month"),
                     "dpt3_on_time",
                     "rotac")
  keep_cols     <- c(metadata_cols, vaccine_cols)[c(metadata_cols, vaccine_cols) %in% names(data)]
  data <- data[, keep_cols, with=FALSE]
  
  for (ea_keep_col in keep_cols) {
    if (all(is.na(data[, get(ea_keep_col)]))) data[, (ea_keep_col) := NULL]
  }
  
  
  
  vet(data, vaccine_stems_in_dataset=vaccine_stems_in_dataset, vaccines_all_in_dataset=vaccines_all_in_dataset, age_drops=non_def_ages)
  
  message("Logs saved")
  
  
  
  
  
  message(paste0("Processed data for NID ", nid, " done"))
  
  
  return(data)
  
  
}






prep_for_tabulation_mics_special_age <- function(nid, team="gbd", vaccines.=vaccines, filter_table=NA, age_start, age_end) {
  
  
  data <- test
  
  
  
  vax_targets <- set_target_ages(data=data)[, vaccine := gsub("vacc_", "", me_name)]
  if (team=="gbd") data <- make_full_coverage(data, targets=vax_targets)
  
  
  if (team=="gbd") {
    for (ea in c(vaccines, "rotac")) {
      if (ea %in% names(data)) data[exists(paste0(gsub("[0-9]", "", ea), "_card"))==1, (paste0(ea, "_CARD")) := get(ea)]
      if (ea %in% names(data)) data[exists(paste0(gsub("[0-9]", "", ea), "_card"))==0 & !is.na(get(ea)), (paste0(ea, "_RECALL")) := get(ea)]
    }
  }
  
 
  
  tab_cols <- c("nid", "survey_name", "survey_module", "file_path", "ihme_loc_id", "year_start", "year_end", "age_year", "age_month")
  gbd_cols <- c("strata", "psu", paste0("pweight", c("_admin_1", "_admin_2", "_admin_3", "_admin_4", "_admin_5")), paste0("admin_", 1:5, "_id"))
  lbd_cols <- c("geospatial_id", "strata", "psu", "pweight", paste0("pweight", c("_admin_1", "_admin_2", "_admin_3", "_admin_4", "_admin_5")))
  if (team=="lbd") tab_cols <- c(tab_cols, "geospatial_id") %>% unique
  if (team=="gbd") tab_cols <- c(tab_cols, "pweight") %>% unique
  check <- check_completeness(data, cols=tab_cols, date=date, nid=nid, team=team)
  
  if (check) {
    
    
    parent <- unique(data$ihme_loc_id)
    if (team=="gbd" & "admin_1_urban_id" %in% names(data) & "pweight_admin_1" %in% names(data)) {
      data_ur <- copy(data)[!is.na(admin_1_urban_id)][, ihme_loc_id := admin_1_urban_id]
      data_ur[, pweight := pweight_admin_1]
      data_ur[, c("admin_1_urban_id", "pweight_admin_1", "admin_1_id", "admin_2_id", "pweight_admin_2", "admin_3_id", "pweight_admin_3") := NULL]
      data_ur[ihme_loc_id==parent, ihme_loc_id := NA_character_]
      data <- rbind(data, data_ur[!is.na(ihme_loc_id)], fill=TRUE)
    }
    
    
    if (team=="gbd" & "admin_2_id" %in% names(data) & "pweight_admin_2" %in% names(data)) {
      data_a2 <- copy(data)[!is.na(admin_2_id)][, ihme_loc_id := admin_2_id]
      data_a2[, pweight := pweight_admin_2]
      data_a2[, c("admin_1_urban_id", "pweight_admin_1", "admin_1_id", "admin_2_id", "pweight_admin_2", "admin_3_id", "pweight_admin_3") := NULL]
      data_a2[ihme_loc_id==parent, ihme_loc_id := NA_character_]
      data <- rbind(data, data_a2[!is.na(ihme_loc_id)], fill=TRUE)
    }
    
    
    if (team=="gbd" & "admin_3_id" %in% names(data) & "pweight_admin_3" %in% names(data)) {
      data_a3 <- copy(data)[!is.na(admin_3_id)][, ihme_loc_id := admin_3_id]
      data_a3[, pweight := pweight_admin_3]
      data_a3[, c("admin_1_urban_id", "pweight_admin_1", "admin_1_id", "admin_2_id", "pweight_admin_2", "admin_3_id", "pweight_admin_3") := NULL]
      data_a3[ihme_loc_id==parent, ihme_loc_id := NA_character_]
      data <- rbind(data, data_a3[!is.na(ihme_loc_id)], fill=TRUE)
    }
    
    
    if (team=="lbd") {
      for (vax_stem_col in unique(gsub("[0-9]", "", vaccines))) {
        if (paste0(vax_stem_col, "_dose") %in% names(data) & !vax_stem_col %in% c("mcv", "mmr", "rcv")) {
          vax_dose_cols <- vaccines[grep(vax_stem_col, vaccines)]
          if (all(vax_dose_cols %in% names(data))) {
            data[eval(parse(text=paste(paste0("is.na(", vax_dose_cols, ")"), collapse=" | "))), (vax_dose_cols) := NA]
          } else {
            data[, vax_dose_cols[vax_dose_cols %in% names(data)] := NA]
          }
        }
      }
    }
    
    
    cols       <- c(tab_cols, get(paste0(team, "_cols")))[c(tab_cols, get(paste0(team, "_cols"))) %in% names(data)] %>% unique
    vax_cols   <- vaccines.[vaccines. %in% names(data)]
    ready_data <- data[, c(cols, vax_cols), with=FALSE]
    ready_data[, individual := row_number(nid)]
    ready_data <- melt.data.table(ready_data, id.vars=cols, measure.vars=vax_cols, variable.name="me_name")
    ready_data[, me_name := paste0("vacc_", me_name)]

    
    ready_data <- ready_data[!is.na(value)]
    
    
    
    vax_targets <- set_target_ages(ready_data, vaccines.=vaccines.)
    ready <- merge(ready_data, vax_targets, by="me_name", all.x=TRUE)
    
    ready[, cohort := ifelse(age_month >= age_start & age_month <= age_end, 1, 0)]
    
    
    
    ready[, year_id := floor( (year_start + year_end) / 2) - cohort - 1]
    
    ready <- ready[(cohort != 0)]
    ready[, c("age_cohort", "cohort") := NULL]
    message("Birth cohort assignment complete")
    
    
    return(list(check, ready))
  } else {
    
    
    return(list(check))
    
  }
}





collapse.run.mics.special.age <- function(df, config, quiet=TRUE, cores=1) {
  
  
  config.default <- list(
    
    
    vars             = c("var"),        
    vars.categ       = NULL,            
    calc.sd          = FALSE,           
    
    cv.manual        = NULL,            
    cv.detect        = TRUE,            
    
    by_sex           = TRUE,            
    
    gbd_age_cuts     = TRUE,            
    aggregate_under1 = TRUE,            
    custom_age_cuts  = NULL,            
    cond_age_cuts    = NULL,            
    
    sample_threshold = NA,              
    
    vars.meta        = c(               
      "nid", "survey_name",       
      "ihme_loc_id", "year_start",
      "year_end", "survey_module",
      "file_path"
    ),
    
    vars.subnat      = c(               
      "admin_1_id", 
      "admin_1_urban_id", 
      "admin_2_id",
      "admin_3_id",
      "admin_4_id",
      "admin_5_id"),  
    
    vars.sex         = c("sex_id"),     
    
    
    
    
    census_data      = FALSE, 
    
    vars.design      = c(               
      "strata", "psu", "pweight", "pweight_admin_1", "pweight_admin_2", "pweight_admin_3"
    )
  )
  
  
  if (!is.null(config)) lapply(names(config), function(x) assign(x, config[[x]], pos=1))
  
  
  lapply(names(config.default), function(x) if (!exists(x)) assign(x, config.default[[x]], pos=1))
  
  
  
  
  if (!is.null(vars.categ)) if (!is.na(vars.categ) & vars.categ != "") {
    for (var in vars.categ) {
      ctb <- categ_to_bin(df, var)
      df <- ctb[['df']]
      new.vars <- ctb[['cols']]
      vars <- c(vars, new.vars)
    }
  }
  
  vars.check <- nonmiss(df, vars)
  if (length(vars.check) < 1L) stop(paste0("BREAK || All vars (", toString(vars), ") are either not in dataset or completely missing"))
  vars <- vars.check
  
  
  vars.stratify <- vars.meta
  if (by_sex) vars.stratify <- c(vars.stratify, vars.sex)
  
  
  if (cv.detect) {
    vars.cv <- grep("^cv_", names(df), value=TRUE)
    if (length(vars.cv) > 0) {
      vars.cv <- nonmiss(df, vars.cv)
      vars.stratify <- c(vars.stratify, vars.cv)
    }
  }
  
  
  if (!is.null(cv.manual)) {
    vars.cv <- nonmiss(df, cv.manual)
    vars.stratify <- c(vars.stratify, vars.cv)
  }
  
  
  vars.subnat <- intersect(vars.subnat, names(df))
  vars.subnat <- nonmiss(df, vars.subnat)
  if (length(vars.subnat) != 0L) {
    vars.stratify <- lapply(c("ihme_loc_id", vars.subnat), function(x) c(vars.stratify, x) %>% unique)
  } else {
    vars.stratify <- list(vars.stratify)
  }
  
  for (var.subnat in vars.subnat) {
    df[get(var.subnat)==get("ihme_loc_id"), (var.subnat) := NA_character_]
  }
  
  
  
  
  
  vars.design.missing <- nonmiss(df, vars.design, reverse=TRUE)
  subnat_pweights <- c("pweight_admin_1", "pweight_admin_2", "pweight_admin_3")
  if (length(vars.design.missing) != 0L) {
    df <- df[, (vars.design.missing) := 1]
    absent_subnat_pweights <- subnat_pweights[subnat_pweights %in% vars.design.missing]
    df <- df[, (absent_subnat_pweights) := NA]
  }
  
  
  for (var in vars.design) {
    if (class(df[[var]]) != "numeric") {
      df[[var]] <- as.numeric(df[[var]])
      if (!quiet) print(paste0("ubCov Collapse || Setup || Coercing ", var, " to numeric"))
    }
  }
  
  
  n <- nrow(df[pweight==0])
  if (n > 0) {
    df <- df[pweight != 0]
    if (!quiet) print(paste0("ubCov Collapse || Setup || Dropping ", n, " rows with pweight == 0"))
  }
  
  
  errors <- list(missing_design=vars.design.missing)
  
  
  
  
  
  
  
  
  
  
  
  bad.vars.stratify <- which(vars.stratify[[1]]=="age_year")
  vars.stratify[[1]] <- vars.stratify[[1]][-bad.vars.stratify]
  cf.list <- expand.grid(vars=vars, vars.stratify=vars.stratify)
  
  
  cf <- mclapply(1:nrow(cf.list), function(i) {
    
    vars <- cf.list$vars[i] %>% as.character
    vars.stratify <- cf.list$vars.stratify[i] %>% unlist
    
    if (!quiet) print(paste0("ubCov Collapse || Collapsing (", vars, ") by (", toString(vars.stratify), ")"))
    
    sf <- collapse.subset(df, var=vars, by_vars=vars.stratify, design_vars=vars.design)
    missing <- sf[['missing']]
    sf <- sf[['data']]
    
    if (nrow(sf) > 0) {
      
      if (census_data) sf.out <- census.collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      if (!census_data) sf.out <- collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      
      sf.out <- clean.subnat(sf.out, vars.subnat)
      sf.out <- rbind(sf.out, missing=missing)
      return(sf.out)
    } else {
      if (!quiet) print(paste0("ubCov Collapse || SKIPPING || Variable (", vars, ") has no observations post subset with (", toString(vars.stratify), ")"))
      return(NULL)
    }
  }, mc.cores=cores) %>% rbindlist(., use.names=TRUE, fill=TRUE)
  
  
  
  if (!is.na(sample_threshold)) cf <- cf[sample_size >= sample_threshold]
  
  
  if (nrow(cf[mean %in% c(0,1)]) > 0) {
    cf.w <- cf[mean %in% c(0,1)]
    sample_size <- cf.w$sample_size
    n <- ifelse(cf.w$mean==0, 0, sample_size)
    ci <- binom.confint(n, sample_size, conf.level = 0.95, methods = "wilson")
    se <- (ci$upper - ci$lower)/3.92
    se <- se * sqrt(2.25) 
    cf[mean %in% c(0,1)]$standard_error <- se
  }
  
  
  
  if ("strata" %in% vars.design.missing & !census_data) {
    cf <- cf[design_effect < 2.25, inflate := 2.25/design_effect]
    cf <- cf[design_effect < 2.25, standard_error := standard_error * sqrt(inflate)]
    cf <- cf[, inflate := NULL]
    cf <- cf[design_effect < 2.25, design_effect := 2.25]
  }
  
  
  return(cf)
  
}
