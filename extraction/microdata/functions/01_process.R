



















'%!in%' <- function(x,y)!('%in%'(x,y))



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


save_datestamp <- function() {

  
  date <- Sys.Date()
  date_extraction_root <- file.path(extraction_root, "log/datestamp", date)
  if (!dir.exists(date_extraction_root)) dir.create(date_extraction_root, recursive=TRUE)

  log_path <- file.path(date_extraction_root, paste0(nid, ".txt"))
  
  if(file.exists(log_path)) unlink(log_path)
  cat(paste0("\n", nid, "\n"), file=log_path, append=TRUE)

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
    cat("||---- File not found")
    cat("\nnidnotexist", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    stop(paste0("No files for NID '", nid, "' exist in '", folder_path, "'"))
  }

  
  cat(paste0("||---- Reading: ", paste(file, collapse=","), "\n"))
  data <- lapply(file, fread) %>% rbindlist(., fill=TRUE)

  
  return(data)
}


check_completeness <- function(data, cols, date, nid, team) {

  
  if (all(cols %in% names(data))) {
    if (length(vaccines[vaccines %in% names(data)]) > 0) {
      check <- TRUE
    } else {
      cat("\nnovaxind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
      cat("Missing at least one vaccine indicator; not moving on to tabulation")
      check <- FALSE
    }
  } else {
    cat("\nmissingind\n", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt\n")), append=TRUE)
    if (team=="lbd" & !"geo_id" %in% names(data) & !"geospatial_id" %in% names(data)) cat("\nmissinggeoind\n", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    if (!"year_id" %in% names(data)) cat("\nmissingyearid", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    cat(paste0("Missing ", paste(cols[!cols %in% names(data)], collapse=", "), " from dataset; not moving on to tabulation\n"))
    check <- FALSE
  }

  
  if ("age_year" %in% names(data)) {
    if (nrow(data[!is.na(age_year)]) == 0) {
      cat("\nnoageind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
      cat(paste0("Missing age data from all observations in dataset; not moving on to tabulation\n"))
      check <- FALSE
    }
  }

  
  return(check)
}


check_and_rename_col <- function(data, old_name, new_name, quiet=TRUE) {
  if (old_name %in% names(data)) {
    setnames(data, old_name, new_name)
    if (!quiet) cat(paste0("Renamed ", old_name, " to ", new_name))
  }
  else {
    if (!quiet) cat(paste0("No column named ", old_name, " to rename"))
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
  intro <- readRDS("FILEPATH/vaccine_intro.rds")[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), cv_intro] %>% unique

  
  if (nrow(doses)==0) doses <- 0
  doses     <- ifelse(doses >= 3, 3, 2)
  rota_dose <- paste0("rota", doses)

  
  if (length(intro)==0) stop(paste0("BREAK | Missing introduction year for ", unique(data$ihme_loc_id), "; need to prep introduction frame for this geography before continuing"))
  if (intro < min(unique(data$int_year)) & rota_dose %in% names(data)) data$rotac <- data[, rota_dose, with=FALSE]
  
  if(!('rotateq' %in% vaccine_stems_in_dataset) &    'rotarix' %in% vaccine_stems_in_dataset)  data[,rotac:=rotarix2]
  if('rotateq' %in% vaccine_stems_in_dataset    &  !('rotarix' %in% vaccine_stems_in_dataset)) data[,rotac:=rotateq3]
  if('rotateq' %in% vaccine_stems_in_dataset    &    'rotarix' %in% vaccine_stems_in_dataset)  data[,rotac:=pmax(rotarix2, rotateq3)]

  
  return(data)
}


set_target_ages <- function(data, vaccines.=vaccines) {

  
  ihme_location_id <- ifelse("ihme_loc_id" %in% names(data),
                             unique(data$ihme_loc_id),
                             unique(data$country))

  
  schedule          <- readRDS("FILEPATH/vaccine_target.rds")
  schedule          <- schedule[ihme_loc_id %in% ihme_location_id]

  
  if(nrow(schedule) > 1) {
    schedule <- rbind(schedule,
                      data.table("ihme_loc_id" = schedule$ihme_loc_id,
                                 "age_cohort"  = schedule$age_cohort,
                                 "me_name"     = paste0(schedule$me_name, "_RECALL")),
                      data.table("ihme_loc_id" = schedule$ihme_loc_id,
                                 "age_cohort"  = schedule$age_cohort,
                                 "me_name"     = paste0(schedule$me_name, "_CARD")))
  }

  
  schedule_antigens <- unique(schedule$me_name)
  vax_targets       <- data.table()
  for (vax_target in paste0("vacc_", vaccines.)) {
    assign(paste0(vax_target, "_target"),
           ifelse(vax_target %in% schedule_antigens,
                  ifelse(vax_target %in% unique(schedule$me_name), unique(schedule[me_name==vax_target, age_cohort]), NA),
                  1))
    vax_targets <- rbind(vax_targets, data.table(me_name=vax_target, age_cohort=get(paste0(vax_target, "_target"))))
  }

  
  return(vax_targets)

}




#' @param age_bin: Indicates what age bin the child is associated with
#' @param age_bin_agg: For data that covers a range of age bins, `age_bin_agg`` is used to show the range. For example, children in the


#' @param age_bin_agg_id: For age-range data that don't fit neatly in to age bins (ie lit data with months 15-26 or 18-29), `age_bin_agg_id`


add_age_bins <- function(data, ref_repo = reference_repo, is_microdata = TRUE) {

  
  min_max <- function(range) {
    if (typeof(range) == "list") range <- unlist(range)
    minimum <- min(range)
    maximum <- max(range)
    min_max <- paste0(minimum, ":", maximum)
    return(min_max)
  }

  
  age_bins <- fread(file.path(ref_repo, "vax_age_lookup.csv"))
  age_bins <- age_bins[age_bin %in% c(1:7), ]

  
  

  
  if (is_microdata) {

    
    if (!"age_month" %in% names(data)) data$age_month <- NA_integer_
    if ("age_year" %in% names(data) & typeof(data$age_year) != "integer")   data$age_year  <- as.integer(data$age_year)
    if ("age_month" %in% names(data) & typeof(data$age_month) != "integer") data$age_month <- as.integer(data$age_month)

    
    if (data[is.na(age_year) & !is.na(age_month), .N] > 0) data[is.na(age_year) & !is.na(age_month), age_year := floor(age_month / 12)]


    
    data[age_year == 0 & age_month > 11, age_month := 11]

    
    data[age_year == 0 & !is.na(age_month), `:=`(age_start = age_month, age_end = age_month)]
    data[age_year == 0 & is.na(age_month), `:=`(age_start = 0, age_end = 11)]
    data[age_year  > 0, `:=`(age_start = as.integer(age_year * 12), age_end = as.integer(age_year * 12))]
  }

  
  data[age_start == age_end, age_bin := paste(age_bins[age_month_start <= age_start & age_month_end >= age_end, unique(age_bin)], collapse = ","), by = c("age_start", "age_end")]
  
  data[age_start != age_end, age_bin := paste(age_bins[age_month_start >= age_start & age_month_end <= age_end, unique(age_bin)], collapse = ","), by = c("age_start", "age_end")]
  data[age_start != age_end & age_bin == "", age_bin := paste(age_bins[age_month_start <= age_start & age_month_end >= age_end, unique(age_bin)], collapse = ","), by = c("age_start", "age_end")]

  
  data[, age_bin_agg := NA_character_]
  data[grepl(",", age_bin), age_bin_agg := paste0(min(unlist(strsplit(age_bin, ","))), ":", max(unlist(strsplit(age_bin, ",")))), by = "age_bin"]
  data[grepl(",", age_bin), age_bin := NA]

  
  
  data[, age_bin_agg_id := FALSE]
  if (!is_microdata) {
    data[!(any(age_start %in% age_bins$age_month_start)) | !(any(age_end %in% age_bins$age_month_end)), age_bin := ""]
    contains_mismatched_age_bins <- data[age_bin == "", .N] > 0
    if (contains_mismatched_age_bins) {
      data[age_bin == "", `:=`(age_bin_agg_id = TRUE,
                               age_bin_agg = min_max(age_bins[(age_month_start <= age_start & age_month_end >= age_start & age_month_end <= age_end) |   
                                                                (age_month_end >= age_end & age_month_start <= age_end & age_month_start >= age_start) | 
                                                                (age_month_start >= age_start & age_month_end <= age_end), unique(age_bin)])),           
           by = c("age_start", "age_end")]
    }
  }

  
  data[age_bin == "", age_bin := NA]

  
  return(data)
}


prep_for_tabulation <- function(nid, team="gbd", vaccines.=vaccines) {

  
  data <- load_nid(nid, folder="processed")

  
  vax_targets <- set_target_ages(data)
  vax_targets[, vaccine := gsub("vacc_", "", me_name)]
  if (team=="gbd") data <- make_full_coverage(data, targets=vax_targets)

  
  data <- add_age_bins(data)

  
  if (team=="gbd") {
    for (ea in c(vaccines, "rotac")) {
      if (ea %in% names(data)) data[exists(paste0(gsub("[0-9]", "", ea), "_card"))==1, (paste0(ea, "_CARD")) := get(ea)]
      if (ea %in% names(data)) data[exists(paste0(gsub("[0-9]", "", ea), "_card"))==0 & !is.na(get(ea)), (paste0(ea, "_RECALL")) := get(ea)]
    }
  }

  
  tab_cols <- c("nid", "survey_name", "survey_module", "file_path", "ihme_loc_id", "year_start", "year_end", "age_year", "age_bin", "age_bin_agg", "age_bin_agg_id")
  gbd_cols <- c("strata", "psu", paste0("pweight", c("_admin_1", "_admin_2", "_admin_3", "_admin_4", "_admin_5")), paste0("admin_", 1:5, "_id"))
  lbd_cols <- c("age_month_start", "age_month_end", "geospatial_id", "strata", "psu", "pweight", paste0("pweight", c("_admin_1", "_admin_2", "_admin_3", "_admin_4", "_admin_5")))
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

    
    
    
    
    
    
    
    
    
    
    
    
    

    
    cols       <- c(tab_cols, get(paste0(team, "_cols")))[c(tab_cols, get(paste0(team, "_cols"))) %in% names(data)] %>% unique
    vax_cols   <- vaccines.[vaccines. %in% names(data)]
    ready_data <- data[, c(cols, vax_cols), with=FALSE]
    ready_data[, individual := row_number(nid)]
    ready_data <- melt.data.table(ready_data, id.vars=cols, measure.vars=vax_cols, variable.name="me_name")
    ready_data[, me_name := paste0("vacc_", me_name)]

    
    if(team == "lbd"){
      data_drop_table <- fread(file.path(extraction_root, "FILEPATH", paste0(nid, ".csv")))
      
      data_drop_table[, n := as.integer(n)]
      for (vax in unique(ready_data$me_name)) {
        data_drop_table[antigen == gsub("vacc_", "", vax), n := ready_data[me_name == (vax) & !is.na(value), .N]]
      }
    }

    
    ready_data <- ready_data[!is.na(value), ]

    
    ready_data[, year_id := floor((year_start + year_end) / 2) - age_year]

    
    if(team == "lbd") return_list <- list(check, ready_data, data_drop_table)
    if(team == "gbd") return_list <- list(check, ready_data)
    return(return_list)

  } else {

    
    return(list(check))
  }
}


log_empty_dataset <- function(nid, date, script, step, object_name) {
  date_extraction_root <- file.path(extraction_root, "log/datestamp", date)
  log_file <- file.path(date_extraction_root, paste0(nid, ".txt"))
  cat(paste0("\nscript ", script, ", step ", step, ": ", object_name, " has zero rows\n"), file=log_file, append=TRUE)
}



get_vaccine_stems <- function(nid) {
  df_tabulated <- load_nid(nid, folder="tabulated")
  vax_info <- fread("FILEPATH/vaccine_info_lookup.csv")
  indicators_in_data <- unique(df_tabulated$me_name)
  vaccines_in_data <- indicators_in_data[!grepl("ratio", indicators_in_data)]
  ratios_in_data <- indicators_in_data[grepl("ratio", indicators_in_data)]
  vaccine_stems_in_data <- vax_info[vaccine %in% gsub("vacc_", "", vaccines_in_data) & multi_vaccine == FALSE, unique(vaccine_stem)]
  ratios_stems_in_data <- gsub("vacc_", "", ratios_in_data)
  vaccine_stems <- c(vaccine_stems_in_data, ratios_stems_in_data)

  return(vaccine_stems)
}


check_nids <- function(nid, gbd, lbd, start_time) {

  
  
  
  processed_files <- list.files(file.path(extraction_root, "01_processed"))
  if (paste0(nid, ".csv") %in% processed_files) {
    if (file.info(file.path(extraction_root, "01_processed", paste0(nid, ".csv")))$mtime > start_time) {
      check_process <- "Success"
    } else {
      check_process <- "Failure"
    }
  }

  if (check_process == "Success") {
    tabulated_files <- list.files(file.path(extraction_root, "02_tabulated/gbd"))
    if (paste0(nid, ".csv") %in% tabulated_files) {
      if (file.info(file.path(extraction_root, "02_tabulated/gbd", paste0(nid, ".csv")))$mtime > start_time) {
        check_gbd <- "Success"
      } else {
        check_gbd <- "Failure"
      }
    }

    tabulated_files <- list.files(file.path(extraction_root, "02_tabulated/lbd"))
    if (paste0(nid, ".csv") %in% tabulated_files) {
      if (file.info(file.path(extraction_root, "02_tabulated/lbd", paste0(nid, ".csv")))$mtime > start_time) {
        check_lbd <- "Success"
      } else {
        check_lbd <- "Failure"
      }
    }
  } else {
    check_gbd <- "Not launched (01 failure)"
    check_lbd <- "Not launched (01 failure)"
  }

  
  
  if (check_lbd == "Success") {
    vaccine_stems <- get_vaccine_stems(nid)

    resample_status <- c()
    if (any(c("dpt", "mcv", "bcg", "polio") %in% vaccine_stems)) {
      resample_status <- c("resampled")
    }

    if (any(c("pcv", "rota", "hepb", "hib", "mcv") %in% vaccine_stems)) {
      resample_status <- c(resample_status, "not_resampled")
    }

    if (length(resample_status) == 0) {
      check_resampled <- "No relevant vaccines in data"
      check_not_resampled <- "No relevant vaccines in data"
    }

    if ("resampled" %!in% resample_status) {
      check_resampled <- "Not checked"
    }

    if ("not_resampled" %!in% resample_status) {
      check_not_resampled <- "Not checked"
    }

    for (res in resample_status) {
      file_status <- c()
      for (vac in vaccine_stems) {
        disaggregated_files <- list.files(file.path(extraction_root, "03_disaggregated/", res, vac))
        if (paste0(nid, ".csv") %in% disaggregated_files) {
          file_status <- c(file_status, file.info(file.path(extraction_root, "03_disaggregated/", res, vac, paste0(nid, ".csv")))$mtime > start_time)
        }
      }

      
      if (res == "resampled") {
        resample_drop_log <- fread(file.path(extraction_root, "FILEPATH", paste0(nid, ".csv")))
        resample_drop_log <- resample_drop_log[resampled == T,]

        if (nrow(resample_drop_log) > 0) {
          resample_drop_log[, per_diff := ((n_after_resample - n_after_geomatch)/n_after_geomatch) * 100]
          if(nrow(resample_drop_log[per_diff < -30 | per_diff > 0]) > 0) {
            problem_drops <- resample_drop_log[per_diff < -30 | per_diff > 0]
            drop_status <- paste0("Data drop warning: ", paste(problem_drops$antigen, collapse = ", "), " with percent difference in rows after resample: ", paste(format(problem_drops$per_diff, digits = 3), collapse = ", "))
          } else {
            drop_status <- "Success"
          }
        } else {
          drop_status <- "Success"
        }
      } else {
        drop_status <- "Success"
      }


      if (all(file_status)) {
        check_disagg <- "Success"
      } else {
        check_disagg <- paste0("Failure for vaccines: ", paste(vaccine_stems[!file_status], collapse = ", "))
      }

      
      if (drop_status != "Success" & check_disagg != "Success") {
        check_disagg <- paste0(check_disagg, "; ", drop_status)
      } else if (drop_status != "Success" & check_disagg == "Success") {
        check_disagg <- drop_status
      }

      assign(paste0("check_", res), check_disagg)
    }
  } else {
    check_resampled <- "Not launched (02_lbd failure)"
    check_not_resampled <- "Not launched (02_lbd failure)"
  }

  if (gbd & !lbd) {
    check_lbd <- "Not launched (GBD only)"
    check_resampled <- "Not launched (GBD only)"
    check_not_resampled <- "Not launched (GBD only)"
  }

  
  note <- readLines(file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")))

  
  reason <- ""
  if (check_process=="Failure") {
    if (any(grepl("abc", note))) reason <- paste(reason, sep=ifelse(nchar(reason) >= 1, "; ", ""))
  }
  if (check_gbd=="Failure" | check_lbd=="Failure") {
    if (any(grepl("nidnotexist", note))) reason <- paste(reason, "This NID doesn't exist FILEPATH", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("novaxind", note))) reason <- paste(reason, "No vaccine indicators found in dataset", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("noageind", note))) reason <- paste(reason, "All observations in dataset missing age information", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("missingind", note))) reason <- paste(reason, "Dataset entirely missing at least one required survey design variable", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("missinggeoind", note))) reason <- paste(reason, "Dataset entirely missing geospatial_id", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("missingyearid", note))) reason <- paste(reason, "Dataset entirely missing year_id (likely because no children with non-missing age older than target age range)", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("DataPre2000", note))) reason <- paste(reason, "Dataset pre-2000 (skipping LBD steps)", sep=ifelse(nchar(reason) >= 1, "; ", ""))
    if (any(grepl("tab_prep_failed", note))) reason <- paste(reason, "Tabulation prep failed (02_lbd)", sep=ifelse(nchar(reason) >= 1, "; ", ""))
  }

  if (any(grepl("has zero rows", note))) {
    zero_rows <- note[grepl("has zero rows", note)]
    zero_rows_reason <- paste0(unlist(lapply(zero_rows, gsub, pattern = "\n", replacement = "")), collapse = "; ")
    reason <- paste(reason, zero_rows_reason, sep=ifelse(nchar(reason) >= 1, "; ", ""))
  }

  
  log <- fread(file.path(extraction_root, "FILEPATH/log.csv"))
  nid_log <- data.table(nid=nid, checked=format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d %H:%M"),
                        check_process=check_process, check_gbd=check_gbd, check_lbd=check_lbd, check_resampled=check_resampled, check_not_resampled=check_not_resampled, notes=reason)
  write.csv(rbind(log, nid_log, fill=TRUE), file.path(extraction_root, "FILEPATH/log.csv"), row.names=FALSE)

  
  cat(paste0("|| NID ", nid, " ||\n"))
  cat(paste0("|| Processing:     ", check_process, " ||\n"))
  cat(paste0("|| GBD tabulation: ", check_gbd, " ||\n"))
  cat(paste0("|| LBD tabulation: ", check_lbd, " ||\n"))

  if("resampled" %in% resample_status) {
    cat(paste0("|| LBD disaggregation (resampled): ", check_resampled, " ||\n"))
  }

  if("not_resampled" %in% resample_status) {
    cat(paste0("|| LBD disaggregation (not_resampled): ", check_not_resampled, " ||\n"))
  }
}


clean_up <- function(nid, lbd=FALSE, gbd=FALSE) {

  
  if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "01_processed")))     system(paste0(SYSTEM_COMMAND))))
  if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "log/details")))   system(paste0(SYSTEM_COMMAND))))
  if (gbd) if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "02_tabulated/gbd"))) system(paste0(SYSTEM_COMMAND))))
  if (lbd) if (paste0(nid, ".csv") %in% list.files(file.path(extraction_root, "02_tabulated/lbd"))) system(paste0(SYSTEM_COMMAND))))

}



get_dpt3_timeliness_ratio <- function(data) {

  
  data[, dpt3_timeliness_ratio := NA]

  
  if ("card_dpt3_day" %in% names(data)) data[, card_dpt3_day := as.integer(card_dpt3_day)]
  data[, card_dpt3_month := as.integer(card_dpt3_month)]
  data[, card_dpt3_year := as.integer(card_dpt3_year)]

  
  data[birth_year == card_dpt3_year, dpt3_timeliness_ratio := TRUE]

  
  data[(birth_year + 1) < card_dpt3_year, dpt3_timeliness_ratio := FALSE]

  
  data[((birth_year + 1) == card_dpt3_year) & (((12 - birth_month) + card_dpt3_month) < 12), dpt3_timeliness_ratio := TRUE]  
  data[((birth_year + 1) == card_dpt3_year) & (((12 - birth_month) + card_dpt3_month) > 12), dpt3_timeliness_ratio := FALSE] 
  if (all(c("card_dpt3_day", "birth_day") %in% names(data))) {                                                               
    data[((birth_year + 1) == card_dpt3_year) & ((12 - birth_month) + card_dpt3_month == 12) & (card_dpt3_day >= birth_day), dpt3_timeliness_ratio := FALSE]
    data[((birth_year + 1) == card_dpt3_year) & ((12 - birth_month) + card_dpt3_month == 12) & (card_dpt3_day < birth_day),  dpt3_timeliness_ratio := TRUE]
  }

  
  return(data)
}



prepare_birth_dates <- function(data) {
  
  contains_birth_month <- "birth_month" %in% names(data)
  contains_birth_year  <- "birth_year"  %in% names(data)

  
  if (!contains_birth_month) {
    data <- cbind(data, data.table("birth_month" = NA_integer_))
  }
  if (!contains_birth_year) {
    data <- cbind(data, data.table("birth_year" = NA_integer_))
  }

  
  date_columns <- c("age_month", "int_year", "int_month")
  if (all(date_columns %in% names(data))) {

    
    data[, age_month := floor(age_month)]

    
    data[is.na(birth_year) & !is.na(age_month) & !is.na(int_year) & !is.na(int_month),
         birth_year := ifelse(age_month < int_month,
                              int_year,
                              ifelse((age_month %% 12) == (int_month %% 12),
                                     int_year + (((int_month - age_month) %/% 12) - 1),
                                     int_year + ((int_month - age_month) %/% 12)))]

    
    data[is.na(birth_month) & !is.na(age_month) & !is.na(int_year) & !is.na(int_month),
         birth_month := ifelse((age_month %% 12) == (int_month %% 12),
                               12,
                               (int_month - age_month) %% 12)]
  }
  return(data)
}


create_data_drop_table <- function(svy_id, antigens){
  data_drop_table <- data.table(nid                  = svy_id,
                                total                = NA_integer_,
                                n_with_age_data      = NA_integer_,
                                n_0_59_months        = NA_integer_,
                                antigen              = antigens,
                                n                    = NA_integer_,
                                n_after_geomatch     = NA_integer_,
                                resampled            = FALSE,
                                n_after_resample     = NA_integer_,
                                n_after_outlier      = NA)
  return(data_drop_table)
}







vax_info  <- fread("FILEPATH/vaccine_info_lookup.csv")
vaccines  <- vax_info$vaccine


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




process <- function(nid) {

  
  
  cat(paste0("======================================================\n||   PROCESS: ", nid, "\n||-- Load data\n"))

  
  data <- load_nid(nid, folder="raw")

  
  save_datestamp()

  
  
  current_year <- as.integer(format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y"))

  
  is_mics <- unique(data$survey_name) %in% c("UNICEF_MICS")

  
  drop_measles_sia <- TRUE

  
  calculate_age_at_vax <- FALSE

  
  all_stems              <- unique(vax_info$vaccine_stem)
  multi_dose_vax_stems   <- vax_info[multi_dose_vaccine == TRUE,  unique(vaccine_stem)]
  single_dose_vax_stems  <- vax_info[multi_dose_vaccine == FALSE, unique(vaccine_stem)]

  
  buddhist_nids <- c(12732,
                     148649,
                     296646,
                     331377)

  
  
  cat("||-- Prep Data\n")

  
  data <- prepare_birth_dates(data)

  
  vaccine_stems_in_dataset <- c()
  for (stem in all_stems){
    max            <- unique(vax_info[vaccine_stem == stem, dose_complete])
    possible_doses <- unlist(ifelse(max > 1, list(1:max), ""))
    possible_vaccine_columns <- c(paste0("recall_", stem, "_ever"),
                                  paste0("recall_", stem, "_times"),
                                  paste0("card_", stem, possible_doses, "_day"),
                                  paste0("card_", stem, possible_doses, "_month"),
                                  paste0("card_", stem, possible_doses, "_year"),
                                  paste0("card_", stem, possible_doses, "_dmy"),
                                  paste0("card_", stem, possible_doses, "_date"),
                                  paste0("response_", stem, possible_doses))
    if (length(possible_vaccine_columns[possible_vaccine_columns %in% names(data)]) > 0) {
      vaccine_stems_in_dataset <- c(vaccine_stems_in_dataset, stem)
    }
  }
  vaccines_all_in_dataset <- vaccines[grep(paste(vaccine_stems_in_dataset, collapse="|"), vaccines)]

  
  if (is_mics){
    vacc_list <- c()
    for (vax in vaccines_all_in_dataset) {
      stem             <- vax_info[vaccine == vax, vaccine_stem]
      recall_columns   <- c(paste0("recall_", stem, "_ever"),
                            paste0("recall_", stem, "_times"))
      card_columns     <- c(paste0("card_", vax, "_day"),
                            paste0("card_", vax, "_date"))
      response_columns <- c(paste0("response_", vax))

      recall_columns_in_data   <- recall_columns[recall_columns %in% names(data)]
      card_columns_in_data     <- card_columns[card_columns %in% names(data)]
      response_columns_in_data <- response_columns[response_columns %in% names(data)]

      if ((length(recall_columns_in_data) == 2 & length(card_columns_in_data) > 0) | length(response_columns_in_data) > 0) {
        vacc_list <- c(vacc_list, vax)
      }
      if (vax %in% c("dpt1", "pcv1", "polio1", "mcv1", "bcg", "hepb1", "hib1", "rota1", "yfv", "pent1", "tetra1", "mmr1", "rcv1")){
        if ((paste0("recall_", stem, "_ever") %in% names(data) & length(card_columns_in_data) > 0) |
            length(response_columns_in_data) > 0) {
          vacc_list <- c(vacc_list, vax)
        }
      }
    }
    vaccines_all_in_dataset <- unique(vacc_list)

    
    get_dropped_vaccine_stems <- function(stem) {
      if(any(grepl(stem, vaccines_all_in_dataset)) == TRUE) {
        return(NULL)
      } else {
        return(stem)
      }
    }
    vaccine_stems_dropped <- unlist(lapply(vaccine_stems_in_dataset, get_dropped_vaccine_stems))

    
    if (length(vaccine_stems_dropped) > 0) {
      cat(paste0("||---- Vaccines being dropped due to indicator missingness (MICS): ", paste(vaccine_stems_dropped, collapse = ", "), "\n"))
      vaccine_stems_in_dataset <- vaccine_stems_in_dataset[!vaccine_stems_in_dataset %in% vaccine_stems_dropped]
    }
  }

  
  if (length(vaccine_stems_in_dataset) == 0) {
    cat("\nnovaxind", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
    stop("No vaccine indicators in dataset; not moving on to data processing")
  }

  
  if (drop_measles_sia) {
    measles_campaign_columns <- names(data)[grepl("recall_measles_campaign", names(data))]
    if (length(measles_campaign_columns) > 0) {
      measles_campaign_equals_1_expression <- paste(paste0(measles_campaign_columns, " == 1 "), collapse = "| ")
      if("card_mcv1_day" %in% names(data))  data[card_mcv1_day  == 66 & eval(parse(text = measles_campaign_equals_1_expression)), c("card_mcv1_day", "card_mcv1_month", "card_mcv1_year") := NA]
      if("card_mcv2_day" %in% names(data))  data[card_mcv2_day  == 66 & eval(parse(text = measles_campaign_equals_1_expression)), c("card_mcv2_day", "card_mcv2_month", "card_mcv2_year") := NA]

      if("card_mcv1_date" %in% names(data)) data[card_mcv1_date == 66 & eval(parse(text = measles_campaign_equals_1_expression)), card_mcv1_date := NA]
      if("card_mcv2_date" %in% names(data)) data[card_mcv2_date == 66 & eval(parse(text = measles_campaign_equals_1_expression)), card_mcv2_date := NA]
    }
  }


  
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

  


  
  
  cat("||-- Dose Determination\n")

  
  for (vacc in vaccines_all_in_dataset) {

    
    cat(paste0("||---- ", vacc, "\n"))

    
    
    

    
    multi_dose_vaccine <- vax_info[vaccine == vacc, multi_dose_vaccine]
    vacc_stem          <- vax_info[vaccine == vacc, vaccine_stem] %>% unique
    dose_num           <- vax_info[vaccine == vacc, dose]
    dose_stem          <- ifelse(multi_dose_vaccine, dose_num, "")

    
    for (var in paste0(vacc_stem, c("_dose", "_card", "_ever", "_dose_from_card", "_dose_from_recall"))) {
      if (!var %in% names(data)) {
        data[, (var) := NA_integer_]
      }
    }

    if (!is_mics){

      
      
      
      

      
      
      if (paste0("recall_", vacc_stem, "_ever") %in% names(data) & "recall_ever_true" %in% names(data) & "recall_ever_false" %in% names(data)) {
        data[, paste0("recall_", vacc_stem, "_ever") := as.character(get(paste0("recall_", vacc_stem, "_ever")))]
        data[get(paste0("recall_", vacc_stem, "_ever")) %in% recall_ever_true_code,  paste0(vacc_stem, "_ever") := 1]
        data[get(paste0("recall_", vacc_stem, "_ever")) %in% recall_ever_false_code, paste0(vacc_stem, "_ever") := 0]
      }

      
      
      recall_doses_variable <- paste0("recall_", vacc_stem, "_times")
      if (recall_doses_variable %in% names(data)) {

        
        data[, (recall_doses_variable) := as.numeric(get(recall_doses_variable))]

        
        if ("recall_times_missing" %in% names(data)) {
          data[(!is.na(get(recall_doses_variable))) & (get(recall_doses_variable) %in% as.numeric(recall_times_missing_code)),
               (recall_doses_variable) := NA_integer_]
        }

        
        data[(is.na(get(paste0(vacc_stem, "_dose"))) | get(recall_doses_variable) > get(paste0(vacc_stem, "_dose"))) & get(recall_doses_variable) <= 7,
             c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_recall")) := as.integer(get(recall_doses_variable))]
      }



      
      
      
      

      card_vacc_root <- paste0("card_", vacc_stem, dose_stem)

      
      for (date_var in c("date")) {

        
        variable <- paste0(card_vacc_root, "_", date_var)

        if (variable %in% names(data)) {

          
          data[, (variable) := as.character(get(variable))]
          codes <- c("NA", ".", "")
          for (card_val in c("missing", "recall")) {
            if (paste0("card_", card_val) %in% names(data)) codes <- c(codes, get(paste0("card_", card_val, "_code")))
          }
          data[get(variable) %in% unique(codes), (variable) := NA_character_]

          
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

      
      for (date_var in c("day", "month", "year")) {
        
        variable <- paste0(card_vacc_root, "_", date_var)
        if (variable %in% names(data)) {
          
          data[, (variable) := as.character(get(variable))]
          codes <- c("NA", ".", "")
          for (card_val in c("missing", "nodate", "recall")) {
            if (paste0("card_", card_val) %in% names(data)) codes <- c(codes, get(paste0("card_", card_val, "_code")))
          }
          data[get(variable) %in% unique(codes), (variable) := NA_character_]

          
          data[, (variable) := as.integer(get(variable))]

          
          if (unique(data$nid) %in% buddhist_nids & date_var=="year") data[, (variable) := get(variable) - 543]

          
          if (date_var=="day")   data[get(variable) > 31, (variable) := NA_integer_]
          if (date_var=="month") data[get(variable) > 12, (variable) := NA_integer_]
          if (date_var=="year")  data[get(variable) > unique(as.integer(data$year_end)), (variable) := NA_integer_]
        }
      }


      
      if (paste0(card_vacc_root, "_month") %in% names(data) & paste0(card_vacc_root, "_year") %in% names(data)) {

        
        if (dose_num == 1) {
          data[(get(paste0(card_vacc_root, "_month")) == 0 | get(paste0(card_vacc_root, "_year")) == 0) & get(paste0(vacc_stem, "_card")) == 1, paste0(vacc_stem, "_dose_from_card") := 0]
          data[(get(paste0(card_vacc_root, "_month")) == 0 | get(paste0(card_vacc_root, "_year")) == 0) & is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := 0]
        }

        
        data[!is.na(get(paste0(card_vacc_root, "_month"))) & get(paste0(card_vacc_root, "_month")) != 0 &
               !is.na(get(paste0(card_vacc_root, "_year"))) & get(paste0(card_vacc_root, "_year")) != 0,
             c(paste0(vacc_stem, "_card"), paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list(1, (dose_num), (dose_num))]
      }


      
      if (paste0(card_vacc_root, "_date") %in% names(data)) {

        if (all(c(paste0(card_vacc_root, "_month"), paste0(card_vacc_root, "_year")) %in% names(data))) {

          
          data[is.na(get(paste0(card_vacc_root, "_month"))) & is.na(get(paste0(card_vacc_root, "_year"))) &
                 !is.na(get(paste0(card_vacc_root, "_date"))),
               paste0(vacc_stem, "_card") := 1]
          data[is.na(get(paste0(card_vacc_root, "_month"))) & is.na(get(paste0(card_vacc_root, "_year"))) & get(paste0(vacc_stem, "_card")) == 1 &
                 !is.na(get(paste0(card_vacc_root, "_date"))) & get(paste0(card_vacc_root, "_date")) != 0,
               c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list((dose_num), (dose_num))]
          if (dose_num == 1) {
            data[is.na(get(paste0(card_vacc_root, "_month"))) & is.na(get(paste0(card_vacc_root, "_year"))) & get(paste0(card_vacc_root, "_date")) == 0 & get(paste0(vacc_stem, "_card")) == 1,
                 paste0(vacc_stem, "_dose_from_card") := 0]
            data[is.na(get(paste0(vacc_stem, "_dose"))) & is.na(get(paste0(card_vacc_root, "_month"))) & is.na(get(paste0(card_vacc_root, "_year"))) & get(paste0(card_vacc_root, "_date")) == 0,
                 paste0(vacc_stem, "_dose") := 0]
          }

        } else {

          
          data[!is.na(get(paste0(card_vacc_root, "_date"))), paste0(vacc_stem, "_card") := 1]
          data[!is.na(get(paste0(card_vacc_root, "_date"))) & get(paste0(card_vacc_root, "_date")) != 0 & get(paste0(vacc_stem, "_card")) == 1,
               c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list((dose_num), (dose_num))]
          if (dose_num == 1) {
            data[get(paste0(card_vacc_root, "_date")) == 0 & get(paste0(vacc_stem, "_card")) == 1, paste0(vacc_stem, "_dose_from_card") := 0]
            data[get(paste0(card_vacc_root, "_date")) == 0 & is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := 0]
          }
        }
      }
    }

    if (is_mics){

      
      
      
      
      
      card_vacc_root       <- paste0("card_", vacc_stem, dose_stem)
      recall_ever_present  <- paste0("recall_", vacc_stem, "_ever") %in% names(data)
      recall_times_present <- paste0("recall_",vacc_stem, "_times") %in% names(data)

      
      
      recall_is_present <- paste0("recall_", vacc_stem, "_ever") %in% names(data)

      
      if (dose_num == 1) {
        
        if (recall_is_present){
          data[as.character(get(paste0("recall_", vacc_stem, "_ever"))) %in% recall_ever_false_code,
               paste0(vacc_stem, "_dose_from_recall") := 0]
        }
      }

      
      if (paste0(card_vacc_root, "_day") %in% names(data)) {
        if (recall_ever_present) {
          if (recall_times_present) {
            
            if (dose_num == 1) {
              
              data[(get(paste0(card_vacc_root, "_day")) == 66 | get(paste0("recall_", vacc_stem, "_ever")) == 1),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num == 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 &
                                                                  get(paste0("recall_",vacc_stem, "_times"))) == dose_num,
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num > 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 &
                                                                  get(paste0("recall_",vacc_stem, "_times")) >= dose_num &
                                                                  get(paste0("recall_", vacc_stem, "_times")) <= 7),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            }
          } else {
            
            if (dose_num == 1) {
              
              data[(get(paste0(card_vacc_root, "_day")) == 66 | get(paste0("recall_", vacc_stem, "_ever")) == 1),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num == 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66,
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num > 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66,
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            }
          }
        } else {
          if (recall_times_present) {
            
            if (dose_num == 1) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66,
                    c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num == 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66 | (get(paste0("recall_", vacc_stem, "_times"))) == dose_num,
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num > 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66 | (get(paste0("recall_",vacc_stem, "_ever")) == 1 &
                                                                  get(paste0("recall_",vacc_stem, "_times")) >= dose_num &
                                                                  get(paste0("recall_", vacc_stem, "_times")) <= 7),
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            }
          } else {
            
            if (dose_num == 1) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66,
                    c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num == 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66,
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

            } else if (dose_num > 2) {
              
              data[get(paste0(card_vacc_root, "_day")) == 66,
                   c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
            }
          }
        }
      } else {

        
        if (paste0(card_vacc_root, "_date") %in% names(data)) {
          if (recall_ever_present) {
            if (recall_times_present) {
              
              if (dose_num == 1) {
                
                data[get(paste0("recall_",vacc_stem, "_ever")) == 1,
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

              } else if (dose_num == 2) {
                
                data[(get(paste0("recall_",vacc_stem, "_ever")) == 1 & paste0("recall_",vacc_stem, "_times") == dose_num),
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

              } else if (dose_num > 2) {
                
                data[(get(paste0("recall_", vacc_stem, "_ever")) == 1 & get(paste0("recall_",vacc_stem, "_times")) >= dose_num & get(paste0("recall_",vacc_stem, "_times")) <= 7),
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
              }
            } else {
              
              if (dose_num == 1) {
                
                data[(get(paste0("recall_",vacc_stem, "_ever")) == 1),
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
              }
            }
          } else {
            if (recall_times_present) {
              
              if (dose_num == 1) {
                
                data[(paste0("recall_",vacc_stem, "_times") == dose_num),
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

              } else if (dose_num == 2) {
                
                data[(paste0("recall_",vacc_stem, "_times") == dose_num),
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]

              } else if (dose_num > 2) {
                
                data[(get(paste0("recall_",vacc_stem, "_times")) >= dose_num & get(paste0("recall_",vacc_stem, "_times")) <= 7),
                     c(paste0(vacc_stem, "_dose_from_recall")) := list((dose_num))]
              }
            }
          }
        }
      }


      
      
      
      

      card_vacc_root <- paste0("card_", vacc_stem, dose_stem)

      
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

        if (dose_num == 1) {
          
          data[get(paste0(card_vacc_root, "_day")) == 0 & is.na(get(paste0(vacc_stem, "_dose"))),
               c(paste0(vacc_stem, "_card"), paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list(1, 0, 0)]
        }

        
        data[(!is.na(get(paste0(card_vacc_root, "_day"))) & get(paste0(card_vacc_root, "_day")) != 0) & ((get(paste0(card_vacc_root, "_day")) >= 1 & get(paste0(card_vacc_root, "_day")) <= 31) | get(paste0(card_vacc_root, "_day")) == 44),
             c(paste0(vacc_stem, "_card"), paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list(1, (dose_num), (dose_num))]
      }

      
      if (paste0(card_vacc_root, "_date") %in% names(data)) {

        if (c(paste0(card_vacc_root, "_day")) %in% names(data)) {
          
          data[is.na(get(paste0(card_vacc_root, "_day"))) & !is.na(get(paste0(card_vacc_root, "_date"))),
               paste0(vacc_stem, "_card") := 1]
          data[is.na(get(paste0(card_vacc_root, "_day"))) & get(paste0(vacc_stem, "_card")) == 1 &
                 !is.na(get(paste0(card_vacc_root, "_date"))) & get(paste0(card_vacc_root, "_date")) != 0,
               c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list((dose_num), (dose_num))]
          if (dose_num == 1) {
            
            
            data[is.na(get(paste0(vacc_stem, "_dose"))) & is.na(get(paste0(card_vacc_root, "_day"))) & get(paste0(card_vacc_root, "_date")) == 0,
                 paste0(vacc_stem, "_dose") := 0]
          }

        } else {
          
          data[!is.na(get(paste0(card_vacc_root, "_date"))), paste0(vacc_stem, "_card") := 1]
          data[!is.na(get(paste0(card_vacc_root, "_date"))) & get(paste0(card_vacc_root, "_date")) != 0 & get(paste0(vacc_stem, "_card")) == 1,
               c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card")) := list((dose_num), (dose_num))]
          if (dose_num == 1) {
            
            data[get(paste0(card_vacc_root, "_date")) == 0 & is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := 0]
          }
        }
      }
    }


    
    
    

    
    if (calculate_age_at_vax) {

      
      age_vars <- c(paste0(card_vacc_root, "_month"), paste0(card_vacc_root, "_year"), "birth_month", "birth_year")
      if (all(age_vars %in% names(data))) {

        
        for (age_var in age_vars) if (!class(data[, get(age_var)]) %in% c("integer", "numeric")) data[, (age_var) := get(age_var) %>% as.integer]

        
        if (sum(!is.na(data[, get(paste0(card_vacc_root, "_month"))])) > 0 &
            sum(!is.na(data[, get(paste0(card_vacc_root, "_year"))]))  > 0) {

          
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

          
          data[, paste0(vacc_stem, dose_stem, "_age_month") := (12 * (get(paste0(card_vacc_root, "_year")) - 1900) + get(paste0(card_vacc_root, "_month")))
               - (12 * (birth_year - 1900) + birth_month)]
        }
      }
    }


    
    
    

    
    
    response_vacc_root <- paste0("response_", vacc_stem, dose_stem)
    if (response_vacc_root %in% names(data)) {

      
      data[, (response_vacc_root) := as.character(get(response_vacc_root))]

      
      if ("response_yes_card" %in% names(data)) {
        
        data[get(response_vacc_root) %in% response_yes_card_code, paste0(vacc_stem, "_dose") := (dose_num)]
        
        data[get(response_vacc_root) %in% response_yes_card_code, paste0(vacc_stem, "_card") := 1]
        data[get(response_vacc_root) %in% response_yes_card_code, paste0(vacc_stem, "_dose_from_card") := (dose_num)]
      }

      if ("response_yes_recall" %in% names(data)) {
        
        
        data[get(response_vacc_root) %in% response_yes_recall_code, paste0(vacc_stem, "_dose") := (dose_num)]
        
        data[get(response_vacc_root) %in% response_yes_recall_code, paste0(vacc_stem, "_dose_from_recall") := (dose_num)]

      }

      
      if ("response_no" %in% names(data) & dose_num == 1) {
        
        
        data[get(response_vacc_root) %in% response_no_code, paste0(vacc_stem, "_dose") := 0]

        data[get(response_vacc_root) %in% response_no_code & is.na(get(paste0(vacc_stem, "_dose_from_recall"))), paste0(vacc_stem, "_dose_from_recall") := 0]
        
        if ("has_vacc_card" %in% names(data)) {
          data[get(response_vacc_root) %in% response_no_code & has_vacc_card == "seen", paste0(vacc_stem, "_card") := 1]
          data[get(response_vacc_root) %in% response_no_code & has_vacc_card == "seen" & is.na(get(paste0(vacc_stem, "_dose_from_card"))), paste0(vacc_stem, "_dose_from_card") := 0]
        }
      }
    }

    
    
    

    
    data[(is.na(get(paste0(vacc_stem, "_card"))) | get(paste0(vacc_stem, "_card")) != 1) & !is.na(get(paste0(vacc_stem, "_dose"))),
         paste0(vacc_stem, "_card") := 0]

    
    if ("ever_vaccinated" %in% names(data)) {
      data[, ever_vaccinated := as.integer(ever_vaccinated)]
      data[is.na(get(paste0(vacc_stem, "_dose"))) & ever_vaccinated == 0, paste0(vacc_stem, "_dose") := 0]
      data[is.na(get(paste0(vacc_stem, "_dose_from_recall"))) & ever_vaccinated == 0, paste0(vacc_stem, "_dose_from_recall") := 0]
    }
  }


  
  
  

  
  if (is_mics){
    for (vacc_stem in all_stems) {
      if (paste0(vacc_stem, "_dose") %in% names(data)) {
        data[,paste0(vacc_stem, "_dose") := NA_integer_]
        data[!is.na(get(paste0(vacc_stem, "_dose_from_card"))) | !is.na(get(paste0(vacc_stem, "_dose_from_recall"))), paste0(vacc_stem, "_dose") := 0]
        
        data[is.na(get(paste0(vacc_stem, "_dose_from_card"))) & !is.na(get(paste0(vacc_stem, "_dose_from_recall"))), paste0(vacc_stem, "_dose") := get(paste0(vacc_stem, "_dose_from_recall"))]
        data[!is.na(get(paste0(vacc_stem, "_dose_from_card"))) & is.na(get(paste0(vacc_stem, "_dose_from_recall"))), paste0(vacc_stem, "_dose") := get(paste0(vacc_stem, "_dose_from_card"))]
        
        data[(get(paste0(vacc_stem, "_dose_from_recall")) >= get(paste0(vacc_stem, "_dose_from_card"))) & get(paste0(vacc_stem, "_dose")) == 0, paste0(vacc_stem, "_dose") := get(paste0(vacc_stem, "_dose_from_recall"))]
        data[(get(paste0(vacc_stem, "_dose_from_recall")) < get(paste0(vacc_stem, "_dose_from_card"))) & get(paste0(vacc_stem, "_dose")) == 0, paste0(vacc_stem, "_dose") := get(paste0(vacc_stem, "_dose_from_card"))]
      }
    }
  }


  
  
  

  
  for (vacc_stem in vaccine_stems_in_dataset) {

    
    for (other_card_hints in vaccine_stems_in_dataset[vaccine_stems_in_dataset != vacc_stem]) {
      if (all(c(paste0(other_card_hints, "_dose"), paste0(other_card_hints, "_card"), paste0(other_card_hints, "_dose_from_card")) %in% names(data))) {
        
        data[!is.na(get(paste0(other_card_hints, "_dose"))) & !is.na(get(paste0(other_card_hints, "_dose_from_card"))) & get(paste0(other_card_hints, "_card")) == 1,
             paste0(vacc_stem, "_card") := 1]
        
        data[!is.na(get(paste0(other_card_hints, "_dose"))) & !is.na(get(paste0(other_card_hints, "_dose_from_card"))) & get(paste0(other_card_hints, "_card")) == 1 &
               is.na(get(paste0(vacc_stem, "_dose"))), paste0(vacc_stem, "_dose") := 0]
        
        data[!is.na(get(paste0(other_card_hints, "_dose"))) & !is.na(get(paste0(other_card_hints, "_dose_from_card"))) & get(paste0(other_card_hints, "_card")) == 1 &
               is.na(get(paste0(vacc_stem, "_dose_from_card"))), paste0(vacc_stem, "_dose_from_card") := 0]
      }
    }

    
    if ("has_vacc_card" %in% names(data) & !is_mics) {
      data[is.na(get(paste0(vacc_stem, "_dose"))) &                
             has_vacc_card == "seen",                              
           c(paste0(vacc_stem, "_dose"), paste0(vacc_stem, "_dose_from_card"), paste0(vacc_stem, "_card")) := list(0, 0, 1)]
    }
  }

  


  
  
  cat("||-- Distribute Multi-Vaccines\n")

  
  if("pent" %in% vaccine_stems_in_dataset){

    unconventional_penta_filepath  <- "FILEPATH/penta_unconventional_component.csv"
    unconventional_penta_countries <- fread(unconventional_penta_filepath)

    if(unique(data$nid) %in% unconventional_penta_countries$survey_id){
      contains_hepb <- unconventional_penta_countries[survey_id == nid, HepB] == 1
      contains_hib  <- unconventional_penta_countries[survey_id == nid, HiB]  == 1
      contains_ipv  <- unconventional_penta_countries[survey_id == nid, IPV]  == 1

      if(contains_hepb & !contains_hib){
        pent_vaccines <- c("dpt", "polio", "hepb")
      } else if (contains_hib & !contains_hepb){
        pent_vaccines <- c("dpt", "polio", "hib")
      } else {
        cat(paste0("Dataset contains pentavalent vaccine comprised of unconventional component vaccines. Code is not currently designed
                      to account for this configuration of component vaccines. Please make sure document containing data on unconventional
                      pentavalent vaccines is up-to-date at: ", unconventional_penta_filepath, ". Stopping...\n"))
        stop()
      }
    } else {
      pent_vaccines <- c("dpt", "hib", "hepb")
    }
  }
  tetra_vaccines <- c("dpt", "hepb")
  mmr_vaccines   <- c("mcv", "rcv")

  
  for (multi_vacc in c("pent", "tetra", "mmr")) {

    if (multi_vacc %in% vaccine_stems_in_dataset) {

      
      cat(paste0("||---- ", multi_vacc, "\n"))

      
      
      
      

      for (col_in_loop in c("_dose", "_dose_from_card", "_dose_from_recall")) {
        
        multi_vacc_column <- paste0(multi_vacc, col_in_loop)
        if (multi_vacc_column %in% names(data)) {
          
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

      
      
      
      

      if (paste0(multi_vacc, "_card") %in% names(data)) {
        
        for (vacc in get(paste0(multi_vacc, "_vaccines"))) {
          
          if (paste0(vacc, "_card") %in% names(data)) {
            data[!is.na(paste0(multi_vacc, "_card")) & is.na(get(paste0(vacc, "_card"))), (paste0(vacc, "_card")) := get(paste0(multi_vacc, "_card"))]
          } else {
            data[!is.na(paste0(multi_vacc, "_card")), (paste0(vacc, "_card")) := get(paste0(multi_vacc, "_card"))]
          }
        }
      }

      
      
      
      

      if (calculate_age_at_vax) {
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
      }
    }
  }

  


  

  
  outvacc <- c()
  for (vacc_stem in all_stems) {
    if (paste0(vacc_stem, "_dose") %in% names(data)) {

      
      max <- vax_info[vaccine_stem == vacc_stem, unique(dose_complete)]
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


  if (!is_mics){

    
    
    for (vacc_stem in all_stems) {

      
      if (grepl(vacc_stem, paste0(single_dose_vax_stems, collapse="|"))) {
        first_dose_stem <- ""
      } else {
        first_dose_stem <- 1
      }

      if (paste0(vacc_stem, "_ever") %in% names(data)) {

        
        for (var in paste0(vacc_stem, c("_dose", "_card", "_ever", "_dose_from_card", "_dose_from_recall"))) {
          if(!var %in% names(data)) {
            data[, (var) := NA_integer_]
          }
        }

        
        
        data[is.na(get(paste0(vacc_stem, "_dose"))) & get(paste0(vacc_stem, "_ever")) == 0, c(paste0(vacc_stem, first_dose_stem), paste0(vacc_stem, "_dose")) := list(0, 0)]
        data[is.na(get(paste0(vacc_stem, "_dose_from_recall"))) & get(paste0(vacc_stem, "_ever")) == 0, paste0(vacc_stem, "_dose_from_recall") := 0]
        
        if (nrow(data[(is.na(get(paste0(vacc_stem, "_dose"))) | get(paste0(vacc_stem, "_dose")) == 0) & get(paste0(vacc_stem, "_ever")) == 1]) > 0) data[, paste0(vacc_stem, "_dose") := as.character(get(paste0(vacc_stem, "_dose")))]
        data[(is.na(get(paste0(vacc_stem, "_dose"))) | get(paste0(vacc_stem, "_dose")) == "0") & get(paste0(vacc_stem, "_ever")) == 1, c(paste0(vacc_stem, first_dose_stem), paste0(vacc_stem, "_dose")) := list(1, "At least 1")]
        if (nrow(data[(is.na(get(paste0(vacc_stem, "_dose_from_recall"))) | get(paste0(vacc_stem, "_dose_from_recall")) == 0) & get(paste0(vacc_stem, "_ever")) == 1]) > 0) data[, paste0(vacc_stem, "_dose_from_recall") := as.character(get(paste0(vacc_stem, "_dose_from_recall")))]
        data[(is.na(get(paste0(vacc_stem, "_dose_from_recall"))) | get(paste0(vacc_stem, "_dose_from_recall")) == "0") & get(paste0(vacc_stem, "_ever")) == 1, paste0(vacc_stem, "_dose_from_recall") := "At least 1"]

        
        data[(is.na(get(paste0(vacc_stem, "_card"))) | get(paste0(vacc_stem, "_card")) != 1) & !is.na(get(paste0(vacc_stem, "_dose"))),
             paste0(vacc_stem, "_card") := 0]
      }
    }
  }


  

  
  if (all(c("card_dpt3_month", "card_dpt3_year") %in% names(data))) {
    data <- get_dpt3_timeliness_ratio(data)
  }

  
  if ("rota" %in% vaccine_stems_in_dataset|
      "rotarix" %in% vaccine_stems_in_dataset|
      "rotateq" %in% vaccine_stems_in_dataset) data <- make_rotac(data)

  

  
  cat("||-- Dropping data with age missingness or outside age-range\n")

  
  if ("age_year" %!in% names(data)) data[, age_year := NA_integer_]
  if (class(data$age_year) %!in% c("numeric", "integer")) data[, age_year := as.numeric(age_year)]
  if ("age_month" %in% names(data)) {
    if (class(data$age_month) %!in% c("numeric", "integer")) data[, age_month := as.numeric(age_month)]
    data[is.na(age_year) & !is.na(age_month), age_year := age_month / 12]
  }

  
  n_total          <- nrow(data)
  n_age_data_drop  <- data[is.na(age_year), .N]
  n_age_range_drop <- data[age_year >= 5, .N]
  non_def_ages     <- (n_age_data_drop + n_age_range_drop) / nrow(data)


  
  data             <- data[!(age_year >= 5 | is.na(age_year))]
  n_after_age_drop <- nrow(data)
  data[, age_year := floor(age_year)]

  if (nrow(data) == 0) {
    log_empty_dataset(nid, date, script = "01", step = 1, object_name = "data")
  }

  
  final_vaccines_in_data <- vax_info[vaccine %in% names(data) & multi_vaccine == FALSE, unique(vaccine)]
  data_drop_table        <- create_data_drop_table(svy_id = nid, antigens = final_vaccines_in_data)
  data_drop_table[, total           := n_total]
  data_drop_table[, n_with_age_data := n_total - n_age_data_drop]
  data_drop_table[, n_0_59_months   := n_after_age_drop]
  write.csv(data_drop_table, file = file.path(extraction_root, "FILEPATH", paste0(nid, ".csv")), row.names = FALSE)

  


  

  
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
                     "dpt3_timeliness_ratio",
                     "rotac")
  keep_cols     <- c(metadata_cols, vaccine_cols)[c(metadata_cols, vaccine_cols) %in% names(data)]
  data          <- data[, keep_cols, with=FALSE]

  
  for (ea_keep_col in keep_cols) {
    if (all(is.na(data[, get(ea_keep_col)]))) data[, (ea_keep_col) := NULL]
  }

  
  vet(data, vaccine_stems_in_dataset=vaccine_stems_in_dataset, vaccines_all_in_dataset=vaccines_all_in_dataset, age_drops=non_def_ages)

  
  archive_folder <- format(Sys.time(), "%Y_%m_%d")
  dir.create(file.path(extraction_root, "01_processed", "Archive", archive_folder), recursive = T)
  write.csv(data, file.path(extraction_root, "01_processed", "Archive", archive_folder, paste0(nid, ".csv")), row.names=FALSE)

  
  cat("||-- Save data\n")
  write.csv(data, file.path(extraction_root, "01_processed", paste0(nid, ".csv")), row.names=FALSE)
  cat(paste0("||-- Process complete: ", nid, "\n"))
  cat("******************************************************\n")

  
  return(nrow(data) > 1)
}



