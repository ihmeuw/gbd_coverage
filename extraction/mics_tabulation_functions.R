




mics_read_save <- function(...) {
  
  
  username <- Sys.info()[["user"]]
  pacman::p_load(data.table, dplyr, parallel, readxl)  
  
  
  source(paste0("FILEPATH", username, "FILEPATH/init.r"))
  source(db_tools)
  source("FILEPATH/get_location_metadata.R")
  locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round)[level >= 3]  
  
  df <- fread("FILEPATH/mics_tabulation_extractions_gbd2019final.csv")
  
  
  
  
  
  duples <- list(c("vacc_dpt1",  "vacc_tetra1"),
                 c("vacc_dpt1",  "vacc_pent1"),
                 c("vacc_dpt2",  "vacc_pent2"),
                 c("vacc_dpt2",  "vacc_tetra2"),
                 c("vacc_dpt3",  "vacc_tetra3"),
                 c("vacc_dpt3",  "vacc_pent3"),
                 c("vacc_hib3",  "vacc_pent3"),
                 c("vacc_hib3",  "vacc_tetra3"),
                 c("vacc_hepb3", "vacc_pent3"),
                 c("vacc_mcv1",  "vacc_mmr1"),
                 c("vacc_mcv2",  "vacc_mmr2"),
                 c("vacc_rcv1",  "vacc_mmr1"),
                 c("vacc_rcv2",  "vacc_mmr2"))
  for (i in duples) {
    
    df <- df[, (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
  }
  
  
  df[, ihme_loc_id := tstrsplit(location_name_short_ihme_loc_id, "[|]")[[2]] ]
  df[is.na(ihme_loc_id), ihme_loc_id := parent_location]

  
  id <- c("nid", "file_path", "ihme_loc_id", "year_start", "year_end", "age_start", "age_end", "sample_size")
  vacc <-  grep("vacc", names(df), value=TRUE)
  id <- c("nid", "file_path", "ihme_loc_id", "year_start", "year_end", "age_start", "age_end", "sample_size")
  df <- melt(df, id=id, measure=patterns("^vacc|^maternal"), value.name="data", variable.name="me_name")
  
  
  
  
  
  
  df <- df[!is.na(data)]
  
  
  df[, data := as.numeric(data) / 100]
  
  df[data > 1, data := 1]
  
  
  df <- df[!is.na(ihme_loc_id)]
  
  df[, sample_size := gsub(",", "", sample_size) %>% as.numeric]
  df[sample_size > 5000, sample_size := NA]
  
  df[, `:=` (age_start = round(age_start / 12), 
             age_end   = round(age_end / 12))]
  df[, age_length := age_end - age_start]
  df[age_length %in% c(0, 1), age_year := age_start]
  df[is.na(age_length), age_year := 1]
  df[age_length > 1, age_year := 1]
  
  df[, year_id := year_start-age_year]  
  df[, c("year_start", "year_end") := NULL]
  
  parsed <- df[, tstrsplit(file_path, "_", fixed=TRUE)]
  parsed <- parsed[, survey_name := ifelse(nchar(V3)==3, paste0(V3, "/", V4), V3)]
  df <- cbind(df, parsed$survey_name)
  setnames(df, "V2", "survey_name")
  df <- df[survey_name=="MICS", survey_name := "UNICEF_MICS"]
  
  
  
  
  saveRDS(df, paste0(data_root, "FILEPATH/mics_tabulations.rds"))
  print("MICS report tabulations prepped and saved for use in GBD pipeline")
  
  
  return(df)
}



saved_tabs <- mics_read_save()










mics_timeliness_reports <- function(...) {
  
  
  pacman::p_load(data.table, dplyr, parallel, readxl)  
  
  
  source(paste0(j, "FILEPATH/init.r"))
  source(db_tools)
  source("FILEPATH/get_location_metadata.R")
  locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round)[level >= 3]  
  
  df <- fread("FILEPATH/mics_timeliness_reports_12192019.csv")
  
  
  
  
  
  setnames(df, c("parent_location.x", "timeliness_dpt3"), c("ihme_loc_id", "data"))
  df[, me_name := "vacc_dpt3_timeliness_ratio"]
  
  df <- df[!is.na(data)]
  df <- df[data < 1]
  
  
  
  
  
  
  
  df <- df[!is.na(ihme_loc_id)]
  
  df[, sample_size := gsub(",", "", sample_size) %>% as.numeric]
  
  
  df[, `:=` (age_start = round(age_start / 12),
             age_end   = round(age_end / 12))]
  df[, age_length := age_end - age_start]
  df[age_length %in% c(0, 1), age_year := age_start]
  df[is.na(age_length), age_year := 1]
  df[age_length > 1, age_year := 1]

  
  df[, year_id := year_start-age_year]
  df[, c("year_start", "year_end") := NULL]
  
  
  
  
  
  
  df <- df[, survey_name := "UNICEF_MICS"]
  
  
  
  
  saveRDS(df, paste0(data_root, "FILEPATH/mics_timeliness_tabulations.rds"))
  print("MICS timeliness report tabulations prepped and saved for use in GBD pipeline")
  
  
  return(df)
}



saved_timeliness <- mics_timeliness_reports()


















