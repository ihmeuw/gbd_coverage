













username        <- Sys.info()[["user"]]
vaccine_repo    <- paste0("FILEPATH", username, "FILEPATH")
extraction_root <- "FILEPATH"
reference_repo  <- "FILEPATH"

suppressMessages(invisible(library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                                 R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))))
suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))
source("FILEPATH/load_packages.R")
suppressMessages(invisible(load_packages(c("proto", "findpython", "getopt", "survey", "gtools"))))
message("Successfully loaded packages")


source(file.path("FILEPATH", username, "FILEPATH/init.r"))
source(paste0(vaccine_repo, "FILEPATH/01_process.R"))
source(paste0(vaccine_repo, "FILEPATH/03_disaggregate_lbd.R"))



outlier_filepath           <- "FILEPATH/vaccination.csv"
vax_info_table_path        <- "FILEPATH/vaccine_info_lookup.csv"
lit_extraction_output_root <- "FILEPATH"


vax_info     <- fread(vax_info_table_path)
rename_table <- vax_info[, .("stem"          = vaccine_stem,
                             "vaccine"       = paste0("vacc_", vaccine),
                             "dose_specific" = paste0(vaccine_stem, "_dose_", dose))]




'%!in%' <- function(x,y)!('%in%'(x,y))


push_lit_extraction_update <- function() {
  wd_copy <- getwd()
  setwd(ref_data_repo)

  system(SYSTEM_COMMAND)
  system(SYSTEM_COMMAND)
  system(SYSTEM_COMMAND)

  setwd(wd_copy)
}


check_and_rename_col <- function(data, old_names, new_names) {
  
  if (length(old_names) != length(new_names)) {
    stop("Length of 'old_names' argument must equal length of 'new_names' argument")
  }
  
  for (i in 1:length(old_names)) {
    if (old_names[i] %in% names(data)) {
      setnames(data, old_names[i], new_names[i])
    }
  }
}


prepare_report_data <- function(report_data){

  
  
  message("||-- Format Data")

  
  data <- copy(report_data)

  
  column_names <- c("sample_size"     = "N_obs",
                    "nid"             = "svy_id",
                    "parent_location" = "ihme_loc_id")
  check_and_rename_col(data      = data,
                       old_names = names(column_names),
                       new_names = unname(column_names))

  
  keep_cols       <- c("svy_id", "file_path", "ihme_loc_id", "cv_admin", "location_code", "location_name_short_ihme_loc_id",
                       "shapefile", "smaller_site_unit", "site_memo", "year_start", "year_end", "age_start", "age_end", "N_obs", "cv_do_not_shift")
  vaccine_columns <- names(data)[grepl("vacc", names(data))]
  data            <- subset(data, select = c(keep_cols, vaccine_columns))

  
  data[location_code  == "", location_code := NA]
  data[shapefile      == "", shapefile := NA]
  data[N_obs          == "", N_obs := NA]
  data[svy_id         == "", svy_id := NA]
  data[site_memo      == "", site_memo := NA]
  data[location_name_short_ihme_loc_id == "", location_name_short_ihme_loc_id := NA]

  
  data$N_obs       <- suppressWarnings(as.numeric(as.character(data$N_obs)))
  data$survey_name <- "Custom"
  data$point       <- 0
  
  
  
  data[, `:=` (age_year_start = round(age_start / 12),
               age_year_end   = round(age_end / 12))]
  data[, age_length := age_year_end - age_year_start]

  data[age_length %in% c(0, 1), age_year := age_year_start]
  data[is.na(age_length), age_year := 1]
  data[age_length > 1, age_year := round((age_year_start + age_year_end)/2)]

  
  data[cv_admin==1 | cv_do_not_shift==1, year_id := year_start]
  data[is.na(cv_admin) & is.na(cv_do_not_shift), year_id := year_start-age_year]

  
  for (column in vaccine_columns) {
    suppressWarnings(data[, (column) := as.numeric(get(column))])
  }

  
  data[grepl(",", N_obs), N_obs := as.numeric(gsub(",", "", N_obs))]

  
  data <- data[!which(cv_admin == 1), ]

  
  data <- data[!is.na(year_end) & year_end > 1999, ]

  
  data <- data[!is.na(N_obs), ]

  
  data <- data[!(svy_id == 467681 & grepl("ad1", shapefile)), ] 

  
  
  message("||-- Distribute Multi-Vaccines")

  
  duples <- list(c("vacc_dpt1",  "vacc_tetra1"),
                 c("vacc_dpt1",  "vacc_pent1"),
                 c("vacc_dpt2",  "vacc_pent2"),
                 c("vacc_dpt2",  "vacc_tetra2"),
                 c("vacc_dpt3",  "vacc_tetra3"),
                 c("vacc_dpt3",  "vacc_pent3"),
                 c("vacc_hib1",  "vacc_pent1"),
                 c("vacc_hib1",  "vacc_tetra1"),
                 c("vacc_hib2",  "vacc_pent2"),
                 c("vacc_hib2",  "vacc_tetra2"),
                 c("vacc_hib3",  "vacc_pent3"),
                 c("vacc_hib3",  "vacc_tetra3"),
                 c("vacc_hepb1", "vacc_pent1"),
                 c("vacc_hepb2", "vacc_pent2"),
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
                       c("vacc_hib1",  "vacc_pent1"),
                       c("vacc_hib1",  "vacc_tetra1"),
                       c("vacc_hib2",  "vacc_pent2"),
                       c("vacc_hib2",  "vacc_tetra2"),
                       c("vacc_hib3",  "vacc_pent3"),
                       c("vacc_hib3",  "vacc_tetra3"),
                       c("vacc_polio1", "vacc_pent1"),  
                       c("vacc_polio2", "vacc_pent2"),  
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
                       c("vacc_polio1",  "vacc_pent1"),  
                       c("vacc_polio2",  "vacc_pent2"),  
                       c("vacc_polio3",  "vacc_pent3"),  
                       c("vacc_hib1",  "vacc_tetra1"),
                       c("vacc_hib2",  "vacc_tetra2"),
                       c("vacc_hib3",  "vacc_tetra3"),
                       c("vacc_hepb1", "vacc_pent1"),
                       c("vacc_hepb2", "vacc_pent2"),
                       c("vacc_hepb3", "vacc_pent3"),
                       c("vacc_mcv1",  "vacc_mmr1"),
                       c("vacc_mcv2",  "vacc_mmr2"),
                       c("vacc_rcv1",  "vacc_mmr1"),
                       c("vacc_rcv2",  "vacc_mmr2"))

  
  message("-- Account for unconentional pentavalent formulation (country-specific)")
  unconventional_penta_filepath <- file.path(ref_data_repo, "penta_unconventional_component.csv")
  alt_penta                     <- fread(unconventional_penta_filepath)
  
  alt_penta_1 <- alt_penta[DTAP==1 & HepB==0 & HiB==1 & IPV==1]
  
  alt_penta_2 <- alt_penta[DTAP==1 & HepB==1 & HiB==0 & IPV==1]

  for (i in duples_alt_1) {  
    
    data <- data[svy_id %in% unique(alt_penta_1$survey_id),
             (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
  }

  for (i in duples_alt_2) {
    
    data <- data[svy_id %in% unique(alt_penta_2$survey_id),
             (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
  }

  
  for (i in duples) {
    
    data <- data[!svy_id %in% unique(alt_penta$survey_id),
             (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
  }

  
  
  
  
  
  
  
  
  
  
  message("||-- Record Data Drops")

  
  subnational_ihme_locations <- lapply(data$location_name_short_ihme_loc_id, function(x) {
    if(!is.na(x)) {
      split_name <- strsplit(x, "\\|")[[1]][2]
      if(grepl("[0-9]", split_name)) {
        return(x)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }) %>% unlist
  data[location_name_short_ihme_loc_id %in% subnational_ihme_locations | !is.na(location_code) | !is.na(shapefile) | !is.na(site_memo), admin_level := 1]
  data[is.na(admin_level) | admin_level != 1, admin_level := 0]

  
  data <- data[admin_level == 1, ]

  
  temp       <- unique(data[, .(svy_id, location_code, shapefile, site_memo, year_start, year_end, age_start, age_end)])
  age_range  <- data.table()
  row_length <- nrow(temp)
  for(i in seq.int(row_length)) {
    
    if(i %% 100 == 0) message(paste0("||---- ", round(i / row_length, digits = 2) * 100, "%"))
    nid                             <- temp[i, svy_id]
    location_code                   <- temp[i, location_code]
    shapefile                       <- temp[i, shapefile]
    site_memo                       <- temp[i, site_memo]
    year_start                      <- temp[i, year_start]
    year_end                        <- temp[i, year_end]
    age_start                       <- temp[i, age_start]
    age_end                         <- temp[i, age_end]
    if(!is.na(age_start) & !is.na(age_end)) {
      ages       <- seq.int(age_start, age_end)
    } else {
      ages       <- NA
    }
    age_range <- rbind(age_range,
                       data.table(svy_id        = nid,
                                  location_code = location_code,
                                  shapefile     = shapefile,
                                  site_memo = site_memo,
                                  year_start    = year_start,
                                  year_end      = year_end,
                                  age_start     = age_start,
                                  age_end       = age_end,
                                  age_range     = ages),
                       fill = TRUE)
  }
  age_overlap <- age_range[, .("age_overlap" = any(duplicated(age_range))), by = .(svy_id, location_code, shapefile, site_memo, year_start, year_end)]
  svy_ids_with_age_overlap    <- age_overlap[age_overlap == TRUE, unique(svy_id), ]
  svy_ids_with_no_age_overlap <- age_overlap[svy_id %!in% svy_ids_with_age_overlap, unique(svy_id)]
  data <- merge(data, age_overlap, all.x = TRUE)

  
  data_svys_with_age_overlap <- data[svy_id %in% svy_ids_with_age_overlap, ]
  data_svys_with_age_overlap[, age_span := age_end - age_start]
  largest_age_span <- data_svys_with_age_overlap[, .("largest_age_span" = max(age_span)), by = c("svy_id", "shapefile", "location_code", "site_memo",
                                                                                                 "age_start", "age_end", "year_start", "year_end")]
  data_age_overlap <- merge(data_svys_with_age_overlap, largest_age_span, all.x = TRUE, by = c("svy_id", "shapefile", "location_code", "site_memo",
                                                                                               "age_start", "age_end", "year_start", "year_end"))
  data_age_overlap_largest_only <- data_age_overlap[age_overlap == FALSE | (age_overlap == TRUE & age_span == largest_age_span), ]
  data_age_overlap_largest_only$age_span <- NULL
  data_age_overlap_largest_only$largest_age_span <- NULL

  
  data_no_age_overlap        <- data[svy_id %in% svy_ids_with_no_age_overlap, ]
  age_overlap_corrected_data <- rbind(data_age_overlap_largest_only, data_no_age_overlap)

  
  total           <- age_overlap_corrected_data[, .("total" = sum(N_obs)), by = svy_id]
  n_with_age_data <- age_overlap_corrected_data[!is.na(age_start) & !is.na(age_end), .("n_with_age_data" = sum(N_obs)), by = svy_id]
  n_age_0_59_data <- age_overlap_corrected_data[!is.na(age_start) & !is.na(age_end) & age_end <= 60, .("n_0_59_months" = sum(N_obs)), by = svy_id]

  
  id_variables       <- c("svy_id", "file_path", "ihme_loc_id", "location_code", "location_name_short_ihme_loc_id", "shapefile", "site_memo", "year_start", "year_end", "age_start", "age_end", "year_id", "N_obs")
  data_long          <- melt.data.table(age_overlap_corrected_data, id.vars=id_variables, measure.vars = vaccine_columns, variable.name="me_name", variable.factor = F, value.name = "value")
  data_long          <- data_long[!is.na(value), ]
  n_antigen_specific <- data_long[!is.na(age_start) & !is.na(age_end) & age_end <= 60, .("n" = sum(N_obs, na.rm = TRUE)), by = c("svy_id", "me_name")]
  n_after_geomatch   <- data_long[!is.na(age_start) & !is.na(age_end) & age_end <= 60 & !is.na(shapefile) & !is.na(location_code), .("n_after_geomatch" = sum(N_obs, na.rm = TRUE)), by = c("svy_id", "me_name")]

  
  data_drop_table <- merge(merge(merge(total, n_with_age_data, all = TRUE),
                                 merge(n_age_0_59_data, n_antigen_specific, all = TRUE), all = TRUE),
                           n_after_geomatch, by = c("svy_id", "me_name"), all = TRUE)

  
  data_drop_table <- data_drop_table[!is.na(n), ]

  
  data_drop_table[!is.na(total) & is.na(n_with_age_data), n_with_age_data := 0]
  data_drop_table[!is.na(n_with_age_data) & is.na(n_0_59_months), n_0_59_months := 0]
  data_drop_table[!is.na(n_0_59_months) & is.na(n), n := 0]
  data_drop_table[!is.na(n) & is.na(n_after_geomatch), n_after_geomatch := 0]

  
  setcolorder(data_drop_table, c("svy_id", "total", "n_with_age_data", "n_0_59_months", "me_name", "n", "n_after_geomatch"))
  data_drop_table <- cbind(data_drop_table,
                           data.table(resampled            = FALSE,
                                      n_after_resample     = NA_integer_,
                                      n_after_outlier      = NA))

  
  data_drop_table[, me_name := gsub("vacc_", "", me_name)]
  setnames(data_drop_table, c("svy_id","me_name"),  c("nid", "antigen"))

  data_drop_table_archive_path <- paste0(extraction_root, "FILEPATH", gsub("-", "_", Sys.Date()), ".csv")
  data_drop_table_regular_path <- paste0(extraction_root, "FILEPATH/data_drops.csv")

  message(paste0("||---- Saving Drops: ", data_drop_table_archive_path))
  write.csv(data_drop_table, file = data_drop_table_archive_path, row.names = FALSE)
  write.csv(data_drop_table, file = data_drop_table_regular_path, row.names = FALSE)


  
  return(data)
}


add_ratios <- function(data, ratio_antigen_1, ratio_antigen_2){
  if (!all(c(ratio_antigen_1, ratio_antigen_2) %in% names(data))) {
    stop("One of the provided ratio_antigens in add_ratios is missing from the extracted report data")
  }
  ratio_name <- gsub("vacc_", "", paste0(ratio_antigen_1, "_", ratio_antigen_2, "_ratio"))
  message(paste0("||---- ", ratio_name))
  data[!is.na(get(ratio_antigen_1)) & !is.na(get(ratio_antigen_2)), (ratio_name) := get(ratio_antigen_1) / get(ratio_antigen_2)]
  data[get(ratio_name) %in% c(Inf, -Inf), (ratio_name) := NA]
  data[is.nan(get(ratio_name)), (ratio_name) := NA]
  return(data)
}







check_mismatched_age_bins <- function(data) {

  
  mismatch <- data[age_bin_agg_id == TRUE, ]
  
  if(nrow(mismatch) == 0) return(NULL)
  
  mismatch <- mismatch[, .("N_obs" = sum(N_obs, na.rm = TRUE)), by = c("svy_id", "ihme_loc_id", "year_start", "year_end", "year_id", "age_start", "age_end")]
  
  drop_countries <- c("KAZ", "MNE", "WSM", "SLB", "TUV")
  mismatch <- mismatch[!ihme_loc_id %in% drop_countries, ]
  
  mismatch <- mismatch[N_obs != 0, ]
  
  raw_files       <- list.files("FILEPATH", full.names = TRUE)
  nids            <- unique(mismatch$svy_id)
  raw_survey_size <- data.table()
  for(nid in nids) {
    
    file_path <- raw_files[grepl(paste0(nid, ".csv"), raw_files)]
    if (length(file_path) > 1) {
      file_path <- file_path[!grepl("_CH_", file_path)]
    } else if (length(file_path) == 0) {
      mismatched_age_range <- paste(unlist(mismatch[svy_id == nid, .(age_start, age_end)]), collapse = "-")
      message(paste0("\nUnable to compare sample size of mismatched age cohort ", mismatched_age_range, " from nid ", nid, " against microdata - no raw microdata available\n"))
      next
    }
    temp <- fread(file_path)
    
    age_start_age_end <- unlist(mismatch[svy_id == nid, .(age_start, age_end)])
    age_start         <- age_start_age_end[[1]]
    age_end           <- age_start_age_end[[2]]
    N                 <- temp[age_month >= age_start & age_month <= age_end, .N]
    raw_survey_size   <- rbind(raw_survey_size,
                               data.table(svy_id = nid,
                                          N_micro = N))
  }

  
  mismatch <- merge(mismatch, raw_survey_size, all = TRUE, by = "svy_id")

  
  mismatch[, diff := round(abs(N_obs - N_micro) / N_obs, digits = 2)]
  mismatch <- mismatch[order(-diff), ]
  setnames(mismatch, "N_obs", "N_report")

  
  message("\nLit data with age cohorts that don't map neatly on to the regular age cohorts are matched to overlapping age_bins. Mismatched age cohorts are signified by age_bin_agg_id == TRUE.
Later in the modeling process, the data from the mismatched age cohorts are distributed according to the distribution of the microdata. If the sample size of the microdata differ
too greatly from the sample size of the report data, this method might not be valid. Inspect mismatched data below for differences of greater than 10% between report and microdata:\n")
  print(mismatch)

  
  return(mismatch)
}


#' @title Check resample data drops

#' @description Look at the data_drops.csv produced in




check_resample_data_drops <- function() {
  resample_drop_log <- fread(file.path(extraction_root, "FILEPATH/data_drops.csv"))
  resample_drop_log <- resample_drop_log[resampled == T,]

  if (nrow(resample_drop_log) > 0) {
    resample_drop_log[, per_diff := ((n_after_resample - n_after_geomatch)/n_after_geomatch) * 100]
    if(nrow(resample_drop_log[per_diff < -30 | per_diff > 0]) > 0) {
      problem_drops <- resample_drop_log[per_diff < -30 | per_diff > 0]
      fwrite(problem_drops, file.path(extraction_root, "FILEPATH/resample_drop_issues.csv"))
      
      warning("Some resamples dropped >30% of rows or increased the number of rows. See ", file.path(extraction_root, "FILEPATH/resample_drop_issues.csv"), " for problematic rows.")
    }
  }
}



extract_report_data <- function(resample = FALSE){

  
  
  message(paste0("======================================================\n||   EXTRACT REPORT DATA\n||-- Load data"))

  
  raw_report_data      <- fread(file.path(ref_data_repo, "literature_extraction_coverage.csv"))
  
  report_data          <- prepare_report_data(raw_report_data)
  report_data          <- add_age_bins(report_data, ref_repo = ref_data_repo, is_microdata = FALSE)
  mismatch_comparison  <- check_mismatched_age_bins(report_data)

  
  outlier <- fread(outlier_filepath)

  
  
  message("||-- Add Ratios")
  report_data    <- add_ratios(report_data, "vacc_hib3", "vacc_dpt3")
  report_data    <- add_ratios(report_data, "vacc_hepb3", "vacc_dpt3")
  report_data    <- add_ratios(report_data, "vacc_pcv3",  "vacc_dpt3")
  report_data    <- add_ratios(report_data, "vacc_mcv2", "vacc_mcv1")
  report_data    <- add_ratios(report_data, "vacc_dpt3", "vacc_dpt1")


  
  
  message("||-- Vaccine Loop")
  
  
  
  vaccines_to_run <- c("dpt", "mcv", "polio", "bcg")
  
  
  

  for (vaccine_to_run in vaccines_to_run){

    message(paste0("||---- ", vaccine_to_run))

    

    is_ratio <- grepl("ratio", vaccine_to_run)

    
    if(!is_ratio) {
      dose_vars <- rename_table[stem == vaccine_to_run, unique(vaccine)]
    } else {
      dose_vars <- vaccine_to_run
    }

    
    keep_cols <- c("svy_id", "file_path", "point", "ihme_loc_id", "location_code", "shapefile",
                   "year_id", "year_start", "year_end", "age_start", "age_end",
                   "age_bin", "age_bin_agg", "age_bin_agg_id", "N_obs",
                   dose_vars)
    df_lbd <- subset(report_data, select = keep_cols[keep_cols %in% names(report_data)])

    
    variable_dose_vaccines <- c("mcv", "rcv", "rota")
    if (vaccine_to_run %!in% variable_dose_vaccines & length(dose_vars) == 3 & vaccine_to_run != "dpt"){
      df_lbd <- df_lbd[!is.na(get(paste0("vacc_", vaccine_to_run, "3"))), ] 
    } else if (vaccine_to_run %!in% variable_dose_vaccines & length(dose_vars) != 3){
      for(var in dose_vars) {
        df_lbd <- df_lbd[!is.na(get(var)), ]
      }
    }else if (vaccine_to_run == "dpt"){
      
      df_lbd <- df_lbd[!(is.na(get(paste0("vacc_", vaccine_to_run, "1"))) & is.na(get(paste0("vacc_", vaccine_to_run, "3")))), ]
    }else {
      
      df_lbd <- df_lbd[!is.na(get(paste0("vacc_", vaccine_to_run, "1"))), ]
    }


    

    
    if (vaccine_to_run == "rota"){

      
      message("||------ Make RotaC")

      
      doses <- readRDS("FILEPATH/vaccine_schedule.rds")
      intro <- readRDS("FILEPATH/vaccine_intro.rds")

      
      df_rotac  <- data.table()
      countries <- sort(unique(df_lbd$ihme_loc_id))
      for (country in countries){
        data                   <- df_lbd[ihme_loc_id == country, ]
        country_specific_doses <- doses[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), .(doses)]
        country_specific_intro <- intro[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), .(cv_intro)] %>% unique

        
        if (nrow(country_specific_doses) == 0) country_specific_doses <- 0
        country_specific_doses <- ifelse(country_specific_doses >= 3, 3, 2)
        rota_dose <- paste0("vacc_rota", country_specific_doses)

        
        if (nrow(intro) == 0) stop(paste0("BREAK | Missing introduction year for ", unique(data$ihme_loc_id), "; need to prep introduction frame for this geography before continuing"))
        if (country_specific_intro < 9999) data$vacc_rotac <- data[, rota_dose, with=FALSE]
        df_rotac <- rbind(df_rotac, data, fill = TRUE)
      }
      df_lbd <- df_rotac
    }

    

    
    message("||------ Apply Design Effect")

    
    if (is_ratio) {
      outlier_names <- gsub("_ratio", "", vaccine_to_run)
      outlier_names <- unlist(strsplit(outlier_names, split = "_"))
      outlier_names <- unique(outlier$me_name)[grep(paste(outlier_names, collapse = "|"), unique(outlier$me_name))]
      outlier_names <- outlier_names[!grepl("ratio",  outlier_names)]
      
      outliered_nids <- outlier[lbd == 1 & me_name %in% outlier_names, unique(nid)]
    } else {
      outlier_names  <- unique(outlier$me_name)[grep(vaccine_to_run, unique(outlier$me_name))]
      outlier_names  <- outlier_names[!grepl("ratio",  outlier_names)]
      outlier_names  <- c(outlier_names, "")
      outliered_nids <- outlier[lbd == 1 & me_name %in% outlier_names, unique(nid)]
    }

    df_de         <- fread("FILEPATH/design_effect_nids.csv")
    df_de         <- df_de[vacc == vaccine_to_run, ]
    df_de         <- df_de[nid %!in% outliered_nids, ]

    
    de <- median(df_de$de, na.rm=T)

    
    df_lbd[ , N := N_obs * de]

    
    
    
    
    
    if(!is_ratio) {
      if(vaccine_to_run == 'rota') {
        dose_vars = c("vacc_rota1","vacc_rota2","vacc_rota3","vacc_rotac")
        for (var in dose_vars) {
          df_lbd[ ,(var) := (get(var)/100) * N]
        }
      } else {
        for (var in dose_vars) {
          df_lbd[ ,(var) := (get(var)/100) * N]
        }
      }
    }

    
    df_lbd[ , weight := 1]

    

    
    message("||------ Calculate Dose-Specific")

    
    setnames(df_lbd, rename_table$vaccine, rename_table$dose_specific, skip_absent = TRUE)

    
    if(vaccine_to_run %in% c("dpt", "pcv", "hib", "hepb", "polio")){
      vars <- paste0(vaccine_to_run, c("_dose_1", "_dose_2", "_dose_3"))
      df_lbd[ , (vars[1]) := get(vars[1]) - get(vars[2])]
      df_lbd[ , (vars[2]) := get(vars[2]) - get(vars[3])]

      
      df_lbd <- df_lbd[get(vars[1]) >= 0 | is.na(get(vars[1])), ]
      df_lbd <- df_lbd[get(vars[2]) >= 0 | is.na(get(vars[2])), ]
    }

    
    if(vaccine_to_run == "mcv" & "mcv_dose_2" %in% names(df_lbd)) {
      vars <- paste0(vaccine_to_run, c("_dose_1", "_dose_2"))
      df_lbd[!is.na(get(vars[2])) , (vars[1]) := get(vars[1]) - get(vars[2])]
    }


    

    
    datestamp <- gsub("-", "_", Sys.Date())
    outdir_current        <- ifelse(resample == TRUE,
                                    file.path(lit_extraction_output_root, "current", "resampled"),
                                    file.path(lit_extraction_output_root, "current", "not_resampled"))
    outdir_datestamped    <- ifelse(resample == TRUE,
                                    file.path(lit_extraction_output_root, datestamp, "resampled"),
                                    file.path(lit_extraction_output_root, datestamp, "not_resampled"))
    if (!dir.exists(outdir_datestamped)){
      dir.create(outdir_datestamped, recursive = TRUE)
    }

    
    if (resample == TRUE) {

      
      message("||------ Resampling")

      
      fwrite(df_lbd, file = file.path(outdir_datestamped, paste0(vaccine_to_run, "_pre_resample.csv")))

      
      if (nrow(df_lbd[point == 0]) > 0) {
        
        
        
        if (!exists("popraster")) {
          if (file.exists("FILEPATH/popraster.Rds")) {
            popraster <- readRDS("FILEPATH/popraster.Rds")
          } else {
            popraster <- load_pop_raster()
          }
        }

        
        df_lbd <- df_lbd[!is.na(df_lbd$location_code),]
        df_lbd <- resample_polygons(data = df_lbd,
                                    cores = 15,
                                    indic = vaccine_to_run,
                                    density = 0.001,
                                    gaul_list = get_adm0_codes('all', shapefile_version = 'current'),
                                    pull_poly_method = "fast")

        
        if(!is_ratio) {
          data_drops_from_resample <- data.table()
          dose_specific_variables  <- rename_table[stem == vaccine_to_run, dose_specific]
          for(dose_specific_variable in dose_specific_variables) {
            n_after_resample <- df_lbd[, .("dose_specific"    = dose_specific_variable,
                                           "resampled"        = TRUE,
                                           "n_after_resample" = sum(weight * N, na.rm = TRUE)), by = svy_id]
            data_drops_from_resample <- rbind(data_drops_from_resample, n_after_resample, fill = TRUE)
          }
          data_drops_from_resample <- merge(rename_table[stem == vaccine_to_run, ],
                                            data_drops_from_resample,
                                            all.x = TRUE, by = "dose_specific")

          
          data_drops_from_resample[, vaccine := gsub("vacc_", "", vaccine)]

          
          data_drop_table_path <- paste0(extraction_root, "FILEPATH/data_drops.csv")

          data_drop_table      <- fread(data_drop_table_path)
          data_drop_table      <- merge(data_drop_table,
                                        data_drops_from_resample[, .("nid"                       = svy_id,
                                                                     "antigen"                   = vaccine,
                                                                     "n_after_resample_recorded" = n_after_resample,
                                                                     "resample_recorded"         = resampled)],
                                        all.x = TRUE, by = c("nid", "antigen"), sort = FALSE)

          
          data_drop_table$n_after_resample <- as.numeric(data_drop_table$n_after_resample)
          data_drop_table[is.na(n_after_resample) & !is.na(n_after_resample_recorded), n_after_resample := n_after_resample_recorded]
          data_drop_table[resampled == FALSE & resample_recorded == TRUE, resampled := resample_recorded]
          data_drop_table$n_after_resample_recorded <- NULL
          data_drop_table$resample_recorded         <- NULL
          setcolorder(data_drop_table, c("nid", "total", "n_with_age_data", "n_0_59_months", "antigen"))

          
          resampled_data_drop_table_path         <- paste0(extraction_root, "FILEPATH/data_drops.csv")
          resampled_data_drop_table_archive_path <- paste0(extraction_root, "FILEPATH", gsub("-", "_", Sys.Date()), ".csv")

          write.csv(data_drop_table, file = resampled_data_drop_table_path, row.names = FALSE)
          write.csv(data_drop_table, file = resampled_data_drop_table_archive_path, row.names = FALSE)
        }
      }
    }


    

    
    message("||------ Saving")

    
    final_vax_variables <- names(df_lbd)[grepl(vaccine_to_run, names(df_lbd))]
    setcolorder(df_lbd, c(names(df_lbd)[!names(df_lbd) %in% final_vax_variables], final_vax_variables))

    if("vacc_rotac" %in% names(df_lbd)) setnames(df_lbd, "vacc_rotac", "rota_dose_c")

    
    file_name <- ifelse(resample == TRUE,
                        paste0(vaccine_to_run, "_resampled.csv"),
                        paste0(vaccine_to_run, ".csv"))
    fwrite(df_lbd, file.path(outdir_current, file_name))
    fwrite(df_lbd, file.path(outdir_datestamped, file_name))
    message(paste0("||-------- ", file.path(outdir_datestamped, file_name)))
  }


  
  check_resample_data_drops()

  message("||-- Lit Extraction Complete: ")
  message("******************************************************\n")
}




extract_report_data(resample = FALSE)
extract_report_data(resample = TRUE)




