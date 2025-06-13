











lit_data_date <- "2024_07_18"
output_date   <- "2024-08-02-test"


code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines")
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"))


if (!exists("config_path")) config_path <- file.path(code_root, "pipeline_config.yaml")

load_pipeline_config(step = "generate_lbd_csvs", config_path = config_path)



username       <- Sys.info()[["user"]]
indic_repo     <- paste0("FILEPATH", username, "FILEPATH")
vaccines_repo  <- paste0("FILEPATH", username, "FILEPATH")
commondir      <- paste0("FILEPATH")

suppressMessages(invisible(library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                                 R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))))
suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))


source("FILEPATH/load_packages.R")
invisible(load_packages(c("proto", "findpython", "getopt", "binom",
                          "dplyr", "gtools", "fasterize")))


source(paste0(indic_repo, "functions/misc_vaccine_functions.R"))
source(paste0(indic_repo, "data_prep/data_prep_functions.R"))


str_match <- stringr::str_match





outdir                  <- paste0("FILEPATH", output_date)

date <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")
archive_outdir                  <- file.path(outdir, "archive", output_date)
outlier_path            <- "FILEPATH/vaccination.csv"
lbd_regions_path        <- "FILEPATH/lbd_regions.csv"
processed_lit_data_path <- "FILEPATH"


mics_decision_path <- "FILEPATH/mics_comparison_decisions_by_me_nid.csv"
mics_decisions     <- fread(mics_decision_path)





'%!in%' <- function(x,y)!('%in%'(x,y))


load_processed_microdata <- function(vax_prefix, resampled = FALSE) {

  
  folder_path     <- ifelse(resample == TRUE,
                            paste0("FILEPATH", vax_prefix),
                            paste0("FILEPATH", vax_prefix))
  processed_files <- list.files(folder_path, full.names = TRUE)

  
  processed_microdata <- lapply(list.files(folder_path, full.names=TRUE, pattern = "*.csv"), fread) %>% rbindlist(., fill=TRUE)

  
  return(processed_microdata)
}



merge_with_decisions <- function(data, mics_decisions, decider_vax, is_ratio){

  
  vaccines_of_interest        <- unique(mics_decisions$me_name)[grepl(paste(paste0("vacc_", decider_vax), collapse = "|"),
                                                                      unique(mics_decisions$me_name))]
  mics_decisions_vax_specific <- mics_decisions[me_name %in% vaccines_of_interest, ]

  
  
  

  
  
  
  

  
  
  
  

  
  
  

  mics_decisions_vax_specific <- mics_decisions_vax_specific[order(nid, ihme_loc_id, me_name), ]

    
  multi_dose_decisions_standardized <-
    ifelse((mics_decisions_vax_specific[, .("N_decisions" = length(unique(decision))), by=.(ihme_loc_id, nid)][N_decisions > 1, .N] > 0),
           FALSE,
           TRUE)

  if (!multi_dose_decisions_standardized) {
    if (decider_vax %!in% c("mcv", "rcv")) {
      stop(paste0("Conflicting decider results for different doses of ", decider_vax, ". \nStop and fix (should be accounted for in decider code)\n "))
    } else {

      
      
      multi_decision_nids <- mics_decisions_vax_specific[, .("N_decisions" = length(unique(decision))), by=.(ihme_loc_id, nid)][N_decisions > 1, .(ihme_loc_id, nid)]
      for (i in 1:nrow(multi_decision_nids)) {
        location_id <- multi_decision_nids[i, ihme_loc_id]
        svy_id      <- multi_decision_nids[i, nid]
        decisions <- mics_decisions_vax_specific[ihme_loc_id == location_id & nid == svy_id, unique(decision)]
        if("drop" %in% decisions) {
          mics_decisions_vax_specific[ihme_loc_id == location_id & nid == svy_id, decision := "drop"]
        } else if ("use_report_extractions" %in% decisions) {
          mics_decisions_vax_specific[ihme_loc_id == location_id & nid == svy_id, decision := "use_report_extractions"]
        }
      }
    }
  }

  
  setnames(mics_decisions_vax_specific, c("ihme_loc_id", "nid"), c("country", "svy_id"))
  mics_decisions_vax_specific[, me_name := NULL]
  mics_decisions_vax_specific <- unique(mics_decisions_vax_specific)

  
  data_with_decider_results <- merge(data, mics_decisions_vax_specific, by=c("country", "svy_id"), all.x=TRUE)
  return(data_with_decider_results)

}


add_lit_extractions <- function(processed_microdata, literature_extractions, decider_vax){

  if ("ihme_loc_id" %in% names(literature_extractions)) {
    setnames(literature_extractions, old = "ihme_loc_id", new = "country")
  }

  literature_extractions <- subset(literature_extractions, select = names(literature_extractions) %in% names(processed_microdata))
  redundant_surveys      <- unique(literature_extractions$svy_id)[unique(literature_extractions$svy_id) %in% unique(processed_microdata$svy_id)]
  literature_extractions <- literature_extractions[!svy_id %in% redundant_surveys, ]
  processed_microdata    <- rbind(processed_microdata, literature_extractions, fill=TRUE)
  return(processed_microdata)
}


load_literature_extraction <- function(path, lit_data_date, vaccine_to_run, resample = FALSE) {
  is_ratio <- grepl("ratio", vaccine_to_run)
  
  if(!resample) {
    folder_path <- file.path(path, lit_data_date, "not_resampled")
  } else {
    folder_path <- file.path(path, lit_data_date, "resampled")
  }
  if (!file.exists(folder_path)) {
    stop(paste0("Lit data folder does not exist: ", folder_path))
  }

  
  file <- list.files(folder_path)[grepl(paste0(vaccine_to_run), list.files(folder_path))]
  if (!is_ratio) {
    file <- file[!grepl("pre_resample|ratio", file)]
  } else {
    file <- file[!grepl("pre_resample", file)]
  }

  if (length(file) == 0) {
    stop("Vaccine not found in resampled file path\n", "Vaccine: ", vaccine_to_run, "\nPath: ", folder_path)
  } else if (length(file) > 1) { stop(paste0("Problem reading in resampled lit data at ", folder_path))
  } else {
    lit_extraction_path      <- file.path(folder_path, file)
    lit_extraction           <- fread(lit_extraction_path)
    lit_extraction$type      <- "report"
    message("||-------- ", lit_extraction_path)
    return(lit_extraction)
  }
}


apply_outliers <- function(data, outlier_path, outlier_names) {

  

  
  format_dates <- function(outlier) {

    
    
    parse_date_range <- function(data, symbol) {
      dates_formatted <- data.table()
      for(i in 1:nrow(data)) {
        year_start_end        <- as.numeric(unlist(strsplit(data[i]$year_id, symbol)))
        year_range            <- year_start_end[1]:year_start_end[2]
        row_formatted         <- data[i][rep.int(1, length(year_range))]
        row_formatted$year_id <- year_range
        dates_formatted       <- rbind(dates_formatted,
                                       row_formatted,
                                       fill = TRUE)
      }
      return(dates_formatted)
    }

    
    
    parse_date_specific <- function(data, symbol) {
      dates_formatted <- data.table()
      for(i in 1:nrow(data)) {
        years                 <- as.numeric(unlist(strsplit(data[i]$year_id, symbol)))
        row_formatted         <- data[i][rep.int(1, length(years))]
        row_formatted$year_id <- years
        dates_formatted       <- rbind(dates_formatted,
                                       row_formatted,
                                       fill = TRUE)
      }
      return(dates_formatted)
    }

    
    outliers_no_date          <- outlier[is.na(year_id) | year_id == ""]
    outliers_date_formatted   <- outlier[!is.na(as.numeric(year_id))]
    outliers_date_unformatted <- outlier[!is.na(year_id) & year_id != "" & is.na(as.numeric(year_id))]
    
    if( nrow(outliers_date_unformatted) > 0){
      hyphen_dates_unformatted <- outliers_date_unformatted[grep("-", year_id), ]
      slash_dates_unformatted  <- outliers_date_unformatted[grep("/", year_id), ]
      comma_dates_unformatted  <- outliers_date_unformatted[grep(",", year_id), ]
      
      hyphen_dates_formatted <- parse_date_range(hyphen_dates_unformatted, "-")
      slash_dates_formatted  <- parse_date_specific(slash_dates_unformatted, "/")
      comma_dates_formatted  <- parse_date_specific(comma_dates_unformatted, ",")
      
      outliers_date_unformatted_fixed <- rbind(hyphen_dates_formatted,
                                               slash_dates_formatted,
                                               comma_dates_formatted)
      
    
      dates_formatted <- rbind(outliers_date_formatted,
                               outliers_date_unformatted_fixed,
                               outliers_no_date)
    } else {
      dates_formatted <- rbind(outliers_date_formatted,
                               outliers_no_date)
    }
    
    return(dates_formatted)
  }

  

  
  outlier <- fread(outlier_path)

  
  outlier <- suppressWarnings(format_dates(outlier))

  
  
  
  
  
  subnational_rows                   <- grepl(outlier$ihme_loc_id, pattern = "_")
  non_batch_outlier_rows             <- outlier$batch_outlier != 1
  non_batch_outlier_subnational_rows <- subnational_rows & non_batch_outlier_rows
  outlier                            <- outlier[!non_batch_outlier_subnational_rows, ]

  
  outlier <- outlier[lbd == 1 & (me_name == "" | (me_name %in% outlier_names))]

  
  outlier_nids <- outlier[me_name == "", nid]

  
  outlier_nids_vax_specific <- outlier[me_name %in% outlier_names & batch_outlier == 1, nid]

  
  outlier_loc_year_vax_specific <- outlier[me_name %in% outlier_names & (batch_outlier != 1|is.na(batch_outlier)) &
                                             !is.na(year_id) & year_id != "" &
                                             !is.na(ihme_loc_id) & ihme_loc_id != "", paste0(nid, "_", year_id, "_", ihme_loc_id)]

  
  data$outlier <- 0

  
  data[svy_id %in% outlier_nids | svy_id %in% outlier_nids_vax_specific, outlier := 1]
  data[paste0(svy_id, "_", year_id, "_", country) %in% outlier_loc_year_vax_specific, outlier := 1]

  
  return(data)
}






save_data_drops_from_outlier <- function(vaccine_to_run, data_drops_from_outlier, microdata_drop_table, report_data_drop_table) {

  
  data_drops_from_outlier[, n_after_outlier := round(n_after_outlier, digits = 0)]

  
  antigen_data_from_microdata <- data_drops_from_outlier[type == "microdata", ]
  antigen_data_from_report    <- data_drops_from_outlier[type == "report", ]

  
  microdata_drop_table$n_after_outlier   <- NULL
  report_data_drop_table$n_after_outlier <- NULL

  antigen_microdata_drop_table <- merge(antigen_data_from_microdata, microdata_drop_table, all.x = TRUE)
  antigen_report_drop_table    <- merge(antigen_data_from_report, report_data_drop_table, all.x = TRUE)

  
  final_data_drop_table <- rbind(antigen_microdata_drop_table, antigen_report_drop_table)

  
  setcolorder(final_data_drop_table,
              c("nid", "total", "n_with_age_data", "n_0_59_months", "antigen", "n",
                "n_after_geomatch", "resampled", "n_after_resample", "n_after_outlier", "type"))

  
  folder_path <- file.path("FILEPATH", gsub("-", "_", Sys.Date()))
  file_path   <- file.path(folder_path, paste0(vaccine_to_run, ".csv"))
  message("||-------- ", file_path)
  if(!dir.exists(folder_path)) dir.create(folder_path)
  write.csv(final_data_drop_table, file = file_path, row.names = FALSE)

}

#' @vax_prefix (string) - stem of vaccine being processed
#' @df (data.table) - Table of microdata


apply_hotfixes <- function(vax_prefix, df) {

  if(vax_prefix == ("mcv")) {
    
    df <- df[!(svy_id == 467681)]
  }
  return(df)
}





generate_csvs <- function(resample = FALSE) {

  
  resample_category <- ifelse(resample == FALSE, "not_resampled", "resampled")
  outdir            <- file.path(outdir, resample_category)
  archive_outdir    <- file.path(archive_outdir, resample_category)
  
  if(!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }

  if(!dir.exists(archive_outdir)) {
    dir.create(archive_outdir, recursive = TRUE)
  }

  
  
  

  
  report_data_drop_path  <- file.path("FILEPATH", ifelse(resample, "resampled", "not_resampled"), "data_drops.csv")
  report_data_drop_table <- fread(report_data_drop_path)

  
  microdata_data_drop_path  <- file.path("FILEPATH", ifelse(resample, "resampled", "not_resampled"))
  microdata_drop_table      <- lapply(list.files(microdata_data_drop_path, full.names = TRUE), fread) %>% rbindlist

  

  ratios <- c("hib3_dpt3_ratio", "mcv2_mcv1_ratio", "pcv3_dpt3_ratio", "rotac_dpt3_ratio") 

  
  
  message("||-- Begin vaccine loop:")
  vaccines_to_run <- if(resample == FALSE)  c("hepb", "hib", "rota", "pcv", "mcv2") else c("dpt", "polio", "bcg", "mcv1")
  for (vaccine_to_run in vaccines_to_run) {

    
    is_ratio <- grepl("ratio", vaccine_to_run)

    
    data_drops_from_outlier <- data.table(svy_id             = NA_integer_,
                                          antigen            = NA_character_,
                                          n_after_outlier    = NA_integer_)

    
    message(paste0("||\n||---- ", vaccine_to_run))

    vaccine_list 	    	  <- NULL
    final_model_vaccines  <- NULL

    if ("hepb3_dpt3_ratio" %in% vaccine_to_run) {
      outlier_names = c("vacc_hepb3", "vacc_dpt3")
      decider_vax   = c("hepb", "dpt")
    }

    if ("hib3_dpt3_ratio" %in% vaccine_to_run) {
      outlier_names = c("vacc_hib3", "vacc_dpt3")
      decider_vax   = c("hib", "dpt")
    }

    if ("pcv3_dpt3_ratio" %in% vaccine_to_run) {
      outlier_names = c("vacc_pcv3", "vacc_dpt3")
      decider_vax   = c("pcv", "dpt")
    }

    if ("rotac_dpt3_ratio" %in% vaccine_to_run) {
      outlier_names = c("vacc_rotac", "vacc_dpt3")
      decider_vax   = c("rota", "dpt")
    }

    if ("mcv2_mcv1_ratio" %in% vaccine_to_run) {
      outlier_names = c("vacc_mcv1", "vacc_mcv2")
      decider_vax   = c("mcv")
    }

    if("dpt3_timeliness_ratio" %in% vaccine_to_run) {
      outlier_names = c("vacc_dpt3")
      decider_vax   = c("dpt")
    }

    if ("polio" %in% vaccine_to_run) {
      if(is.null(vaccine_list)) vaccine_list <- list()
      vaccine_list$polio <- add_vaccine(prefix  = "polio",
                                        title   = "Polio",
                                        doses   = 3,
                                        age_min = 0,
                                        age_max = 59)$polio

      vaccines <- names(vaccine_list)
      if(is.null(final_model_vaccines)) final_model_vaccines  <- character()
      final_model_vaccines <- c(final_model_vaccines, "polio3_cov", "polio1_cov", "polio1_3_abs_dropout", "polio1_3_rel_dropout")
      outlier_names <- c("vacc_polio1", "vacc_polio2", "vacc_polio3")
    }

    if ("bcg" %in% vaccine_to_run) {

      vaccine_list <- add_vaccine(prefix  = "bcg",
                                  title   = "BCG",
                                  doses   = 1,
                                  age_min = 0,
                                  age_max = 59)

      vaccines <- names(vaccine_list)

      final_model_vaccines <- c("bcg_cov")
      outlier_names <- c("vacc_bcg")

    }

    if ("rcv" %in% vaccine_to_run) {

      vaccine_list <- add_vaccine(prefix  = "rcv",
                                  title   = "RCV",
                                  doses   = 1,
                                  age_min = 0,
                                  age_max = 59)

      vaccines <- names(vaccine_list)

      final_model_vaccines <- c("rcv_cov")
      outlier_names <- c("vacc_rcv")

    }
    if ("yfv" %in% vaccine_to_run) {

      vaccine_list <- add_vaccine(prefix  = "yfv",
                                  title   = "YFV",
                                  doses   = 1,
                                  age_min = 0,
                                  age_max = 59)

      vaccines <- names(vaccine_list)

      final_model_vaccines <- c("yfv_cov")
      outlier_names <- c("vacc_yfv")
    }
    if ("dpt" %in% vaccine_to_run) {
      if(is.null(vaccine_list)) vaccine_list <- list()
      vaccine_list$dpt <- add_vaccine(prefix  = "dpt",
                                      title   = "DPT",
                                      doses   = 3,
                                      age_min = 0,
                                      age_max = 59)$dpt

      vaccine_list$dpt["cond_doses"] <- c(12)
      vaccine_list$dpt["cond_vaccines"] <- "dpt12_cond"

      vaccines <- names(vaccine_list)
      if(is.null(final_model_vaccines)) final_model_vaccines  <- character()
      final_model_vaccines <- c(final_model_vaccines, "dpt12_cond", "dpt3_cov", "dpt1_cov", "dpt1_3_abs_dropout", "dpt1_3_rel_dropout")
      outlier_names <- c("vacc_dpt1", "vacc_dpt2", "vacc_dpt3")
    }
    if ("pcv" %in% vaccine_to_run) {
      if(is.null(vaccine_list)) vaccine_list <- list()
      vaccine_list$pcv <- add_vaccine(prefix  = "pcv",
                                      title   = "PCV",
                                      doses   = 3,
                                      age_min = 0,
                                      age_max = 59)$pcv

      vaccines <- names(vaccine_list)
      if(is.null(final_model_vaccines)) final_model_vaccines  <- character()
      final_model_vaccines <- c(final_model_vaccines, "pcv3_cov", "pcv1_cov", "pcv1_3_abs_dropout", "pcv1_3_rel_dropout")
      outlier_names <- c("vacc_pcv", "vacc_pcv1", "vacc_pcv2", "vacc_pcv3")
    }
    if ("hib" %in% vaccine_to_run) {
      if(is.null(vaccine_list)) vaccine_list <- list()
      vaccine_list$hib <- add_vaccine(prefix  = "hib",
                                      title   = "hib",
                                      doses   = 3,
                                      age_min = 0,
                                      age_max = 59)$hib

      vaccines <- names(vaccine_list)
      if(is.null(final_model_vaccines)) final_model_vaccines  <- character()
      final_model_vaccines <- c(final_model_vaccines, "hib3_cov", "hib1_cov", "hib1_3_abs_dropout", "hib1_3_rel_dropout")
      outlier_names <- c("vacc_hib", "vacc_hib1", "vacc_hib2", "vacc_hib3")
    }
    if ("hepb" %in% vaccine_to_run) {
      if(is.null(vaccine_list)) vaccine_list <- list()
      vaccine_list$hepb <- add_vaccine(prefix  = "hepb",
                                       title   = "hepb",
                                       doses   = 3,
                                       age_min = 0,
                                       age_max = 59)$hepb

      vaccines <- names(vaccine_list)
      if(is.null(final_model_vaccines)) final_model_vaccines  <- character()
      final_model_vaccines <- c(final_model_vaccines, "hepb3_cov", "hepb1_cov", "hepb1_3_abs_dropout", "hepb1_3_rel_dropout")
      outlier_names <- c("vacc_hepb", "vacc_hepb1", "vacc_hepb2", "vacc_hepb3")
    }
    if ("rota" %in% vaccine_to_run) {
      if(is.null(vaccine_list)) vaccine_list <- list()
      vaccine_list$rota <- add_vaccine(prefix  = "rota",
                                       title   = "ROTA",
                                       doses   = 1,
                                       age_min = 0,
                                       age_max = 59)$rota
      vaccine_list$rota$all_titles <- c("rota0", "rotac")
      vaccine_list$rota$cond_vaccines <- c("rotac_cond", "rota0_cond")
      vaccine_list$rota$cond_doses <- c("c", "0")
      vaccine_list$rota$all_doses <- c("0", "c")

      vaccines <- names(vaccine_list)
      if(is.null(final_model_vaccines)) final_model_vaccines  <- character()
      final_model_vaccines <- c(final_model_vaccines, "rotac_cov")
      outlier_names <- c("vacc_rota", "vacc_rotac", "vacc_rota1", "vacc_rota2", "vacc_rota3")
    }
    if (vaccine_to_run == "mcv1" ) {

      vaccine_list <- add_vaccine(prefix  = "mcv",
                                  title   = "MCV",
                                  doses   = 1,
                                  age_min = 0,
                                  age_max = 59)

      vaccines <- names(vaccine_list)

      
      final_model_vaccines       <- c("mcv1_cov")
      outlier_names              <- c("vacc_mcv1")
      vaccine_list$mcv$all_doses <- c("1")
    }
    if (vaccine_to_run == "mcv2") {

      vaccine_list <- add_vaccine(prefix  = "mcv",
                                  title   = "MCV",
                                  doses   = 2,
                                  age_min = 0,
                                  age_max = 59)
      vaccines     <- names(vaccine_list)

      
      final_model_vaccines       <- c("mcv12_cond", "mcv2_cov")
      outlier_names              <- c("vacc_mcv1", "vacc_mcv2")
      vaccine_list$mcv$all_doses <- c("1", "2")
    }

    
    if (!is_ratio & !grepl("mcv", vaccine_to_run)) {
      vax_prefix  <- vaccine_list[[vaccine_to_run]][["prefix"]]
      vax_doses   <- vaccine_list[[vaccine_to_run]][["all_doses"]]
      cond_vax    <- vaccine_list[[vaccine_to_run]][["cond_vaccines"]]
      decider_vax <- vaccine_to_run
    } else if (!is_ratio & grepl("mcv", vaccine_to_run)) {
      
      
      vax_prefix  <- "mcv"
      vax_doses   <- vaccine_list[[vax_prefix]][["all_doses"]]
      cond_vax    <- vaccine_list[[vax_prefix]][["cond_vaccines"]]
      decider_vax <- vax_prefix
    } else {
      vax_prefix <- vaccine_to_run
    }

    

    
    message("||------ Load processed survey data")
    processed_microdata_raw <- load_processed_microdata(vax_prefix, resample)

    
    processed_microdata_raw <- apply_hotfixes(vax_prefix, processed_microdata_raw)

    
    message("||------ Incorporate Decider results")
    processed_microdata_with_decisions <- merge_with_decisions(copy(processed_microdata_raw), mics_decisions, decider_vax, is_ratio)

    
    if (vaccine_to_run == "mcv1" & resample == TRUE){
      processed_microdata_with_decisions[processed_microdata_with_decisions$svy_id == 424884]$decision <- "use_microdata"
      processed_microdata_with_decisions[processed_microdata_with_decisions$svy_id == 287639]$decision <- "use_microdata"
      processed_microdata_with_decisions[processed_microdata_with_decisions$svy_id == 464103]$decision <- "use_microdata"
    }

    
    
    removal_decisions            <- c("use_report_extractions", "drop")
    processed_microdata          <- processed_microdata_with_decisions[!decision %in% removal_decisions, ]
    processed_microdata$decision <- NULL
    processed_microdata$type     <- "microdata"

    

    message("||------ Load processed literature extractions:")
    literature_extractions <- load_literature_extraction(processed_lit_data_path, lit_data_date, vax_prefix, resample)
    data                   <- add_lit_extractions(processed_microdata, literature_extractions, vax_prefix)

    
    
    message("||------ Identify outliers")
    data <- apply_outliers(data, outlier_path, outlier_names)

    
    
    lbd_regions <- fread(lbd_regions_path)
    lbd_regions <- lbd_regions[, .(country, region)]
    data        <- merge(data, lbd_regions, all.x = TRUE, by = "country")


    

    
    data$row_id <- seq.int(nrow(data))

    
    
    
    if(!is_ratio){
      
      message("||------ Calculate 0-dose children")
      
      last_dose    <- max(vax_doses)
      csv_vaccines <- unique(c(cond_vax,
                               paste0(vax_prefix, last_dose, "_cov"),
                               final_model_vaccines))
      if(vaccine_to_run == "rota") { csv_vaccines <- "rotac_cov" }

      
      if(vax_prefix=="rota")  {data[, rota_dose_0  := N - (rota_dose_1  + rota_dose_2  + rota_dose_3)]}
      if(vax_prefix=="polio") {data[, polio_dose_0 := N - (polio_dose_1 + polio_dose_2 + polio_dose_3)]}
      if(vax_prefix=="dpt")   {data[, dpt_dose_0   := N - (dpt_dose_1   + dpt_dose_2   + dpt_dose_3)]}
      if(vax_prefix=="dpt")   {data[, dpt_dose_0   := N - (dpt_dose_1   + dpt_dose_2   + dpt_dose_3)]}
      if(vax_prefix=="pcv")   {data[, pcv_dose_0   := N - (pcv_dose_1   + pcv_dose_2   + pcv_dose_3)]}
      if(vax_prefix=="hepb")  {data[, hepb_dose_0  := N - (hepb_dose_1  + hepb_dose_2  + hepb_dose_3)]}
      if(vax_prefix=="hib")   {data[, hib_dose_0   := N - (hib_dose_1   + hib_dose_2   + hib_dose_3)]}
      if(vax_prefix=="bcg")   {data[, bcg_dose_0   := N - (bcg_dose_1)]}
      if(vax_prefix=="yfv")   {data[, yfv_dose_0   := N - (yfv_dose_1)]}
      if(vax_prefix=="rcv")   {data[, rcv_dose_0   := N - (rcv_dose_1)]}
      if(vax_prefix=="mcv") {
        data[, mcv_dose_0 := N - mcv_dose_1]
        data[!is.na(mcv_dose_2), mcv_dose_0 := N - (mcv_dose_1 + mcv_dose_2)]
      }
    }


    

    message("||------ Save CSV's")
    setnames(data, c("survey_name", "year_id"), c("source", "year"))

    if (!is_ratio){

      
      if (last_dose == 1) csv_vaccines <- paste0(vax_prefix, last_dose, "_cov")

      for (ind in csv_vaccines) {

        message(paste0("||-------- ", ind))

        

        df_temp <- copy(data)

        
        if(ind == "rotac_cov") {
          dose <- 1
          last_dose <- 1
        } else {
          dose <- str_match(ind, "^[a-z]*([0-9]*)_.*")[2] %>% as.numeric
        }

        if (grepl("_cov", ind)) {

          
          if (vaccine_to_run == "rota"){
            drop_vars <- ""
            names(df_temp)[names(df_temp) == "rota_dose_c"] <- "outcome"
          } else if (vaccine_to_run == "mcv1") {
            df_temp[, outcome := mcv_dose_1]
            df_temp[!is.na(mcv_dose_2), outcome := rowSums(.SD),
                    .SDcols = c("mcv_dose_1", "mcv_dose_2")]
          
          } else if (ind == "dpt1_cov") {
            df_temp[!is.na(dpt_dose_1), outcome := rowSums(.SD, na.rm = TRUE),
                    .SDcols = paste0(vax_prefix, "_dose_", dose:last_dose)]
          } else {
            df_temp[, outcome := rowSums(.SD),
                    .SDcols = paste0(vax_prefix, "_dose_", dose:last_dose)]
           }

          
          drop_vars <- names(df_temp)[grepl(paste0(vax_prefix, "_dose"), names(df_temp))]
          df_temp   <- subset(df_temp, select = !names(df_temp) %in% drop_vars)

        } else if (grepl("_cond", ind)) {

          
          
          
          if (dose == 12){
            
            if (ind == "dpt12_cond") dose <- 2
            
            
            if (ind == "mcv12_cond") {
              df_temp <- df_temp[!is.na(mcv_dose_2), ]
              dose <- 1
            }

            
            setnames(df_temp, paste0(vax_prefix, "_dose_", 0:dose), paste0("d", 0:dose))

            
            drop_vars <- names(df_temp)[grepl(paste0(vax_prefix, "_dose"), names(df_temp))]
            df_temp   <- subset(df_temp, select = !(names(df_temp) %in% drop_vars))

            
            df_temp[, N := rowSums(.SD), .SDcols = paste0("d", 0:dose)]

            
            df_temp[, paste0("d", dose) := rowSums(.SD), .SDcols = paste0("d", 1:dose)]

          } else {
            
            setnames(df_temp, paste0(vax_prefix, "_dose_", 0:dose), paste0("d", 0:dose))

            
            drop_vars <- names(df_temp)[grepl(paste0(vax_prefix, "_dose"), names(df_temp))]

            
            df_temp <- subset(df_temp, select = !(names(df_temp) %in% drop_vars))

            
            df_temp[, N := rowSums(.SD), .SDcols = paste0("d", 0:dose)]
          }

          
          df_temp <- subset(df_temp, select = !(names(df_temp) %in% paste0("d", 0:(dose-1))))

          
          setnames(df_temp, paste0("d", dose), "outcome")

          
          df_temp <- subset(df_temp, N > 0)

        } else if (grepl("1_3_abs_dropout", ind)) {

          setnames(df_temp, paste0(vax_prefix, "_dose_", 0:last_dose), paste0("d", 0:last_dose))
          df_temp[, outcome := d1 + d2]

          
          df_temp <- subset(df_temp, select = !(names(df_temp) %in% paste0("d", 0:last_dose)))

        } else if (grepl("1_3_rel_dropout", ind)) {

          
          df_temp[, denom := rowSums(.SD),
                  .SDcols = paste0(vaccine_to_run, "_dose_", 1:last_dose)]

          setnames(df_temp, paste0(vax_prefix, "_dose_", 0:last_dose), paste0("d", 0:last_dose))

          
          df_temp[, outcome := (d1 + d2) / denom]

          
          df_temp <- subset(df_temp, select = !(names(df_temp) %in% paste0("d", 0:last_dose)))
          df_temp[, denom := NULL]

        }

        
        df_over <- df_temp[as.numeric((outcome/N) > 1.02),]
        if(nrow(df_over) > 0){
          greater_than_100_coverage_nids <- unique(df_over$svy_id)
          message(paste0("||---------- ", "WARNING for ", ind, ": nids have observations with > 100% coverage and a difference of more than 2%."))
          message(paste0("||---------- NIDS: ", paste(greater_than_100_coverage_nids, collapse = ", ")))
        }
        df_temp[(outcome - N) > .01, outcome := N]

        
        df_temp <- df_temp[!is.na(outcome), ]
        df_temp <- df_temp[outcome >= 0 | is.na(outcome), ]

        
        setnames(df_temp, "outcome", ind)

        
        if(ind %in% c("mcv1_cov", "bcg_cov", "polio3_cov", "dpt1_cov", "dpt3_cov", "hib3_cov", "hepb_cov", "pcv_cov", "rotac_cov", "bcg1_cov", "hepb3_cov", "pcv3_cov")){
          fwrite(df_temp, file = file.path(outdir, paste0(ind, "_outliers_inc.csv")))
          fwrite(df_temp, file = file.path(archive_outdir, paste0(ind, "_outliers_inc.csv")))
        }

        
        df_temp <- df_temp[outlier == 0, ]
        if (df_temp[is.na(point), .N] > 0) message("data is being saved for which point == NA. Consider removing")

        
        data_drops_from_outlier <- rbind(data_drops_from_outlier,
                                         df_temp[, .("antigen" = ind, "n_after_outlier" = sum(weight * N)), by = c("svy_id", "type")],
                                         fill = TRUE)
        write.csv(data_drops_from_outlier, file = paste0("FILEPATH", ind, "_", gsub("-", "_", Sys.Date()), ".csv"), row.names = FALSE)

        message(paste0("||---------- ", file.path(outdir, paste0(ind, ".csv"))))
        fwrite(df_temp, file = file.path(outdir, paste0(ind, ".csv")))
        fwrite(df_temp, file = file.path(archive_outdir, paste0(ind, ".csv")))
      }
      if (vaccine_to_run == "dpt") {
      message(paste0("||-------- ", "dpt3_dpt1_ratio"))
      
      dpt1 <- fread(file.path(outdir, "dpt1_cov.csv"))
      dpt3 <- fread(file.path(outdir, "dpt3_cov.csv"))
      
      dpt31 <- merge(dpt1, dpt3, all=T)
      
      dpt31[, N:=dpt1_cov]
      
      dpt31[, dpt3_dpt1_ratio:=dpt3_cov]
      dpt31 <- dpt31[!is.na(dpt31$dpt3_dpt1_ratio)]
      dpt31[, N_obs:=NULL]
      
      dpt31 <- dpt31[dpt31$N > 0]
      message(paste0("||---------- ", file.path(outdir, "dpt3_dpt1_ratio.csv")))
      fwrite(dpt31, file = file.path(outdir,  "dpt3_dpt1_ratio.csv"))
      fwrite(dpt31, file = file.path(archive_outdir, "dpt3_dpt1_ratio.csv"))
    }
      
      message("||------ Save data drops")
      data_drops_from_outlier <- data_drops_from_outlier[grepl("cov", antigen), ] 
      data_drops_from_outlier[, antigen := gsub("_cov", "", antigen)]             
      setnames(data_drops_from_outlier, "svy_id", "nid")
      save_data_drops_from_outlier(vaccine_to_run, data_drops_from_outlier, microdata_drop_table, report_data_drop_table)
    } else {
      
      message(paste0("||-------- ", vaccine_to_run))
      df_vax <- copy(data)
      df_vax <- df_vax[outlier == 0, ]
      df_vax <- df_vax[get(vaccine_to_run) != Inf & get(vaccine_to_run) != -Inf & !is.nan(get(vaccine_to_run)), ]
      fwrite(df_vax, file = file.path(outdir, paste0(vaccine_to_run, ".csv")))
      fwrite(df_vax, file = file.path(archive_outdir, paste0(vaccine_to_run, ".csv")))
    }
  }
  
  message("******************************************************\n")
}




generate_csvs(resample = FALSE)
generate_csvs(resample = TRUE)

