
















#' @param compare_ratios [lgl] if TRUE, run comparisons on ratio vaccines, otherwise on straight vaccines
#' @param modeling [chr] if compare_ratios == FALSE vector of straight vaccines to model e.g. c('vacc_bcg', 'vacc_polio', etc.)
#' @param date [chr] date_version from config
#' @param data_root [path] e.g. "FILEPATH"
#' @param ref_data_repo [path] e.g. "FILEPATH" 
#' @param covid_years [int] e.g. 2020L:2022L

#' @return
#' @export

#' @examples
match_stockouts_detected_reported <- function(
    compare_ratios = FALSE,
    modeling       = NULL,
    date,
    
    data_root,
    ref_data_repo,
    covid_years
) {

  stopifnot(dir.exists(data_root))
  stopifnot(dir.exists(ref_data_repo))
  stopifnot(is.integer(covid_years))

  if (!compare_ratios) {
    
    if(is.null(modeling)) {
      stop(
        "When modeling straight vaccines (compare_ratios == FALSE), submit a vector of me_names without doses.
       e.g. c('vacc_bcg', 'vacc_polio', 'vacc_dpt', 'vacc_hepb', 'vacc_hib', 'vacc_mcv', 'vacc_pcv', 'vacc_rotac', 'vacc_rcv')"
      )
    }

    
    
    
    
    
    predicted <- readRDS(paste0(data_root, "FILEPATH/detected_stockouts_stgpr.rds"))
    reported  <- readRDS(file.path(ref_data_repo, "who_reported_stockouts.rds"))

    
    
    
    
    covid_stockouts <- as.data.table(expand.grid("ihme_loc_id" = unique(predicted$ihme_loc_id),
                                                 "me"          = unique(reported$me),
                                                 "year_id"     = covid_years))
    factor_vars <- c("ihme_loc_id", "me")
    covid_stockouts[, c(factor_vars) := lapply(.SD, defactor_vector), .SDcols = factor_vars]
    
    reported <- rbind(reported, covid_stockouts)

    predicted[, cv_stockout_dose := me_name]
    predicted$me_name <- gsub("[0-9]+", "", predicted$me_name)
    setnames(predicted, "me_name", "me")
    predicted <- predicted[, c("ihme_loc_id", "year_id", "me", "data", "pred_stgpr", "cv_stockout_dose", "diff")]  
    predicted <- predicted[!duplicated(predicted)]

    predicted <- predicted[me %in% modeling]
    reported  <- reported[me %in% modeling]

    predicted[, cv_stockout_ratio := diff]


    
    
    
    
    reported_years <- unique(reported$year_id)
    predicted      <- predicted[year_id %in% reported_years]

    reported$year_id <- as.numeric(as.character(reported$year_id))

    exact_match <- merge(predicted, reported, by = c("ihme_loc_id", "year_id", "me"))

    exact_match[, nid := 203321]

    setnames(exact_match, "me", "me_name")

    exact_match[, me_name := cv_stockout_dose][, cv_stockout_dose := NULL]

    vacc.intro  <- file.path(ref_data_repo, "vaccine_intro.rds")
    intros      <- readRDS(vacc.intro)
    exact_match <- merge(exact_match, intros, by = c("ihme_loc_id", "year_id", "me_name"), all.x = TRUE)
    exact_match <- exact_match[cv_intro_years > 2 | is.na(cv_intro_years)][, c("cv_intro", "cv_intro_years", "cv_outro", "location_id") := NULL]  

    fwrite(exact_match, file = paste0(data_root, "FILEPATH", date, ".csv"))

    exact_match <- exact_match[, `:=` (data = NULL, pred_stgpr = NULL, diff = NULL)]

    fwrite(exact_match, file = paste0(data_root, "FILEPATH/exact_matches.csv"), row.names = FALSE)

    message("All done -- 'exact_matches' file made FILEPATH")


  } else if (compare_ratios) {

    
    
    predicted_ratios <- readRDS(paste0(data_root, "FILEPATH/detected_ratio_stockouts_stgpr.rds"))
    reported         <- readRDS(file.path(ref_data_repo, "who_reported_stockouts.rds"))

    covid_stockouts <- as.data.table(expand.grid("ihme_loc_id" = unique(predicted_ratios$ihme_loc_id),
                                                 "me"          = unique(reported$me),
                                                 "year_id"     = covid_years))
    factor_vars <- c("ihme_loc_id", "me")
    covid_stockouts[, c(factor_vars) := lapply(.SD, defactor_vector), .SDcols = factor_vars]

    reported <- rbind(reported, covid_stockouts)

    predicted_ratios[, cv_stockout_dose := me_name]
    predicted_ratios$me <- gsub("[0-9]+", "", predicted_ratios$me_name)
    predicted_ratios <- predicted_ratios[, c("ihme_loc_id", "year_id", "me", "data", "pred_stgpr", "cv_stockout_dose", "diff")]  
    predicted_ratios <- predicted_ratios[!duplicated(predicted_ratios)]
    predicted_ratios[, cv_stockout_ratio := diff]

    predicted_ratios$me <- unlist(lapply(predicted_ratios$me, function(x) {
      numerator <- unlist(strsplit(x, "_"))[2]
      return(paste0("vacc_", numerator))
    }))


    
    
    

    
    reported_years   <- unique(reported$year_id)
    predicted_ratios <- predicted_ratios[year_id %in% reported_years]

    
    reported$year_id <- as.numeric(as.character(reported$year_id))
    
    
    ratio_exact_match <- merge(predicted_ratios, reported, by = c("ihme_loc_id", "year_id", "me"))
    
    ratio_exact_match[, nid := 203321]

    setnames(ratio_exact_match, "me", "me_name")

    ratio_exact_match[, me_name := cv_stockout_dose][, cv_stockout_dose := NULL]

    vacc.intro  <- file.path(ref_data_repo, "vaccine_intro.rds")
    intros      <- readRDS(vacc.intro)
    ratio_exact_match <- merge(
      x     = ratio_exact_match, 
      y     = intros, 
      by    = c("ihme_loc_id", "year_id", "me_name"), 
      all.x = TRUE
    )
    ratio_exact_match <- ratio_exact_match[cv_intro_years > 2 | is.na(cv_intro_years)][, c("cv_intro", "cv_intro_years", "cv_outro", "location_id") := NULL]  

    
    direct_exact_match <- fread(paste0(data_root, "FILEPATH", date, ".csv"))
    exact_match        <- rbind(direct_exact_match, ratio_exact_match)
    exact_match        <- exact_match[, `:=` (data=NULL, pred_stgpr=NULL, diff=NULL)]

    
    fwrite(exact_match, file = paste0(data_root, "FILEPATH", date, ".csv"), row.names = FALSE)
    fwrite(exact_match, file = paste0(data_root, "FILEPATH/exact_ratio_matches.csv"), row.names = FALSE)

    message("All done -- 'exact_matches' file made FILEPATH")

    
  }
}

