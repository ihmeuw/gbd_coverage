











library(data.table)



gbd_date      <- "2020-06-15"  
gbd_cycle     <- 2020

vaccines_root <- "FILEPATH"
vacc_path     <- paste0(vaccines_root, "FILEPATH", gbd_cycle, "/", gbd_date, "/vaccination.rds")
who_path      <- paste0(vaccines_root, "FILEPATH/wuenic_svy_list_2020-06-10.csv")  

ihme_nid_titles_path <- paste0(vaccines_root, "FILEPATH/all_nids_with_full_meta.csv")


ihme          <- readRDS(vacc_path)
who           <- fread(who_path)



nid_titles    <- fread(ihme_nid_titles_path)





keep_columns <- c("ihme_loc_id", "nid", "survey_name", "year_start", "year_end")
ihme <- unique(ihme[, keep_columns, with = FALSE])


ihme <- ihme[nid != 203321, ]


ihme[ihme_loc_id %like% "_", ihme_loc_id := substr(ihme_loc_id, 1, 3)]


ihme <- merge(ihme, nid_titles, all.x = TRUE, by = c("nid", "ihme_loc_id"))


setnames(ihme, c("survey_name", "title"), c("survey_series", "survey"))



ihme[survey %like% "MICS" | survey %like% "Multiple Indicator Cluster Survey" |
       survey_series %like% "MICS", survey_series := "MICS"]


ihme[survey %like% "EPI" | 
       survey %like% "Expanded Programme on Immunization" | 
       survey %like% "Expanded Programm on Immunization", survey_series := "EPI"]


ihme[survey %like% "Coverage Evaluation Survey", survey_series := "CES"]


ihme[survey %like% "DHS" | survey %like% "Demographic and Health Survey" | 
       survey_series %like% "DHS", survey_series := "DHS"]


ihme[survey %like% "FHS" | 
       survey %like% "Family Health Survey", survey_series := "Family Health Survey / FHS"]


ihme[survey %like% "HNS" | 
       survey %like% "Nutrition and Health Survey" | 
       survey %like% "Health and Nutrition Survey", survey_series := "HNS"]


ihme[survey %like% "Demographic and Health and Multiple Indicator" | 
       survey %like% "Multiple Indicator Cluster Survey / Demographic and Health Survey" | 
       survey %like% "Joint DHS and MICS", survey_series := "Joint DHS and MICS"]


ihme[survey %like% "Child Health Survey" & 
       !survey %like% "Maternal and Child Health" &
       !survey %like% "Mother and Child Health" & 
       !survey %like% "Reproductive and Child Health", survey_series := "CHS"]


ihme[survey %like% "National Immunization Survey" | survey %like% "National Immunization Coverage Survey" |
       survey_series %like% "NATIONAL_IMMUNIZATION_SURVEY" , survey_series := "NIS"]


ihme[survey %like% "Household Health Survey", survey_series := "HHS"]


ihme[survey %like% "Maternal and Child Health" & 
       !survey %like% "Demographic and Maternal and Child Health", survey_series := "MCH"]


ihme[survey %like% "PAPFAM", survey_series := "PAPFAM"]


ihme[survey %like% "District Level Household" | survey %like% "District-Level Household", survey_series := "DHLS"]


ihme[survey %like% "National Nutrition Survey" | survey %like% "Health and Nutrition Survey" |
       survey_series %like% "HEALTH_AND_NUTRITION" |survey_series %like% "NATIONAL_IMMUNIZTION_SURVEY", survey_series := "NNS (National Nutrition Survey)"]


ihme[survey %like% "Reproductive Health Survey", survey_series := "RHS"]


ihme[survey %like% "Social and Living Standards", survey_series := "SLM"]


ihme[survey %like% "National Maternal and Child Health Family Planning Survey", survey_series := "MCH/FP"]






default_survey_series <- c("MICS", "EPI", "CES", "DHS", "Family Health Survey / FHS", "HNS", "Joint DHS and MICS", 
                           "CHS", "NIS", "HHS", "MCH", "PAPFAM", "DHLS", "NNS (National Nutrition Survey)", "RHS", 
                           "SLM", "MCH/FP")


ihme[, survey_series := ifelse(any(survey_series %in% default_survey_series), 
                               default_survey_series[match(survey_series, default_survey_series)],
                               survey_series), by = c("nid", "ihme_loc_id")]


ihme[, survey_series := ifelse(any(is.na(survey_series)), 
                               unique(survey_series[!is.na(survey_series)]),
                               survey_series), by = c("nid", "ihme_loc_id")]


ihme[, year_start := ifelse(any(is.na(year_start)), 
                            unique(year_start[!is.na(year_start)]),
                            year_start), by = c("nid", "ihme_loc_id")]


ihme[, year_end := ifelse(any(is.na(year_end)), 
                          unique(year_end[!is.na(year_end)]),
                          year_end), by = c("nid", "ihme_loc_id")]


ihme[, survey := ifelse(any(is.na(survey)), 
                        unique(survey[!is.na(survey)]),
                        survey), by = c("nid", "ihme_loc_id")]


ihme[, years := ifelse(any(is.na(years)), 
                       unique(years[!is.na(years)]),
                       years), by = c("nid", "ihme_loc_id")]


ihme <- unique(ihme)


ihme[, year_start := as.integer(year_start)]
ihme[, year_end := as.integer(year_end)]
ihme[!years %like% "-" & is.na(year_start), year_start := as.integer(years)]
ihme[!years %like% "-" & is.na(year_end), year_end := as.integer(years)]
ihme[years %like% "-" & is.na(year_start), year_start := as.integer(substr(years, 1, 4))]
ihme[years %like% "-" & is.na(year_end), year_end := as.integer(substr(years, 6, 9))]
ihme$years <- NULL


setnames(ihme, c("nid", "survey", "ihme_loc_id"), c("ihme_id", "ihme_survey", "iso3"))




old <- c("ISO3", "surveyId", "surveyTypeDescription", "surveyNameEnglish", "surveyNameOrigLang", "collectBegin", "collectEnd")
new <- c("iso3", "who_id", "survey_series", "who_survey_english", "who_survey", "date_start", "date_end")
setnames(who, old, new)


who$surveyType <- NULL
who$surveyNameProduction <- NULL





who[, date_start := as.integer(substr(date_start, 1, 4))]
who[, date_end := as.integer(substr(date_end, 1, 4))]
setnames(who, c("date_start", "date_end"), c("year_start", "year_end"))




who[who_survey_english %like% "MICS" | who_survey_english %like% "Multiple Indicator Cluster Survey" |
       survey_series %like% "MICS", survey_series := "MICS"]


who[who_survey_english %like% "EPI" | 
       who_survey_english %like% "Expanded Programme on Immunization" | 
       who_survey_english %like% "Expanded Programm on Immunization", survey_series := "EPI"]


who[who_survey_english %like% "Coverage Evaluation Survey", survey_series := "CES"]


who[who_survey_english %like% "DHS" | who_survey_english %like% "Demographic and Health Survey" | 
       survey_series %like% "DHS", survey_series := "DHS"]


who[who_survey_english %like% "FHS" | 
       who_survey_english %like% "Family Health Survey", survey_series := "Family Health Survey / FHS"]


who[who_survey_english %like% "HNS" | 
       who_survey_english %like% "Nutrition and Health Survey" | 
       who_survey_english %like% "Health and Nutrition Survey", survey_series := "HNS"]


who[who_survey_english %like% "Demographic and Health and Multiple Indicator" | 
       who_survey_english %like% "Multiple Indicator Cluster Survey / Demographic and Health Survey" | 
       who_survey_english %like% "Joint DHS and MICS", survey_series := "Joint DHS and MICS"]


who[who_survey_english %like% "Child Health Survey" & 
       !who_survey_english %like% "Maternal and Child Health" &
       !who_survey_english %like% "Mother and Child Health" & 
       !who_survey_english %like% "Reproductive and Child Health", survey_series := "CHS"]


who[who_survey_english %like% "National Immunization Survey" | who_survey_english %like% "National Immunization Coverage Survey" |
       survey_series %like% "NATIONAL_IMMUNIZATION_SURVEY" , survey_series := "NIS"]


who[who_survey_english %like% "Household Health Survey", survey_series := "HHS"]


who[who_survey_english %like% "Maternal and Child Health" & 
       !who_survey_english %like% "Demographic and Maternal and Child Health", survey_series := "MCH"]


who[who_survey_english %like% "PAPFAM", survey_series := "PAPFAM"]


who[who_survey_english %like% "District Level Household" | who_survey_english %like% "District-Level Household", survey_series := "DHLS"]


who[who_survey_english %like% "National Nutrition Survey" | who_survey_english %like% "Health and Nutrition Survey" |
       survey_series %like% "HEALTH_AND_NUTRITION" |survey_series %like% "NATIONAL_IMMUNIZTION_SURVEY", survey_series := "NNS (National Nutrition Survey)"]


who[who_survey_english %like% "Reproductive Health Survey", survey_series := "RHS"]


who[who_survey_english %like% "Social and Living Standards", survey_series := "SLM"]


who[who_survey_english %like% "National Maternal and Child Health Family Planning Survey", survey_series := "MCH/FP"]




temp <- merge(who, ihme, all.x = TRUE, all.y = TRUE, by = c("iso3", "survey_series", "year_start", "year_end"))
temp <- temp[order(iso3, year_start, year_end), .(iso3, ihme_id, who_id, survey_series, ihme_survey, who_survey_english, year_start, year_end)]
mismatch <- temp[(is.na(ihme_id) & !is.na(who_id)) | (!is.na(ihme_id) & is.na(who_id)), ]
match <- temp[!((is.na(ihme_id) & !is.na(who_id)) | (!is.na(ihme_id) & is.na(who_id))), ]

fwrite(temp, file = paste0(vaccines_root, "FILEPATH/source_comparison_all.csv"), row.names = FALSE)
fwrite(mismatch, file = paste0(vaccines_root, "FILEPATH/source_comparison_mismatch.csv"), row.names = FALSE)
fwrite(match, file = paste0(vaccines_root, "FILEPATH/source_comparison_match.csv"), row.names = FALSE)





