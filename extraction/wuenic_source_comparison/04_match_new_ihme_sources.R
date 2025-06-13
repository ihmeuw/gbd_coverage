





library(data.table)



vaccines_root <- "FILEPATH"
source("FILEPATH/get_location_metadata.R")



last_match_date <- ""
gbd_date        <- "2020-05-15"
gbd_cycle       <- "gbd2020"
gbd_round       <- 7
decomp_step     <- "iterative"

locations <- get_location_metadata(location_set_id=22, gbd_round_id=gbd_round, decomp_step=decomp_step)[level >= 3, ]








source_comparison_citations <- fread(paste0(vaccines_root, "FILEPATH/source_comparison_with_metadata.csv"))
who_missing_surveys         <- fread(paste0(vaccines_root, "FILEPATH/who_missing_surveys.csv"))
ihme_missing_surveys        <- fread(paste0(vaccines_root, "FILEPATH/ihme_missing_surveys.csv"))
updated_wuenic              <- fread(paste0(vaccines_root, "FILEPATH/wuenic_svy_list_2020-06-10.csv"))  


vaccination_data <- readRDS(paste0(vaccines_root, "FILEPATH", gbd_cycle, "/", gbd_date, "/vaccination.rds"))
cols <- c("ihme_loc_id", "nid", "survey_name", "year_start", "year_end")
gbd_data <- vaccination_data[, cols, with=F] %>% unique %>% .[!is.na(nid) & nid != 203321] %>% .[, new_gbd := 1] 
gbd_data <- merge(gbd_data, locations[,.(ihme_loc_id, level)], by="ihme_loc_id", all.x=T) 


new_nids <- setdiff(unique(gbd_data$nid), source_comparison_citations$ihme_id)
new_sources <- gbd_data[nid %in% new_nids]


prev_ihme_missing <- copy(ihme_missing_surveys)
setnames(prev_ihme_missing, "who_survey", "survey_name")
test_match <- merge(prev_ihme_missing, new_sources, by="survey_name", all=T)
new_sources <- new_sources[order(ihme_loc_id)]
prev_ihme_missing <- prev_ihme_missing[order(iso3)]

compare_dir <- file.path("FILEPATH", gbd_date)
ifelse(!dir.exists(compare_dir), dir.create(compare_dir), FALSE)









length(unique(prev_ihme_missing$who_id)) - 25  
length(unique(who_missing_surveys$ihme_nid)) + 27



outlier_cols <- c(cols, "cv_outlier")  
input_data <- vaccination_data[!is.na(nid) & nid != 203321, outlier_cols, with=F]
input_data[!is.na(cv_outlier), cv_outlier := 1]
input_data <- unique(input_data)
used_data <- input_data[is.na(cv_outlier)] %>% unique

used_data <- used_data[nid %in% unique(who_missing_surveys$ihme_nid)]
used_data <- used_data[, c("survey_name", "year_start", "year_end") := NULL] %>% unique
used_data[, cv_outlier := 0]

used_data <- merge(used_data, locations[,.(ihme_loc_id, level)], by="ihme_loc_id", all.x=T)
used_data[, count := .N, by=nid]
used_data_sub_only <- used_data[count==1 & level != 3]
used_data_nats <- used_data[level==3]
used_data_complete <- rbind(used_data_nats, used_data_sub_only)

setnames(who_missing_surveys, "ihme_nid", "nid")
setnames(who_missing_surveys, "iso3", "ihme_loc_id")
who_missing_surveys[, c("ghdx_url", "citation") := NULL]
who_missing_checks <- merge(who_missing_surveys, used_data_complete, by=c("ihme_loc_id", "nid"), all=T)  


length(unique(who_missing_checks[is.na(cv_outlier)]$nid))


ihme_models <- who_missing_checks[!is.na(cv_outlier)]
dim(ihme_models)


length(unique(ihme_models[!ihme_survey %like% "survey" & !ihme_survey %like% "Survey"]$nid))


sum(c(length(unique(ihme_models[ihme_survey %like% "Annual Report"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Health Statistics"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Statistical Data"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Health Bulletin"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "National Report"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Statistical Yearbook"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Health in Figures"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Statistical Booklet"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Statistics Yearbook"]$nid)),
      length(unique(ihme_models[ihme_survey %like% "Statistical Bulletin"]$nid))))
review_admin <- ihme_models[ihme_survey %like% "Annual Report" | 
                              ihme_survey %like% "Health Statistics" |
                              ihme_survey %like% "Statistical Data" |
                              ihme_survey %like% "Health Bulletin" |
                              ihme_survey %like% "National Report" |
                              ihme_survey %like% "Statistical Yearbook" |
                              ihme_survey %like% "Health in Figures" |
                              ihme_survey %like% "Statistical Booklet" |
                              ihme_survey %like% "Statistics Yearbook" |
                              ihme_survey %like% "Statistical Bulletin" |
                              ihme_survey %like% "Vaccinations in Poland" |
                              ihme_survey %like% "Statistical Abstract"]

review_surveys <- ihme_models[!ihme_survey %like% "Annual Report" & 
                                !ihme_survey %like% "Health Statistics" & 
                                !ihme_survey %like% "Statistical Data" &
                                !ihme_survey %like% "Health Bulletin" &
                                !ihme_survey %like% "National Report" &
                                !ihme_survey %like% "Statistical Yearbook" &
                                !ihme_survey %like% "Health in Figures" &
                                !ihme_survey %like% "Statistical Booklet" &
                                !ihme_survey %like% "Statistics Yearbook" &
                                !ihme_survey %like% "Statistical Bulletin" & 
                                level==3]












