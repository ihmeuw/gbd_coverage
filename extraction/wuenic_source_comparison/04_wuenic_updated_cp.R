





library(data.table)
library(openxlsx)



vaccines_root <- "FILEPATH"
source("FILEPATH/get_location_metadata.R")



last_match_date <- ""
gbd_date        <- "2020-06-15-2"
gbd_cycle       <- "gbd2020"
gbd_round       <- 7
decomp_step     <- "iterative"

locations <- get_location_metadata(location_set_id=22, gbd_round_id=gbd_round, decomp_step=decomp_step)[level >= 3, ]






source_comparison_citations <- fread(paste0(vaccines_root, "FILEPATH/source_comparison_with_metadata.csv"))
who_missing_surveys         <- fread(paste0(vaccines_root, "FILEPATH/who_missing_surveys.csv"))
ihme_missing_surveys        <- fread(paste0(vaccines_root, "FILEPATH/ihme_missing_surveys.csv"))


orig_wuenic_ids             <- fread(paste0(vaccines_root, "FILEPATH/wuenic_svy_list.csv"))
updated_wuenic_ids          <- fread(paste0(vaccines_root, "FILEPATH/wuenic_svy_list_2020-06-10.csv"))  









setnames(updated_wuenic_ids, "surveyId", "who_id")
setnames(updated_wuenic_ids, "ISO3", "iso3")
setnames(updated_wuenic_ids, "surveyNameEnglish", "who_survey_english_updated")

source_comparison_citations[, who_id := ifelse(!is.na(who_id), paste0(iso3, who_id), NA)]




matched_plus_updated <- merge(source_comparison_citations, updated_wuenic_ids, by=c("who_id", "iso3"), all.x=T, all.y=T)  
matched <- matched_plus_updated[!is.na(ihme_id) & !is.na(who_id)]
unmatched <- matched_plus_updated[(!is.na(ihme_id) & is.na(who_id)) | (is.na(ihme_id) & !is.na(who_id))]
only_in_wuenic <- unmatched[!is.na(who_id)]
only_in_ihme <- unmatched[!is.na(ihme_id)]















ihme_only_updated <- read.xlsx(paste0("FILEPATH", gbd_date, "/unmatched_only_in_ihme.xlsx")) %>% data.table
wuenic_only_updated <- read.xlsx(paste0("FILEPATH", gbd_date, "/unmatched_only_in_wuenic.xlsx")) %>% data.table
dhs_mics_matched <- read.xlsx(paste0("FILEPATH", gbd_date, "FILEPATH/unmatched_dhs_mics_in_wuenic_JN_clean.xlsx")) %>% data.table


ihme_only_updated <- ihme_only_updated[!is.na(ihme_id) & !is.na(who_id)][, c("who_id", "ihme_id")] %>% setnames(., "who_id", "new_who_id")


updated_match <- merge(source_comparison_citations, ihme_only_updated, by="ihme_id", all.x=T, all.y=T)  
updated_match <- updated_match[!is.na(new_who_id), who_id := new_who_id][, new_who_id := NULL]


wuenic_only_updated <- wuenic_only_updated[!is.na(ihme_id) & !is.na(who_id)][, c("who_id", "ihme_id")] %>% setnames(., "ihme_id", "new_ihme_id")


updated_match <- merge(updated_match, wuenic_only_updated, by="who_id", all.x=T, all.y=T)  
updated_match <- updated_match[!is.na(new_ihme_id), ihme_id := new_ihme_id][, new_ihme_id := NULL]


dhs_mics_matched <- dhs_mics_matched[!is.na(ihme_id) & !is.na(who_id)][, c("who_id", "ihme_id")] %>% setnames(., "ihme_id", "new_ihme_id")


updated_match <- merge(updated_match, dhs_mics_matched, by="who_id", all.x=T, all.y=T)  
updated_match <- updated_match[!is.na(new_ihme_id), ihme_id := new_ihme_id][, new_ihme_id := NULL]


matches <- updated_match[!is.na(ihme_id) & !is.na(who_id)]
still_ihme_only <- updated_match[!is.na(ihme_id) & is.na(who_id)]
still_wuenic_only <- updated_match[is.na(ihme_id) & !is.na(who_id)]
blanks <- updated_match[is.na(ihme_id) & is.na(who_id)]



ihme_only_id_check <- unique(still_ihme_only$ihme_id)
wuenic_only_id_check <- unique(still_wuenic_only$who_id)
matches_ihme_id_check <- unique(matches$ihme_id)
matches_wuenic_id_check <- unique(matches$who_id)

remove_ihme <- matches[ihme_id %in% ihme_only_id_check, ihme_id]
still_ihme_only <- still_ihme_only[!ihme_id %in% remove_ihme]

remove_wuenic <- matches[who_id %in% wuenic_only_id_check, who_id]
still_wuenic_only <- still_wuenic_only[!who_id %in% remove_wuenic]


vaccination_data <- readRDS(paste0(vaccines_root, "FILEPATH", gbd_cycle, "/", gbd_date, "/vaccination.rds"))
cols <- c("ihme_loc_id", "nid", "survey_name", "year_start", "year_end")
outlier_cols <- c(cols, "cv_outlier")  
input_data <- vaccination_data[!is.na(nid) & nid != 203321, outlier_cols, with=F]
input_data[!is.na(cv_outlier), cv_outlier := 1]
input_data <- unique(input_data)
used_data <- input_data[is.na(cv_outlier)] %>% unique


matches[!ihme_id %in% unique(vaccination_data$nid), need_to_extract := 1]
still_wuenic_only[, need_to_extract := 1]
still_ihme_only[, need_to_extract := NA]
all <- rbind(matches, still_ihme_only, still_wuenic_only)

fwrite(all, paste0("FILEPATH",
                   gbd_date, "FILEPATH/all_sources.csv"), row.names=F)


need_to_extract <- all[need_to_extract==1]
nrow(need_to_extract)  
nrow(need_to_extract[is.na(ihme_id)])  
nrow(need_to_extract[!is.na(ihme_id)])  

fwrite(need_to_extract, paste0("FILEPATH", 
                               gbd_date, "FILEPATH/need_to_extract_after_matching.csv"), row.names=F)





need_to_extract <- fread(paste0("FILEPATH", 
                                gbd_date, "FILEPATH/need_to_extract_after_matching.csv"))
jason_matched <- read_excel(paste0("FILEPATH", 
                              gbd_date, "FILEPATH/unmatched_other_in_wuenic_JN_062520.xlsx")) %>% data.table
setnames(need_to_extract, "who_survey_english", "who_survey_english_updated")


both <- merge(need_to_extract[is.na(ihme_id)], INDIVIDUAL_NAME_matched[,.(who_survey_english_updated, who_id, ihme_id)], by=c("who_survey_english_updated", "who_id"), all.x=T)
both[is.na(ihme_id.x) & !is.na(ihme_id.y), ihme_id.x := ihme_id.y]
setnames(both, "ihme_id.x", "ihme_id")
both[, ihme_id.y := NULL]

need_to_extract_jn <- need_to_extract[!is.na(ihme_id)]
need_to_extract_jn <- rbind(need_to_extract_jn, both)  

need_to_extract_jn$ihme_id <- as.integer(need_to_extract_jn$ihme_id)
jason_matched$ihme_id <- as.integer(INDIVIDUAL_NAME_matched$ihme_id)



setnames(need_to_extract_jn, "who_survey_english_updated", "who_survey_english")

all_minus_jn_match <- all[is.na(need_to_extract)]  

matched_reviewed <- rbind(all_minus_jn_match, need_to_extract_jn)  


status_matched <- merge(matched_reviewed, INDIVIDUAL_NAME_matched[,.(ihme_id, who_id, status)], by=c("ihme_id", "who_id"), all.x=T)

status_matched[ihme_id %in% unique(used_data$nid) & !is.na(ihme_id), ihme_included := 1]

status_matched[ihme_included==1 & need_to_extract==1, need_to_extract := NA]

fwrite(status_matched, paste0("FILEPATH", 
                              gbd_date, "FILEPATH/matched_w_status_update.csv"), row.names=F)








dim(status_matched[is.na(ihme_id)])
dim(status_matched[!is.na(ihme_id) & need_to_extract==1])
unique(status_matched[!is.na(ihme_id) & need_to_extract==1]$status)
dim(status_matched[!is.na(ihme_id) & need_to_extract==1 & status=="Excluded"])
dim(status_matched[!is.na(ihme_id) & need_to_extract==1 & (status=="Accepted but want to replace" | status=="Need more information to assess")])


dim(status_matched[!is.na(ihme_id) & need_to_extract==1 & (status=="Accepted but want to replace" | 
                                                              status=="Need more information to assess" |
                                                              status=="lit extracted" |
                                                              status=="extracted" | 
                                                              status=="extracted but not processed")])
dim(status_matched[!is.na(ihme_id) & need_to_extract==1 & status=="not in cooper"])
dim(status_matched[!is.na(ihme_id) & need_to_extract==1 & (status=="not in cooper" | is.na(status))])

nids_to_be_reviewed <- status_matched[!is.na(ihme_id) & need_to_extract==1 & (status=="Accepted but want to replace" | 
                                                                            status=="Need more information to assess" |
                                                                            status=="lit extracted" |
                                                                            status=="extracted" | 
                                                                            status=="extracted but not processed" |
                                                                            status=="not in cooper" |
                                                                            is.na(status))]
no_nid <- status_matched[is.na(ihme_id)]
to_be_reviewed <- rbind(nids_to_be_reviewed, no_nid)

fwrite(to_be_reviewed, paste0("FILEPATH", 
                              gbd_date, "FILEPATH/matched_w_status_update.csv"), row.names=F)

View(to_be_reviewed)
unique(to_be_reviewed$survey_series)
dim(to_be_reviewed[survey_series=="MICS"])
dim(to_be_reviewed[survey_series=="DHS"])
dim(to_be_reviewed[survey_series=="EPI"])



dim(status_matched[!is.na(ihme_id) & is.na(who_id)])
dim(status_matched[!is.na(ihme_id) & is.na(who_id) & ihme_included==1])
View(status_matched[!is.na(ihme_id) & is.na(who_id) & ihme_included==1])









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











