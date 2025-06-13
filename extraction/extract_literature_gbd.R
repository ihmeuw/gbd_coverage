









library(data.table)
library(dplyr)
library(parallel)
library(readxl)


username <- Sys.info()[["user"]]
source(paste0("FILEPATH", username, "FILEPATH/init.r"))
source(db_tools)
source("FILEPATH/get_location_metadata.R")



'%!in%' <- function(x,y)!('%in%'(x,y))


push_lit_extraction_update <- function() {
  wd_copy <- getwd()
  setwd(ref_data_repo)

  system(SYSTEM_COMMAND)
  system(SYSTEM_COMMAND)
  system(SYSTEM_COMMAND)

  setwd(wd_copy)
}



df <- fread(file.path(ref_data_repo, "literature_extraction_coverage.csv"))
push_lit_extraction_update()


decomp_step <- "iterative"
locations   <- get_location_metadata(location_set_id = location_set_id,
                                     release_id = release_id)[level >= 3]


cohorts <- readRDS(file.path(ref_data_repo, "vaccine_target.rds"))


rotac_schedule <- readRDS(file.path(ref_data_repo, "vaccine_schedule.rds"))





message("Swapping out coverage from component vaccine where combination vaccine is higher")
duples <- list(c("vacc_dpt1",  "vacc_tetra1"),
               c("vacc_dpt1",  "vacc_pent1"),
               c("vacc_dpt2",  "vacc_pent2"),
               c("vacc_dpt2",  "vacc_tetra2"),
               c("vacc_dpt3",  "vacc_tetra3"),
               c("vacc_dpt3",  "vacc_pent3"),
               c("dpt3_timeliness",  "pent3_timeliness"),
               c("vacc_hib3",  "vacc_pent3"),
               c("vacc_hib3",  "vacc_tetra3"),
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
                     c("dpt3_timeliness",  "pent3_timeliness"),
                     c("vacc_hib3",  "vacc_pent3"),
                     c("vacc_hib3",  "vacc_tetra3"),
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
                     c("dpt3_timeliness",  "pent3_timeliness"),
                     c("vacc_polio3",  "vacc_pent3"),  
                     c("vacc_hib3",  "vacc_tetra3"),
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
  
  df <- df[nid %in% unique(alt_penta_1$survey_id),
           (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
}

for (i in duples_alt_2) {
  
  df <- df[nid %in% unique(alt_penta_2$survey_id),
           (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
}


for (i in duples) {
  
  df <- df[!nid %in% unique(alt_penta$survey_id),
           (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
}


df[, ihme_loc_id := tstrsplit(location_name_short_ihme_loc_id, "[|]")[[2]]]


setnames(df, "dpt3_timeliness", "vacc_dpt3_timeliness_ratio")


id   <- c("nid", "file_path", "ihme_loc_id", "year_start", "year_end", "age_start", "age_end", "sample_size", "cv_admin", "cv_do_not_shift")
vacc <-  grep("vacc", names(df), value=TRUE)
df   <- melt(df, id=id, measure=patterns("^vacc|^maternal"), value.name="data", variable.name="me_name", variable.factor = FALSE)




message("Cleaning data")


df <- df[!is.na(data)]


df[me_name != "vacc_dpt3_timeliness_ratio", data := (as.numeric(data) / 100)]


df[data > 1, data := 1]


df <- df[!is.na(data)]


df <- df[!is.na(ihme_loc_id)]


df$data <- as.numeric(df$data)


setnames(rotac_schedule, "me_name", "rota_complete")
rotac_schedule[, me_name := paste0("vacc_rota", doses)]
rotac <- merge(df, rotac_schedule, by=c("ihme_loc_id", "me_name"),
               all.x = TRUE, all.y = TRUE)[!is.na(rota_complete) & !is.na(data)]
rotac[, `:=` (me_name = rota_complete, location_id = NULL, doses = NULL, rota_complete = NULL)]
df    <- rbind(df, rotac)



df[, sample_size := gsub(",", "", sample_size) %>% as.numeric]
df[sample_size > 5000, sample_size := NA]


df_months <- copy(df)


df_months <- df_months[is.na(age_start) & is.na(age_end) & is.na(cv_admin), `:=` (age_start=12, age_end=23)]
saveRDS(df_months, file.path(ref_data_repo, "vaccine_lit_months.rds"))



df[, age_start_months  := age_start]
df[, age_end_months    := age_end]
df[, age_length_months := age_end_months - age_start_months]


df$special <- TRUE
df[paste(age_start_months, age_end_months, sep = "-") %in% c("12-23", "24-35", "36-47", "48-59", "60-71"), special := FALSE]





df[, `:=` (age_start = round(age_start / 12),
           age_end   = round(age_end / 12))]
df[, age_length := age_end - age_start]

df[age_length %in% c(0, 1), age_year := age_start]
df[is.na(age_length), age_year := 1]
df[age_length > 1, age_year := round((age_start + age_end)/2)]


df[cv_admin==1 | cv_do_not_shift==1, year_id := year_start]
df[is.na(cv_admin) & is.na(cv_do_not_shift), year_id := year_start-age_year]


df <- df[(age_year > 0 & age_year < 5) |
           cv_admin==1 |
           cv_do_not_shift==1]




df[, row_id := 1:nrow(df)]
df[, keep := TRUE]
overlap <- df[is.na(cv_admin), .N, by = c("nid", "ihme_loc_id", "me_name", "year_id")][N > 1, ]

message("Resolving duplicate extractions")

for(i in 1:nrow(overlap)) {

  
  if(i %% 10 == 0) {
    percentage_complete <- round(i / nrow(overlap), digits = 2) * 100
    message(paste0("-- ", percentage_complete, "%"))
  }

  
  overlap_fixed <- FALSE

  i_nid         <- overlap[i, nid]
  i_ihme_loc_id <- overlap[i, ihme_loc_id]
  i_me_name     <- overlap[i, me_name]
  i_year_id     <- overlap[i, year_id]

  overlap_row_ids <- df[nid == i_nid & ihme_loc_id == i_ihme_loc_id & me_name == i_me_name & year_id == i_year_id, row_id]

  
  duplicates <- df[row_id %in% overlap_row_ids, setdiff(names(df), c("file_path", "row_id")), with = FALSE] %>% duplicated
  if(any(duplicates)) {
    duplicated_row_ids <- overlap_row_ids[duplicates]
    df[row_id %in% duplicated_row_ids, keep := FALSE]
    overlap_row_ids <- df[row_id %in% overlap_row_ids & keep == TRUE, row_id]
    if(length(overlap_row_ids) == 1) {
      overlap_fixed <- TRUE
    }
  }

  
  if(!overlap_fixed){
    regular_cohort_exists <- df[row_id %in% overlap_row_ids, any(!special)]
    if(regular_cohort_exists) {
      df[row_id %in% overlap_row_ids & special == TRUE, keep := FALSE]
      overlap_row_ids <- df[row_id %in% overlap_row_ids & keep == TRUE, row_id]
      if(length(overlap_row_ids) == 1) {
        overlap_fixed <- TRUE
      }
    }
  }

  
  
  if(!overlap_fixed){
    special_cohorts_only <- df[row_id %in% overlap_row_ids, all(special)]
    if(special_cohorts_only) {
      different_age_lengths <- length(df[row_id %in% overlap_row_ids, unique(age_length_months)]) > 1
      if(different_age_lengths) {
        incorrect_age_length_row_ids <- df[row_id %in% overlap_row_ids, .SD[which(abs(age_length_months - 11) != min(abs(age_length_months - 11))), row_id],
                                           .SDcols = c("nid", "row_id", "age_length_months")]
        df[row_id %in% incorrect_age_length_row_ids, keep := FALSE]
        overlap_row_ids <- df[row_id %in% overlap_row_ids & keep == TRUE, row_id]
      }
      if(length(overlap_row_ids) == 1) {
        overlap_fixed <- TRUE
      }
    }
  }

  
  

  if(!overlap_fixed){
    special_cohorts_only <- df[row_id %in% overlap_row_ids, all(special)]
    if(special_cohorts_only) {
      same_age_lengths <- length(df[row_id %in% overlap_row_ids, unique(age_length_months)]) == 1
      if(same_age_lengths) {
        overlap_row_id_largest_sample_size <- df[row_id %in% overlap_row_ids, .SD[sample_size == max(sample_size), row_id]]
        df[row_id %in% overlap_row_ids & row_id %!in% overlap_row_id_largest_sample_size, keep := FALSE]
        overlap_row_ids <- df[row_id %in% overlap_row_ids & keep == TRUE, row_id]
        if(length(overlap_row_ids) == 1) {
          overlap_fixed <- TRUE
        }
      }
    }
  }

  
  if(!overlap_fixed){
    error_message <- paste0("Multiple data points for ", i_year_id, " ", i_ihme_loc_id, " ", i_me_name, " from lit extraction sheet. Review lit extraction age-processing logic.")
    stop(error_message)
  }
}

message("Duplicate extractions resolved")


df <- df[keep == TRUE, ]
df[, c("age_start_months", "age_end_months", "year_start", "year_end", "row_id", "keep") := NULL]




df <- merge(df, cohorts, by=c("ihme_loc_id", "me_name"), all.x=T)
df[!is.na(age_cohort) & ((age_end==1 & age_start != age_end) | age_end < age_cohort) & is.na(cv_admin), drop := "drop"]  

df <- df[is.na(drop)]

df[, c("age_cohort", "drop") := NULL]






parsed <- df[, tstrsplit(file_path, "/", fixed=TRUE)]
parsed <- parsed[, survey_name := ifelse(nchar(V3)==3, paste0(V3, "/", V4), V3)]  
df     <- cbind(df, parsed$survey_name)
setnames(df, "V2", "survey_name")




prep.nisreport <- function() {

  
  length <- year.est.end - 2009
  path   <- paste0(data_root, "FILEPATH/dataView2011_3_MMRonly.xls")  
  nis    <- read_excel(path, sheet="SVV Coverage Trend 2016-17 Data", skip=2,
                       col_types=c("text", rep(c("text", "skip", "text", "text", "text", "text"), length))) %>% data.table

  
  nis <- nis[!Names %in% c("HP 2020 Target", "Median")]
  nis <- nis[, colnames(nis)[grep("SURVEY TYPE|TARGET|TOTAL KINDERGARTEN POPULATION|PERCENT SURVEYED", colnames(nis))] := NULL]

  
  yrs <- 2009:(year.est.end - 1)
  names <- c("name", paste0("x_", yrs))
  colnames(nis) <- names

  
  nis[, (paste0("coverage_", yrs)) := lapply(paste0("x_", yrs), function(cols) as.numeric(get(cols)))]
  nis <- melt(nis[, c("name", paste0("coverage_", yrs)), with=FALSE], value.name="data")

  
  nis[, year_start := NA_integer_]
  nis[, year_end := NA_integer_]
  nis[, year_start := substring(variable, 10, 13) %>% as.integer]
  nis[, year_end := year_start + 1]
  nis[, year_id := floor((year_start + year_end) / 2)]
  nis[, variable := NULL]
  setnames(nis, "name", "location_name")
  nis <- merge(nis[!grep("Median", location_name)], locations[parent_id==102, .(ihme_loc_id, location_name)], by="location_name", all.x=TRUE) %>%
    .[, location_name := NULL]

  
  nis[, data := data / 100]

  
  nis[, nid := 334866]
  nis[, file_path := path]
  nis[, variance := data * (1 - data) / 50]
  nis[, cv_survey := 0]
  nis[, age_group_id := 22]
  nis[, sex_id := 3]
  nis[, survey_name := "School Vaccination Assessment Program"]
  nis <- nis[!is.na(data), ]
  nis_mmr <- copy(nis)[, me_name := "vacc_mmr2"]
  nis_mcv <- copy(nis)[, me_name := "vacc_mcv2"]
  nis <- rbind(nis_mmr, nis_mcv)

  
  return(nis)
}

year.est.end <- 2017  
nis <- prep.nisreport()

df  <- rbind(df, nis, fill=TRUE)



message(paste0("Literature extractions prepped and saved: ", file.path(ref_data_repo, "vaccine_lit.rds")))
saveRDS(df, file.path(ref_data_repo, "vaccine_lit.rds"))


archive_folder <- format(Sys.time(), "%Y_%m_%d")
dir.create(file.path(extraction_root, "gbd_lit_extraction", archive_folder), recursive = T)
saveRDS(df, file.path(extraction_root, "gbd_lit_extraction", archive_folder, "vaccine_lit.rds"))


