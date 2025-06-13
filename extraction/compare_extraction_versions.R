













os <- .Platform$OS.type
if (os == "windows") {
  j_root <- "J:"
} else {
  j_root <- "FILEPATH"
  username <- Sys.info()[["user"]]
}


library(data.table)
library(magrittr)


source(paste0("FILEPATH", username, "FILEPATH/init.r"))
'%!in%'  <- function(x,y)!('%in%'(x,y))

code_root <- file.path("FILEPATH", username, "vaccines")
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), verbose = T)


if (!exists("config_path")) config_path <- file.path(code_root, "pipeline_config.yaml")

load_pipeline_config(step = "gbd_lit_extraction", config_path = config_path)

main_dir_path <- "FILEPATH"


cols_to_compare <- c("nid", "survey_name", "ihme_loc_id", "year_id", "age_year", "me_name", "data",
                     "cv_admin", "cv_survey", "sample_size")
exact_mes <- c("vacc_bcg", "vacc_dpt1", "vacc_dpt3", "vacc_hepb3", "vacc_hib3", "vacc_mcv1", "vacc_mcv2",
               "vacc_pcv3", "vacc_polio3", "vacc_rcv1", "vacc_rotac")
mes_to_compare <- c(exact_mes, "vacc_dpt12_cond", "vacc_dpt3_timeliness_ratio")


to_model_dir_data <- paste0(main_dir_path, "FILEPATH", date_new_)
ifelse(!dir.exists(to_model_dir_data), dir.create(to_model_dir_data), FALSE)


cat(paste0(date_new_, "/", file_new_, " vs ", date_old_, "/", file_old_, " data comparison"), file=file.path(to_model_dir_data, "data_comparison_dates.txt"))






create_data_comparisons <- function(date_new=date_new_, cycle_new=cycle_new_,
                                    date_old=date_old_, cycle_old=cycle_old_,
                                    file_new=file_new_, file_old=file_old_,
                                    cols=cols_to_compare,
                                    mes=mes_to_compare) {
  
  
  message("Reading in and subsetting the new and old data versions")
  print(file.path(main_dir_path, date_new, file_new))
  data_new <- readRDS(file.path(main_dir_path, date_new, file_new)) %>%
    .[, ..cols] %>% .[me_name %in% mes]
  data_old <- readRDS(file.path(main_dir_path, date_old, file_old)) %>%
    .[, ..cols] %>% .[me_name %in% mes]
  
  
  data_new[, new := 1]
  data_old[, old := 1]
  setnames(data_new, c("data", "sample_size", "cv_survey"), c("data_new", "sample_size_new", "cv_survey_new"), skip_absent = T)
  setnames(data_old, c("data", "sample_size", "cv_survey"), c("data_old", "sample_size_old", "cv_survey_old"), skip_absent = T)
  
  
  message("Merging data versions together")
  data_cp <- merge(data_new, data_old, by=names(data_new)[!names(data_new) %in% c("data_new", "new", "sample_size_new", "cv_survey_new")], all.x=T, all.y=T)
  
  
  message("Making the comparisons")
  data_cp <- data_cp[!(is.na(data_new) & is.na(data_old))]
  
  
  data_cp_all <- copy(data_cp)
  
  
  data_cp_same <- data_cp[new==1 & old==1]
  
  
  data_new_only <- data_cp[new==1 & is.na(old)]  
  data_old_only <- data_cp[is.na(new) & old==1]
  
  data_cp_same[, data_diff := ((data_new - data_old) / data_old) * 100]
  data_cp_same[, sample_size_diff := ((sample_size_new - sample_size_old) / sample_size_old) * 100]
  
  colorder <- c("nid", "survey_name", "ihme_loc_id", "year_id", "age_year", "me_name", "cv_admin", "data_new", "data_old", "data_diff", "sample_size_new", "sample_size_old", "sample_size_diff", "cv_survey_new", "cv_survey_old")
  
  setcolorder(data_cp_same, colorder)
  
  
  data <- list(data_cp_same,
               data_new_only,
               data_old_only)
  
  message("Done")
  return(data)
  
  
}








data_list <- create_data_comparisons()


fwrite(data_list[[1]], file.path(to_model_dir_data, "1_same_new_and_old_data.rds"))
fwrite(data_list[[2]], file.path(to_model_dir_data, "2_new_data_or_cohorts.rds"))
fwrite(data_list[[3]], file.path(to_model_dir_data, "3_old_or_dropped_data.rds"))




new_data <- nrow(data_list[[2]])
dropped_data <- nrow(data_list[[3]])
changed_data <- nrow(data_list[[1]][abs(data_diff) > 0])
changed_sample_size <- nrow(data_list[[1]][abs(sample_size_diff) > 0])

message("Comparing ", date_new_, "/", file_new_, " from ", cycle_new_, " with ", date_old_, "/", file_old_, " from ", cycle_old_)
message("by nid, ihme_loc_id, year, me_name. Data is saved in ", to_model_dir_data)
message("There were ", new_data, " row(s) of data added in the new version, see 2_new_data_or_cohorts.csv")
message("There were ", dropped_data, " row(s) of data dropped in the new version, see 3_old_or_dropped_data.csv")
message("Of the rows present in both versions - ")
message("    ", changed_data, " row(s) had different values in the data column")
message("    ", changed_sample_size, " row(s) had different values in the sample_size column")
message("see diff columns in 1_same_new_and_old_data.csv for more info")
