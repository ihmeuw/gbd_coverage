












username        <- Sys.info()[["user"]]
vaccine_repo    <- paste0("FILEPATH", username, "FILEPATH")
extraction_root <- "FILEPATH"


suppressMessages(invisible(library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                                 R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))))
suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))
source("FILEPATH/load_packages.R")
suppressMessages(invisible(load_packages(c("proto", "findpython", "getopt", "survey", "gtools"))))


source(paste0(vaccine_repo, "FILEPATH/process.r"))
source(paste0(vaccine_repo, "FILEPATH/resample.r"))

print("Successfully loaded packages")



datestamp <- gsub("-", "_", Sys.Date())
outdir    <- paste0("FILEPATH", datestamp, "/")
if (!dir.exists(paste0(outdir, "/resampled_lit_extractions_nid"))) {
  dir.create(paste0(outdir, "/resampled_lit_extractions_nid"), recursive = TRUE)
}


outlier_filepath <- "FILEPATH/vaccination.csv"




push_lit_extraction_update <- function() {
  wd_copy <- getwd()
  setwd(ref_data_repo)

  system(SYSTEM_COMMAND)
  system(SYSTEM_COMMAND)
  system(SYSTEM_COMMAND)

  setwd(wd_copy)
}


prepare_lit_data <- function(dt_lit_extraction){

  
  dt_lit_extraction <- dt_lit_extraction[!is.na(location_code) & !is.na(shapefile), ]

  
  dt_lit_extraction <- dt_lit_extraction[!which(cv_admin == 1), ]

  
  column_names <- c("sample_size"     = "N_obs",
                    "nid"             = "svy_id",
                    "parent_location" = "ihme_loc_id")
  setnames(dt_lit_extraction, names(column_names), unname(column_names))

  
  vaccine_columns <- names(dt_lit_extraction)[grepl("vacc", names(dt_lit_extraction))]

  for (column in vaccine_columns) {
    dt_lit_extraction[, (column) := as.numeric(get(column))]
  }

  dt_lit_extraction$N_obs <- as.numeric(as.character(dt_lit_extraction$N_obs))
  dt_lit_extraction$survey_name <- "Custom"
  dt_lit_extraction <- dt_lit_extraction[paste(dt_lit_extraction$age_start, dt_lit_extraction$age_end, sep="_") %in% c("12_23", "24_35", "36_47", "48_59")]
  dt_lit_extraction[, year_id := floor((year_start + year_end)/2) - (floor(age_end/12))]
  dt_lit_extraction <- dt_lit_extraction[year_id > 1998, ]
  dt_lit_extraction[, point := 0]


  
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
  for (i in duples) {
    
    if(i[2] %in% names(dt_lit_extraction)){
      dt_lit_extraction <- dt_lit_extraction[, (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))), get(i[2]), get(i[1]))]
    }
  }
  return(dt_lit_extraction)
}



clean_lit_extraction_filter_table <- function(){
  all_filter_tables <- list.files(filter_table_path)
  lit_filter_tables <- all_filter_tables[grepl("lit", all_filter_tables)]
  if (length(lit_filter_tables > 0)) {
    lit_filter_tables_paths <- paste0(filter_table_path, lit_filter_tables)
    file.remove(lit_filter_tables_paths)
  }
}



create_lit_extraction_filter_tables <- function(dt_lit_extraction){
  lit_filter_table <- create_filter_table()[0]

  lit_nids <- unique(dt_lit_extraction$svy_id)
  for (lit_nid in lit_nids) {
    
    nid_lit_extraction <- dt_lit_extraction[svy_id == lit_nid, ]
    lit_total          <- nid_lit_extraction[, sum(N_obs, na.rm = T)]

    nid_lit_extraction <- nid_lit_extraction[!is.na(N_obs), ]
    lit_with_age_data  <- nid_lit_extraction[!is.na(age_start) & !is.na(age_end), sum(N_obs, na.rm = T)]

    nid_lit_extraction <- nid_lit_extraction[!is.na(age_start) & !is.na(age_end), ]
    lit_12_59_months   <- nid_lit_extraction[paste(nid_lit_extraction$age_start, nid_lit_extraction$age_end, sep="_") %in% c("12_23", "24_35", "36_47", "48_59"), sum(N_obs, na.rm = T)]

    nid_lit_extraction <- nid_lit_extraction[paste(nid_lit_extraction$age_start, nid_lit_extraction$age_end, sep="_") %in% c("12_23", "24_35", "36_47", "48_59"), ]
    lit_with_mcv_data  <- nid_lit_extraction[!is.na(vacc_mcv1), sum(N_obs, na.rm = T)]

    nid_lit_extraction <- nid_lit_extraction[!is.na(vacc_mcv1), ]
    lit_in_year_range  <- nid_lit_extraction[year_id > 1998, .N] > 1

    
    filter_table <- create_filter_table()

    filter_table[, survey_id            := lit_nid]
    filter_table[, total                := lit_total]
    filter_table[, n_with_age_data      := lit_with_age_data]
    filter_table[, n_12_59_months       := lit_12_59_months]
    filter_table[, n_with_mcv_data      := lit_with_mcv_data]
    filter_table[, survey_in_year_range := lit_in_year_range]

    if(lit_in_year_range == FALSE){
      filter_table[, record_filter_successful := TRUE]
      filter_table[, setdiff(names(filter_table), c("survey_id", "missing_indicator", "survey_in_year_range", "outlier", "record_filter_successful")) := NA]
    } else {
      filter_table[, n_without_age_data  := total - n_with_age_data]
      filter_table[, n_outside_age_range := n_with_age_data - n_12_59_months]
      filter_table[, n_without_mcv_data  := n_12_59_months - n_with_mcv_data]
    }
    write.csv(filter_table, file = paste0(filter_table_path, "lit_", lit_nid, ".csv"), row.names = F)
  }
}



add_ratios <- function(data, ratio_vaccines){
  for (ratio_vaccine in ratio_vaccines) {
    data[!is.na(vacc_dpt3) & !is.na(get(ratio_vaccine)), paste0(ratio_vaccine, "_dpt3_ratio") := get(ratio_vaccine) / vacc_dpt3]
    data[get(paste0(ratio_vaccine, "_dpt3_ratio")) == Inf, paste0(ratio_vaccine, "_dpt3_ratio") := NA]
    data[is.nan(get(paste0(ratio_vaccine, "_dpt3_ratio"))), paste0(ratio_vaccine, "_dpt3_ratio") := NA]
  }
  data
}


wait_for_job_completion <- function(job_name, output_directory, vaccine_to_run) {

  
  Sys.sleep(5)

  
  start.time <- proc.time()

  
  flag <-  0
  while (flag == 0) {
    
    if (system(paste0(SYSTEM_COMMAND), intern=T) == 0) {
      
      flag <- 1
    } else {
      Sys.sleep(5)
    }
  }

  
  job.runtime <- proc.time() - start.time
  job.runtime <- job.runtime[3]

  
  Sys.sleep(10)

  
  print(paste0("\nNo more jobs running for ", vaccine_to_run, ". Time elapsed: ", job.runtime))

  
  resampled_nid_n <- length(list.files(output_directory))
  if (resampled_nid_n > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


combine_resampled_lit_data <- function(outdir_vaccine) {
  resampled_files         <- list.files(outdir_vaccine, full.names = TRUE)
  resampled_lit_data_list <- lapply(resampled_files, fread)
  resampled_lit_data      <- rbindlist(resampled_lit_data_list)
  return(resampled_lit_data)
}




parser <- ArgumentParser()
parser$add_argument("--action",  help="'parent' to launch the parent job; 'child' to launch a child job for each nid", default="parent", type="character")
parser$add_argument("--project", help="prod cluster project", default="proj_geospatial", type="character")
parser$add_argument("--nid", help="NID(s) to process", default="all", type="character")
parser$add_argument("--antigen", help="Antigens to process: dpt,hepb,mcv or 'all'", default="all", type="character")


args <- parser$parse_args()
message(args)
list2env(args, environment()); rm(args)


if (antigen=="all") {
  vaccines_to_run <- c("bcg", "dpt", "hepb", "hib", "mcv", "pcv", "polio", "rcv", "rota", "yfv")
} else {
  vaccines_to_run      <- strsplit(gsub(",", " ", antigen), split=" +")[[1]]
}


if (nid=="all") {
  nids      <- gsub(".csv", "", gsub(".*_\\s*|_.*", "", list.files(file.path(extraction_root, "raw")))) %>% unique %>% as.numeric %>% sort
} else {
  nids      <- as.numeric(strsplit(gsub(",", " ", nid), split=" +")[[1]])
}



if(action == "parent") {

  
  message("    Beginning parent section of loop")

  
  dt_lit_extraction <- fread(file.path(ref_data_repo, "literature_extraction_coverage.csv"))
  push_lit_extraction_update()
  dt_lit_extraction <- dt_lit_extraction[!nid %in% c(448116, 989898), ]
  dt_lit_extraction <- prepare_lit_data(dt_lit_extraction)

  
  ratio_vaccines    <- c("vacc_hib3", "vacc_hepb3")
  dt_lit_extraction <- add_ratios(dt_lit_extraction, ratio_vaccines)

  
  outlier <- fread(outlier_filepath)

  
  for (vaccine_to_run in vaccines_to_run) {
    
    message("    Beginning vax-specific loop")

    message(paste0("\n\n    ", vaccine_to_run, "    \n\n"))

    
    if (grepl("ratio", vaccine_to_run)) {
      
      outlier_names <- gsub("_ratio", "", vaccine_to_run)
      outlier_names <- unlist(strsplit(outlier_names, split = "_"))
      outlier_names <- unique(outlier$me_name)[grep(paste(outlier_names, collapse = "|"), unique(outlier$me_name))]
      outlier_names <- outlier_names[!grepl("ratio",  outlier_names)]
      outlier_lbd   <- outlier[lbd == 1 & me_name %in% outlier_names,]
    } else {
      outlier_names <- unique(outlier$me_name)[grep(vaccine_to_run, unique(outlier$me_name))]
      outlier_names <- outlier_names[!grepl("ratio",  outlier_names)]
      outlier_names <- c(outlier_names, "")
      outlier_lbd   <- outlier[lbd == 1 & me_name %in% outlier_names,] 
    }

    if(vaccine_to_run == "bcg")   { dose_vars <- c("vacc_bcg")}
    if(vaccine_to_run == "yfv")   { dose_vars <- c("vacc_yfv")}
    if(vaccine_to_run == "rcv")   { dose_vars <- c("vacc_rcv1")}
    if(vaccine_to_run == "mcv")   { dose_vars <- c("vacc_mcv1", "vacc_mcv2") }
    if(vaccine_to_run == "dpt")   { dose_vars <- c("vacc_dpt1", "vacc_dpt2", "vacc_dpt3") }
    if(vaccine_to_run == "hib")   { dose_vars <- c("vacc_hib1", "vacc_hib2", "vacc_hib3") }
    if(vaccine_to_run == "hepb")  { dose_vars <- c("vacc_hepb1", "vacc_hepb2", "vacc_hepb3","vacc_hepb3_dpt3_ratio") }
    if(vaccine_to_run == "pcv")   { dose_vars <- c("vacc_pcv1", "vacc_pcv2", "vacc_pcv3") }
    if(vaccine_to_run == "rota")  { dose_vars <- c("vacc_rota1", "vacc_rota2", "vacc_rota3") }
    if(vaccine_to_run == "polio") { dose_vars <- c("vacc_polio1", "vacc_polio2", "vacc_polio3") }
    if(vaccine_to_run == "hib_dpt_ratio")  { dose_vars <- c("vacc_hib3_dpt3_ratio") }
    if(vaccine_to_run == "hepb_dpt_ratio") { dose_vars <- c("vacc_hepb3_dpt3_ratio") }

    
    keep_cols <- c("svy_id", "file_path", "point", "ihme_loc_id", "location_code", "shapefile",
                   "year_id", "year_start", "year_end", "age_start", "age_end", "N_obs",
                   dose_vars)
    df_lbd <- subset(dt_lit_extraction, select = names(dt_lit_extraction) %in% keep_cols)

    
    if (vaccine_to_run != "mcv"){
      for(var in dose_vars) {
        df_lbd <- df_lbd[!is.na(get(var)), ]
      }
    } else {
      df_lbd <- df_lbd[!is.na("vacc_mcv1"), ]
    }

    
    
    if(vaccine_to_run == "rota"){
      df_rotac <- data.table()
      for(country in unique(df_lbd$ihme_loc_id)){
        data <- df_lbd[ihme_loc_id == country,]
        doses <- readRDS("FILEPATH/vaccine_schedule.rds")[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), .(doses)]
        intro <- readRDS("FILEPATH/vaccine_intro.rds")[me_name=="vacc_rotac" & ihme_loc_id==unique(data$ihme_loc_id), .(cv_intro)] %>% unique

        
        if (nrow(doses)==0) doses <- 0
        doses <- ifelse(doses >= 3, 3, 2)
        rota_dose <- paste0("vacc_rota", doses)

        
        if (nrow(intro)==0) stop(paste0("BREAK | Missing introduction year for ", unique(data$ihme_loc_id), "; need to prep introduction frame for this geography before continuing"))
        if (intro < 9999) data$rotac <- data[, rota_dose, with=FALSE]
        df_rotac <- data.table(rbind(df_rotac, data))
      }
      df_lbd <- df_rotac
    }

    
    df_de <- fread("FILEPATH/design_effect_nids.csv")
    df_de <- df_de[vacc == vaccine_to_run, ]
    df_de <- df_de[!nid %in% outlier_lbd$nid ]

    de <- median(df_de$de, na.rm=T)
    message(paste0("Median design effect for ", vaccine_to_run, " ~ ", round(de, 4)))
    df_lbd[ ,N := N_obs * de]

    for (var in dose_vars) {
      df_lbd[ ,(var) := get(var)/100 * N]
    }

    names(df_lbd)[names(df_lbd) == "vacc_yfv"]  <- "yfv_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_bcg"]  <- "bcg_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_rcv1"]  <- "rcv_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_mcv1"]  <- "mcv_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_mcv2"]  <- "mcv_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_dpt1"]  <- "dpt_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_dpt2"]  <- "dpt_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_dpt3"]  <- "dpt_dose_3"
    names(df_lbd)[names(df_lbd) == "vacc_hib1"]  <- "hib_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_hib2"]  <- "hib_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_hib3"]  <- "hib_dose_3"
    names(df_lbd)[names(df_lbd) == "vacc_hepb1"] <- "hepb_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_hepb2"] <- "hepb_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_hepb3"] <- "hepb_dose_3"
    names(df_lbd)[names(df_lbd) == "vacc_pcv1"]  <- "pcv_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_pcv2"]  <- "pcv_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_pcv3"]  <- "pcv_dose_3"
    names(df_lbd)[names(df_lbd) == "vacc_polio1"]  <- "polio_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_polio2"]  <- "polio_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_polio3"]  <- "polio_dose_3"
    names(df_lbd)[names(df_lbd) == "vacc_rota1"]  <- "rota_dose_1"
    names(df_lbd)[names(df_lbd) == "vacc_rota2"]  <- "rota_dose_2"
    names(df_lbd)[names(df_lbd) == "vacc_rota3"]  <- "rota_dose_3"

    df_lbd[ , weight := 1]

    if (vaccine_to_run %in% c("dpt", "pcv", "hib", "hepb", "polio")) {
      
      vars <- paste0(vaccine_to_run, c("_dose_1", "_dose_2", "_dose_3"))
      df_lbd[ , (vars[1]) := get(vars[1]) - get(vars[2])]
      df_lbd[ , (vars[2]) := get(vars[2]) - get(vars[3])]

      
      df_lbd[get(vars[1]) > 0, ]
      df_lbd[get(vars[2]) > 0, ]
    }

    if (vaccine_to_run == "mcv" & "mcv_dose_2" %in% names(df_lbd)) {
      vars <- paste0(vaccine_to_run, c("_dose_1", "_dose_2"))
      df_lbd[!is.na(get(vars[2])) , (vars[1]) := get(vars[1]) - get(vars[2])]
    }

    
    write.csv(df_lbd, paste0(outdir, vaccine_to_run, "_custom_presample.csv"), row.names=FALSE)
    message("    Prepped data saved")

    
    for(nid in df_lbd[, unique(svy_id)]) {
      
      job_name <- paste0("CHILD_", nid, "_", vaccine_to_run, "_lit_resample")
      shell    <- "FILEPATH/singR.sh"
      job      <- paste0("qsub -l m_mem_free=8G ", "-P ", project,
                         " -q geospatial.q",
                         " -l fthread=12",
                         " -l archive=TRUE",
                         
                         " -N ", job_name,
                         " ", shell, " -e s ",
                         vaccine_repo, "extraction/lit_ex_lbd_parallel.R",
                         " --action child ",  "--project ", project, " --nid ", nid, " --antigen ", vaccine_to_run)
      print(job)
      system(SYSTEM_COMMAND); print(job)
    }
    
    outdir_vaccine <- paste0(outdir, "FILEPATH", vaccine_to_run, "/")

    
    jobs_completed <- wait_for_job_completion(paste0(vaccine_to_run, "_lit_resample"), outdir_vaccine, vaccine_to_run)

    
    if (jobs_completed) {
      resampled_lit_data <- combine_resampled_lit_data(outdir_vaccine)
      write.csv(resampled_lit_data, file = paste0(outdir, vaccine_to_run, "_lit_resample.csv"), row.names = FALSE)
      print(paste0(vaccine_to_run, ": Successfully resampled"))
    }
  }
} else {
  message("    Beginning child loop (!!)")
  vaccine_to_run <- vaccines_to_run
  
  prepped_lit_data <- fread(paste0(outdir, vaccine_to_run, "_custom_presample.csv"))
  
  print(nid)
  nid <- as.integer(nid)
  df_lbd <- prepped_lit_data[svy_id == nid, ]

  message(paste0("    Beginning polygon resample for nid: ", nid, " vaccine: ", vaccine_to_run))

  
  df_pointpoly <- resample_polygons(data = df_lbd,
                                    cores = 15,
                                    indic = vaccine_to_run,
                                    density = 0.001,
                                    gaul_list = get_adm0_codes('all', shapefile_version = 'current'),
                                    pull_poly_method = "fast")

  message(paste0("    Saving polygon resample for nid: ", nid))

  
  outdir_vaccine <- paste0(outdir, "FILEPATH", vaccine_to_run, "/")
  if (!dir.exists(outdir_vaccine)) dir.create(outdir_vaccine, recursive = TRUE)
  write.csv(df_pointpoly, paste0(outdir_vaccine, nid, ".csv"), row.names=FALSE)
}



