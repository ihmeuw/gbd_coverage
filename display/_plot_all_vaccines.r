#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    Feb 2018
# Purpose: Create time series plots and maps of custom ST-GPR output (national level for time series, all subnationals for maps) for all vaccines
# Run:     source("/FILEPATH/_plot_all_vaccines.r", echo=TRUE)
#***********************************************************************************************************************


#----FUNCTION-------------------------------------------------------------------------------------------------------------
plot_gpr <- function(date,
                     current_gbd   = TRUE,
                     data_date     = "NULL",
                     output.path,
                     add.regions   = FALSE,
                     add.outliers  = TRUE,
                     y.axis        = FALSE,
                     all_vaccines  = FALSE,
                     compare       = "NULL",
                     compare_round = 2019,
                     add_wuenic    = TRUE,
                     add_mbg       = FALSE, 
                     compare_more  = "NULL", 
                     compare_more_round = 2020, 
                     data_compare=data_compare, 
                     data_compare_date=data_compare_date, 
                     data_compare_rd=data_compare_rd, 
                     publication_plots=publication, 
                     last_pub_year=last_year) {
  
  ### make sure using on cluster
  if (Sys.info()[1] == "Windows") stop("Must be run on the cluster, not Windows.")
  
  ### list mes to facet
  if (all_vaccines) {  
    mes <- paste0("vacc_", c("dpt3", "mcv1", "bcg", "polio3", "hepb3", "hib3", "pcv3", "rotac", "rcv1", "mcv2", "dpt1")) 
  } else {
    mes <- paste0("vacc_", c("dpt3", "mcv1", "bcg", "polio3", "hepb3", "hib3", "pcv3", "rotac"))
  }
  
  ### where to get model output
  if (current_gbd) {
    results_path <- file.path(root_path, "FILEPATH/modeled", gbd_cycle, date) 
  } else {
    results_path <- paste0(root_path, "FILEPATH/", date) 
  }
  
  ### get administrative bias
  # get model results
  model_root <- "FILEPATH/utilities"; setwd(model_root); source("utility.r")
  source(paste0("FILEPATH/init.r"))  

    message("Pulling admin bias adjustment via hybridized ST-GPR/MR-BRT bias file")  
    if (date=="2021-02-11-covid_disrupt") bias_date="2021-02-11" else bias_date=date
    bias_dir <- file.path(data_root, "exp/to_model", gbd_cycle, bias_date, "admin_bias/hybridized_bias.rds")
    bias <- readRDS(bias_dir)
    

  # compute bias for ratios
  ratios   <- c("vacc_hepb3_dpt3_ratio", "vacc_mcv2_mcv1_ratio", "vacc_pcv3_dpt3_ratio", "vacc_rotac_dpt3_ratio", "vacc_hib3_dpt3_ratio", "vacc_rcv1_mcv1_ratio", "vacc_dpt3_dpt1_ratio") 
  straight <- c("dpt3", "mcv1", "bcg", "polio3")
   
    for (me in mes[!mes %in% paste0("vacc_", straight)]) {
      me_clean <- gsub("vacc_", "", me)
      ratio    <- ratios[grep(me_clean, ratios)]
      denom    <- gsub(paste0("vacc|_|ratio|", me_clean), "", ratio)
      bias_new <- bias[me_name==paste0("vacc_", denom)]
      bias     <- rbind(bias, bias_new[, me_name := me])
    }
    
  ### refresh
  source(paste0("FILEPATH/init.r"))  
  
  ### pull input data
  if (data_date=="NULL") data_date <- date
  if (current_gbd) {
    to_model_dir <- paste0(root_path, "FILEPATH/", data_date)
  } else {
    to_model_dir <- paste0(root_path, "FILEPATH/", data_date)
  }
  
  print("Trying to read data rds...")
  data             <- readRDS(file.path(to_model_dir, "vaccination.rds"))[, location_id := NULL]
  admin_ratio_data <- data[me_name %in% ratios & !is.na(data) & cv_admin==1]
  data             <- data[me_name %in% mes]
  if (data_compare) {
    data_cp_dir <- paste0(root_path, "FILEPATH/", data_compare_date)
    data_cp     <- readRDS(file.path(data_cp_dir, "vaccination.rds"))[, location_id := NULL][me_name %in% mes]
    new_nids <- setdiff(unique(data$nid), unique(data_cp$nid))
    new_data <- data[nid %in% new_nids]
    fwrite(new_data, file.path(data_cp_dir, "new_data.csv"))  
    data <- rbind(data, new_data, fill=T)
    data[nid %in% new_nids, new_data := 1]
  }
  print("...done reading data rds")
  
  # merge on bias ratio
  bias[, cv_admin := 1]
  
  

    # prep
    # subset out ratio data and set names for merge prep
    ratio_data <- readRDS(file.path(to_model_dir, "vaccination.rds"))[me_name %in% ratios & cv_admin==1 & !is.na(data), .(ihme_loc_id, nid, year_id, me_name, data, cv_admin)]
    ratio_data[grepl("hepb3", me_name), me_name := "vacc_hepb3"]
    ratio_data[grepl("pcv3", me_name), me_name := "vacc_pcv3"]
    ratio_data[grepl("hib3", me_name), me_name := "vacc_hib3"]
    ratio_data[grepl("rotac", me_name), me_name := "vacc_rotac"]
    ratio_data[grepl("rcv1", me_name), me_name := "vacc_rcv1"]
    ratio_data[grepl("mcv2", me_name), me_name := "vacc_mcv2"]
    setnames(ratio_data, "data", "input_ratio_data")
    
    denom_data <- data[me_name %in% c("vacc_dpt3", "vacc_mcv1") & cv_admin==1 & !is.na(data), .(ihme_loc_id, nid, year_id, me_name, data, cv_admin)]
    setnames(denom_data, c("me_name", "data"), c("denom", "denom_data"))
    data[me_name %in% paste0("vacc_", c("hepb3", "hib3", "rotac", "pcv3")), denom := "vacc_dpt3"]
    data[me_name %in% paste0("vacc_", c("rcv1", "mcv2")), denom := "vacc_mcv1"]
    data <- merge(data, denom_data, by=c("ihme_loc_id", "year_id", "nid", "denom", "cv_admin"), all.x=T)
    
    data <- merge(data, ratio_data, by=c("ihme_loc_id", "nid", "year_id", "me_name", "cv_admin"), all.x=T)
    data <- merge(data, bias[!me_name %in% paste0("vacc_", straight)], by=c("me_name", "ihme_loc_id", "year_id", "age_group_id", "sex_id", "cv_admin"), all.x=TRUE)

    # copy raw data to cv_admin_orig
    data[!me_name %in% paste0("vacc_", straight) & cv_admin==1 & !is.na(cv_admin_bias_ratio.y) & is.na(cv_admin_orig), cv_admin_orig := data]  

    data[!me_name %in% c(paste0("vacc_", straight), "vacc_dpt1") & cv_admin==1 & !is.na(input_ratio_data) & !is.na(data), data := input_ratio_data*denom_data]
    data[!me_name %in% c(paste0("vacc_", straight), "vacc_dpt1") & cv_admin==1 & !is.na(data) & is.na(input_ratio_data) & is.na(denom_data), `:=` (data=NA, cv_outlier=cv_admin_orig)]
    
    data[me_name=="vacc_dpt1" & cv_admin==1 & !is.na(cv_admin_bias_ratio.y) & !is.na(cv_admin_orig), data := cv_admin_orig*cv_admin_bias_ratio.y]

    # NULL new columns don't need anymore
    data[, c("input_ratio_data", "denom_data") := NULL]
  
  
  
  if (dpt12) {
    if ("fake_data" %in% names(data))  data[me_name=="vacc_dpt1" & nid==451398 & !is.na(fake_data), `:=` (nid=203321, survey_name=underlying_survey_name)]  
    
  # compute for dpt1 via dpt12_cond
  print("Starting merge on conditional and adjusting DPT1 data")
  conditional <- c("vacc_dpt1")
  for (me in conditional) {
    if (current_gbd) {  
      cond <- readRDS(paste0(data_root, "/FILEPATH/vacc_dpt12_cond.rds")) 
    } else {
      cond <- readRDS(paste0(data_root, "/FILEPATH/vacc_dpt12_cond.rds"))
    }
    cond <- cond[, c("location_id", "year_id", "gpr_mean")]
    setnames(cond, "gpr_mean", "cond_ratio")
    # set up the bias-corrected dpt3 col for the math
    dpt.data <- readRDS(file.path(to_model_dir, "vaccination.rds"))[me_name %in% c("vacc_dpt3") & cv_admin==1][, c("ihme_loc_id", "year_id", "data", "nid")]
    setnames(dpt.data, "data", "bias.adj.dpt3")
    dpt.data <- merge(dpt.data, locations[, c("ihme_loc_id", "location_id")], by = "ihme_loc_id", all.x=TRUE)
    # merge modeled conditional means and bias correct dpt3 admin data to 'data' as cols
    add_on <- merge(dpt.data, cond, by=c("location_id", "year_id"), all.x=TRUE) %>% unique
    data <- merge(data, add_on[!nid %in% c(341003)], by=c("ihme_loc_id", "year_id", "nid"), all.x=TRUE)  
    print("Merged 'data' and 'add_on'")
    
    # do the math 
    data[me_name==me & cv_admin==1 & !is.na(cv_admin_bias_ratio.y) & !is.na(data), data := (cond_ratio * (1 - bias.adj.dpt3) + bias.adj.dpt3)]
    data[, c("cond_ratio", "bias.adj.dpt3", "location_id") := NULL]
    cond.data  <- data[me_name %in% c("vacc_dpt1", "vacc_dpt3")]
    cond.data[, unique_pair_id := paste(ihme_loc_id, year_id, age_year, nid, survey_name, age_start, sep = "_")]  
    keep       <- cond.data[, .N, by = "unique_pair_id"][N == 2, unique_pair_id]
    test_pairs <- cond.data[unique_pair_id %in% keep]
    test_pairs <- test_pairs[nid != 120905]
    test_pairs <- test_pairs[!(is.na(data)) | !is.na(cv_outlier)]  
    keep2      <- test_pairs[, .N, by = "unique_pair_id"][N == 2, unique_pair_id]
    test_pairs <- test_pairs[unique_pair_id %in% keep2]
    
    # subset out the outliered dpt1 data to add back as outliers in plot 
    outliered_dpt1 <- test_pairs[is.na(data) & !is.na(cv_outlier) & me_name=="vacc_dpt1"]  
    
    # continue on as before
    wide       <- dcast.data.table(test_pairs, unique_pair_id ~ me_name, value.var = "data") 
    wide       <- wide[, dpt_diff := vacc_dpt1 - vacc_dpt3]
    positive   <- wide[dpt_diff >= 0]
    keep3      <- unique(positive$unique_pair_id)
    test_pairs <- test_pairs[unique_pair_id %in% keep3]
    test_pairs_1 <- test_pairs[me_name=="vacc_dpt1"]
    data <- merge(data, test_pairs, by=names(data), all.x=TRUE)
    data <- data[me_name != "vacc_dpt1" | (me_name=="vacc_dpt1" & !is.na(unique_pair_id))]
    
    # rbind the outliered data so it shows up as outlierd in plots
    data <- rbind(data, outliered_dpt1)
    
    if ("fake_data" %in% names(data))  data[me_name=="vacc_dpt1" & !is.na(fake_data) & year_id <= 2000, `:=` (nid=451398, survey_name="IHME Imputed DPT1 Administrative Coverage Data")]
  } 
  print("Done merging on conditional and adjusting DPT1 data")
  }
  
  
  data[data >= 1, data := 0.99]
  if ("cv_outlier" %in% names(data)) {
    data <- data %>%
      .[(!is.na(data) | !is.na(cv_outlier)) & survey_name %in% c("MACRO_DHS", "DHS"), source_type := "DHS"] %>%
      .[(!is.na(data) | !is.na(cv_outlier)) & survey_name=="UNICEF_MICS", source_type := "MICS"] %>%
      .[(!is.na(data) | !is.na(cv_outlier)) & survey_name=="WHO/UNICEF Admin Data" | nid==203321, source_type := "Country-reported"] %>%
      .[(!is.na(data) | !is.na(cv_outlier)) & is.na(source_type) & cv_survey==1 & (is.na(cv_admin) | cv_admin==0), source_type := "Other microdata"] %>%
      .[(!is.na(data) | !is.na(cv_outlier)) & is.na(source_type), source_type := "Other tabulation"]
    if ("fake_data" %in% names(data)) {
      data <- data %>%
        .[((!is.na(data) | !is.na(cv_outlier))) & !is.na(fake_data), source_type := "Imputed"]
    }
  } else {
    data <- data %>%
      .[!is.na(data) & survey_name=="MACRO_DHS", source_type := "DHS"] %>%
      .[!is.na(data) & survey_name=="UNICEF_MICS", source_type := "MICS"] %>%
      .[!is.na(data) & survey_name=="WHO/UNICEF Admin Data" | nid==203321, source_type := "Country-reported"] %>%
      .[!is.na(data) & is.na(source_type) & cv_survey==1 & (is.na(cv_admin) | cv_admin==0), source_type := "Other microdata"] %>%
      .[!is.na(data) & is.na(source_type), source_type := "Other tabulation"]
  }
  data         <- merge(data, locations[, .(location_id, ihme_loc_id)], by="ihme_loc_id", all.x=TRUE)
  vars         <- c("location_id", "year_id", "age_group_id", "sex_id", "me_name")
  cols         <- c(vars, "data", "variance", "cv_admin_orig", "cv_outlier", "cv_admin", "cv_survey", "cv_lit", "source_type", "cv_intro", "new_data", "fake_data", "nid")  
  data         <- data[, cols[cols %in% names(data)], with=FALSE]
  dt           <- lapply(file.path(results_path, paste0(mes, ".rds")), readRDS) %>% rbindlist(., fill=TRUE)
  dt           <- merge(dt, data[!is.na(location_id), ], by=vars, all=TRUE)
  dt           <- dt[me_name %in% mes, ]
  print("Data is ready to go")
  
  ### WUENIC estimates
  if (add_wuenic) {
    wuenic_list  <- list()
    me.db <- file.path(ref_data_repo, "me_db.csv") %>% fread 
    for (me in mes) {
      who_me_name <- me.db[me_name==me, who_name]
      if (me=="vacc_rotac") who_me_name <- "RotaC"
      wuenic <- read_excel(file.path(root_path, "FILEPATH/coverage_estimates_series.xls"), sheet=who_me_name) %>% data.table %>% 
        .[, c("Region", "Cname", "Vaccine") := NULL]
      wu <- melt(wuenic, variable.name="year_id") %>% 
        setnames(., "ISO_code", "ihme_loc_id") %>% 
        .[, coverage_wuenic := value / 100] %>% 
        .[, year_id := as.integer(as.character(year_id))] %>%
        .[, me_name := me] %>% 
        .[, value := NULL]
      wuenic_list[[me]] <- wu
    }
    wuenic_est <- do.call(rbind, wuenic_list)
    wuenic_est <- merge(wuenic_est, locations[, .(location_id, ihme_loc_id)], by="ihme_loc_id", all.x=TRUE) %>% .[, ihme_loc_id := NULL]
    dt[, year_id := as.integer(year_id)]; dt <- merge(dt, wuenic_est, by=c("location_id", "year_id", "me_name"), all.x=TRUE)
    print("WUENIC estimates merged on")
  }
  
  ### prep comparison lines
  if (!"compare_version" %in% names(dt) & !is.null(compare) & compare != "NULL") {
    # flag to specify GBD round
    if (is.null(compare_round) | compare_round == "NULL" | !compare_round %in% c(2015, 2016, 2017, 2019, 2020)) {
      stop("Can't compare two versions without specifying 'compare_round', the GBD cycle to pull from (e.g. 2016 or 2017)")
    }
    # pull path to results for comparison
    if (compare_round==2016) {
      compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_round))
    } else {
      compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_round), compare)
    }
    # get old results
    files <- list.files(compare_path, full.names=TRUE)
    want  <- file.path(compare_path, paste0(mes, ".rds"))
    pull  <- want[want %in% files]
    if (length(pull) < 1) stop(paste0("No files to compare in ", compare_path))
    dt_compare <- lapply(pull, readRDS) %>% rbindlist(., fill=TRUE)
    # merge on to dataset
    setnames(dt_compare, c("gpr_mean", "gpr_lower", "gpr_upper"), c("gpr_mean_compare", "gpr_lower_compare", "gpr_upper_compare"))
    dt <- merge(dt, dt_compare[, c(vars, "gpr_mean_compare"), with=FALSE], by=vars, all.x=TRUE)
  }
  
  ### prep second comparison line
  if (!"compare_second" %in% names(dt) & !is.null(compare_more) & compare_more != "NULL") {
    # flag to specify GBD round
    if (is.null(compare_more_round) | compare_more_round == "NULL" | !compare_more_round %in% c(2015, 2016, 2017, 2019, 2020)) {
      stop("Can't compare two versions without specifying 'compare_round_more', the GBD cycle to pull from (e.g. 2016 or 2017)")
    }
    # pull path to results for comparison
    if (compare_more_round==2016) {
      compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_more_round))
    } else {
      compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_more_round), compare_more)
    }
    # get old results
    files <- list.files(compare_path, full.names=TRUE)
    want  <- file.path(compare_path, paste0(mes, ".rds"))
    pull  <- want[want %in% files]
    if (length(pull) < 1) stop(paste0("No files to compare in ", compare_path))
    dt_compare <- lapply(pull, readRDS) %>% rbindlist(., fill=TRUE)
    # merge on to dataset
    setnames(dt_compare, c("gpr_mean", "gpr_lower", "gpr_upper"), c("gpr_mean_compare_more", "gpr_lower_compare_more", "gpr_upper_compare_more"))
    dt <- merge(dt, dt_compare[, c(vars, "gpr_mean_compare_more"), with=FALSE], by=vars, all.x=TRUE)
  }
  
  ### add useful id names
  # locations
  check_cols <- c("location_name", "ihme_loc_id", "sort_order", "most_detailed", "region_id", "level_3", "region_name")
  merge_cols <- NULL
  for (col in check_cols) if (!(col %in% names(dt))) merge_cols <- c(merge_cols, col)
  if (!is.null(merge_cols)) {
    geo <- get_location_hierarchy()[, c("location_id", merge_cols), with=FALSE]  
    setnames(geo, "level_3", "national_id")
    geo[national_id==location_id, national_id := NA_integer_]
    dt <- merge(dt, geo, by="location_id", all.x=TRUE)
  }
  print("Missing location-related columns merged on")
  # ages
  if (!("age_group_name" %in% names(dt))) {
    ages <- get_ids("age_group")
    dt <- merge(dt, ages, by="age_group_id", all.x=TRUE)
  }
  dt[, age_group_name := factor(age_group_name, 
                                levels = c("Early Neonatal","Late Neonatal","Post Neonatal","1 to 4","5 to 9",
                                           "10 to 14","15 to 19","20 to 24","25 to 29","30 to 34",
                                           "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
                                           "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84",
                                           "85 to 89","90 to 94","95 plus","All Ages"))]
  # sexes
  bysex <- ifelse(3 %in% unique(dt$sex_id), 0, 1)
  if (!("sex_name" %in% names(dt))) {
    dt[sex_id==1, sex_name := "Male"]
    dt[sex_id==2, sex_name := "Female"]
    dt[sex_id==3, sex_name := "Both"]
  }
  print("Prepped with age group and sex names")
  
  
  # Publication plot settings 
  if (publication_plots) {
    
    # drop estimates after last publication year
    dt <- dt[year_id <= last_pub_year]
    
    me.db <- fread(file.path(ref_data_repo, "me_db.csv")) %>% .[me_name %in% unique(dt$me_name), .(me_name, display_name)]
    dt <- merge(dt, me.db, by="me_name", all.x=T)
    dt[, me_name := display_name][, display_name := NULL]
    dt[me_name=="Polio3", me_name := "Pol3"]
    
    test <- dt[cv_admin==1, .N, by=c("me_name", "year_id", "location_id")]
    test <- test[N>1, admin_id := paste(me_name, year_id, location_id, sep="_")][N>1]
    
    dt[, admin_id := paste(me_name, year_id, location_id, sep="_")]
  
    remove_dupe <- NULL
    for (id in unique(test$admin_id)) {
      d_review <- dt[admin_id==id]
      d_review_nids <- unique(d_review$nid)
      if (203321 %in% d_review_nids) remove_dupe <- c(remove_dupe, id)
    }    
  
    dt[admin_id %in% remove_dupe & nid != 203321 & cv_admin==1 & is.na(data), drop := 1]
    dt <- dt[is.na(drop)][, c("admin_id", "drop") := NULL]
    
    dt <- merge(dt, locations[,.(location_id, lancet_label)], by="location_id", all.x=T)
    dt[, location_name := lancet_label]
    
    # Done
    
  }
  
  
  ### order by location order
  # if alphabetical, order by ihme_loc_id ABC, otherwise order by region, then alphabetically by ihme_loc_id (sort_order in hierarchy)
  if (abc) { dt <- dt[order(ihme_loc_id) ,] } else { dt <- dt[order(sort_order) ,] }
  print("Dataset sorted")
  
  ### make temp folder for output and save a temp file to be passed to graphing
  run <- 10259  
  data_path <- file.path("FILEPATH/plot_gpr_temp.rds")  
  temp_root <- file.path("FILEPATH/")  
  dir.create(temp_root, showWarnings=FALSE)
  print("Directory created")
  
  ### keep only locations specified
  locations_nat <- locations[level==3, location_id] %>% unique
  if (national_only) dt <- dt[location_id %in% locations_nat, ]
  
  dt <- dt[location_id %in% locations$location_id]
  dt <- dt[!is.na(year_id)]
  
  
  ### save dataset for plotting
  print("trying to save dataset for plotting")
  saveRDS(dt, file=data_path)
  print(paste0("Dataset saved to data path: ", data_path))
  
  ### launch jobs to graph
  file_list <- NULL
  for (loc in unique(dt$location_id)) {  
    
    output_path <- paste0(temp_root, loc, ".pdf")
    file_list <- c(file_list, output_path)
    
    # cluster:
    job <- paste0("qsub -N plot_gpr_", date, "_", loc, " -l m_mem_free=2G -l fthread=1 -l archive -l h_rt=00:10:00 -q long.q -P proj_cov_vpd",
                  " -o FILEPATH/", username,  
                  " FILEPATH/ihme_rstudio_3631.img",  
                  " -s FILEPATH/jobs_to_plot.r",
                  " --data_path ", data_path,                    
                  " --output_path ", output_path,
                  " --loc_id ", loc,
                  " --add_regions ", add.regions,
                  " --add_outliers ", add.outliers,
                  " --add_mbg ", add_mbg,
                  " --add_wuenic ", add_wuenic,
                  " --y_axis ", y.axis,
                  " --all_vaccines ", all_vaccines,
                  " --compare_version ", compare, 
                  " --compare_second ", compare_more,  
                  " --data_compare ", data_compare)  
    system(job); print(job)
    
  }
  job_hold(paste0("plot_gpr_", date, "_", loc), file_list=file_list)
  
  ### append graph, order by specified sort  
  files <- gsub(",", "", toString(paste0(temp_root, "/", unique(dt$location_id), ".pdf"))) 
  append_pdf(files, output.path)
  
  ### END OF FUNCTION
  print(paste0("Graph complete, saved to ", output.path))
  
}
#***********************************************************************************************************************


#----CONFIG-------------------------------------------------------------------------------------------------------------
username <- Sys.info()[["user"]]
### runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j" 
} else { 
  j_root <- "J:"
}

### load packages
pacman::p_load(data.table, haven, magrittr, ini, stringr, rhdf5, grid, RMySQL)

### set options
root_path <- file.path(j_root, "FILEPATH")  

### load functions
source("FILEPATH/read_excel.R")
source(paste0("FILEPATH/init.r"))  
source("FILEPATH/helpers.R")
source("FILEPATH/cluster_tools.r") 
source("FILEPATH/ubcov_tools.r") 
source("FILEPATH/get_ids.R")
source("FILEPATH/get_location_metadata.R")
decomp_step <- "iterative"
locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step=decomp_step)  
age_ids <- get_ids("age_group")
sex_ids <- get_ids("sex")
#***********************************************************************************************************************


#----TIME SERIES--------------------------------------------------------------------------------------------------------
date           <- "2021-02-11" 
current_gbd    <- TRUE   
  if (!current_gbd) cycle_year <- 2019 else cycle_year <- "NULL"  
  if (date=="2018-12-12") year_end <- 2017  
national_only  <- TRUE    
abc            <- TRUE    
add_wuenic     <- TRUE    
add_mbg        <- FALSE   
  mbg_mcv1     <- "unraked"
  mbg_dpt3     <- "unraked"
  mbg_rotac    <- "raked"
  mbg_pcv3     <- "raked"
all_11         <- TRUE    
add_old_vers   <- "NULL"  
  old_vers_round <- 2020          
add_second_ver <- "NULL"  
  second_ver_rd  <- 2020   
data.date      <- date    
dpt12          <- TRUE
data_compare   <- FALSE   
  data_compare_date <- "2020-05-15"
  data_compare_rd   <- 2020
publication    <- TRUE    
  last_year         <- 2020 
  if (publication) add_wuenic <- TRUE  
  if (publication) national_only <- TRUE  
  if (publication) add_old_vers <- "NULL"
  if (publication) add_second_ver <- "NULL"

# set options 
if (all_11)                                          name <- paste("all_11_vaccines", data.date, sep="_") else name <- paste("vaccines", data.date, sep="_")
if (national_only)                                   name <- paste(name, "national", sep="_") else name <- paste(name, "with_subnational", sep="_")
if (!is.null(add_old_vers) & add_old_vers != "NULL") name <- paste(name, "vs", paste0("gbd", old_vers_round), add_old_vers, sep="_")
if (!is.null(add_second_ver) & add_second_ver != "NULL") name <- paste(name, "vs", paste0("gbd", second_ver_rd), add_second_ver, sep="_")
if (add_mbg)                                         name <- paste(name, "with_mbg", sep="_")
if (!add_wuenic)                                     name <- paste(name, "no_wuenic", sep="_") else name <- paste(name, "w_wuenic", sep="_")
if (data_compare)                                    name <- paste(name, "data_compare", data_compare_date, sep="_")
if (publication)                                     name <- paste("publication_lancet_label", name, sep="_")

outpath <- paste0(root_path, "/FILEPATH/", name, ".pdf")  

# get time-series plots
plot_gpr(date=date, 
         current_gbd=current_gbd, 
         data_date=data.date, 
         output.path=outpath, 
         add.regions=FALSE, 
         y.axis=TRUE, 
         all_vaccines=all_11, 
         compare=add_old_vers, 
         compare_round=old_vers_round,
         add_wuenic=add_wuenic, 
         add_mbg=add_mbg, 
         add.outliers=TRUE, 
         compare_more=add_second_ver, 
         compare_more_round=second_ver_rd, 
         data_compare=data_compare, 
         data_compare_date=data_compare_date, 
         data_compare_rd=data_compare_rd, 
         publication_plots=publication, 
         last_pub_year=last_year)
#***********************************************************************************************************************
