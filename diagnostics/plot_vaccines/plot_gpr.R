


#' @param code_root [chr] e.g. "FILEPATH"
#' @param date [chr] date_version for the results e.g. "2024_07_26"
#' @param covid_status [chr] e.g. c(""_covidimputed", "_covidfree") - file subtype for covid scenario
#' @param current_gbd [lgl] config arg - TRUE if primary plotting a GBD model of the current cycle (e.g. FALSE if plotting a GBD 2020 model and init.r is set up for GBD 2022)
#' @param data_date [chr] config arg - same as `date`
#' @param output.path [chr] full path to output file e.g. "FILEPATH/all_11_vaccines_2024_08_06_SUBNAT_vs_2023_2024_07_26_vs_2023_2023-11-02_end_2023_v1.pdf"
#' @param add.regions [lgl] include regions as well as nations?
#' @param add.outliers [lgl] show outliers as pale points?
#' @param y.axis [lgl] show y-axis? 
#' @param all_vaccines [lgl] config arg - include all (11) vaccines in the plot, or "just the 8"? (whatever that means)
#' @param compare [lgl] config arg `add_old_vers` - compare `date` results against a second `run_date`?
#' @param compare_round [chr] config arg `old_vers_round` - round of the comparison results e.g. `gbd2023`
#' @param compare_covid [chr] `covid_status` for `compare` run date version
#' @param add_wuenic [lgl] config arg - add blue line for wuenic results?
#' @param compare_more [lgl] config arg `add_second_ver` - compare `date` results against a third `run_date`?
#' @param compare_more_round [chr] config arg `second_ver_rd` - round of the comparison results e.g. `gbd2023`
#' @param compare_more_covid [chr] `covid_status` for `compare_more` run date version
#' @param data_compare [lgl] config arg - show new data points in red against the data from `date`?
#' @param data_compare_date [chr] config arg - run_date of the data to compare against
#' @param data_compare_rd [chr] config arg - round of the data to compare against
#' @param publication_plots [lgl] config arg - TRUE if making plots for publication (simplifies things quite a bit)
#' @param locations [data.table] get_location_metadata(location_set_id = location_set_id, release_id = release_id)
#' @param year_id_start [int] first year to show in plots, regardless of whether publication_plots = TRUE
#' @param year_id_end [int] last year to show in plots, regardless of whether publication_plots = TRUE
#' @param dpt12 [lgl] config arg - something controlling display of dpt12 (dpt12_conditional?) - not clear
#' @param path_intros_for_outro [chr] file with outro information - used to remove visible data points that appear after modeled years e.g. vacc_bcg in Finland

#' @return [none] side effects - write plot pdf files to disk
#' @export

#' @examples
plot_gpr <- function(
      code_root,
      date,
      covid_status       = "",
      current_gbd        = TRUE,
      data_date          = "NULL",
      output.path,
      add.regions        = FALSE,
      add.outliers       = TRUE,
      y.axis             = FALSE,
      all_vaccines       = FALSE,
      compare            = "NULL",
      compare_round      = 2019,
      compare_covid      = "",
      add_wuenic         = TRUE,
      compare_more       = "NULL",
      compare_more_round = 2020,
      compare_more_covid = "",
      data_compare       = data_compare,
      data_compare_date  = data_compare_date,
      data_compare_rd    = data_compare_rd,
      publication_plots  = publication,
      custom_subset_mes  = NULL,
      custom_subset_locs = NULL,
      custom_data_path   = NULL,
      locations,
      year_id_start,
      year_id_end,
      dpt12,
      path_intros_for_outro,
      to_model_root,
      gbd_cycle
) {
   
   
   
   if (Sys.info()[1] == "Windows") stop("Must be run on the cluster, not Windows.")
   
   
   if (all_vaccines) {
      mes <- paste0("vacc_", c("dpt3", "mcv1", "bcg", "polio3", "hepb3", "hib3", "pcv3", "rotac", "rcv1", "mcv2", "dpt1"))
   } else {
      mes <- paste0("vacc_", c("dpt3", "mcv1", "bcg", "polio3", "hepb3", "hib3", "pcv3", "rotac"))
   }
   
   
   if (current_gbd) {
      results_path <- file.path(modeled_root, date)
   } else {
      results_path <- file.path(root, "FILEPATH", cycle_year, date)
   }
   
   
   message("Pulling admin bias adjustment via hybridized ST-GPR/MR-BRT bias file")
   bias_dir <- file.path(to_model_root, date, "admin_bias/hybridized_bias.rds")
   bias <- readRDS(bias_dir)
   bias[, old_data := 0]
   bias <- bias[me_name!='vacc_dpt3']
   
   
   bias_dpt3 <- readRDS(bias_dir)[me_name == 'vacc_dpt3']
   bias_dpt3[, old_data := 0]
   if(data_compare) {
     bias_dpt3_old <- readRDS(bias_dir)[me_name == 'vacc_dpt3']
     bias_dpt3_old[, old_data := 1]
     bias_dpt3 <- rbind(bias_dpt3, bias_dpt3_old)
   }
   
   if(data_compare) {
      bias_old <- readRDS(paste0(data_root, "FILEPATH", data_compare_rd, '/', data_compare_date, "FILEPATH/hybridized_bias.rds"))
      bias_old[, old_data := 1]
      bias <- rbind(bias, bias_old)
   }
   
   
   
   ratios   <- c("vacc_hepb3_dpt3_ratio", 
                 "vacc_mcv2_mcv1_ratio", 
                 "vacc_pcv3_dpt3_ratio", 
                 "vacc_rotac_dpt3_ratio", 
                 "vacc_hib3_dpt3_ratio", 
                 "vacc_rcv1_mcv1_ratio", 
                 "vacc_dpt3_dpt1_ratio")  
   straight <- c("dpt1", 
                 "mcv1", 
                 "bcg", 
                 "polio3")
   
   for (me in mes[!mes %in% paste0("vacc_", straight)]) {
      me_clean <- gsub("vacc_", "", me)
      if(me_clean !='dpt3') ratio  <- ratios[grep(me_clean, ratios)] else ratio <- 'vacc_dpt3_dpt1_ratio'
      denom    <- gsub(paste0("vacc|_|ratio|", me_clean), "", ratio)
      if(denom=='dpt3') bias_denom <- 'dpt1' else bias_denom <- denom
      bias_new <- bias[me_name == paste0("vacc_", denom)]
      bias     <- rbind(bias, bias_new[, me_name := me])
   }
   bias[, cv_admin := 1]
   
   
   
   to_model_dir <- file.path(to_model_root, data_date)
   
   
   message("Trying to read data rds...")
   data             <- readRDS(file.path(to_model_dir, "vaccination.rds"))[, location_id := NULL]
   admin_ratio_data <- data[me_name %in% ratios & !is.na(data) & cv_admin==1]
   data             <- data[me_name %in% mes]

   
   
   
   data[, old_data := 0]
   if (data_compare) {
      
      data_cp_dir <- paste0(root_path, "FILEPATH", data_compare_rd, "/", data_compare_date)
      data_cp     <- readRDS(file.path(data_cp_dir, "vaccination.rds"))[, location_id := NULL][me_name %in% mes]
      data_cp[, old_data := 1]
      
      new_nids <- setdiff(unique(data$nid), unique(data_cp$nid))
      
      new_data <- data[nid %in% new_nids]
      fwrite(new_data, file.path(data_cp_dir, "new_data.csv"))  
      data <- rbind(data, new_data, fill=TRUE)
      data[nid %in% new_nids, new_data := 1]
      
      data <- rbind(data, data_cp, fill=TRUE)
   }
   message("...done reading data rds")
   
   
   
   
   
   
   ratio_data <- readRDS(file.path(to_model_dir, "vaccination.rds"))[me_name %in% ratios & cv_admin==1 & !is.na(data), .(ihme_loc_id, nid, year_id, me_name, data, cv_admin)]
   ratio_data[grepl("hepb3", me_name), me_name := "vacc_hepb3"]
   ratio_data[grepl("pcv3", me_name), me_name := "vacc_pcv3"]
   ratio_data[grepl("hib3", me_name), me_name := "vacc_hib3"]
   ratio_data[grepl("rotac", me_name), me_name := "vacc_rotac"]
   ratio_data[grepl("rcv1", me_name), me_name := "vacc_rcv1"]
   ratio_data[grepl("mcv2", me_name), me_name := "vacc_mcv2"]
   ratio_data[grepl("dpt3_dpt1", me_name), me_name := "vacc_dpt3"]
   setnames(ratio_data, "data", "input_ratio_data")
   ratio_data[, old_data := 0]
   
   if(data_compare){
      ratio_data_old <- readRDS(file.path(data_cp_dir, "vaccination.rds"))[me_name %in% ratios & cv_admin==1 & !is.na(data), .(ihme_loc_id, nid, year_id, me_name, data, cv_admin)]
      ratio_data_old[grepl("hepb3", me_name), me_name := "vacc_hepb3"]
      ratio_data_old[grepl("pcv3", me_name), me_name := "vacc_pcv3"]
      ratio_data_old[grepl("hib3", me_name), me_name := "vacc_hib3"]
      ratio_data_old[grepl("rotac", me_name), me_name := "vacc_rotac"]
      ratio_data_old[grepl("rcv1", me_name), me_name := "vacc_rcv1"]
      ratio_data_old[grepl("mcv2", me_name), me_name := "vacc_mcv2"]
      ratio_data_old[grepl("dpt3_dpt1", me_name), me_name := "vacc_dpt3"]
      setnames(ratio_data_old, "data", "input_ratio_data")
      ratio_data_old[, old_data := 1]
      ratio_data <- rbind(ratio_data, ratio_data_old)
   }
   
   
   
   
   source(file.path(code_root, "FILEPATH/build_outlier_table.R"))
   outliers_applied <- fread(file.path(to_model_dir, "reference/outliers_applied.csv"))
   outlier_method_to_invert <- "data_decision - Outlier DPT3 > DPT1"
   varname_to_revert <- find_column_by_val(outliers_applied, outlier_method_to_invert)
   outliers_to_merge <- outliers_applied[!is.na(get(varname_to_revert)), .(ihme_loc_id, me_name, year_id, nid, n_outlier_methods, outlier_method_invert = get(varname_to_revert))]
   
   denom_data_raw <- data[me_name %in% c("vacc_dpt3", "vacc_mcv1","vacc_dpt1") & cv_admin == 1, .(ihme_loc_id, nid, year_id, me_name, data, cv_admin, old_data, cv_outlier)]
   
   denom_data_raw <- merge(
      x = denom_data_raw
      , y = outliers_to_merge
      , by = c("ihme_loc_id", "me_name", "year_id", "nid")
      , all.x = TRUE
   )
   denom_data_outliers_inverted <- .invert_cv_outlier_by_method(denom_data_raw, outlier_method_to_invert)
   denom_data_outliers_inverted <- denom_data_outliers_inverted[!is.na(outlier_method_invert), .(ihme_loc_id, nid, year_id, me_name, data, cv_admin, old_data, outlier_method_invert)] 
   
   denom_data_outliers_inverted <- merge(
     denom_data_outliers_inverted, 
     bias_dpt3[, .(me_name, ihme_loc_id, year_id, old_data, cv_admin_bias_ratio, cv_admin = 1)], 
     by = c("me_name", "ihme_loc_id", "year_id", "old_data", "cv_admin"),
     all.x = TRUE
   )
   
   denom_data_outliers_inverted[is.na(cv_admin_bias_ratio), cv_admin_bias_ratio := 1] 
   denom_data_outliers_inverted[, data := data * cv_admin_bias_ratio]
   denom_data_outliers_inverted[, cv_admin_bias_ratio := NULL]
   
   
   denom_data <- data[me_name %in% c("vacc_dpt3", "vacc_mcv1","vacc_dpt1") & cv_admin == 1 & !is.na(data), .(ihme_loc_id, nid, year_id, me_name, data, cv_admin, old_data, outlier_method_invert  = NA)]
   denom_data <- rbind(denom_data, denom_data_outliers_inverted)
   
   setnames(denom_data, c("me_name", "data"), c("denom", "denom_data"))
   denom_data[denom=='vacc_dpt1' & nid == 451398, nid:= 203321] 
   data[me_name %in% paste0("vacc_", c("hepb3", "hib3", "rotac", "pcv3")), denom := "vacc_dpt3"]
   data[me_name %in% paste0("vacc_", c("rcv1", "mcv2")), denom := "vacc_mcv1"]
   data[me_name =='vacc_dpt3', denom := "vacc_dpt1"]
   
   
   
   
   
   loc_ids_nat       <- locations[level == 3, location_id]
   loc_ids_subnat    <- se$children_of_parents(parent_loc_ids = loc_ids_nat, hierarchy = locations, output = 'loc_ids')
   locs_admin_subnat <- locations[location_id %in% loc_ids_subnat, ihme_loc_id]
   data[ihme_loc_id %in% locs_admin_subnat & denom == "vacc_dpt1" & cv_admin == 1 & !is.na(data), `:=`(nid_orig_plots = nid, nid = 203321)]
   
   
   
   data <- merge(data, denom_data, by=c("ihme_loc_id", "year_id", "nid", "denom", "cv_admin", "old_data"), all.x=TRUE)
   data <- merge(data, ratio_data, by=c("ihme_loc_id", "nid", "year_id", "me_name", "cv_admin", "old_data"), all.x=TRUE)
   data <- merge(data, bias[!me_name %in% paste0("vacc_", straight)], by = c("me_name", "ihme_loc_id", "year_id", "age_group_id", "sex_id", "cv_admin", "old_data"), all.x=TRUE)
   
   data[!me_name %in% paste0("vacc_", straight) & cv_admin==1 & !is.na(cv_admin_bias_ratio.y) & is.na(cv_admin_orig), cv_admin_orig := data]
   
   
   data[!me_name %in% paste0("vacc_", straight) & cv_admin==1 & !is.na(input_ratio_data) & !is.na(data), data := input_ratio_data * denom_data]
   
   
   data[!me_name %in% paste0("vacc_", straight) & cv_admin==1 & !is.na(data) & is.na(input_ratio_data) & is.na(denom_data), `:=` (data = NA, cv_outlier = cv_admin_orig)]
   
   
   data[, c("input_ratio_data", "denom_data") := NULL]
   

   if (dpt12) {
      
      if ("fake_data" %in% names(data))  data[me_name=="vacc_dpt1" & nid==451398 & !is.na(fake_data), `:=` (nid=203321, survey_name=underlying_survey_name)]
      
      
      message("Starting merge on conditional and adjusting DPT1 data")
      conditional <- c("vacc_dpt1")
      for (me in conditional) {
         
         
         
         if (current_gbd) {  
            cond <- readRDS(paste0(data_root, "FILEPATH", gbd_cycle, "/", data_date, "/vacc_dpt12_cond.rds"))
         } else {
            cond <- readRDS(paste0(data_root, "FILEPATH", cycle_year, "/", data_date, "/vacc_dpt12_cond.rds"))
         }
         cond <- cond[, c("location_id", "year_id", "gpr_mean")]
         setnames(cond, "gpr_mean", "cond_ratio")
         
         dpt.data <- readRDS(file.path(to_model_dir, "vaccination.rds"))[me_name %in% c("vacc_dpt3") & cv_admin==1][, c("ihme_loc_id", "year_id", "data", "nid")]
         setnames(dpt.data, "data", "bias.adj.dpt3")
         dpt.data <- merge(dpt.data, locations[, c("ihme_loc_id", "location_id")], by = "ihme_loc_id", all.x=TRUE)
         
         add_on <- merge(dpt.data, cond, by=c("location_id", "year_id"), all.x=TRUE) %>% unique
         
         add_on[, old_data := 0]
         if(data_compare){
            cond <- readRDS(paste0(data_root, "FILEPATH", data_compare_rd, "/", data_compare_date, "/vacc_dpt12_cond.rds"))
            cond <- cond[, c("location_id", "year_id", "gpr_mean")]
            setnames(cond, "gpr_mean", "cond_ratio")
            
            dpt.data <- readRDS(file.path(data_cp_dir, "vaccination.rds"))[me_name %in% c("vacc_dpt3") & cv_admin==1][, c("ihme_loc_id", "year_id", "data", "nid")]
            setnames(dpt.data, "data", "bias.adj.dpt3")
            dpt.data <- merge(dpt.data, locations[, c("ihme_loc_id", "location_id")], by = "ihme_loc_id", all.x=TRUE)
            
            add_on_old <- merge(dpt.data, cond, by=c("location_id", "year_id"), all.x=TRUE) %>% unique
            add_on_old[, old_data := 1]
            add_on <- rbind(add_on, add_on_old)
         }
         data <- merge(data, add_on[!nid %in% c(341003)], by=c("ihme_loc_id", "year_id", "nid","old_data"), all.x=TRUE)
         
         
         data <- unique(data)
         
         message("Merged 'data' and 'add_on'")
         
         
         data[me_name==me & cv_admin==1 & !is.na(cv_admin_bias_ratio.y) & !is.na(data), data := (cond_ratio * (1 - bias.adj.dpt3) + bias.adj.dpt3)]
         
         data[, c("cond_ratio", "bias.adj.dpt3", "location_id") := NULL]
         
         cond.data  <- data[me_name %in% c("vacc_dpt1", "vacc_dpt3")]
         cond.data[, unique_pair_id := paste(ihme_loc_id, year_id, age_year, nid, survey_name, age_start, old_data, sep = "_")]
         keep       <- cond.data[, .N, by = "unique_pair_id"][N == 2, unique_pair_id]
         test_pairs <- cond.data[unique_pair_id %in% keep]
         test_pairs <- test_pairs[nid != 120905]
         test_pairs <- test_pairs[!(is.na(data)) | !is.na(cv_outlier)]
         keep2      <- test_pairs[, .N, by = "unique_pair_id"][N == 2, unique_pair_id]
         test_pairs <- test_pairs[unique_pair_id %in% keep2]
         
         
         outliered_dpt1 <- test_pairs[is.na(data) & !is.na(cv_outlier) & me_name=="vacc_dpt1"]
         
         
         wide       <- dcast.data.table(test_pairs, unique_pair_id ~ me_name, value.var = "data")
         wide       <- wide[, dpt_diff := vacc_dpt1 - vacc_dpt3]
         positive   <- wide[dpt_diff >= 0]
         keep3      <- unique(positive$unique_pair_id)
         test_pairs <- test_pairs[unique_pair_id %in% keep3]
         
         test_pairs_1 <- test_pairs[me_name=="vacc_dpt1"]
         
         data <- merge(data, test_pairs, by=names(data), all.x=TRUE)
         data <- data[me_name != "vacc_dpt1" | (me_name=="vacc_dpt1" & !is.na(unique_pair_id))]
         
         
         data <- rbind(data, outliered_dpt1)
         
         if ("fake_data" %in% names(data))  data[me_name=="vacc_dpt1" & !is.na(fake_data) & year_id <= 2000, `:=` (nid=451398, survey_name="IHME Imputed DPT1 Administrative Coverage Data")]
      }
      message("Done merging on conditional and adjusting DPT1 data")
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
   
   
   
   dt_intro_outro <- readRDS(path_intros_for_outro)
   
   dt_intro_outro[cv_outro < 1980 | cv_outro > year_id_end, cv_outro := NA]
   data <- merge(
      x = data, 
      y = dt_intro_outro[, .(ihme_loc_id, year_id, me_name, year_id_outro = cv_outro)], 
      by = c("ihme_loc_id", "year_id", "me_name"),
      all.x = TRUE
   )
   data[!is.na(data) & year_id >= year_id_outro, outro_remove := 1]
   data <- data[is.na(outro_remove)]
   .vars_drop <- c("year_id_outro", "outro_remove")
   data[, c(.vars_drop) := NULL]
   
   
   data <- merge(data, locations[, .(location_id, ihme_loc_id)], by="ihme_loc_id", all.x=TRUE)
   vars <- c("location_id", "year_id", "age_group_id", "sex_id", "me_name")
   cols <- c(vars, "data", "variance", "cv_admin_orig", "cv_outlier", "cv_admin", "cv_survey", "cv_lit", "source_type", "cv_intro", "new_data", "fake_data", "nid", "old_data")  
   data <- data[, cols[cols %in% names(data)], with=FALSE]
   dt   <- lapply(file.path(results_path, paste0(mes, covid_status, ".rds")), readRDS) %>% rbindlist(., fill=TRUE)
   dt   <- merge(dt, data[!is.na(location_id), ], by=vars, all=TRUE)
   dt   <- dt[me_name %in% mes, ]
   message("Data is ready to go")
   
   
   dt_dpt3 <- dt[me_name=='vacc_dpt3' & !is.na(data),list(location_id,year_id,nid,age_group_id,sex_id,dpt3_data=round(data,4),cv_admin,cv_survey,cv_lit,source_type)]
   dt_dpt1 <- dt[me_name=='vacc_dpt1' & !is.na(data),list(location_id,year_id,nid,age_group_id,sex_id,dpt1_data=round(data,4),cv_admin,cv_survey,cv_lit,source_type)]
   
   
   dt_dpt1[source_type== 'Imputed', source_type:='Country-reported'] 
   dt_dpt1[nid == 451398, nid:=203321] 
   
   dt_dpts <- merge(dt_dpt3,dt_dpt1,by=c('location_id','year_id','age_group_id','sex_id','cv_admin','cv_survey','cv_lit','source_type','nid'),all=TRUE, allow.cartesian = TRUE)
   dt_dpts <- dt_dpts[ !is.na(dpt3_data) & is.na(dpt1_data)]  
   dt_dpts[,dpt1_data:=NULL]
   dt_dpts[,me_name:='vacc_dpt3']
   
   dt<-merge(dt, dt_dpts, ,by=c('me_name','location_id','year_id','age_group_id','sex_id','cv_admin','cv_survey','cv_lit','source_type','nid'),all=TRUE)
   dt[!is.na(dpt3_data), data:=NA]
   dt[!is.na(dpt3_data), variance:=NA]
   dt[!is.na(dpt3_data), cv_outlier:=dpt3_data]
   dt[,dpt3_data:=NULL]
   
   
   
   if (add_wuenic) {
      
      wuenic <- readRDS(file.path(ref_data_repo, "wuenic_estimates.rds"))[, c("group", "coverage_category_description") := NULL]
      setnames(wuenic, "data", "coverage_wuenic")
      wuenic <- merge(wuenic, locations[,.(ihme_loc_id, location_id)], by="ihme_loc_id", all.x=TRUE)[, ihme_loc_id := NULL]
      dt <- merge(dt, wuenic, by=c("location_id", "year_id", "me_name"), all.x=TRUE)
      message("WUENIC estimates merged on")
   }
   
   
   if (!"compare_version" %in% names(dt) & !is.null(compare) & compare != "NULL") {
      
      if (is.null(compare_round) | compare_round == "NULL" | !compare_round %in% c(2015, 2016, 2017, 2019, 2020,2022,2023)) {
         stop("Can't compare two versions without specifying 'compare_round', the GBD cycle to pull from (e.g. 2016 or 2017)")
      }
      
      if (compare_round==2016) {
         compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_round))
      } else {
         compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_round), compare)
      }
      
      files <- list.files(compare_path, full.names=TRUE)
      want  <- file.path(compare_path, paste0(mes, compare_covid, ".rds"))
      pull  <- want[want %in% files]
      if (length(pull) < 1) stop(paste0("No files to compare in ", compare_path))
      dt_compare <- lapply(pull, readRDS) %>% rbindlist(., fill=TRUE)
      
      setnames(dt_compare, c("gpr_mean", "gpr_lower", "gpr_upper"), c("gpr_mean_compare", "gpr_lower_compare", "gpr_upper_compare"))
      dt <- merge(dt, dt_compare[, c(vars, "gpr_mean_compare"), with=FALSE], by=vars, all.x=TRUE)
   }
   
   
   if (!"compare_second" %in% names(dt) & !is.null(compare_more) & compare_more != "NULL") {
      
      if (is.null(compare_more_round) | compare_more_round == "NULL" | !compare_more_round %in% c(2015, 2016, 2017, 2019, 2020,2022,2023)) {
         stop("Can't compare two versions without specifying 'compare_round_more', the GBD cycle to pull from (e.g. 2016 or 2017)")
      }
      
      compare_path <- file.path(root_path, "FILEPATH", paste0("gbd", compare_more_round), compare_more)
      
      files <- list.files(compare_path, full.names=TRUE)
      want  <- file.path(compare_path, paste0(mes, compare_more_covid, ".rds"))
      pull  <- want[want %in% files]
      if (length(pull) < 1) stop(paste0("No files to compare in ", compare_path))
      dt_compare <- lapply(pull, readRDS) %>% rbindlist(., fill=TRUE)
      
      setnames(dt_compare, c("gpr_mean", "gpr_lower", "gpr_upper"), c("gpr_mean_compare_more", "gpr_lower_compare_more", "gpr_upper_compare_more"))
      dt <- merge(dt, dt_compare[, c(vars, "gpr_mean_compare_more"), with=FALSE], by=vars, all.x=TRUE)
   }
   
   
   
   check_cols <- c("location_name", "ihme_loc_id", "sort_order", "most_detailed", "region_id", "level_3", "region_name")
   merge_cols <- NULL
   for (col in check_cols) if (!(col %in% names(dt))) merge_cols <- c(merge_cols, col)
   if (!is.null(merge_cols)) {
      geo <- get_location_hierarchy(release_id=release_id)[, c("location_id", merge_cols), with=FALSE]
      setnames(geo, "level_3", "national_id")
      geo[national_id==location_id, national_id := NA_integer_]
      dt <- merge(dt, geo, by="location_id", all.x=TRUE)
   }
   message("Missing location-related columns merged on")
   
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
   
   bysex <- ifelse(3 %in% unique(dt$sex_id), 0, 1)
   if (!("sex_name" %in% names(dt))) {
      dt[sex_id==1, sex_name := "Male"]
      dt[sex_id==2, sex_name := "Female"]
      dt[sex_id==3, sex_name := "Both"]
   }
   message("Prepped with age group and sex names")
   
   
   
   if (publication_plots) {
      
      
      me.db <- fread(file.path(ref_data_repo, "me_db.csv")) %>% .[me_name %in% unique(dt$me_name), .(me_name, display_name)]
      dt <- merge(dt, me.db, by="me_name", all.x=TRUE)
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
      
      
      dt <- merge(dt, locations[,.(location_id, lancet_label)], by="location_id", all.x=TRUE)
      dt[, location_name := lancet_label]
      
      
   }
   
   
   run              <- 10259
   plot_parent_root <- file.path("FILEPATH", run)
   data_path        <- file.path(plot_parent_root, "plot_gpr_temp.rds")
   temp_root        <- file.path(plot_parent_root, "plot_gpr_temp/")
   dir.create(temp_root, recursive = TRUE, showWarnings=TRUE)
   message("Directory created")
   
   
   locations_nat <- locations[level==3, location_id] %>% unique
   if (national_only) dt <- dt[location_id %in% locations_nat, ]
   
   dt <- dt[location_id %in% locations$location_id]
   
   
   dt <- dt[!is.na(year_id)]
   dt <- dt[year_id %in% year_id_start:year_id_end, ]
   
   
   if(data_compare){
      dup_check <- unique(dt[source_type=='Country-reported', list(location_id, year_id, me_name, source_type, nid, old_data)])
      dup_check <- dup_check[source_type=='Country-reported', list(old_data=sum(old_data)), by=c('location_id', 'year_id', 'me_name', 'source_type', 'nid')]
      dup_check <- dup_check[old_data==0]
      dup_check[, new_admin_data := 1]
      dt <- merge(dt, dup_check, by=c('location_id', 'year_id', 'me_name', 'source_type', 'nid', 'old_data'), all.x=TRUE)
      dt[new_admin_data==1, new_data := 1]
   }
   
   
   
   
   if (abc) dt <- dt[order(ihme_loc_id),]
   message("Dataset sorted")
   
   message("Saving dataset for plotting")
   if (!is.null(custom_subset_mes)){
      message("Subsetting vaccines to: ", toString(custom_subset_mes))
      dt <- dt[me_name %in% custom_subset_mes, ]
   } 
   saveRDS(dt, file=data_path)
   message(paste0("Dataset saved to data path: ", data_path))
   
   
   loc_map <- locations[, .(ihme_loc_id, location_id)]
   
   if(!is.null(custom_subset_locs)){
      fwrite(locations, file.path(dirname(temp_root), "locations.csv"))
      rgx_locs <- paste(custom_subset_locs, collapse = "|")
      loc_ids_to_plot <- locations[ihme_loc_id %like% rgx_locs, location_id]
   } else {
      
      loc_ids_to_plot <- unique(dt$location_id)
   }
   
   
   
   launch_grid <- unique(dt[, .(location_id = as.integer(location_id), ihme_loc_id)])[location_id %in% loc_ids_to_plot]
   saveRDS(launch_grid, file = file.path(plot_parent_root, "launch_grid.rds"))
   log_dir <- file.path("FILEPATH", Sys.info()["user"], "output", sprintf("plot_all_vaccines_%s", se$datetime_stamp(std_out = FALSE)))
   se$make_directory(log_dir)
   
   job_id_legend <- se$submit_job(
     script_path             = file.path(code_root, "FILEPATH/jobs_to_plot.r")
     , threads               = 1
     , mem                   = "2G"
     , runtime_min           = 1
     , archiveTF             = TRUE
     , verbose               = FALSE
     , job_name              = "plot_all_vaccines_legend"
     , partition             = "all.q,long.q"
     , account               = "proj_cov_vpd"
     , send_email            = TRUE
     , console_style_log_tf  = TRUE
     , std_out_root          = log_dir
     , array_tasks_int       = which(launch_grid$ihme_loc_id=="AFG") 
     , arg_vecs_to_comma_str = TRUE
     , dry_runTF             = FALSE
     , args_list             = list(
       data_path         = data_path,
       plot_parent_root  = plot_parent_root,
       temp_root         = temp_root,
       add_regions       = add.regions,
       add_outliers      = add.outliers,
       add_wuenic        = add_wuenic,
       y_axis            = y.axis,
       all_vaccines      = all_vaccines,
       compare_version   = paste0(compare, compare_covid),
       compare_second    = paste0(compare_more, compare_more_covid),
       data_compare      = data_compare,
       custom_subset_mes = custom_subset_mes,
       custom_data_path  = custom_data_path,
       code_root         = code_root,
       save_legend       = TRUE
     )
   )
   
   se$wait_on_slurm_job_id(job_id = job_id_legend, cycle_sleep_sec = 15, break_on_failure = TRUE)

   
   job_id_loc_set <- se$submit_job(
      script_path             = file.path(code_root, "FILEPATH/jobs_to_plot.r")
      , threads               = 1
      , mem                   = "2G"
      , runtime_min           = 1
      , archiveTF             = TRUE
      , verbose               = FALSE
      , job_name              = "plot_all_vaccines"
      , partition             = "all.q,long.q"
      , account               = "proj_cov_vpd"
      , send_email            = TRUE
      , console_style_log_tf  = TRUE
      , std_out_root          = log_dir
      , array_tasks_int       = 1:nrow(launch_grid)
      , arg_vecs_to_comma_str = TRUE
      , dry_runTF             = FALSE
      , args_list             = list(
         data_path         = data_path,
         plot_parent_root  = plot_parent_root,
         temp_root         = temp_root,
         add_regions       = add.regions,
         add_outliers      = add.outliers,
         add_wuenic        = add_wuenic,
         y_axis            = y.axis,
         all_vaccines      = all_vaccines,
         compare_version   = paste0(compare, compare_covid),
         compare_second    = paste0(compare_more, compare_more_covid),
         data_compare      = data_compare,
         custom_subset_mes = custom_subset_mes,
         custom_data_path  = custom_data_path,
         code_root         = code_root,
         save_legend       = FALSE
      )
   )
      
   se$wait_on_slurm_job_id(job_id = job_id_loc_set, cycle_sleep_sec = 15, break_on_failure = TRUE)
   
   
   files <- gsub(",", "", toString(paste0(temp_root, "/", loc_ids_to_plot, ".pdf")))
   append_pdf(files, output.path)
   
   unlink(log_dir, recursive = TRUE)
   
   
   message(paste0("Plots saved to ", output.path))
   
}
