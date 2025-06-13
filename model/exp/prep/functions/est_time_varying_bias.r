#' @title Estimate time varying bias

#' @description Function to use ST-GPR to smooth administrative bias using SDI as a


#' @param df (data.table) data being prepped by prep_exp.r


#' @param j_root (str) platform dependent prefix 
#' @param username (str) user running code
#' @param locations (data.table) output of get_location_data
#' @param date (str) run_date folder passed as `--run_date` to MR-BRT submission



#' @param to_model_dir (str) path to model directory
#' @param ref_data_repo (str) path to reference directory



#' @param NOTES (str) notes on what has changed in this run through of prep_exp.r


#' @param run_bias_correction_via_bundle (bool) TRUE when want to run ST-GPR models via bundle/xw_version


#' @param bundle.version.dir (str) Path to bundle version directory e.g. `file.path(ref_data_repo, "gbd_covariate_bundle_versions.csv")`
#' @param launch_dpt1_dpt3_dropout TRUE to launch the dpt dropout ST-GPR model, only



#' @param launch (boolean) if T, re-launch admin bias models else load in saved version and


#' @param alt_bias_cycle (str) cycle for pulling in previous set of estimates if launch is FALSE, i.e. "gbd2020"
#' @param alt_bias_date run date for pulling in previous set of estimates if launch is FALSE, i.e. "2021-08-09"
#' @param manual_offset_remove (bool) Only TRUE when want to manually remove the 0.01 ST-GPR offset from the bias adjustment models


CLUSTER_NAME
CLUSTER_NAME
#' @param plot_mrbrt_bias (bool) argument passed to MR-BRT submission script


#' @param on_time_cohort_only (boolean) if T, only keep the survey data corresponding to



#' @param straight_models (str vector) straight models to launch

#' @param mark_best (int) make this run best in tracking log? default 0
#' @param nparallel (int) cores to parallelize the ST-GPR run over, default 50

#' @param code_root (chr) e.g. "FILEPATH"
#' @param mrbrt_bias_covariate_id 
#' @param fpath_outlier_invert_bias 
#' @param fpath_outlier_except_bias 
#' @param n_draws_bias (int) how many draws to run the bias model for


#' @concept admin_bias_correction_functions
est_time_varying_bias <- function(df,
                                  code_root                      = code_root,
                                  launch                         = launch_bias_correction,
                                  NOTES                          = NOTES,
                                  straight_models                = straight_models,
                                  alt_bias_cycle                 = alt_bias_cycle,
                                  alt_bias_date                  = alt_bias_date,
                                  on_time_cohort_only            = on_time_cohort_only,
                                  locations                      = locs,
                                  j_root                         = j_root,
                                  username                       = username,
                                  date                           = date,
                                  to_model_dir                   = to_model_dir,
                                  ref_data_repo                  = ref_data_repo,
                                  run_bias_correction_via_bundle = run_bias_correction_via_bundle,
                                  bundle.version.dir             = bundle.version.dir,
                                  manual_offset_remove           = manual_offset_remove,
                                  proj                           = proj,
                                  queue                          = queue,
                                  mrbrt_bias_covariate_id        = mrbrt_bias_covariate_id,
                                  plot_mrbrt_bias                = plot_mrbrt_bias,
                                  launch_dpt1_dpt3_dropout       = launch_dpt1_dpt3_dropout,
                                  fpath_outlier_invert_bias      = fpath_outlier_invert_bias,
                                  fpath_outlier_except_bias      = fpath_outlier_except_bias,
                                  mark_best                      = 0,
                                  nparallel                      = 50,
                                  n_draws_bias
                                  ) {

  to_model_bias_dir <- file.path(to_model_dir, "admin_bias")
  dir.create(to_model_bias_dir, showWarnings = FALSE, recursive = TRUE)

  if(launch) {

    
    message("Prepping dataset")
    df.include <- .invert_cv_outlier_from_table(
      dtvc                       = df,
      fpath_outlier_invert_table = fpath_outlier_invert_bias,
      fpath_outlier_exceptions   = fpath_outlier_except_bias
    )
    df.include <- df.include[!is.na(data) & me_name %in% straight_models, ]

    
    if (on_time_cohort_only) {

      
      df.include_admin <- df.include[cv_admin==1]

      df.include_1 <- df.include[cv_admin != 1 & !me_name %in% "vacc_mcv1" & age_year==2]

      df.age <- readRDS(file.path("FILEPATH/03_data_post_outliering.rds")) %>%
        .[cv_admin != 1] %>% .[, cv_admin_bias_ratio := NULL]

      
      df.age <- df.age[age_bin==4 & me_name=="vacc_mcv1"]  

      
      df.include <- rbind(df.age, df.include_1, df.include_admin, fill=T)

    }

    
    df.include <- df.include[cv_lit==1 & !is.na(sample_size), variance := data * (1 - data) / sample_size]
    df.include <- df.include[cv_lit==1 & is.na(variance), variance := data * (1 - data) / 100]
    df.include[is.na(standard_error) & !is.na(variance), standard_error := sqrt(variance)]
    
    
    ifelse(
       on_time_cohort_only, 
       merge_cols <- c("me_name", "ihme_loc_id", "year_id", "sex_id"),
       merge_cols <- c("me_name", "ihme_loc_id", "year_id", "sex_id", "age_group_id")
    )  
    varnames_admin  <- c(merge_cols, "data", "standard_error", "variance", "nid")
    varnames_survey <- c(merge_cols, "data", "standard_error", "variance", "nid", "survey_name", "year_start")
    df.adjust <- merge(x        = df.include[cv_admin == 1, ..varnames_admin],
                       y        = df.include[cv_admin == 0, ..varnames_survey],
                       by       = merge_cols,
                       all      = TRUE)
    
    
    
    
    
    
    df.adjust <- impute_dpt1_survey_for_bias(df.adjust)
    
    df.adjust <- df.adjust[!is.na(data.x) & !is.na(data.y), ]
    df.adjust[, data := data.y / data.x]   
    df.adjust[, variance := data.y ^ 2 / data.x ^ 2 * (variance.y / data.y ^ 2 + 0 / data.x ^ 2)]
    df.adjust <- merge(x     = df.adjust,
                       y     = locs[, .(location_id, ihme_loc_id)],
                       by    = "ihme_loc_id",
                       all.x = TRUE)
    df.adjust[, sample_size := NA_integer_]

    
    if (on_time_cohort_only) df.adjust <- df.adjust[, age_group_id := 22]

    
    saveRDS(df.adjust, file = file.path(to_model_bias_dir, "bias_correction_data_pairs_pre_outlier.rds"))
    

    
    path_stdout <- file.path("FILEPATH", username, "output")
    path_stderr <- file.path("FILEPATH", username, "error")
    dir.create(path_stdout, showWarnings = FALSE, recursive = TRUE)
    dir.create(path_stderr, showWarnings = FALSE, recursive = TRUE)

    
    
    work_dir <- paste0(j_root, "FILEPATH", date)
    dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
    
    
    message("Launching MR-BRT admin bias model.")
    
    run_id_mrbrt <- list()
    for(me_name_i in straight_models){
       message(me_name_i)
       run_id_mrbrt[[me_name_i]] <- submit_job(
          script_path    = file.path(code_root, "FILEPATH/mrbrt_cascade.R")
          , threads      = 1
          , mem_G        = "3G"
          , runtime_min  = 10
          , archiveTF    = TRUE
          , job_name     = paste0("mrbrt_cascade_", me_name_i)
          , partition    = queue
          , Account      = proj
          , std_err_root = path_stderr
          , std_out_root = path_stdout
          , dry_runTF    = FALSE
          , args_list    = list(
             run_date            = date
             , code_root         = code_root
             , plot_mrbrt_bias   = plot_mrbrt_bias
             , cov_id            = as.integer(mrbrt_bias_covariate_id)
             , work_dir          = work_dir 
             , to_model_bias_dir = to_model_bias_dir
             , me_name_i         = me_name_i
          )
       )
    }
    
    
    message("MR-BRT admin bias model is running...")
    wait_on_slurm_job_id(unlist(run_id_mrbrt), initial_sleep_sec = 20, break_on_failure = TRUE)
    message('MR-BRT admin bias model is finished!')  
    writeLines(paste("MRBRT bias cascade done."), file.path(work_dir, "done.csv"))

    cat("MR-BRT admin bias model\n", file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)
    cat(paste0("   FILEPATH", username, "/\n"), file = file.path(to_model_dir, "log_tracker.txt"), append = TRUE)

    
    prep_mrbrt_output_custom_stage_1(to_model_bias_dir = to_model_bias_dir,
                                     share_model_dir   = share_model_dir,
                                     j_root            = j_root,
                                     me_names          = straight_models)

    
    
    outlier <- df.adjust[
      (data > 2) |
        (ihme_loc_id=="GNB" & data > 1.4) |
        (ihme_loc_id=="TGO" & year_id == 1996) |
        (ihme_loc_id=="VUT" & year_id %in% 2003:2005) |
        (ihme_loc_id=="MHL" & year_id == 2004) |
        (ihme_loc_id=="GNB" & year_id == 2016) |        
        (ihme_loc_id=="NGA" & year_id == 2017) |        
        (ihme_loc_id=="UKR" & year_id %in% 2010:2011) | 
        (ihme_loc_id=="BGD" & year_id %in% 2017:2018 & me_name %in% c("vacc_mcv1", "vacc_dpt3")) |
        (ihme_loc_id=="BRA" & me_name == "vacc_mcv1" & year_id %in% c(1989, 1994)) |
        (ihme_loc_id=="LSO" & (year_id %in% 2016 | (year_id %in% 2017 & me_name == 'vacc_mcv1'))) |
        (ihme_loc_id=="WSM" & year_id %in% 2018 & me_name == 'vacc_dpt1') | 
        (ihme_loc_id=="BRA" & year_id %in% c(1985, 1990, 1995) & me_name == 'vacc_dpt1') | 
        (ihme_loc_id %like% c("BRA_") & year_id %in% c(1995) & me_name %in% c("vacc_dpt1")) | 
        (ihme_loc_id=="PRK" & year_id %in% c(1997) & me_name == 'vacc_dpt3') | 
        (ihme_loc_id=="UKR" & year_id %in% c(2008:2011) & me_name == 'vacc_dpt1') | 
        (ihme_loc_id=="VEN" & year_id %in% c(1998) & me_name == 'vacc_dpt1') | 
        (ihme_loc_id=="KAZ" & year_id %in% c(1993, 1994) & me_name %in% c("vacc_dpt1", "vacc_dpt3")) | 
        
        (ihme_loc_id %like% c("PHL_") & year_id %in% c(2015) & me_name %in% c("vacc_dpt1")) | 
        (ihme_loc_id=='PNG' & me_name == 'vacc_mcv1' & year_id == 1995) 
      , 
    ] 
    
    fwrite(outlier, file.path(to_model_bias_dir, "outliers.csv"), row.names=FALSE)
    df.adjust <- fsetdiff(df.adjust, outlier, all = TRUE)
    
    
    
    
    df.adjust[, nid.y := NULL]
    setnames(df.adjust, "nid.x", "nid")
    
    df.adjust <- df.adjust[, .(me_name, nid, location_id, year_id, age_group_id, sex_id, data, variance, sample_size)]

    
    df.adjust <- df.adjust[, measure_id := 18]
    df.adjust <- df.adjust[, is_outlier := 0]

    
    saveRDS(df.adjust, file = file.path(to_model_bias_dir, "bias_correction_data_pairs.rds"))

    
    for (me_name_i in straight_models) {
      df.sub <- copy(df.adjust[me_name == me_name_i, ])
      setnames(df.sub, "data", "val")
      df.sub[, me_name := paste0("bias_", me_name)]
      fwrite(df.sub, file.path(to_model_bias_dir,         paste0(me_name_i, ".csv")), row.names=FALSE, na="")
      fwrite(df.sub, file.path(share_model_dir,   "bias", paste0(me_name_i, ".csv")), row.names=FALSE, na="")
    }

    if (run_bias_correction_via_bundle) {
      source(file.path(code_root, "FILEPATH/upload_model_xw.r"))
      for (i in straight_models) {
        message(paste0("Preparing to upload bias_", i, " bundle"))
        upload_model_xw(data=df.adjust, me=i, data_date=date, bundle_map=me.db, path_to_version_dir=bundle.version.dir,
                        release_id=release_id, note=NOTES, bias=TRUE)
      }
    }


    
    
    
    message("Calculating administrative bias across vaccinations")

    
    if (launch_dpt1_dpt3_dropout | exists("RUNS")) rm(RUNS)

    RUNS <- launch_bias_adjustment(
          ref_data_repo                  = ref_data_repo,
          to_model_dir                   = to_model_dir,
          to_model_bias_dir              = to_model_bias_dir,
          date                           = date,
          n_draws                        = n_draws_bias,
          run_bias_correction_via_bundle = run_bias_correction_via_bundle,
          bundle.version.dir             = bundle.version.dir,
          straight_models                = straight_models,
          NOTES                          = NOTES,
          proj                           = proj,
          mark_best                      = 0,
          nparallel                      = 50,
          rel_id                         = release_id,
          location_set_id                = location_set_id,
          year_start                     = year_start,
          year_end                       = year_end
       )
    
    record_stgpr_model_status(to_model_dir)

    
    if (manual_offset_remove) {

      message("Need to pull draws, remove offset, then collapse, then assign run_id")
      
      read.draws <- function(run_id) {
        path  <- paste0("FILEPATH", run_id, "/draws_temp_0")
        files <- list.files(path, full.names=TRUE)
        if (length(files)==0) stop(paste0("No draws for this run_id (", run_id, ")"))
        df    <- mclapply(files, fread, mc.cores=10) %>% rbindlist(., use.names=TRUE)
        df[, run_id := run_id]
        key   <- c("location_id", "year_id", "age_group_id", "sex_id")
        setkeyv(df, cols=key)
        df    <- unique(df)
        cols  <- grep("draw_", names(df), value=TRUE)
        df    <- df[, (cols) := .SD-0.01, .SDcols=cols]
        return(df)
      }
      
      bias <- rbindlist(lapply(RUNS, read.draws)) %>% collapse_point
      old <- c("mean", "lower", "upper")
      new <- c("gpr_mean", "gpr_lower", "gpr_upper")
      setnames(bias, old, new)

    } else {

      
      bias <- rbindlist(lapply(RUNS, function(x) get_estimates(x, entity="final") %>% data.table %>% .[, run_id := x]))
      
      
      bias <- setnames(bias, c("val", "lower", "upper"), c("gpr_mean", "gpr_lower", "gpr_upper"))
      message("Just pulled admin adjustment model results from database using get_estimates()")

    }

    
    logs_path        <- file.path(ref_data_repo, "gbd_model/bias_run_log.csv")
    logs_file        <- fread(logs_path)
    logs_file        <- logs_file[, .(run_id, me_name)]
    logs_file$run_id <- suppressWarnings(as.numeric(logs_file$run_id))  
    bias             <- merge(bias, logs_file, by = "run_id", all.x = TRUE)
    setnames(bias, "gpr_mean", "cv_admin_bias_ratio")
    bias             <- merge(bias, locs[, .(location_id, ihme_loc_id)], by = "location_id", all.x = TRUE)
    bias             <- bias[, .(me_name, ihme_loc_id, year_id, age_group_id, sex_id, cv_admin_bias_ratio)]

    
    data_vacc_locs <- unique(df.adjust[, list(location_id, me_name)])
    
    has_subs <- locs[level == 3 & most_detailed != 1, location_id]
    
    
    
    
    
    
    
    

    data_vacc_locs <- rbindlist(
      lapply(
        unique(data_vacc_locs$me_name),
        function(me){
          data_vacc_locs_me <- data_vacc_locs[me_name == me]$location_id
          sub_ids1 <- locs[level > 3 & parent_id %in% has_subs & parent_id %in% data_vacc_locs_me, location_id]
          sub_ids2 <- locs[level > 3 & parent_id %in% sub_ids1, location_id]
          sub_ids3 <- locs[level > 3 & parent_id %in% sub_ids2, location_id]
          sub_ids4 <- locs[level > 3 & parent_id %in% sub_ids3, location_id]
          
          data_vacc_locs_me <- c(data_vacc_locs_me, sub_ids1, sub_ids2, sub_ids3, sub_ids4) %>% unique
          
          data_vacc_locs_me <- unique(data.table(location_id = data_vacc_locs_me, me_name = me))
          return(data_vacc_locs_me)
        }
      )
    )

    
    global_vacc_locs <- rbindlist(
      lapply(
        unique(data_vacc_locs$me_name),
        function(me){
          global_vacc_locs_me <- setdiff(locs[level >= 3, location_id], data_vacc_locs[me_name == me]$location_id)  
          global_vacc_locs_me <- unique(data.table(location_id = global_vacc_locs_me, me_name = me))
          return(global_vacc_locs_me)
        }
      )
    )

    
    mrbrt_results <- rbindlist(
      lapply(
        straight_models,
        function(me_name_i) {
          fread(file.path(to_model_bias_dir, me_name_i, paste0("mrbrt_", me_name_i, "_results.csv"))) 
        }
      )
    )
    
    
    mrbrt_results <- merge(global_vacc_locs, mrbrt_results, by = c('location_id', 'me_name'), all.y = FALSE)
    setnames(mrbrt_results, "pred", "cv_admin_bias_ratio")
    mrbrt_results <- merge(
      x     = mrbrt_results, 
      y     = locs[, .(location_id, ihme_loc_id)], 
      by    = "location_id", 
      all.x = TRUE)[, `:=` (location_id = NULL, mean_value = NULL, age_group_id = 22, sex_id = 3)]
    
    
    bias <- bias[!mrbrt_results, on=c('ihme_loc_id', 'me_name')]
    
    bias <- rbind(bias, mrbrt_results)
    
    saveRDS(bias, file.path(to_model_bias_dir, "hybridized_bias.rds"))

  } else { 
    
    message(paste0("Did NOT re-launch admin bias models and adjusting current data based on ", alt_bias_cycle, " ", alt_bias_date, " saved estimates."))
    
    alt_bias_path <- file.path(data_root, "exp/to_model", alt_bias_cycle, alt_bias_date, "admin_bias/hybridized_bias.rds")
    bias          <- readRDS(alt_bias_path)
    
    saveRDS(bias, file.path(to_model_bias_dir, "hybridized_bias.rds"))

  }

  
  

  
  bias[, cv_admin := 1]
  
  df <- merge(
      x     = df,
      y     = bias,
      by    = c("me_name", "ihme_loc_id", "year_id", "age_group_id", "sex_id", "cv_admin"),
      all.x = TRUE
    )

  
  message("Shifting administrative estimates")
  df <- df[cv_admin == 1 & !is.na(cv_admin_bias_ratio), cv_admin_orig := data]
  df <- df[cv_admin == 1 & !is.na(cv_admin_bias_ratio), data          := data * cv_admin_bias_ratio]

  return(df)

}
