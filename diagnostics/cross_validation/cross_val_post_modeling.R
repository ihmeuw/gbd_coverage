










r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
  message("Starting arg-parser.")
  se$parse_all_named_cli_args(required_args = list(
    code_root = "character"
    , config_path = "character"
    , output_root = "character"
    , prepare_draws_ratios = "logical"
    , prepare_draws_straight = "logical"
    , prepare_mrbrt_draws = "logical"
  ))
} else {
  if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
    code_root <- Sys.getenv()['CODE_ROOT'] 
  } else {
    code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
  }
  
  output_root <- file.path("FILEPATH")
  config_path <- file.path("FILEPATH/pipeline_config.yaml")
  prepare_draws_ratios   <- TRUE
  prepare_draws_straight <- TRUE
  prepare_mrbrt_draws    <- TRUE
}

message("code_root:   ", code_root)
message("output_root: ", output_root)
message("config_path: ", config_path)

source(file.path(code_root, "init.r"))
library("pbmcapply", lib.loc = r_lib_team_dir)
R.utils::sourceDirectory(file.path(code_root, "FILEPATH"), modifiedOnly = FALSE)

config <- se$read_file(config_path)
config <- config$cross_validation

metadata_prep <- se$read_file(file.path(output_root, "metadata.yaml"))


PARAMS <- list(
  
  
  
  prepare_draws_ratios     = prepare_draws_ratios
  , prepare_draws_straight = prepare_draws_straight
  , prepare_mrbrt_draws    = prepare_mrbrt_draws
  , dv_cross_val           = basename(output_root)
  , n_cores                = as.integer(system(SYSTEM_COMMAND))
  , dv_gbd_best            = basename(metadata_prep$params$paths$dir_to_model_best)
)

message("PARAMS:\n", prt_multiline(PARAMS))

metadata_post <- list(metadata = se$build_metadata_shell(code_root = code_root), params = PARAMS)
se$save_file(metadata_post, file.path(output_root, "metadata_post.yaml"))

options(mc.cores = PARAMS$n_cores)

vaccines <- c(
  "vacc_bcg",
  "vacc_dpt1",
  "vacc_dpt3",
  "vacc_hepb3",
  "vacc_hib3",
  "vacc_mcv1",
  "vacc_mcv2",
  "vacc_pcv3",
  "vacc_polio3",
  "vacc_rotac",
  "vacc_rcv1"
)

vaccines_straight <- c(
  "vacc_mcv1",
  "vacc_dpt1",
  
  "vacc_bcg",
  "vacc_polio3"
)





vaccine_ratios <- c(
  "vacc_hib3_dpt3_ratio",
  "vacc_pcv3_dpt3_ratio",
  "vacc_rotac_dpt3_ratio",
  "vacc_rcv1_mcv1_ratio",
  "vacc_hepb3_dpt3_ratio",
  "vacc_mcv2_mcv1_ratio",
  "vacc_dpt3_dpt1_ratio"
)

vaccine_ratios_cs1 <- setdiff(
  vaccine_ratios, 
  "vacc_dpt3_dpt1_ratio"
)

vars_draws <- sprintf("draw_%s", 0:(config$n_draws-1))
vars_keep_draws <- c("location_id", "year_id", "me_name", vars_draws)




fold_names <- sprintf("fold_%s", 1:config$k_folds)


PATHS <- list(
  dirs_cross_val = file.path(output_root, fold_names)
  , dirs_cross_val_to_model = file.path(to_model_root, sprintf("%s_%s", PARAMS$dv_cross_val, fold_names))
  , roots_draws = file.path(draws_root_gbdxx, paste0(PARAMS$dv_cross_val, "_", fold_names, "_covidimputed"))
)
PATHS[["dt_nids"]] <- file.path(PATHS$dirs_cross_val, "dt_nids.csv")
PATHS[["dt_nids_ratios"]] <- file.path(PATHS$dirs_cross_val, "dt_nids_ratios.csv")
PATHS[["dt_ratios"]] <- file.path(PATHS$dirs_cross_val_to_model, "04.5_pre_ratio_stockouts.rds")
PATHS <- lapply(PATHS, function(sublist){
  names(sublist) <- fold_names
  sublist
})
PATHS$fpath_dt_ratios = metadata_prep$params$paths$to$data_with_ratios
PATHS$to_model_cs1_gbd_best <- file.path(to_model_root, PARAMS$dv_gbd_best, "coverage_ratio_custom_stage_1")


locations <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)
nids_list <- se$read_file(file.path(output_root, "nids_all_folds.yaml"))
nids_all <- unname(unlist(nids_list))
message("Reading by-fold NID tables")
dt_nid_list <- lapply(PATHS$dt_nids, function(path) {
  dt <- se$read_file(path)
  dt <- merge(dt, locations[, .(location_id, ihme_loc_id)], by = "ihme_loc_id", all.x = TRUE)
  dt
})

dt_nid_list_ratios <- lapply(PATHS$dt_nids_ratios, function(path) {
  dt <- se$read_file(path)
  dt <- merge(dt, locations[, .(location_id, ihme_loc_id)], by = "ihme_loc_id", all.x = TRUE)
  dt
})
dt_nids_ratios <- rbindlist(dt_nid_list_ratios)
nids_ratios_all <- unique(unname(unlist(lapply(dt_nid_list_ratios, function(dt) dt$nid))))





if (PARAMS$prepare_draws_straight == TRUE){
  
  message("Loading straight vacciene draws")
  
  for (fold in fold_names){
    
    message(fold)
    draws <- load_fold_draws(dt_nid_list     = dt_nid_list
                             , PATHS         = PATHS
                             , fold          = fold
                             , vaccine_names = vaccines_straight
                             , vars_keep     = vars_keep_draws)
    message("--saving draws")
    se$save_file(draws, file.path(PATHS$dirs_cross_val[[fold]], "draws_coverage_and_nids_straight.rds"))
    rm(draws)
    gc()
  }
  
  message("Reading GBD-best draws for in-sample metrics")
  dt_nids_all_straight <- se$read_file(file.path(output_root, "dt_nids_all_folds.csv"))
  load_and_save_gbd_draws(
    output_root        = output_root
    , dt_nids_all      = dt_nids_all_straight
    , locations        = locations
    , draws_root_gbdxx = draws_root_gbdxx
    , PARAMS           = PARAMS
    , vaccine_names    = vaccines_straight
    , fname_suffix     = "straight" 
  )
  
} else {
  message("Skipping prep for straight draws")
}






if (PARAMS$prepare_draws_ratios == TRUE){
  message("Making survey ratios")
  
  
  vars_keep <- c("me_name", "ihme_loc_id", "year_id", "nid", "data")
  dt_ratios_all <- se$read_file(PATHS$fpath_dt_ratios) 
  dt_ratios_all <- dt_ratios_all[
    me_name %in% vaccine_ratios
    & nid %in% nids_all
    , ..vars_keep
  ]
  se$save_file(dt_ratios_all, file.path(output_root, "dt_ratio_data_all.csv"))
  
  
  message("Loading ratio vacciene draws")
  for (fold in fold_names){
    
    message(fold)
    draws <- load_fold_draws(dt_nid_list     = dt_nid_list_ratios
                             , PATHS         = PATHS
                             , fold          = fold
                             , vaccine_names = vaccine_ratios
                             , vars_keep     = vars_keep_draws)
    message("--saving draws")
    se$save_file(draws, file.path(PATHS$dirs_cross_val[[fold]], "draws_coverage_and_nids_ratios.rds"))
    rm(draws)
    gc()
  }
  
  message("Reading GBD-best draws for in-sample metrics")
  dt_nids_all_ratios <- se$read_file(file.path(output_root, "dt_nids_ratios_all_folds.csv"))
  load_and_save_gbd_draws(
    output_root        = output_root
    , dt_nids_all      = dt_nids_all_ratios
    , locations        = locations
    , draws_root_gbdxx = draws_root_gbdxx
    , PARAMS           = PARAMS
    , vaccine_names    = vaccine_ratios
    , fname_suffix     = "ratios" 
  )
  
  
  
  
  
  draws_straight <- se$read_file(file.path(output_root, "draws_coverage_and_nids_gbd_best_straight.rds"))
  draws_ratios <- se$read_file(file.path(output_root, "draws_coverage_and_nids_gbd_best_ratios.rds"))
  draws_combined <- rbind(draws_straight, draws_ratios)
  se$save_file(draws_combined, file.path(output_root, "draws_coverage_and_nids_gbd_best.rds"))
  rm(draws_straight, draws_ratios, draws_combined)
  gc()
  
  for(fold in fold_names){
    message(fold)
    draws_straight <- se$read_file(file.path(PATHS$dirs_cross_val[[fold]], "draws_coverage_and_nids_straight.rds"))
    draws_ratios <- se$read_file(file.path(PATHS$dirs_cross_val[[fold]], "draws_coverage_and_nids_ratios.rds"))
    draws_combined <- rbind(draws_straight, draws_ratios)
    se$save_file(draws_combined, file.path(PATHS$dirs_cross_val[[fold]], "draws_coverage_and_nids.rds"))
    rm(draws_straight, draws_ratios, draws_combined)
    gc()
  }
} else {
  message("Skipping prep for ratio draws")
}





message("Calculating fit metrics for coverage.")

output_dir_fits <- file.path(output_root, "fits")
se$make_directory(output_dir_fits)

fpaths_draws_nids <- c(
  file.path(PATHS$dirs_cross_val, "draws_coverage_and_nids.rds")
  , file.path(output_root, "draws_coverage_and_nids_gbd_best.rds")
)
names(fpaths_draws_nids) <- c(
  fold_names
  , "gbd_best"
)
draws_list <- mclapply(fpaths_draws_nids, se$read_file)


message("  RMSE")

rmse <- purrr::imap(draws_list, function(dt, dt_name){
  dt[, rmse := sqrt(mean((data - value)^2)), by = c("me_name")]
  dt[, fold := dt_name]
  dt[, .(rmse = mean(rmse, na.rm = TRUE)), by = .(me_name, fold)]
})

message("  Median MAE")

mae <- purrr::imap(draws_list, function(dt, dt_name){
  dt[, mae := median(abs(data - value)), by = c("me_name")]
  dt[, fold := dt_name]
  dt[, .(mae = mean(mae, na.rm = TRUE)), by = .(me_name, fold)]
})

message(  " Median Error ME")


mederr <- purrr::imap(draws_list, function(dt, dt_name){
  dt[, me := median(data - value), by = c("me_name")]
  dt[, fold := dt_name]
  dt[, .(me = mean(me, na.rm = TRUE)), by = .(me_name, fold)]
})






dt_rmse <- format_oos_metrics(rmse, fold_names, "rmse")
se$save_file(dt_rmse, file.path(output_dir_fits, "root_mean_squared_error_coverage_all_folds.csv"))
dt_mae <- format_oos_metrics(mae, fold_names, "mae")
se$save_file(dt_mae, file.path(output_dir_fits, "median_absolute_error_coverage_all_folds.csv"))
dt_mederr <- format_oos_metrics(mederr, fold_names, "me")
se$save_file(dt_mederr, file.path(output_dir_fits, "median_error_coverage_all_folds.csv"))



message("Calculating fit metrics after pooling all folds.")
draws_list_folds_combined <- list(
  folds = rbindlist(draws_list[fold_names])
  , gbd_best = draws_list[["gbd_best"]]
)
message("  RMSE")
rmse_folds_combined <- purrr::imap(draws_list_folds_combined, function(dt, dt_name){
  dt[, rmse := sqrt(mean((data - value)^2)), by = c("me_name")]
  dt[, .(rmse = mean(rmse, na.rm = TRUE)), by = .(me_name)]
})
message("  Median MAE")
mae_folds_combined <- purrr::imap(draws_list_folds_combined, function(dt, dt_name){
  dt[, mae := median(abs(data - value)), by = c("me_name")]
  dt[, .(mae = mean(mae, na.rm = TRUE)), by = .(me_name)]
})
message("  Median Error ME")
mederr_folds_combined <- purrr::imap(draws_list_folds_combined, function(dt, dt_name){
  dt[, me := median(data - value), by = c("me_name")]
  dt[, .(me = mean(me, na.rm = TRUE)), by = .(me_name)]
})

dt_rmse <- format_oos_metrics_folds_combined(rmse_folds_combined, "rmse")
se$save_file(dt_rmse, file.path(output_dir_fits, "root_mean_squared_error_coverage_folds_combined.csv"))
dt_mae <- format_oos_metrics_folds_combined(mae_folds_combined, "mae")
se$save_file(dt_mae, file.path(output_dir_fits, "median_absolute_error_coverage_folds_combined.csv"))
dt_mederr <- format_oos_metrics_folds_combined(mederr_folds_combined, "me")
se$save_file(dt_mederr, file.path(output_dir_fits, "median_error_coverage_folds_combined.csv"))






if (PARAMS$prepare_mrbrt_draws == TRUE) {
  
  message("Loading MR-BRT custom stage 1 ratio models")
  
  
  
  
  vars_Vs_mrbrt <- sprintf("V%s", 1:500)
  vars_draws_mrbrt <- sprintf("draw_%s", 0:499)
  vars_keep_mrbrt_preds <- c("location_id", "year_id", "me_name", "nid", "preds", "years_since_intro")
  vars_keep_mrbrt_preds_gbd_best <- c(vars_keep_mrbrt_preds, "data")
  vars_keep_mrbrt_draws <- c("location_id", "year_id", "me_name", "nid", "preds", "years_since_intro", vars_Vs_mrbrt)
  
  
  
  for (idx in seq_along(fold_names)) {
    
    i_chr <- as.character(idx)
    fold <- fold_names[idx]
    message("Fold: ", fold)
    to_model_cs1_dir <- file.path(PATHS$dirs_cross_val_to_model[[fold]], 'coverage_ratio_custom_stage_1')
    mrbrt_draws_paths <- file.path(to_model_cs1_dir, vaccine_ratios_cs1, 'draws2.rds')
    mrbrt_draws_list <- lapply(mrbrt_draws_paths, se$read_file)
    names(mrbrt_draws_list) <- vaccine_ratios_cs1
    
    
    
    message("MRBRT preds list")
    mrbrt_pred_list <- lapply(mrbrt_draws_list, function(dt){
      
      vacc_name <- unique(dt$me_name)
      vacc_name <- vacc_name[!is.na(vacc_name)]
      message("--", vacc_name)
      dt <- dt[, ..vars_keep_mrbrt_preds]
      setnames(dt, "preds", "value")
      
      if (any(nids_list[i_chr] %in% dt$nid)) {
        stop("Some NIDs are in the holdout set: ", vacc_name)
      }
      
      
      
      dt <- merge(
        dt_nid_list_ratios[[fold]][me_name == vacc_name, .(location_id, year_id, me_name, nid, data)]
        , dt[, .(location_id, year_id, value, years_since_intro)]
        , by = c("location_id", "year_id")
        , all.x = TRUE
      )
      
      message("--Filtering MRBRT to post-intro years")
      dt <- dt[!is.na(years_since_intro)]
      dt[, c("years_since_intro") := NULL]
      
      if (any(is.na(dt))) stop("NA values in MRBRT preds")
      if (any(dt$value == 0)) stop("Zero values in MRBRT preds")
      dt
    })
    dt_mrbrt_pred <- rbindlist(mrbrt_pred_list)
    se$save_file(dt_mrbrt_pred, file.path(PATHS$dirs_cross_val[[fold]], "preds_mrbrt_and_nids_cs1_ratios.rds"))
    rm(mrbrt_pred_list, dt_mrbrt_pred)
    
    
    message("MRBRT draws list")
    mrbrt_draws_list <- lapply(mrbrt_draws_list, function(dt){
      
      vacc_name <- unique(dt$me_name)
      vacc_name <- vacc_name[!is.na(vacc_name)]
      message("--", vacc_name)
      dt <- dt[, ..vars_keep_mrbrt_draws]
      setnames(dt, vars_Vs_mrbrt, vars_draws_mrbrt)
      
      dt <- melt(dt, id.vars = c("location_id", "year_id", "me_name", "nid", "preds", "years_since_intro"), variable.name = "draw", value.name = "value")
      dt[, draw := defactor_vector(draw)]
      
      dt[, value := invlogit(value)]
      if (any(nids_list[i_chr] %in% dt$nid)) {
        stop("Some NIDs are in the holdout set: ", vacc_name)
      }
      
      
      
      
      dt <- merge(
        dt_nid_list_ratios[[fold]][me_name == vacc_name, .(location_id, year_id, me_name, nid, data)]
        , dt[, .(location_id, year_id, draw, value, years_since_intro)]
        , by = c("location_id", "year_id")
        , all.x = TRUE
      )
      
      message("--Filtering MRBRT to post-intro years")
      dt <- dt[!is.na(years_since_intro)]
      dt[, c("years_since_intro") := NULL]
      
      if (any(is.na(dt))) stop("NA values in MRBRT draws")
      if (any(dt$value == 0)) stop("Zero values in MRBRT draws")
      dt
    })
    dt_mrbrt_draws <- rbindlist(mrbrt_draws_list)
    se$save_file(dt_mrbrt_draws, file.path(PATHS$dirs_cross_val[[fold]], "draws_mrbrt_and_nids_cs1_ratios.rds"))
    
  }
  
  
  
  message("Loading & saving MR-BRT GBD best") 
  
  message("--preds")
  
  mrbrt_gbd_preds_list <- lapply(seq_along(vaccine_ratios_cs1), function(idx){
    
    vacc_name <- vaccine_ratios_cs1[idx]
    message("--", vacc_name)
    dt <- se$read_file(file.path(PATHS$to_model_cs1_gbd_best, vacc_name, sprintf("mrbrt_custom_stage_1_%s_results.csv", vacc_name)))
    dt <- dt[!is.na(years_since_intro) & nid %in% nids_ratios_all, ..vars_keep_mrbrt_preds]
    setnames(dt, "preds", "value")
    
    
    dt <- merge(
      dt
      , dt_nids_ratios[, .(location_id, year_id, me_name, nid, data)]
      , by = c("location_id", "me_name", "year_id", "nid")
      , all.x = TRUE
    )
    if (any(is.na(dt))) stop("NA values in GBD best MRBRT preds")
    if (any(dt$value == 0)) stop("Zero values in GBD best  MRBRT preds")
    dt[, years_since_intro := NULL]
    dt
  })
  names(mrbrt_gbd_preds_list) <- vaccine_ratios_cs1
  dt_mrbrt_gbd <- rbindlist(mrbrt_gbd_preds_list)
  se$save_file(dt_mrbrt_gbd, file.path(output_root, "preds_mrbrt_and_nids_gbd_best.rds"))
  
  message("--draws")
  
  to_model_cs1_dir_best <- file.path(PATHS$to_model_cs1_gbd_best)
  mrbrt_gbd_draws_paths <- file.path(to_model_cs1_dir_best, vaccine_ratios_cs1, 'draws2.rds')
  mrbrt_gbd_draws_list <- lapply(mrbrt_gbd_draws_paths, se$read_file)
  names(mrbrt_gbd_draws_list) <- vaccine_ratios_cs1
  mrbrt_gbd_draws_list <- lapply(mrbrt_gbd_draws_list, function(dt){
    
    vacc_name <- unique(dt$me_name)
    vacc_name <- vacc_name[!is.na(vacc_name)]
    message("--", vacc_name)
    dt <- dt[, ..vars_keep_mrbrt_draws]
    setnames(dt, vars_Vs_mrbrt, vars_draws_mrbrt)
    
    dt <- melt(dt, id.vars = c("location_id", "year_id", "me_name", "nid", "preds", "years_since_intro"), variable.name = "draw", value.name = "value")
    dt[, draw := defactor_vector(draw)]
    
    dt[, value := invlogit(value)]
    
    
    
    
    dt <- merge(
      dt_nids_ratios[me_name == vacc_name, .(location_id, year_id, me_name, nid, data)]
      , dt[, .(location_id, year_id, draw, value, years_since_intro)]
      , by = c("location_id", "year_id")
      , all.x = TRUE
    )
    
    message("--Filtering MRBRT to post-intro years")
    dt <- dt[!is.na(years_since_intro)]
    dt[, c("years_since_intro") := NULL]
    
    if (any(is.na(dt))) stop("NA values in MRBRT draws")
    if (any(dt$value == 0)) stop("Zero values in MRBRT draws")
    dt
  })
  dt_gbd_mrbrt_draws <- rbindlist(mrbrt_gbd_draws_list)
  se$save_file(dt_gbd_mrbrt_draws, file.path(output_root, "draws_mrbrt_and_nids_cs1_ratios_gbd_best.rds"))
  
  
  
  
  
}  else {
  message("Skipping prep for MR-BRT CS1 draws")
}




message("Calculating fit metrics for MRBRT Scale-up models.")

fpaths_mrbrt_preds <- c(
  file.path(PATHS$dirs_cross_val, "preds_mrbrt_and_nids_cs1_ratios.rds")
  , file.path(output_root, "preds_mrbrt_and_nids_gbd_best.rds")
)
names(fpaths_mrbrt_preds) <- c(
  fold_names
  , "gbd_best"
)
mrbrt_preds_list <- mclapply(fpaths_mrbrt_preds, se$read_file)

fpaths_mrbrt_draws <- c(
  file.path(PATHS$dirs_cross_val, "draws_mrbrt_and_nids_cs1_ratios.rds")
  , file.path(output_root, "draws_mrbrt_and_nids_cs1_ratios_gbd_best.rds")
)
names(fpaths_mrbrt_draws) <- c(
  fold_names
  , "gbd_best"
)
mrbrt_draws_list <- mclapply(fpaths_mrbrt_draws, se$read_file)


message("  RMSE")

rmse_mrbrt_preds <- purrr::imap(mrbrt_preds_list, function(dt, fold_name){
  dt[, rmse := sqrt(mean((data - value)^2)), by = c("me_name")]
  dt[, fold := fold_name]
  dt[, .(rmse = mean(rmse, na.rm = TRUE)), by = .(me_name, fold)]
})
rmse_mrbrt_draws <- purrr::imap(mrbrt_draws_list, function(dt, fold_name){
  dt[, rmse := sqrt(mean((data - value)^2)), by = c("me_name")]
  dt[, fold := fold_name]
  dt[, .(rmse = mean(rmse, na.rm = TRUE)), by = .(me_name, fold)]
})
message("  Median MAE")

mae_mrbrt_preds <- purrr::imap(mrbrt_preds_list, function(dt, fold_name){
  dt[, mae := median(abs(data - value)), by = c("me_name")]
  dt[, fold := fold_name]
  dt[, .(mae = mean(mae, na.rm = TRUE)), by = .(me_name, fold)]
})
mae_mrbrt_draws <- purrr::imap(mrbrt_draws_list, function(dt, fold_name){
  dt[, mae := median(abs(data - value)), by = c("me_name")]
  dt[, fold := fold_name]
  dt[, .(mae = mean(mae, na.rm = TRUE)), by = .(me_name, fold)]
})
message("  Median Error ME")

mederr_mrbrt_preds <- purrr::imap(mrbrt_preds_list, function(dt, fold_name){
  dt[, me := median(data - value), by = c("me_name")]
  dt[, fold := fold_name]
  dt[, .(me = mean(me, na.rm = TRUE)), by = .(me_name, fold)]
})
mederr_mrbrt_draws <- purrr::imap(mrbrt_draws_list, function(dt, fold_name){
  dt[, me := median(data - value), by = c("me_name")]
  dt[, fold := fold_name]
  dt[, .(me = mean(me, na.rm = TRUE)), by = .(me_name, fold)]
})





dt_rmse_mrbrt_preds <- format_oos_metrics(rmse_mrbrt_preds, fold_names, "rmse")
se$save_file(dt_rmse_mrbrt_preds, file.path(output_dir_fits, "root_mean_squared_error_mrbrt_cs1_preds_all_folds.csv"))
dt_rmse_mrbrt_draws <- format_oos_metrics(rmse_mrbrt_draws, fold_names, "rmse")
se$save_file(dt_rmse_mrbrt_draws, file.path(output_dir_fits, "root_mean_squared_error_mrbrt_cs1_draws_all_folds.csv"))
dt_mae_mrbrt_preds <- format_oos_metrics(mae_mrbrt_preds, fold_names, "mae")
se$save_file(dt_mae_mrbrt_preds, file.path(output_dir_fits, "median_absolute_error_mrbrt_cs1_preds_all_folds.csv"))
dt_mae_mrbrt_draws <- format_oos_metrics(mae_mrbrt_draws, fold_names, "mae")
se$save_file(dt_mae_mrbrt_draws, file.path(output_dir_fits, "median_absolute_error_mrbrt_cs1_draws_all_folds.csv"))
dt_mederr_mrbrt_preds <- format_oos_metrics(mederr_mrbrt_preds, fold_names, "me")
se$save_file(dt_mederr_mrbrt_preds, file.path(output_dir_fits, "median_error_mrbrt_cs1_preds_all_folds.csv"))
dt_mederr_mrbrt_draws <- format_oos_metrics(mederr_mrbrt_draws, fold_names, "me")
se$save_file(dt_mederr_mrbrt_draws, file.path(output_dir_fits, "median_error_mrbrt_cs1_draws_all_folds.csv"))


message("Calculating fit metrics after pooling all folds.")
draws_list_folds_combined <- list(
  folds = rbindlist(mrbrt_draws_list[fold_names])
  , gbd_best = mrbrt_draws_list[["gbd_best"]]
)
message("  RMSE")
rmse_folds_combined <- purrr::imap(draws_list_folds_combined, function(dt, dt_name){
  dt[, rmse := sqrt(mean((data - value)^2)), by = c("me_name")]
  dt[, .(rmse = mean(rmse, na.rm = TRUE)), by = .(me_name)]
})
message("  Median MAE")
mae_folds_combined <- purrr::imap(draws_list_folds_combined, function(dt, dt_name){
  dt[, mae := median(abs(data - value)), by = c("me_name")]
  dt[, .(mae = mean(mae, na.rm = TRUE)), by = .(me_name)]
})
dt_rmse <- format_oos_metrics_folds_combined(rmse_folds_combined, "rmse")
se$save_file(dt_rmse, file.path(output_dir_fits, "root_mean_squared_error_mrbrt_cs1_draws_folds_combined.csv"))
dt_mae <- format_oos_metrics_folds_combined(mae_folds_combined, "mae")
se$save_file(dt_mae, file.path(output_dir_fits, "median_absolute_error_mrbrt_cs1_folds_draws_combined.csv"))





message("Combining all fit metrics into one table")
fpaths <- list(
  rmse_coverage = file.path(output_dir_fits, "root_mean_squared_error_coverage_folds_combined.csv")
  , mae_coverage = file.path(output_dir_fits, "median_absolute_error_coverage_folds_combined.csv")
  , rmse_cs1 = file.path(output_dir_fits, "root_mean_squared_error_mrbrt_cs1_draws_folds_combined.csv")
  , mae_cs1 = file.path(output_dir_fits, "median_absolute_error_mrbrt_cs1_folds_draws_combined.csv")
)

pres_list <- lapply(fpaths, se$read_file)
pres_list <- purrr::imap(pres_list, function(dt, dt_name){
  metric_name <- strsplit(dt_name, "_")[[1]][1]
  model_name <- strsplit(dt_name, "_")[[1]][2]
  dt[, metric := metric_name]
  dt[, model := model_name]
  setnames(dt, "combined_folds", "value")
})

dt_coverage <- rbindlist(pres_list[c("rmse_coverage", "mae_coverage")])
dt_coverage <- dcast(dt_coverage, me_name ~ metric, value.var = c("in_sample", "value"))
setnames(dt_coverage, c("value_mae", "value_rmse"), c("mae", "rmse"))
dt_coverage[, model := "coverage"]
setcolorder(dt_coverage, c("model", "me_name", "in_sample_mae", "mae", "in_sample_rmse", "rmse"))
se$save_file(dt_coverage, file.path(output_dir_fits, "fit_metrics_all_models_coverage.csv"), csv_opt = "data.table::fwrite", scipen = 4L)

dt_cs1 <- rbindlist(pres_list[c("rmse_cs1", "mae_cs1")])
dt_cs1 <- dcast(dt_cs1, me_name ~ metric, value.var = c("in_sample", "value"))
setnames(dt_cs1, c("value_mae", "value_rmse"), c("mae", "rmse"))
dt_cs1[, model := "mrbrt_cs1"]
setcolorder(dt_cs1, c("model", "me_name", "in_sample_mae", "mae", "in_sample_rmse", "rmse"))
se$save_file(dt_cs1, file.path(output_dir_fits, "fit_metrics_all_models_mrbrt_cs1.csv"), csv_opt = "data.table::fwrite", scipen = 4L)

message("Done.")
