#' @title Prepare vaccine ratio estimates

#' @description This function prepares vaccine ratio estimates by reading in the raw or adjusted





#' @param me The measure to prepare
#' @param gbd_run_date A string indicating the date of the model run
#' @param stgpr_run_log_root [chr] e.g. to_model_dir - where is the date_version specific stgpr run log saved? 
#' @param save_draws Logical indicating whether to save the resulting vaccine ratio draws.
#' @param save_collapsed Logical indicating whether to save the resulting vaccine ratio

#' @param special_draw_root An optional root directory where the adjusted vaccine coverage


#' @param path The directory where the vaccine ratio estimates will be saved.
#' @param save_ratio_draws Logical indicating whether to save the resulting vaccine ratio draws.
#' @param offset_remove Logical indicating whether to remove a user-specified offset from the vaccine

#' @param stockout_ratios Logical indicating whether to adjust the vaccine ratio estimates

#' @param quiet Logical indicating whether to suppress messages.
#' @param me_db [data.table] me_db.csv from reference repo

#' @return NULL

#' @concept prep_draws_composite_functions
prep_ratio <- function(me,
                       gbd_run_date,
                       stgpr_run_log_root      = input_root,
                       save_draws              = FALSE,
                       save_collapsed          = FALSE,
                       special_draw_root       = NULL,
                       path                    = save_root,
                       save_ratio_draws        = TRUE,
                       offset_remove           = FALSE,
                       stockout_ratios         = FALSE,
                       quiet                   = FALSE,
                       me_db) {

  
  if(!quiet){
    message(paste0("-- Preparing ", me))
  }

  
  key   <- c("location_id", "year_id", "age_group_id", "sex_id")
  mes   <- unlist(strsplit(me, "_"))[2:3]
  num   <- paste0("vacc_", mes[1])
  denom <- paste0("vacc_", mes[2])

  
  
  
  if (is.null(special_draw_root)) {
    run_log <- read_stgpr_best_runs(root = stgpr_run_log_root, model_type = "vaccine_coverage")
    id.ratio <- run_log[me_name == me, run_id]
    df.ratio <- read_draws(id.ratio, offset=offset_remove)
  } else {
    draw_files <- list.files(file.path(special_draw_root, me), full.names = TRUE)
    df.ratio   <- lapply(draw_files, fread) %>% rbindlist(., use.names=TRUE)
  }
  df.ratio <- set_intro(df.ratio, me)
  df.ratio <- cap_est(df.ratio)
  
  draw_files <- list.files(file.path(save_root, denom), full.names = TRUE)
  df.denom   <- lapply(draw_files, fread) %>% rbindlist(., use.names=TRUE)
  df.denom   <- cap_est(df.denom)


  cols     <- grep("draw_", names(df.ratio), value=TRUE)
  df.ratio <- merge(df.ratio, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
  
  if (me %in% "vacc_rotac_dpt3_ratio") {
    df.ratio <- df.ratio[grepl("PHL", ihme_loc_id) & year_id > 2015, (cols) := 0]
    df.ratio <- df.ratio[grepl("VEN", ihme_loc_id) & year_id > 2017, (cols) := 0]
  }
  if (me %in% "vacc_hib3_dpt3_ratio") {
    df.ratio <- df.ratio[grepl("TUN", ihme_loc_id) & year_id %in% 2006:2010, (cols) := 0]
  }
  if (me %in% "vacc_rcv1_mcv1_ratio") {
    df.ratio <- df.ratio[grepl("MAR", ihme_loc_id) & year_id %in% 2008:2013, (cols) := 0]
  }
  if (me %in% "vacc_mcv2_mcv1_ratio") {
    df.ratio <- df.ratio[grepl("MAR", ihme_loc_id) & year_id %in% 2008:2013, (cols) := 0]
  }
  if (me %in% "vacc_pcv3_dpt3_ratio") {
    df.ratio <- df.ratio[grepl("VEN", ihme_loc_id) & year_id > 2016, (cols) := 0]
  }
  
  df.ratio[, ihme_loc_id := NULL]

  
  if (save_ratio_draws) {
    save_draws_general(df.ratio, me, path)
    if(!quiet){
      message(paste0("---- Saved ratio draws of ", me, " in ", path))
    }
  }

  
  df.ratio <- melt(df.ratio, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="ratio")
  df.denom <- melt(df.denom, id.vars=key, measure=patterns("^draw"), variable.name="draw", value.name="denom")
  df       <- merge(df.ratio, df.denom, by=c(key, "draw"))

  
  df <- df[, est := ratio * denom]
  df <- df[, c("ratio", "denom") := NULL]
  df <- df[est >= 1, est := 0.99]

  
  
  df <- aggregate_most_detailed(df, varname="est")

  
  if (num %in% me_db$me_name) cov_id <- me_db[me_name==num, covariate_id]

  
  if (save_draws) {
    df2 <- dcast.data.table(df, location_id + year_id + age_group_id + sex_id ~ draw, value.var="est")
    df2 <- df2[, measure_id := 18]
    if (num %in% me_db$me_name) df2[, covariate_id := cov_id]
    save_draws_general(df2, num, path)
    if(!quiet){
      message(paste0("---- Saved draws of ", num, " in ", path))
    }
  }

  
  if (save_collapsed) {
    df <- df[, gpr_mean := mean(est), by=key]
    df <- df[, gpr_lower := quantile(est, 0.025), by=key]
    df <- df[, gpr_upper := quantile(est, 0.975), by=key]
    df <- df[, c(key, "gpr_mean", "gpr_lower", "gpr_upper"), with=FALSE] %>% unique
    df <- df[, me_name := num]
    df <- df[, measure_id := 18]
    if (num %in% me_db$me_name) df[, covariate_id := cov_id]
    save_collapsed_general(df, num, results_root)
  }
}
