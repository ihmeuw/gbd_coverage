#' @title Prep Draws

#' @description This function reads in the vaccine-specific raw draw-level estimates produced by the primary ST-GPR coverage model




#' @param me The measure to prepare
#' @param me_names_epi_intro [chr] Vaccines modeled as 'straight' from prep_exp that need an intro applied in prep_draws. 

#' @param run_date A string indicating the date of the model run
#' @param stgpr_run_log_root [chr] e.g. to_model_dir - where is the date_version specific stgpr run log saved?
#' @param save_draws A logical indicating whether to save the prepared raw draw-level estimates to the Vaccines directory
#' @param save_collapsed A logical indicating whether to save the prepared collapsed mean summaries to the Vaccines directory
#' @param special_draw_root A path to a directory containing draws
#' @param me_db A data.table containing information about the measures (me_db.csv in the reference repo)
#' @param path A path to the root directory where the prepared draws will be saved
#' @param offset_remove A logical indicating whether to remove the offset from the draws
#' @param quiet A logical indicating whether to suppress messages printed during the function's execution

#' @return NULL (draws and collapsed summaries are saved to file)

#' @concept prep_draws_composite_functions
prep_draws <- function(me,
                       me_names_epi_intro,
                       run_date,
                       stgpr_run_log_root = input_root,
                       save_draws         = TRUE,
                       save_collapsed     = TRUE,
                       special_draw_root  = NULL,
                       me_db              = me_db,
                       path               = save_root,
                       offset_remove      = FALSE,
                       quiet              = FALSE) {
   
   run_log_dt <- read_stgpr_best_runs(root = stgpr_run_log_root, model_type = "vaccine_coverage")
   id         <- run_log_dt[me_name == me, run_id]

  
  if (me %in% me_db$me_name) cov_id <- me_db[me_name == me, covariate_id]
  if(!is.null(special_draw_root)) special_draw_root <- file.path(special_draw_root, me)
  df <- read_draws(id, special = special_draw_root, offset=offset_remove)
  df <- cap_est(df)
  df <- df[, run_id := id]

  
  if (me %in% me_names_epi_intro) df <- set_intro_epi(df, me)

  
  if (me %in% c("vacc_bcg")) {
    df <- apply_outro_year(df, me)
    df <- apply_outro_grl(df, me)
  }
  if (me %in% "vacc_polio3") {  
    
    
    cols <- grep("draw_", names(df), value=TRUE)
    df <- merge(df, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    df <- df[grepl("CUB", ihme_loc_id), (cols) := 0]
    df$ihme_loc_id <- NULL
  }
  
  assert_data_schema(my_dt = df)

  
  if (save_draws) {
    df2 <- copy(df)
    if (me %in% me_db$me_name) df2[, covariate_id := cov_id]
    save_draws_general(df2, me, path)
    if(!quiet){
      print(paste0("Saved draws of ", me, " under run_id ", id, " in ", path))
    }
  }

  
  if (save_collapsed){
    df <- collapse_draws(df)
    df <- df[, me_name := me]
    if (me %in% me_db$me_name) df[, covariate_id := cov_id]
    save_collapsed_general(df, me, results_root)
  }
}
