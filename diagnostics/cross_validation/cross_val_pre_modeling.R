







r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
   message("Starting arg-parser.")
   se$parse_all_named_cli_args(required_args = list(code_root = "character"
                                                    , config_path = "character"
                                                    , output_root = "character"))
} else {
   if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
      code_root <- Sys.getenv()['CODE_ROOT'] 
   } else {
      code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
   }
   
   config_path <- file.path(code_root, "pipeline_config.yaml")
   output_root <- file.path("FILEPATH", "gbd2023", "2025_03_17")
}
message("code_root: ", code_root)
message("config_path: ", config_path)
message("output_root: ", output_root)




source(file.path(code_root, "init.r"))
source(file.path(code_root, "FILEPATH/outlier_build_table_entry.R"))
pipeline_config <- se$read_file(config_path)





message("Making cross validation reference directories")
output_dir_list <- lapply(1:pipeline_config$cross_validation$k_folds, function(fold){
   message("-- Fold: ", fold)
   output_dir <- file.path(output_root, paste0("fold_", fold))
   se$make_directory(output_dir)
   return(output_dir)
})

dir_to_model_best <- normalizePath(file.path(to_model_root, "best"))



params <- list(
   
   paths = list(
      dir_to_model_best = dir_to_model_best
      
      
      
      , from = list(
         outliers = file.path(ref_data_repo, "vaccination.csv")
         
         , data_post_decider = file.path(dir_to_model_best, "02.5_pre_dpt1_imputation.rds")
         , data_with_ratios = file.path(dir_to_model_best, "04.5_pre_ratio_stockouts.rds")
         , vaccine_intros = file.path(ref_data_repo, "vaccine_intro.rds")
      )
      , to = list(
         outliers = file.path(output_root, "vaccination.csv")
         , data_post_decider = file.path(output_root, "02.5_pre_dpt1_imputation.rds")
         , data_with_ratios = file.path(output_root, "04.5_pre_ratio_stockouts.rds")
         , vaccine_intros = file.path(output_root, "vaccine_intro.rds")
      )
   )
   
   , years = list(year_last = as.integer(year_end_gbd - 7))
   , vaccines_modeled = list(
      "vacc_bcg",
      "vacc_polio3",
      "vacc_dpt1",
      "vacc_dpt3",
      "vacc_hepb3",
      "vacc_hib3",
      "vacc_mcv1",
      "vacc_mcv2",
      "vacc_pcv3",
      "vacc_rotac",
      "vacc_rcv1"
   )
   , vaccine_ratios = list(
      "vacc_hib3_dpt3_ratio",
      "vacc_pcv3_dpt3_ratio",
      "vacc_rotac_dpt3_ratio",
      "vacc_rcv1_mcv1_ratio",
      "vacc_hepb3_dpt3_ratio",
      "vacc_mcv2_mcv1_ratio",
      "vacc_dpt3_dpt1_ratio"
   )
)




metadata <- list(
   metadata = se$build_metadata_shell(code_root = code_root)
   , init_list = init_list
   , params = params
)
se$save_file(metadata, file.path(output_root, "metadata.yaml"))





for(idx in seq_along(params$paths$from)){
   stopifnot(length(params$paths$from) == length(params$paths$to))
   from <- params$paths$from[[idx]]
   to   <- params$paths$to[[idx]]
   message("Copying:  ", from, "\n  -- to:  ", to, "\n")
   file.copy(from, to, overwrite = TRUE)
}





dt <- se$read_file(params$paths$to$data_post_decider)
dt_survey <-
   dt[
      cv_survey == 1 
      & !is.na(data)
      & is.na(cv_outlier)
      & year_end < params$years$year_last
      & me_name %in% params$vaccines_modeled
   ]
dt_nids <- dt_survey[
   , .(
      me_name
      , ihme_loc_id
      , year_id
      , year_start
      , year_end
      , data
      , cv_survey
      , nid
      , survey_name
   )
]


nids <- unique(dt_nids$nid)


dt_ratios <- se$read_file(params$paths$to$data_with_ratios)
dt_survey_ratios <-
   dt_ratios[
      nid %in% nids 
      & me_name %in% unlist(params$vaccine_ratios)
   ]
dt_nids_ratios <- dt_survey_ratios[
   , .(
      me_name
      , ihme_loc_id
      , year_id
      , year_start
      , year_end
      , data
      , cv_survey
      , nid
      , survey_name
   )
]



set.seed(123)
nids                <- as.integer(sample(nids))
remainder           <- length(nids) %% pipeline_config$cross_validation$k_folds
nids_remainder      <- nids[1:remainder]
nids                <- setdiff(nids, nids_remainder)
nids_folds          <- split(nids, 1:pipeline_config$cross_validation$k_folds)
folds_for_remainder <- 1:length(nids_remainder)

for(idx in seq_along(nids_remainder)){
   i_chr <- as.character(idx)
   nids_folds[[i_chr]] <- c(nids_folds[[i_chr]], nids_remainder[idx])
}


any_intersection <- any(sapply(combn(nids_folds, 2, simplify = FALSE), function(x) {
   length(intersect(x[[1]], x[[2]])) > 0
}))
if(any_intersection == TRUE) stop("Some nids are in more than one fold")





se$save_file(nids_folds, file.path(output_root, "nids_all_folds.yaml"))
se$save_file(dt_nids, file.path(output_root, "dt_nids_all_folds.csv"))
se$save_file(dt_nids_ratios, file.path(output_root, "dt_nids_ratios_all_folds.csv"))

message("Saving out k-fold nids and summary tables")
for (idx in 1:pipeline_config$cross_validation$k_folds) {
   
   message("--Fold ", idx)
   i_chr <- as.character(idx)
   output_dir <- output_dir_list[[idx]]
   
   se$save_file(nids_folds[[i_chr]], file.path(output_dir, "nids.yaml"))
   
   
   dt_nids_fold <- dt_nids[nid %in% nids_folds[[i_chr]], ]
   se$save_file(dt_nids_fold, file.path(output_dir, "dt_nids.csv"))
   
   dt_nids_fold_ratios <- dt_nids_ratios[nid %in% nids_folds[[i_chr]], ]
   se$save_file(dt_nids_fold_ratios, file.path(output_dir, "dt_nids_ratios.csv"))
   
   
   
   
   
   dt_summary <- copy(dt_nids_fold)
   dt_summary[, parent_loc := substr(ihme_loc_id, 1, 3)]
   dt_summary <- dt_summary[, .(N_location_years = .N), by = .(me_name, parent_loc)]
   dt_summary <- dt_summary[, N_max := max(N_location_years), by = parent_loc][order(-N_max, parent_loc, me_name)]
   se$save_file(dt_summary, file.path(output_dir, "dt_vaccine_country_year_summary.csv"))
   
   dt_summary_ratio <- copy(dt_nids_fold_ratios)
   dt_summary_ratio[, parent_loc := substr(ihme_loc_id, 1, 3)]
   dt_summary_ratio <- dt_summary_ratio[, .(N_location_years = .N), by = .(me_name, parent_loc)]
   dt_summary_ratio <- dt_summary_ratio[, N_max := max(N_location_years), by = parent_loc][order(-N_max, parent_loc, me_name)]
   se$save_file(dt_summary_ratio, file.path(output_dir, "dt_vaccine_country_year_summary_ratios.csv"))
   
   
   dt_summary_vaccine <- copy(dt_nids_fold)
   dt_summary_vaccine <- dt_summary_vaccine[, .(unique_nids = uniqueN(nid)), by = .(me_name)]
   se$save_file(dt_summary_vaccine, file.path(output_dir, "dt_nid_by_vaccine_summary.csv"))
   dt_summary_ratio_vaccine <- copy(dt_nids_fold_ratios)
   dt_summary_ratio_vaccine <- dt_summary_ratio_vaccine[, .(unique_nids = uniqueN(nid)), by = .(me_name)]
   se$save_file(dt_summary_ratio_vaccine, file.path(output_dir, "dt_nid_by_vaccine_summary_ratios.csv"))
}


dt_summary_vaccine_all <- rbindlist(lapply(1:pipeline_config$cross_validation$k_folds, function(fold){
   i_chr <- as.character(fold)
   dt_summary_vaccine <- se$read_file(file.path(output_dir_list[[fold]], "dt_nid_by_vaccine_summary.csv"))
   dt_summary_vaccine[, fold := i_chr]
   return(dt_summary_vaccine)
}))
se$save_file(dt_summary_vaccine_all, file.path(output_root, "dt_nid_by_vaccine_summary_all.csv"))

dt_summary_vaccine_all_ratios <- rbindlist(lapply(1:pipeline_config$cross_validation$k_folds, function(fold){
   i_chr <- as.character(fold)
   dt_summary_vaccine <- se$read_file(file.path(output_dir_list[[fold]], "dt_nid_by_vaccine_summary_ratios.csv"))
   dt_summary_vaccine[, fold := i_chr]
   return(dt_summary_vaccine)
}))
se$save_file(dt_summary_vaccine_all_ratios, file.path(output_root, "dt_nid_by_vaccine_summary_ratios_all.csv"))





message("Building k-fold outlier tables")
for (idx in 1:pipeline_config$cross_validation$k_folds) {
   
   output_dir <- output_dir_list[[idx]]
   file.copy(
      from = file.path(output_root, "vaccination.csv")
      , to = file.path(output_dir, "outliers_cross_val.csv")
   )
   
   i_chr <- as.character(idx)
   message("Fold: ", idx)
   
   outlier_entry_list <- list()
   for(nid_i in nids_folds[[i_chr]]){
      
      survey_name_i <- dt_nids[nid == nid_i, unique(survey_name)]
      if(length(survey_name_i) > 1) stop(nid_i)
      outlier_entry_list <- append(outlier_entry_list, list(outliers_build_table_entry(
         ihme_loc_id     = NA
         , me_names      = NA
         , year_ids      = NA
         , batch_outlier = 0L
         , gbd_outlier   = 1L
         , lbd_outlier   = 1L
         , nid           = nid_i
         , survey_name   = survey_name_i
         , notes_varname = "cross_validation"
         , notes_entry   = sprintf("cross_validation fold %s", i_chr)
         , fpath_outlier_sheet = file.path(output_dir, "outliers_cross_val.csv")
         , require_input = FALSE
         
      )))
   }
   
   (outlier_entry <- rbindlist(outlier_entry_list, fill = TRUE))
   
   outliers_append_to_reference_sheet(
      outlier_entry_dt          = outlier_entry
      , outlier_sheet_path      = file.path(output_dir, "outliers_cross_val.csv")
      , archive_tf              = FALSE
      , update_version_register = FALSE
      , require_input           = FALSE
   )
}

message('Done.')
