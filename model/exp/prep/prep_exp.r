








r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
  message("Starting arg-parser.")
  se$parse_all_named_cli_args(required_args = list(code_root     = "character"
                                                   , config_path = "character"))
} else {
  if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
    code_root <- Sys.getenv()[['CODE_ROOT']]
  } else {
    code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
  }
  config_path <- file.path(code_root, "pipeline_config.yaml")
}
message("code_root : ", code_root)
message("config_path: ", config_path)

source(file.path(code_root, "init.r"))



library(parallel)
library(readxl)
library(ggplot2)
library(boot)
library(lme4)
library(purrr)
library(splines)
library(binom)
library(stringr)
library(mgcv)


R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), recursive = FALSE, modifiedOnly = FALSE)
R.utils::sourceDirectory(file.path(code_root, "FILEPATH"), recursive = TRUE, modifiedOnly = FALSE)
source(file.path(code_root, "FILEPATH/ubcov_tools.r"))
source("FILEPATH/collapse_point.R")


source(file.path(code_root, "FILEPATH/plot_vc_data.R"))
source(file.path(code_root, "FILEPATH/prep_mrbrt_output_coverage_ratio_stage_1.R"))
source(file.path(cc_r_lib_root, "get_population.R"))
source(file.path(cc_r_lib_root, "get_location_metadata.R"))
source(file.path(cc_r_lib_root, "get_covariate_estimates.R"))
source(file.path(cc_r_lib_root, 'save_bundle_version.R'))
source(file.path(cc_r_lib_root, 'get_bundle_version.R'))
source(file.path(cc_r_lib_root, 'save_crosswalk_version.R'))
source(file.path(cc_r_lib_root, 'get_crosswalk_version.R'))
source(file.path(cc_r_lib_root, "get_bundle_data.R"))
source(file.path(cc_r_lib_root, "upload_bundle_data.R"))


source("FILEPATH/bundle_wrapper.R")


load_pipeline_config(step = "prep_exp", config_path = config_path)



if (is.null(date)) stop("Provide a date version in the `date` field of the pipeline config file.")

to_model_dir    <- file.path(to_model_root, date)  
share_model_dir <- file.path("FILEPATH")
slt_prep <- SLT$new(user_root_list = list(to_model_root = to_model_root), 
                    user_central_log_root = to_model_root)
slt_prep$create_date_version_folders_with_logs(date); rm(slt_prep)

if(bias_only) {
  on_time_cohort_only <- TRUE
  run.stockout        <- TRUE  
} else {
  on_time_cohort_only <- FALSE 
}




year_end_stockout <- year_end
if(fhs_run_TF){
  year_end_gbd        <- year_end
  year_end            <- fhs_year_end
  
  year_end_stockout   <- year_end_gbd + 2 
  location_set_id_gbd <- location_set_id
  location_set_id     <- fhs_location_set_id
  release_id_gbd      <- release_id
  release_id          <- fhs_release_id
}

locs <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)[level >= 3, ]





straight_models <- c(
  
  "vacc_mcv1",
  "vacc_dpt1",
  "vacc_dpt3",
  "vacc_bcg",
  "vacc_polio3"
)

ratios <- c(
  "vacc_hib3_dpt3_ratio",
  "vacc_pcv3_dpt3_ratio",
  "vacc_rotac_dpt3_ratio",
  "vacc_rcv1_mcv1_ratio",
  "vacc_hepb3_dpt3_ratio",
  "vacc_mcv2_mcv1_ratio",
  "vacc_dpt3_dpt1_ratio"
)




ratios_cs1 <- setdiff(
  ratios, 
  "vacc_dpt3_dpt1_ratio"
)

mes_to_launch <- c(
  straight_models, 
  ratios, 
  "vacc_dpt12_cond"
)

admin.vacc <- c(
  "vacc_bcg",
  "vacc_polio3",
  "vacc_dpt1",
  "vacc_dpt3",
  "vacc_hepb3",
  "vacc_hib3",
  "vacc_mcv1",
  "vacc_mcv2",
  "vacc_pcv1",
  "vacc_pcv3",
  "vacc_rotac",
  "vacc_yfv",
  "vacc_rcv1"
)

if (dpt_timeliness){
  mes_to_launch    <- c(mes_to_launch, "vacc_dpt3_timeliness_ratio")  
}





message("Beginning model prep")

dir.create(file.path(to_model_dir, "reference"), showWarnings = FALSE, recursive = TRUE)


vacc.intro                <- file.path(ref_data_repo, "vaccine_intro.rds")
vacc.outliers             <- file.path(ref_data_repo, "vaccination.csv")

vacc.outliers             <- validate_path(fpath_outliers_config)
if (is.null(vacc.outliers)) vacc.outliers <- file.path(ref_data_repo, "vaccination.csv")
assert_files_exist(vacc.outliers)
vacc.lit                  <- file.path(ref_data_repo, "vaccine_lit.rds")
vacc.lit.raw              <- file.path(ref_data_repo, "literature_extraction_coverage.csv")
vacc.schedule             <- file.path(ref_data_repo, "vaccine_schedule.rds")
vacc.epi                  <- file.path(ref_data_repo, "vaccine_intro_epi.rds")
stockout.cov.out          <- file.path(ref_data_repo, "gbd_model/stockout_cov.csv")
age.specific.stockout.out <- file.path(ref_data_repo, "age_specific_stockouts_outliers.csv")
who.reported.stockouts    <- file.path(ref_data_repo, "who_reported_stockouts.rds")

vacc.official             <- file.path(data_root, "FILEPATH/who_official.rds")
vacc.admin                <- file.path(data_root, "FILEPATH/who_admin.rds")
vacc.survey               <- file.path(data_root, "FILEPATH")
vacc.stockouts            <- file.path(data_root, "FILEPATH/exact_matches.csv")
vacc.ratio.stockouts      <- file.path(data_root, "FILEPATH/exact_ratio_matches.csv")

to_model_cv_dir   <- file.path(to_model_dir, "covariates")
root_cs1_central  <- 'FILEPATH'
root_cs1_to_model <- file.path(to_model_dir, "coverage_ratio_custom_stage_1")


fpath_outlier_invert_bias <- file.path(to_model_dir,"reference/outliers_dpt3_admin_survey_lit_matches.csv") 
fpath_outlier_except_bias <- file.path(ref_data_repo, "outlier_tables/outliers_admin_bias_dpt3_exceptions.csv") 

fpath_outliers_stockout_ratio_model <- file.path(ref_data_repo, "outlier_tables/outliers_stockout_ratio_model.csv")


me.db <- file.path(ref_data_repo, "me_db.csv") %>% fread
bundle.version.dir <- file.path(ref_data_repo, "gbd_covariate_bundle_versions.csv")


year.est.start <- as.integer(year_start)
year.est.end   <- as.integer(year_end)


max_bin   <- 7  
age_bins  <- fread(file.path(ref_data_repo, "vax_age_lookup.csv"))[age_bin <= max_bin][,.(age_bin, definition, gbd_ids, age_month_start, age_month_end, age_year_start, age_year_end)]  
age_bins[, `:=` (age_month_start = as.numeric(age_month_start),
                 age_month_end = as.numeric(age_month_end))]
age_bins[, month_range := Map(":", age_month_start, age_month_end)]








if(TRUE) {
  
  if(file.exists(file.path(to_model_dir,"00_MODELING_ISSUE_FLAGS.txt"))) {
    log_modeling_issue(to_model_dir, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }
  
  .files <- c("vacc.intro")
  for(.fname in .files){
    .DT <- read_file(get(.fname))
    for(.yr in year_start:year_end){
      if(!.yr %in% .DT$year) log_modeling_issue(to_model_dir, paste(.fname, "has no rows for year:", .yr))
    }
  }
  
  .new_admin_season <- Sys.Date() >= as.Date(paste0(year_end, "-07-01"))
  .yr_admin_end <- ifelse(.new_admin_season, year_end - 1, year_end - 2)
  .files <- c("vacc.intro", "vacc.epi", "vacc.official", "vacc.admin")
  for(.fname in .files){
    .DT <- read_file(get(.fname))
    for(.yr in year_start:.yr_admin_end){
      if(!.yr %in% .DT$year) log_modeling_issue(to_model_dir, paste(.fname, "has no rows for year:", .yr))
    }
  }
  
  if(.new_admin_season) {
    .DT <- fread(fpath_outlier_except_bias)
    if(any(grepl("review_later", names(.DT))) & nrow(.DT[review_later == 1, ])){
      log_modeling_issue(to_model_dir, paste(fpath_outlier_except_bias, "has records marked for review."))
    }
  }
  
  rm(.files, .fname, .DT, .yr, .new_admin_season, .yr_admin_end)
}



if(commit_vax_model_db) { 
   git_commit_repo(
      repo_root        = ref_data_repo
      , add_files      = "gbd_model/vaccination_model_db.csv"
      , commit_message = paste("automated _prep_exp update - bumping vaccination_model_db.csv history -", date)
      , dryrun         = FALSE
   )
}


path_log_tracker <- file.path(to_model_dir, "log_tracker.txt")
if(!file.exists(path_log_tracker)) file.create(path_log_tracker)

source("FILEPATH/public.R")
stgpr_quota_list <- stgpr_check_launch_quotas(
  launch_bias_correction     = launch_bias_correction
  , run.stockout             = run.stockout
  , ratio_stockouts          = ratio_stockouts
  , launch_dpt1_dpt3_dropout = launch_dpt1_dpt3_dropout
  , mes_to_launch            = mes_to_launch
  , release_id               = release_id
  , n_draws_coverage         = n_draws_coverage
  , root_quota_log           = to_model_dir
)


if (!exists("NOTES")) NOTES <- paste0(date)


metadata <- se$build_metadata_shell(code_root = code_root)
save_config(
  step            = "prep_exp",
  config_path     = config_path,
  save_root       = to_model_dir,
  copy_to_archive = TRUE,
  dir_archive     = file.path(to_model_root, "archive_configs"),
  run_date        = date,
  add_list        = list(
    on_time_cohort_only = on_time_cohort_only,
    straight_models     = straight_models,
    ratios              = ratios,
    ratios_cs1          = ratios_cs1,
    admin.vacc          = admin.vacc,
    mes_to_launch       = mes_to_launch,
    metadata            = metadata,
    init_list           = init_list
  )
)





fpath_raw_data_alt <- validate_path(path = fpath_raw_data_alt)

if(!is.null(fpath_raw_data_alt)){
  
  message("Loading raw data from alternate config path: ", fpath_raw_data_alt)
  df <- readRDS(fpath_raw_data_alt)
  
} else {
  
  message(" -> Loading in the data")
  
  
  df.survey <- get_survey_data(locs = locs)
  
  
  df.survey <- df.survey[nid %!in% c(20798)]
  
  df.survey <- df.survey[!(nid == 467681 & grepl("mcv", me_name))]
  
  
  df.official  <- readRDS(vacc.official)[, year_id := as.numeric(as.character(year_id))]
  
  
  
  
  df.survey2 <- copy(df.survey)
  df.survey2 <- df.survey2[, match_id := paste0(nid, ihme_loc_id, me_name)]
  
  df.lit <- readRDS(vacc.lit) %>%
    .[cv_admin != 1 | is.na(cv_admin), cv_lit := 1] %>%
    .[, sample_size := as.numeric(gsub(",", "", sample_size))] %>%
    .[, match_id := paste0(nid, ihme_loc_id, me_name)] %>%
    .[!(match_id %in% unique(df.survey2$match_id)), ] %>%
    .[, match_id := NULL]
  
  
  
  df <- rbind(df.survey, df.official, df.lit, fill=TRUE)
}





fsm$assert_data_source_type(df, require_at_least_one = TRUE, hard_stop = TRUE)
message("Saving checkpoint data step 1 (01_rbind_survey_lit_admin.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "01_rbind_survey_lit_admin.rds"))






df[, nid_orig := nid]




message("Data Hardcodes")



df <- hardcode_nid_203321_to_admin(
  df          = df
  , ihme_locs = c("TWN")
)

df <- hardcode_sample_size(
  df             = df
  , vacc.lit.raw = vacc.lit.raw
  , ihme_locs    = c("NGA")
)




message(" -> Integrating Decider decisions")


decider_results    <- fread(file.path(ref_data_repo, "mics_comparison_decisions_by_me_nid.csv"))
decider_comparison <- fread(file.path(ref_data_repo, "mics_for_review_with_subnationals.csv"))


df.lit.decider <- readRDS(vacc.lit) %>%
  .[, cv_lit := 1] %>%
  .[, sample_size := as.numeric(gsub(",", "", sample_size))]


tabs <- df.lit.decider[grepl("UNICEF_MICS", survey_name) | grepl("UNICEF_MICS", file_path),
                       .(nid, file_path, ihme_loc_id, age_start, age_end, sample_size, me_name,
                         data, age_length, age_year, year_id, survey_name)]
tabs[is.na(survey_name) | survey_name=="", survey_name := "UNICEF_MICS"]


decisions <- merge(decider_comparison, decider_results, by=c("me_name", "nid"), all.x=TRUE, all.y=TRUE)
decisions <- decisions[me_name=="ratio_dpt1_dpt3",  me_name := "vacc_dpt3_dpt1_ratio"] 
decisions <- decisions[me_name=="ratio_hib3_dpt3",  me_name :="vacc_hib3_dpt3_ratio"]
decisions <- decisions[me_name=="ratio_pcv3_dpt3",  me_name :="vacc_pcv3_dpt3_ratio"]
decisions <- decisions[me_name=="ratio_rotac_dpt3", me_name :="vacc_rotac_dpt3_ratio"]
decisions <- decisions[me_name=="ratio_rcv1_mcv1",  me_name :="vacc_rcv1_mcv1_ratio"]
decisions <- decisions[me_name=="ratio_hepb3_dpt3", me_name :="vacc_hepb3_dpt3_ratio"]
decisions <- decisions[me_name=="ratio_mcv2_mcv1",  me_name :="vacc_mcv2_mcv1_ratio"]
if (dpt_timeliness) {
  
  dpt3_decisions <- decisions[me_name=="vacc_dpt3"]
  dpt3_timeliness_decisions <- dpt3_decisions[, me_name := "vacc_dpt3_timeliness_ratio"]
  decisions <- rbind(decisions, dpt3_timeliness_decisions)
}
fwrite(decisions, file = paste0(to_model_dir, "/mics_decider_decisions.csv"), row.names = F)
need_decision <- decisions[abs(difference) > 0.10 & is.na(decision)]
if (nrow(need_decision) > 1) {
  fwrite(need_decision, file = paste0(to_model_dir, "/mics_decider_need_decisions.csv"))
  message(paste0("There are data with differences > 10% missing a decision, see: ", to_model_dir))
}


decisions[, id := paste(nid, me_name, sep="_")]
drop_ids <- decisions[decision == "drop", unique(id)]
df[, id := paste(nid, me_name, sep="_")]
df <- df[!(id %in% drop_ids), ]


decisions[, id := paste(nid, me_name, sep = "_")]
report_ids <- decisions[decision == "use_report_extractions", unique(id)]
df <- df[!(id %in% report_ids), ]


tabs <- tabs[, match := paste(nid, me_name, sep="_")]
tabs <- tabs[, cv_lit := 1]
tabs_to_add <- tabs[match %in% report_ids]
df <- rbind(df, tabs_to_add, fill=TRUE)
df <- df[, match := NULL]


add.df <- decisions[decision == "use_report_extractions" & !is.na(report) & is.na(tabulation)]
add.df <- add.df[, match := paste(nid, me_name, report, sep="_")]


df     <- df[survey_name=="UNICEF_MICS", match := paste(nid, me_name, data, sep="_")]
add.df <- add.df[!match %in% unique(df$match)]
df     <- df[, match := NULL]
tabs   <- tabs[, match := paste(nid, me_name, data, sep="_")]
tabs_to_add_2 <- tabs[match %in% unique(add.df$match)]
df     <- rbind(df, tabs_to_add_2, fill=TRUE)
df     <- df[, match := NULL]





message("Saving checkpoint data step 2 (02_after_decider_swaps.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "02_after_decider_swaps.rds"))





message(" -> Applying outliers")






df <- split_loc_data(all_data = df, locations = locs, fhs_run_TF = fhs_run_TF)



drop.locs <- df[!(ihme_loc_id %in% locs$ihme_loc_id), ihme_loc_id] %>% unique
if (length(drop.locs) > 0 ) {
  message(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
  df <- df[!(ihme_loc_id %in% drop.locs)]
}










if(file.exists(file.path(to_model_dir, "reference/outliers_applied.csv"))) {
  message("Removing outliers_applied.csv from reference directory.")
  file.remove(file.path(to_model_dir, "reference/outliers_applied.csv"))
}

df <- build_outlier_table(
  dtvc                = df
  , to_model_dir      = to_model_dir
  , path_outliers_ref = vacc.outliers 
  , gbd               = TRUE
  , dpt_timeliness    = dpt_timeliness
  , loc_table         = locs
  , sections_to_run   = c("outlier_sheet_decision"
                        , "survey_decision"
                        , "data_decision_dpt3")
  , remove_outlier_method_vars = FALSE 
)


df <- recode_survey_as_admin(df)
df <- clean_data(df)



df <- build_outlier_table(
  dtvc                = df
  , to_model_dir      = to_model_dir
  , path_outliers_ref = vacc.outliers 
  , gbd               = TRUE
  , dpt_timeliness    = dpt_timeliness
  , loc_table         = locs
  , sections_to_run   = c("small_sample_size")
  , remove_outlier_method_vars = FALSE 

)

df <- clean_data(df)

message("Done with outliers round 1.")




.find_dpt3_admin_outliers_paired_with_surveys(df = df, out_path = fpath_outlier_invert_bias)



message("Saving checkpoint data step 2.5 (02.5_pre_dpt1_imputation.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "02.5_pre_dpt1_imputation.rds"))




message(" -> DPT1 imputation modeling and integration")


if (launch_dpt1_dpt3_dropout) {
  
  dpt_dropout <- prep_dpt_dropout(df)

  
  dpt_dropout <- dpt_dropout[variance >= 0, ]
  
  to_model_dropout_dir <- file.path(to_model_dir, "dpt_dropout")
  dir.create(to_model_dropout_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(dpt_dropout, file.path(to_model_dropout_dir, "vacc_dpt1_dpt3_admin_dropout.csv"))
  fwrite(dpt_dropout, file.path(share_model_dir, "dpt_dropout", "vacc_dpt1_dpt3_admin_dropout.csv"))
  
  RUNS <- launch_dpt_dropout(
    ref_data_repo        = ref_data_repo,
    to_model_dropout_dir = to_model_dropout_dir,
    date                 = date,
    n_draws              = n_draws_dropout,
    NOTES                = NOTES,
    proj                 = proj,
    nparallel            = 50,
    mark_best            = 0,
    rel_id               = release_id,
    location_set_id      = location_set_id,
    year_start           = year_start,
    year_end             = year_end
  )
  
  record_stgpr_model_status(to_model_dir)


  print('All done with ST-GPR dropout model -- moving along')

} else {

  .log_stgpr_dropout <- read_stgpr_best_runs(
    file.path(data_root, "exp/to_model", alt_dropout_cycle, alt_dropout_date), 
    "dpt_dropout"
  )
  alt_dropout_run_id <- .log_stgpr_dropout$run_id
  if(length(alt_dropout_run_id) != 1) stop("Require one alt_dropout_run_id.  Currently have: ", toString(alt_dropout_run_id))
  print(paste0("Did NOT re-launch dpt dropout model and adjusting current data based on run_date ", alt_dropout_date, " estimates."))
  RUNS <- alt_dropout_run_id

}




varnames_unique_id <- c(
   'nid',
   'ihme_loc_id',
   'year_start',
   'year_end',
   'year_id',
   'survey_name',
   'survey_module',
   'file_path',
   'age_end'
)



df <- impute_dpt1_admin(
   df,
   locations          = locs,
   runs               = RUNS,
   years_to_impute    = year_start:year_end,
   varnames_unique_id = varnames_unique_id
)


df <- clean_data(df)
df <- create_id(df, varnames_unique_id)



message(" -> DTP1,2 conditional calculation")


df <- calculate_dpt_dropout(df)



message(" -> Data variance calculation")



df <- set_admin_variance(df)

df[!is.na(sample_size) & is.na(variance), variance := data * (1 - data) / sample_size]

df[(cv_lit==1) & is.na(variance), variance := data * (1 - data) / 100]

df[!is.na(data) & is.na(sample_size), sample_size := data * (1 - data) / variance]

df[me_name=="vacc_dpt12_cond" & data==0 | variance <= 0, variance := 0.001]






message("Saving checkpoint data step 3 (03_pre_bias_adj.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "03_pre_bias_adj.rds"))



















message(" -> Admin bias modeling and integration")

df <- clean_data(df)


df[cv_admin == 1, cv_admin_orig := data]





df <- apply_special_admin_disruption(
  df                              = df,
  vacc.admin                      = vacc.admin,
  countries_with_admin_disruption = c("COM", "IDN", "MLI", "MOZ", "TZA", "MUS"),
  adjust_dpt12_conditional        = TRUE
)


df <- est_time_varying_bias(df,
                            code_root                      = code_root,
                            launch                         = launch_bias_correction, 
                            straight_models                = straight_models,
                            NOTES                          = NOTES,
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
                            n_draws_bias                   = n_draws_bias,
                            nparallel                      = 50)

df <- clean_data(df)





message("Saving checkpoint data step 4 (04_post_bias_adj.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "04_post_bias_adj.rds"))













message(" -> Stockout detection")


if (run.stockout){

  print("Starting stockout detection on bias-adjusted JRF data: ")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  df_stockout <- merge(df, locs[, .(location_id, ihme_loc_id)], by = c("ihme_loc_id"), all.x = TRUE)
  df_stockout[, is_outlier := FALSE]
  df_stockout[!is.na(cv_outlier), is_outlier := TRUE]
  df_stockout$measure_id <- 18 
  df_stockout <- df_stockout[, .(ihme_loc_id, location_id, me_name, year_id, age_group_id, sex_id, cv_admin, survey_name, nid, data, sample_size, variance, is_outlier, measure_id)]

  
  who_reported_stockouts <- readRDS(who.reported.stockouts)
  who_reported_stockouts$who_stockout <- TRUE
  saveRDS(who_reported_stockouts, file = file.path(to_model_dir, "reference", "who_reported_stockouts.rds"))
  df_stockout$me <- gsub("[0-9]+", "", df_stockout$me_name)
  df_stockout    <- merge(df_stockout, who_reported_stockouts, by = c("ihme_loc_id", "year_id", "me"), all.x = TRUE)
  df_stockout[nid == 203321 & year_id %in% COVID_years, who_stockout := TRUE]
  df_stockout[who_stockout == TRUE, who_stockout_value := data]
  df_stockout[who_stockout == TRUE, data := NA]

  to_model_stockout_dir <- file.path(to_model_dir, "stockout")
  dir.create(to_model_stockout_dir, recursive = TRUE, showWarnings = FALSE)

  
  stockout_vacc_to_launch <- c()
  for (vax in admin.vacc) {
    df.sub <- df_stockout[nid==203321 & !is.na(data) & me_name==vax, ]
    
    
    if(nrow(df.sub) > 1) stockout_vacc_to_launch <- c(stockout_vacc_to_launch, vax)
    setnames(df.sub, "data", "val")
    df.sub[, me_name := paste0("bias_", me_name)]
    fwrite(df.sub, file.path(to_model_stockout_dir, paste0(vax, ".csv")), na="")
    fwrite(df.sub, file.path(share_model_dir, "stockout", paste0(vax, ".csv")), na="") 
  }
  
  
  RUNS <- launch_stockout(
     ref_data_repo           = ref_data_repo,
     to_model_dir            = to_model_dir,
     to_model_stockout_dir   = to_model_stockout_dir,
     date                    = date,
     n_draws                 = n_draws_stockout,
     stockout_vacc_to_launch = stockout_vacc_to_launch,
     stgpr_config_section    = "STOCKOUT_MODEL_LAUNCH_STRAIGHT",
     NOTES                   = NOTES,
     proj                    = proj,
     mark_best               = 0,
     nparallel               = 50,
     rel_id                  = release_id,
     location_set_id         = location_set_id,
     year_start              = year_start,
     year_end                = year_end_stockout
  )
  
  record_stgpr_model_status(to_model_dir)

  
  stockouts <- rbindlist(lapply(RUNS, function(x) get_estimates(x, entity="final") %>% data.table %>% .[, run_id := x]))
  stockouts <- setnames(stockouts, c("val", "lower", "upper"), c("gpr_mean", "gpr_lower", "gpr_upper"))
  message("Just pulled admin adjustment model results!")

  
  logs_path <- file.path(ref_data_repo, "gbd_model/stockout_run_log.csv")
  logs_file <- fread(logs_path)
  logs_file <- logs_file[, .(run_id, me_name)]
  stockouts <- merge(stockouts, logs_file, by="run_id", all.x=TRUE)
  setnames(stockouts, "gpr_mean", "pred_stgpr")
  stockouts <- merge(stockouts, locs[, .(location_id, ihme_loc_id)], by="location_id", all.x=TRUE)
  stockouts <- stockouts[, .(me_name, run_id, location_id, ihme_loc_id, year_id, age_group_id, sex_id, pred_stgpr)]

  
  stockouts <- merge(x     = stockouts[, .(location_id, ihme_loc_id, run_id, year_id, me_name, pred_stgpr, sex_id, age_group_id)],
                     y     = df_stockout[nid==203321 & (!is.na(data) | !is.na(who_stockout_value)), 
                                         .(location_id, ihme_loc_id, year_id, me_name, data, who_stockout, who_stockout_value)],
                     all.x = TRUE, 
                     by    = c("ihme_loc_id", "location_id", "year_id", "me_name"))
  
  stockouts[!is.na(who_stockout_value) & is.na(data), data := who_stockout_value]
  stockouts[, diff_stgpr_normal := data - pred_stgpr]

  stockouts[, diff := logit(pred_stgpr) - logit(data)]

  fwrite(stockouts, paste0(data_root, "FILEPATH/full_detected_stockouts_stgpr.csv")) 
  fwrite(stockouts, paste0(to_model_dir, "/full_detected_stockouts_stgpr_", date, ".csv"))

  
  stockouts <- stockouts[diff_stgpr_normal < 0, ]

  
  saveRDS(stockouts, paste0(data_root, "FILEPATH/detected_stockouts_stgpr.rds"))
  saveRDS(stockouts, paste0(to_model_dir, "/stgpr_detected_stockouts_", date, ".rds"))
  
  
  modeling_mes <- setdiff(unique(gsub("\\d", "", admin.vacc)), "vacc_yfv")
  match_stockouts_detected_reported(
     compare_ratios  = FALSE
     , modeling      = modeling_mes
     , date          = date
     , data_root     = data_root
     , ref_data_repo = ref_data_repo
     , covid_years   = COVID_years
  )


} else {

   modeling_mes <- setdiff(unique(gsub("\\d", "", admin.vacc)), "vacc_yfv")
   match_stockouts_detected_reported(
      compare_ratios  = FALSE
      , modeling      = modeling_mes
      , date          = date
      , data_root     = data_root
      , ref_data_repo = ref_data_repo
      , covid_years   = COVID_years
   )
   
}






message(" -> Making ratios of new vaccines")


df <- df[me_name == "vacc_full_sub",     me_name := "vacc_fullsub"]
df <- df[me_name == "vacc_dpt3_on_time", me_name := "vacc_dpt3time"]









ratios_using_bias_adjusted_data <- c("vacc_dpt3_dpt1_ratio")
ratios_using_bias_unadjusted_data <- setdiff(ratios, ratios_using_bias_adjusted_data)







df_ratios_admin_subnat <- rbindlist(lapply(ratios_using_bias_adjusted_data, function(x)
      make_ratios_admin_subnat(
         df = df,
         me = x,
         loc_table = locs,
         ratios_using_bias_adjusted_data = ratios_using_bias_adjusted_data
      )
))


df_ratios_bias_adjusted <- rbindlist(lapply(ratios_using_bias_adjusted_data, function(x)
      make_ratios(
         df = df,
         me = x,
         ratios_using_bias_adjusted_data = ratios_using_bias_adjusted_data
      )
))

if(use_outlier_dpt3_denom_for_ratios){
  
  
  
  
  
  
  
  df    <- .invert_cv_outlier_by_method(df, outlier_method_text = "data_decision - Outlier DPT3 > DPT1")
  
  df_io <- df[invert_outlier == 1, ]
  df_og <- df[invert_outlier == 0, ]
  
  df_io <- clean_data(df_io)
  df_io[, cv_admin_orig := data]
  
  df_io <- set_admin_variance(df_io)
  df    <- rbind(df_og, df_io)
}

df_ratios_bias_unadjusted <- rbindlist(lapply(ratios_using_bias_unadjusted_data, function(x)
  make_ratios(
    df = df,
    me = x,
    ratios_using_bias_adjusted_data = ratios_using_bias_adjusted_data
  )
))


if(use_outlier_dpt3_denom_for_ratios) df <- .reinstate_inverted_outlier(df)

df <- rbindlist(list(df,
                     df_ratios_admin_subnat,
                     df_ratios_bias_adjusted,
                     df_ratios_bias_unadjusted),
                fill = TRUE, use.names = TRUE)


df <- clean_data(df)






message("Saving checkpoint data step 4.5 (04.5_pre_ratio_stockouts.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "04.5_pre_ratio_stockouts.rds"))









if (ratio_stockouts) {
  if (run.stockout) {

    
    df_stockout <- merge(df, locs[, .(location_id, ihme_loc_id)], by = c("ihme_loc_id"), all.x = TRUE)
    df_stockout[, is_outlier := FALSE]
    df_stockout[!is.na(cv_outlier), is_outlier := TRUE]
    df_stockout$measure_id <- 18
    df_stockout <- df_stockout[, .(ihme_loc_id, location_id, me_name, year_id, age_group_id, sex_id, cv_admin, survey_name, nid, data, sample_size, variance, is_outlier, measure_id)]

    
    
    
    
    who_reported_stockouts <- readRDS(who.reported.stockouts)
    who_reported_stockouts$who_stockout <- TRUE

    who_reported_stockouts_dpt_ratios <- who_reported_stockouts[me %in% c("vacc_hepb", "vacc_hib", "vacc_pcv", "vacc_rotac"), ]
    who_reported_stockouts_mcv_ratios <- who_reported_stockouts[me %in% c("vacc_rcv", "vacc_mcv"), ]
    who_reported_stockouts_dpt_cond   <- who_reported_stockouts[me %in% c("vacc_dpt"), ]
    who_reported_stockouts_dpt13_ratio <- who_reported_stockouts[me %in% c("vacc_dpt"), ]

    who_reported_stockouts_dpt_ratios[, me := paste0(me, "_dpt_ratio")]
    who_reported_stockouts_mcv_ratios[, me := paste0(me, "_mcv_ratio")]
    who_reported_stockouts_dpt_cond[, me := paste0(me, "_cond")]
    who_reported_stockouts_dpt13_ratio[, me := paste0(me, "_dpt_ratio")]
    
    who_reported_stockouts_ratios <- rbind(who_reported_stockouts_dpt_ratios,
                                           who_reported_stockouts_mcv_ratios,
                                           who_reported_stockouts_dpt_cond,
                                           who_reported_stockouts_dpt13_ratio)

    df_stockout$me <- gsub("[0-9]+", "", df_stockout$me_name)
    df_stockout    <- merge(df_stockout, who_reported_stockouts_ratios, by = c("ihme_loc_id", "year_id", "me"), all.x = TRUE)

    
    
    df_stockout[nid == 203321 & year_id %in% COVID_years, who_stockout := TRUE]
    
    
    
    
    
    df_stockout <- outlier_from_stockout_ratio_model(
      df_stockout = df_stockout,
      fpath_outliers_stockout_ratio_model = fpath_outliers_stockout_ratio_model
    )
    
    df_stockout[who_stockout == TRUE, who_stockout_value := data]
    df_stockout[who_stockout == TRUE, data := NA]

    to_model_stockout_dir <- file.path(to_model_dir, "stockout")
    dir.create(to_model_stockout_dir, showWarnings = FALSE, recursive = TRUE)

    
    stockout_vacc_to_launch <- c()
    ratios_plus_dpt12_conditional <- c(ratios, "vacc_dpt12_cond")
    
    for (vax in ratios_plus_dpt12_conditional) {
      message("--Making df.sub for ratio stockouts: ", vax)
      df.sub <- df_stockout[nid==203321 & !is.na(data) & me_name==vax, ]
      
      
      if(nrow(df.sub) > 1) stockout_vacc_to_launch <- c(stockout_vacc_to_launch, vax)
      setnames(df.sub, "data", "val")
      df.sub[, me_name := paste0("bias_", me_name)]
      
      stopifnot(assertthat::noNA(df.sub[, .(ihme_loc_id, year_id, me, me_name, age_group_id, sex_id, measure_id, val, variance)]))
      fwrite(df.sub, file.path(to_model_stockout_dir, paste0(vax, ".csv")), na="")
      fwrite(df.sub, file.path(share_model_dir, "stockout", paste0(vax, ".csv")), na="")
    }

    message("Launching stockout models.")
    
    
    RUNS <- launch_stockout(
       ref_data_repo           = ref_data_repo, 
       to_model_dir            = to_model_dir,
       to_model_stockout_dir   = to_model_stockout_dir,
       date                    = date,
       n_draws                 = n_draws_stockout,
       stockout_vacc_to_launch = stockout_vacc_to_launch,
       stgpr_config_section    = "STOCKOUT_MODEL_LAUNCH_RATIOS",
       NOTES                   = NOTES,
       proj                    = proj,
       mark_best               = 0,
       nparallel               = 50,
       rel_id                  = release_id,
       location_set_id         = location_set_id,
       year_start              = year_start,
       year_end                = year_end_stockout
    )
    
    record_stgpr_model_status(to_model_dir)
    
    message("Reading stockout model results.")

    
    stockouts <- rbindlist(lapply(RUNS, function(x) get_estimates(x, entity="final") %>% data.table %>% .[, run_id := x]))
    stockouts <- setnames(stockouts, c("val", "lower", "upper"), c("gpr_mean", "gpr_lower", "gpr_upper"))
    message("Just pulled admin adjustment model results!")

    
    logs_path <- file.path(ref_data_repo, "gbd_model/stockout_run_log.csv")
    logs_file <- fread(logs_path)
    logs_file <- logs_file[, .(run_id, me_name)]
    stockouts <- merge(stockouts, logs_file, by="run_id", all.x=TRUE)
    setnames(stockouts, "gpr_mean", "pred_stgpr")
    stockouts <- merge(stockouts, locs[, .(location_id, ihme_loc_id)], by="location_id", all.x=TRUE)
    stockouts <- stockouts[, .(me_name, location_id, run_id, ihme_loc_id, year_id, age_group_id, sex_id, pred_stgpr)]

    
    stockouts <- merge(stockouts[, .(location_id, ihme_loc_id, run_id, year_id, me_name, pred_stgpr, sex_id, age_group_id)],
                       df_stockout[nid == 203321 & (!is.na(data) | !is.na(who_stockout_value)), .(location_id, ihme_loc_id, year_id, me_name, data, who_stockout, who_stockout_value)],
                       all.x = TRUE, by = c("ihme_loc_id", "location_id", "year_id", "me_name"))
    stockouts[!is.na(who_stockout_value) & is.na(data), data := who_stockout_value] 
    stockouts[, diff_stgpr_normal := data - pred_stgpr]
    stockouts[, diff := logit(pred_stgpr) - logit(data)]

    
    fwrite(stockouts, paste0(data_root, "FILEPATH/full_ratio_detected_stockouts_stgpr.csv"))
    fwrite(stockouts, paste0(to_model_dir, "/full_ratio_detected_stockouts_stgpr_", date, ".csv"))

    
    stockouts_pos <- stockouts[diff_stgpr_normal > 0, ]
    stockouts     <- stockouts[diff_stgpr_normal < 0, ]
    

    
    saveRDS(stockouts,     paste0(data_root, "FILEPATH/detected_ratio_stockouts_stgpr.rds"))
    saveRDS(stockouts_pos, paste0(data_root, "FILEPATH/detected_ratio_pos_stockouts_stgpr.rds"))
    saveRDS(stockouts,     paste0(to_model_dir, "/detected_ratio_stockouts_stgpr_", date, ".rds"))
    saveRDS(stockouts_pos, paste0(to_model_dir, "/detected_ratio_pos_stockouts_stgpr_", date, ".rds"))

    
    match_stockouts_detected_reported(
       compare_ratios  = TRUE
       , date          = date
       , data_root     = data_root
       , ref_data_repo = ref_data_repo
       , covid_years   = COVID_years
    )

  } else {
    
    match_stockouts_detected_reported(
       compare_ratios  = TRUE
       , date          = date
       , data_root     = data_root
       , ref_data_repo = ref_data_repo
       , covid_years   = COVID_years
    )

  }
}


if(run.stockout){
  message("Sending stockout diagnostics")
  submit_job(script_path   = file.path(code_root, "FILEPATH/submit_stockout_dx.R")
             , threads     = 1
             , mem_G       = "2G"
             , runtime_min = 20
             , archiveTF   = TRUE
             , args_list   = list(code_root      = code_root
                                  , to_model_dir = to_model_dir
                                  , date         = date
                                  , COVID_years  = vec_to_comma_string(COVID_years))
             , dry_runTF   = FALSE)
  
  
  if(submit_data_plots) submit_job(script_path   = file.path(code_root, "FILEPATH/submit_plot_vc_data_dx.R")
                                   , mem_G = "2G"
                                   , args_list   = list(
                                     code_root            = code_root
                                     , to_model_dir      = to_model_dir
                                     , data_fname        = paste0("stgpr_detected_stockouts_", date, ".rds")
                                     
                                     
                                     , nationals_only_tf = plot_nationals_only_tf))
  
  if(submit_data_plots) submit_job(script_path   = file.path(code_root, "FILEPATH/submit_plot_vc_data_dx.R")
                                   , mem_G = "2G"
                                   , args_list   = list(
                                     code_root            = code_root
                                     , to_model_dir      = to_model_dir
                                     , data_fname        = paste0("detected_ratio_stockouts_stgpr_", date, ".rds")
                                     , nationals_only_tf = plot_nationals_only_tf))
}





message("Saving checkpoint data step 4.6 (04.6_post_ratio_stockouts.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "04.6_post_ratio_stockouts.rds"))






message(" -> Setting introduction years")




df.intro <- readRDS(vacc.intro)


df.outro <- copy(df.intro)
if(fhs_run_TF) df.outro <- fhs_fix_df_intros(df.intro = df.outro, locs_fhs = locs)
saveRDS(df.outro, file.path(to_model_dir, "reference/vaccine_intro_for_outro.rds"))


delayed.epi <- readRDS(vacc.epi)[, c("age_group_id", "sex_id", "source", "notes", "notes2", "ignore") := NULL]
delayed.epi <- merge(
  x     = delayed.epi, 
  y     = locs[, .(location_id, ihme_loc_id)], 
  by    = "location_id", 
  all.x = TRUE
)



df.intro <- df.intro[!me_name %in% unique(delayed.epi$me_name)]  
df.intro <- rbind(df.intro, delayed.epi, fill = TRUE)

df.intro <- df.intro[!duplicated(df.intro)]
df.intro <- df.intro[!is.na(ihme_loc_id)]


if(fhs_run_TF) df.intro <- fhs_fix_df_intros(df.intro = df.intro, locs_fhs = locs)


saveRDS(df.intro, file = file.path(to_model_dir, "reference", "vaccine_intro.rds"))

df <- merge(
  x   = df, 
  y   = df.intro[ihme_loc_id %in% locs$ihme_loc_id], 
  by  = c("ihme_loc_id", "year_id", "me_name"), 
  all.x = TRUE
)
df <- adjust_intro_frame(df)  
df <- clean_data(df)




ckpt <- copy(df)
df <- copy(ckpt)

if(fhs_run_TF){
  message("Outliers - FHS hotfixes")
  df <- build_outlier_table(
    dtvc                = df
    , to_model_dir      = to_model_dir
    , path_outliers_ref = vacc.outliers 
    , gbd               = TRUE
    , dpt_timeliness    = dpt_timeliness
    , loc_table         = locs
    , sections_to_run   = c("fhs_hotfixes")
    , remove_outlier_method_vars = FALSE
  )
}


df <- build_outlier_table(
  dtvc                = df
  , to_model_dir      = to_model_dir
  , path_outliers_ref = vacc.outliers 
  , gbd               = TRUE
  , dpt_timeliness    = dpt_timeliness
  , loc_table         = locs
  , sections_to_run   = c("intro_year_decision")
  , remove_outlier_method_vars = TRUE 
)

message("Done with outliers round 2.")







message("Saving checkpoint data step 5 (05_ratios_and_intros_applied.rds): ", to_model_dir)
saveRDS(df, file = file.path(to_model_dir, "05_ratios_and_intros_applied.rds"))














df <- pre_intro_outliers(df)  






message(" -> Integrating detected stockout and calculating deflection ratios")


exact_match <- fread(vacc.stockouts) %>% unique

exact_match[, id := paste0(ihme_loc_id, year_id, me_name)]
df[,          id := paste0(ihme_loc_id, year_id, me_name)]
exact_match[, stockout := 1]


stockout_outliers <- fread(stockout.cov.out)
fwrite(stockout_outliers, file.path(to_model_dir, "reference", "stockout_outliers.csv"))

if (bias_only) {
  
  age.specific.stockout.outliers <- fread(age.specific.stockout.out)
  stockout_outliers <- rbind(stockout_outliers, age.specific.stockout.outliers, fill=TRUE)
}

stockout_outliers[outlier == 1, id := paste0(ihme_loc_id, year_id, me_name)]  
exact_match <- exact_match[id %!in% stockout_outliers$id]

if (!ratio_stockouts) {

  
  exact_match_cond <- exact_match[me_name=="vacc_dpt1"][, me_name := "vacc_dpt12_cond"]
  exact_match <- rbind(exact_match, exact_match_cond)

  
  exact_match_straight <- exact_match[me_name %in% c(straight_models, "vacc_dpt12_cond")]

  
  df <- merge(df, exact_match_straight, by = c("ihme_loc_id", "nid", "year_id", "me_name", "id"), all.x = TRUE)

  
  
  exact_match_ratios <- copy(exact_match)  
  exact_match_ratios[me_name=="vacc_hepb3", me_name := "vacc_hepb3_dpt3_ratio"]
  exact_match_ratios[me_name=="vacc_mcv2", me_name := "vacc_mcv2_mcv1_ratio"]
  exact_match_ratios[me_name=="vacc_pcv3", me_name := "vacc_pcv3_dpt3_ratio"]
  exact_match_ratios[me_name=="vacc_dpt1", me_name := "vacc_dpt12_cond"]
  exact_match_ratios[me_name=="vacc_rcv1", me_name := "vacc_rcv1_mcv1_ratio"]
  exact_match_ratios[me_name=="vacc_hib3", me_name := "vacc_hib3_dpt3_ratio"]
  exact_match_ratios[me_name=="vacc_rotac", me_name := "vacc_rotac_dpt3_ratio"]

  
  exact_match_ratios <- exact_match_ratios[me_name %in% ratios | me_name=="vacc_dpt12_cond"]

  
  exact_match_ratios[, id := paste0(ihme_loc_id, year_id, me_name)][, nid := NULL]

  
  df <- merge(df, exact_match_ratios, by = c("ihme_loc_id", "year_id", "me_name", "id"), all.x = TRUE)[, id := NULL]
  
  df[is.na(stockout.x) & !is.na(stockout.y), stockout.x := stockout.y]
  setnames(df, "stockout.x", "stockout")
  df[, stockout.y := NULL]
  df[is.na(cv_stockout_ratio.x) & !is.na(cv_stockout_ratio.y), cv_stockout_ratio.x := cv_stockout_ratio.y]
  setnames(df, "cv_stockout_ratio.x", "cv_stockout_ratio")
  df[, cv_stockout_ratio.y := NULL]

} else {

  
  exact_match_ratios <- fread(vacc.ratio.stockouts) %>% unique
  
  exact_match_ratios[, id := paste0(ihme_loc_id, year_id, me_name)]
  df[, id := paste0(ihme_loc_id, year_id, me_name)]
  exact_match_ratios[, stockout := 1]

  
  stockout_outliers <- fread(stockout.cov.out)
  stockout_outliers[outlier == 1, id := paste0(ihme_loc_id, year_id, me_name)]
  exact_match_ratios <- exact_match_ratios[id %!in% stockout_outliers$id]  

  
  df <- merge(
    x     = df, 
    y     = exact_match_ratios, 
    by    = c("ihme_loc_id", "nid", "year_id", "me_name", "id"), 
    all.x = TRUE
  )
  
}



df <- interpolate_stockouts(
  df                     = df,
  interpolated_ihme_locs = c("SDN"),
  to_model_dir           = to_model_dir
)


df[is.na(stockout), stockout := 0]

df[stockout == 1, cv_stockout := cv_stockout_ratio]
df[stockout == 0, cv_stockout := 0]




message(" -> Making and saving vaccination.rds")



df[!is.na(fake_data), `:=` (
  nid                    = 451398,
  underlying_survey_name = survey_name,
  survey_name            = "IHME Imputed DPT1 Administrative Coverage Data"
)]  



split   <- split_meta(df)
df      <- split$df.mod   
df.full <- split$df       
meta    <- split$meta     









df.full[, location_id := NULL]
df.full <- merge(df.full, locs[, .(ihme_loc_id, location_id)], by = "ihme_loc_id", all.x = TRUE)
assertthat::noNA(df.full[, .(location_id)])


message("Saving final data (vaccination.rds): ", to_model_dir)
saveRDS(df.full, file.path(to_model_dir, "vaccination.rds"))



if(submit_data_plots) submit_job(script_path   = file.path(code_root, "FILEPATH/submit_plot_vc_data_dx.R")
                                 , runtime_min = 30L
                                 , mem_G = "2G"
                                 , args_list   = list(
                                    to_model_dir        = to_model_dir
                                    , code_root         = code_root
                                    , data_fname        = "vaccination.rds"
                                    , nationals_only_tf = plot_nationals_only_tf))


message(" -> Preparing path_to_cvs input data files and custom covariates for modeling")




df <- df[me_name %in% mes_to_launch]


df <- merge(df, locs[, .(ihme_loc_id, location_id)], by=c('ihme_loc_id'), all.x=TRUE)  
if (nrow(df[is.na(location_id)]) > 0) stop("STOP | Unmapped locations \n Maybe there are rows of mis-tagged or non-GBD subnationals accidentally in the data df?")
df <- df[!is.na(location_id)]


df <- df[, measure_id := 18]
df[, is_outlier := ifelse(is.na(data), 1, 0)]


df <- df[!is.na(data)]





message(message("Saving checkpoint data step 6 (06_pre_custom_covs.rds): ", to_model_dir))
saveRDS(df, file = file.path(to_model_dir, "06_pre_custom_covs.rds"))





message(" -> Preparing custom covariates for modeling")





cv_covs <- CJ(location_id  = unique(locs$location_id),
              year_id      = year_start:year_end,
              age_group_id = 22,
              sex_id       = 3)





custom_covs <- df.full[nid %in% c(203321, 451398), ] 
custom_covs <- custom_covs[, c("ihme_loc_id", "year_id", "me_name", "cv_intro", "cv_intro_years", "stockout", "cv_stockout")]
custom_covs <- merge(
  x     = custom_covs, 
  y     = locs[, c("ihme_loc_id", "location_id")], 
  by    = "ihme_loc_id", 
  all.x = TRUE
)[, ihme_loc_id := NULL]

assert_data_schema(custom_covs)

custom_covs[is.na(cv_stockout) & is.na(stockout), cv_stockout := 0]
custom_covs[me_name %in% ratios_cs1 & is.na(cv_intro), cv_intro := 9999]
custom_covs[me_name %in% ratios_cs1 & is.na(cv_intro_years), cv_intro_years := ifelse((year_id - (cv_intro - 1)) >= 0, year_id - (cv_intro - 1), 0)]



war_covar <- fread(file.path(ref_data_repo, fname_cv_war))
war_covar[, location_name := NULL]

if(fhs_run_TF) {
message("\nFHS: Resolving hierarchy mismatches in the war covariate.\n")
  locs_gbd <- get_location_metadata(location_set_id = location_set_id_gbd, release_id = release_id_gbd)
  pop_gbd  <- get_population(location_set_id = location_set_id_gbd, release_id = release_id_gbd, year_id = "all", location_id = "all")
  
  war_covar  <- fhs_fix_war_covar(
    war_covar         = war_covar,
    year_end          = fhs_year_end,
    locs_fhs          = locs,
    locs_gbd          = locs_gbd,
    pop_gbd           = pop_gbd,
    location_ids_SNNP = c(60908L, 94364L, 95069L)
  )
}

message("asserting covariates contain no missingness in required columns")
assert_no_na(custom_covs, c("location_id", "year_id", "cv_stockout"))
assert_no_na(custom_covs[me_name %in% ratios_cs1], c("location_id", "year_id", "cv_stockout", "cv_intro", "cv_intro_years"))
assert_no_na(war_covar, c("location_id", "year_id", "cv_war"))

dir.create(to_model_cv_dir, showWarnings = FALSE, recursive = TRUE)

message(" ---- straight vaccines")

for (me in straight_models) {
  message(" ------ ", me)
  me_covs <- custom_covs[me_name == me]
  
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  
  me_full <- merge(me_full, war_covar, by=c('year_id', 'location_id'), all.x = TRUE)
  assert_data_schema(me_full)
  
  me_full <- me_full[is.na(cv_stockout) & is.na(stockout), cv_stockout := 0]
  
  cv_me <- me_full[, `:=` (me_name = NULL, cv_intro = NULL, cv_intro_years = NULL, stockout = NULL)]  
  
  cv_me <- cv_me[!duplicated(cv_me), ]
  cv_me <- cv_me[!duplicated(cv_me, by = c("location_id", "year_id")), ] 
  
  assertable::assert_values(data = cv_me, colnames = names(cv_me), quiet = TRUE)
  assert_square(dt             = cv_me,
                id_varnames    = c("location_id", "year_id"),
                no_na_varnames = c("location_id", "year_id", "cv_stockout", "cv_war"),
                verbose        = FALSE)
  
  fwrite(cv_me, file = file.path(share_model_dir, "to_model/covariates", paste0(me, "_cv_covariates.csv")))
  fwrite(cv_me, file = file.path(to_model_cv_dir, paste0(me, "_cv_covariates.csv")))
}

message(" ---- DPT12_conditional")

for (me in "vacc_dpt12_cond") {
  me_covs <- custom_covs[me_name == me]
  
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  
  me_full <- merge(me_full, war_covar, by=c('year_id', 'location_id'), all.x = TRUE)
  assert_data_schema(me_full)
  
  me_full <- me_full[is.na(cv_stockout) & is.na(stockout), cv_stockout := 0]
  
  cv_me <- me_full[, `:=` (me_name = NULL, cv_intro = NULL, cv_intro_years = NULL, stockout = NULL)]  
  
  cv_me <- cv_me[!duplicated(cv_me), ]
  
  assertable::assert_values(data = cv_me, colnames = names(cv_me), quiet = TRUE)
  assert_square(dt             = cv_me,
                id_varnames    = c("location_id", "year_id"),
                no_na_varnames = c("location_id", "year_id", "cv_stockout", "cv_war"),
                verbose        = FALSE)
  
  fwrite(cv_me, file = file.path(share_model_dir, "to_model/covariates", paste0(me, "_cv_covariates.csv")))
  fwrite(cv_me, file = file.path(to_model_cv_dir, paste0(me, "_cv_covariates.csv")))
}

message(" ---- DPT3_DPT1_ratio")





for (me in "vacc_dpt3_dpt1_ratio") {
  me_covs <- custom_covs[me_name == me]
  
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  
  me_full <- merge(me_full, war_covar, by=c('year_id', 'location_id'), all.x = TRUE)
  assert_data_schema(me_full)
  
  me_full <- me_full[is.na(cv_stockout) & is.na(stockout), cv_stockout := 0]
  
  cv_me <- me_full[, `:=` (me_name = NULL, cv_intro = NULL, cv_intro_years = NULL, stockout = NULL)]  
  
  cv_me <- cv_me[!duplicated(cv_me), ]
  
  assertable::assert_values(data = cv_me, colnames = names(cv_me), quiet = TRUE)
  assert_square(dt             = cv_me,
                id_varnames    = c("location_id", "year_id"),
                no_na_varnames = c("location_id", "year_id", "cv_stockout", "cv_war"),
                verbose        = FALSE)
  
  fwrite(cv_me, file = file.path(share_model_dir, "to_model/covariates", paste0(me, "_cv_covariates.csv")))
  fwrite(cv_me, file = file.path(to_model_cv_dir, paste0(me, "_cv_covariates.csv")))
}


message(" ---- vacc_dpt3_timeliness_ratio")

for (me in "vacc_dpt3_timeliness_ratio") {
  me_covs <- custom_covs[me_name == "vacc_dpt3"]
  me_covs <- me_covs[, `:=` (me_name = me, cv_stockout = 0)]  
  
  me_full <- merge(cv_covs, me_covs, by = c("location_id", "year_id"), all.x = TRUE)
  assert_data_schema(me_full)
  
  me_full <- me_full[is.na(cv_stockout) & is.na(stockout), cv_stockout := 0]
  
  cv_me <- me_full[, `:=` (me_name = NULL, cv_intro = NULL, cv_intro_years = NULL, stockout = NULL)]  
  
  cv_me <- cv_me[!duplicated(cv_me), ]
  assert_square(dt             = cv_me,
                id_varnames    = c("location_id", "year_id"),
                no_na_varnames = c("location_id", "year_id", "cv_stockout"),
                verbose        = FALSE)
  
  
  assertable::assert_values(data = cv_me, colnames = names(cv_me), quiet = TRUE)
  fwrite(cv_me, file = file.path(share_model_dir, "to_model/covariates", paste0(me, "_cv_covariates.csv")))
  fwrite(cv_me, file = file.path(to_model_cv_dir, paste0(me, "_cv_covariates.csv")))
}


message(" ---- ratio vaccines")



intros <- readRDS(vacc.intro)[, c("cv_outro", "ihme_loc_id") := NULL][me_name %in% ratios] 



custom_covs   <- df.full[me_name %in% ratios_cs1, c("ihme_loc_id", "year_id", "me_name", "stockout", "cv_stockout", "nid")]


if(fhs_run_TF){
  locs_fhs <- get_location_metadata(location_set_id = fhs_location_set_id, release_id = fhs_release_id)[level >=3]
  
  
  custom_covs <- fhs_fix_cs1_covs(custom_covs  = custom_covs
                                  , year_end   = year_end
                                  , ratios_cs1 = ratios_cs1
                                  , locs_fhs   = locs_fhs)
  
  
  intros <- fhs_fix_intros(intros = intros, locs_fhs = locs_fhs)
}

saveRDS(intros, file.path(to_model_dir, "reference/vaccine_intro_ratios.rds"))

custom_covs   <- merge(custom_covs, locs[, c("ihme_loc_id", "location_id")], by = "ihme_loc_id", all.x = TRUE)[, ihme_loc_id := NULL]
custom_covs   <- unique(custom_covs)
multiple_data <- custom_covs[
  me_name %in% ratios_cs1, 
  .N, 
  by = c("location_id", "year_id", "me_name")
][
  N >= 2
][
  , mult_id := paste0(location_id, year_id, me_name)
]
custom_covs[, mult_id := paste0(location_id, year_id, me_name)]
data_1        <- custom_covs[!mult_id %in% unique(multiple_data$mult_id)]
data_2        <- custom_covs[mult_id %in% unique(multiple_data$mult_id) & nid==203321]
custom_covs   <- rbind(data_1, data_2)
custom_covs   <- custom_covs[, c("nid", "mult_id") := NULL] %>% unique

custom_covs   <- merge(
  x     = custom_covs,
  y     = intros,
  
  by    = c("year_id", "location_id", "me_name"),
  all.x = TRUE,
  all.y = TRUE
)[year_id >= year.est.start]

custom_covs <- custom_covs[location_id %in% locs$location_id]





custom_covs[, cv_intro := zoo::na.locf(cv_intro, na.rm = FALSE), by = c("location_id", "me_name")]
custom_covs[is.na(cv_intro_years), cv_intro_years := year_id - cv_intro + 1]





custom_covs[, cv_intro_1 := ifelse(cv_intro_years == 1, 1, 0)]
custom_covs[, cv_intro_2 := ifelse(cv_intro_years == 2, 1, 0)]
custom_covs[, cv_intro_3 := ifelse(cv_intro_years == 3, 1, 0)]
custom_covs[, cv_intro_4 := ifelse(cv_intro_years == 4, 1, 0)]
custom_covs[, cv_intro_5 := ifelse(cv_intro_years == 5, 1, 0)]
custom_covs[, cv_intro_5_plus := ifelse(cv_intro_years > 5, 1, 0)]



for (me in ratios_cs1) {
  message(" ------ ", me)
  me_covs <- custom_covs[me_name == me]
  me_full <- copy(me_covs)
  
  me_full <- me_full[is.na(cv_stockout) & is.na(stockout), cv_stockout := 0]
  cv_me   <- me_full[, `:=` (me_name=NULL, cv_intro=NULL, stockout=NULL, cv_intro_years=NULL)]
  
  cv_me   <- cv_me[!duplicated(cv_me), ]
  
  cv_me[, `:=` (age_group_id=22, sex_id=3)]
  
  assertable::assert_values(data = cv_me, colnames = names(cv_me), quiet = TRUE)
  varname_intro <- grep("cv_intro_", names(cv_me), value = TRUE)
  assert_square(dt             = cv_me,
                id_varnames    = c("location_id", "year_id"),
                no_na_varnames = c("location_id", "year_id", "cv_stockout", varname_intro),
                verbose        = FALSE)
  fwrite(cv_me, file = file.path(share_model_dir, "to_model/covariates", paste0(me, "_cv_covariates.csv")))
  fwrite(cv_me, file = file.path(to_model_cv_dir, paste0(me, "_cv_covariates.csv")))
}

submit_job(script_path = file.path(code_root, "FILEPATH/submit_plot_vc_covariates_dx.R")
           , runtime_min = 20
           , mem_G = "2G"
           , args_list = list(code_root               = code_root, 
                              to_model_covariates_dir = file.path(to_model_dir, "covariates"),
                              nationals_only_tf       = FALSE))


df_for_ysi <- copy(df)

df <- df[, `:=` (cv_intro = NULL, cv_intro_years = NULL, stockout = NULL, cv_stockout = NULL)]
df[is.na(sample_size), sample_size := 0]







message(message("Saving checkpoint data step 7 (07_pre_custom_stage_1.rds): ", to_model_dir))
saveRDS(df, file = file.path(to_model_dir, "07_pre_custom_stage_1.rds"))
saveRDS(df_for_ysi, file = file.path(to_model_dir, "07.1_pre_custom_stage_1_ysi.rds"))
saveRDS(custom_covs, file = file.path(to_model_dir, "07.2_custom_covariates.rds"))





message(" -> About to launch Custom Stage 1 MR-BRT models.")

for (me_name_i in mes_to_launch) {
  message("Writing custom stage 1 inputs for ", me_name_i, " to file.")
  df.sub <- copy(df[me_name == me_name_i, ])
  
  setnames(df.sub, "data", "val")  
  df.sub[!is.na(variance), sample_size := 0]  
  fwrite(df.sub, file.path(to_model_dir, paste0(me_name_i, ".csv")), row.names = FALSE, na = "")
  fwrite(df.sub, file.path(share_model_dir, "to_model", paste0(me_name_i, ".csv")), row.names = FALSE, na = "")
}









root_custom_stage_1 <- file.path(root_cs1_central, ifelse(launch_stage_1_ratios, date, alt_stage_1_ratio_date))
dir.create(root_cs1_to_model, showWarnings = FALSE, recursive = TRUE)
dir.create(root_custom_stage_1, showWarnings = FALSE, recursive = TRUE)


if(custom_stage_1_ratios){
  if(launch_stage_1_ratios){
    message(" -> Launching custom stage 1 models for ratios: ", toString(ratios_cs1))

    jobid_list_cs1 <- list()
    for(vr in ratios_cs1){
      
      outputdir        <- file.path(root_cs1_central, date, vr) 
      to_model_cs1_dir <- file.path(root_cs1_to_model, vr) 
      dir.create(outputdir, recursive = TRUE, showWarnings = FALSE)
      dir.create(to_model_cs1_dir, showWarnings = FALSE, recursive = TRUE)
      
      jobid_list_cs1[[vr]] <- submit_job(
        script_path   = file.path(code_root, "FILEPATH/coverage_ratio_mrbrt_cascade.R")
        , threads     = 2
        , mem_G       = "50G"
        , runtime_min = 60
        , archiveTF   = TRUE
        , job_name    = paste0("mrbrt_ratio_stage_1_cascade_", vr)
        , args_list   = list(
          vaccine_ratio      = vr
          , release_id       = release_id
          , outputdir        = outputdir 
          , to_model_dir     = to_model_dir 
          , to_model_cs1_dir = to_model_cs1_dir 
          , mrbrt_env_path   = "FILEPATH"
          , fhs_run_TF       = fhs_run_TF
          , fhs_year_end     = fhs_year_end
        )
        , dry_runTF = FALSE 
      )
      
    }
      
    wait_on_slurm_job_id(unlist(jobid_list_cs1), initial_sleep_sec = 10, cycle_sleep_sec = 30)
    
    .names_to_verify <- c("_betas.csv", "_global_priors.csv", "_results.csv")
    .fpaths_cs1 <- unlist(lapply(ratios_cs1, function(vr){
        file.path(
          file.path(root_custom_stage_1, vr),
          paste0("mrbrt_custom_stage_1_", vr, .names_to_verify))
      }))
    assert_files_exist(.fpaths_cs1)
    
    mrbrt_cs1_J_dir <- file.path("FILEPATH", date)
    to_model_coverage_ratio_custom_stage_1_dir <- file.path(to_model_dir, "coverage_ratio")
    dir.create(to_model_coverage_ratio_custom_stage_1_dir, showWarnings = FALSE, recursive = TRUE)
    
    prep_mrbrt_output_coverage_ratio_custom_stage_1(
      to_model_dir      = to_model_dir
      , share_model_dir = share_model_dir
      , mes             = ratios_cs1
      , mrbrt_cs1_J_dir = mrbrt_cs1_J_dir
      , to_model_coverage_ratio_custom_stage_1_dir = to_model_coverage_ratio_custom_stage_1_dir
    )
    
    message("Building square grid of vaccine ratios_cs1")
    
    for(vr in ratios_cs1){
      
      message("  ", vr)
      stage_1_ratio <- fread(file.path(to_model_coverage_ratio_custom_stage_1_dir, paste0(vr, "_mrbrt_coverage_ratio_custom_stage_1.csv")))

      stage_1_ratio <- merge(stage_1_ratio, intros[me_name == vr, list(location_id, cv_intro_years, year_id, cv_intro)], by = c('location_id', 'year_id'))
      stage_1_ratio[, year_id_orig := year_id]
      stage_1_ratio[, cv_intro := pmax(cv_intro, year_start)]
      stage_1_ratio[, cv_intro_years := year_id - cv_intro]

      stage_1_ratio <- stage_1_ratio[cv_intro_years >= 0]
      stage_1_ratio[, year_id := year_start + cv_intro_years]
      
      
      stage_1_ratio[cv_custom_stage_1 > 0.9999999, cv_custom_stage_1 := 0.9999999]

      tofill <- data.table(expand.grid(location_id = unique(custom_covs$location_id), year_id = year_start:year_end, me_name = vr, age_group_id = 22, sex_id = 3))

      stage_1_ratio <- merge(stage_1_ratio, tofill, by = c('year_id', 'location_id', 'me_name', 'age_group_id', 'sex_id'), all = TRUE)

      
      stage_1_ratio     <- rbindlist(lapply(unique(stage_1_ratio$location_id), function(loc_id) {
        stage_1_ratio_l <- stage_1_ratio[location_id == loc_id]
        last_val_year   <- max(stage_1_ratio_l[!is.na(cv_custom_stage_1)]$year_id)
        fake_value      <- stage_1_ratio_l[year_id == last_val_year]$cv_custom_stage_1
        if(is.null(last_val_year) | is.na(last_val_year) | is.infinite(last_val_year)){
          stage_1_ratio_l[is.na(cv_custom_stage_1), cv_custom_stage_1 := 0.001]  
        } else {
          stage_1_ratio_l[is.na(cv_custom_stage_1) & year_id  >= last_val_year, cv_custom_stage_1 := fake_value]
        }
        return(stage_1_ratio_l)
      }))


      stage_1_ratio <- stage_1_ratio[, `:=` (cv_intro = NULL, cv_intro_years = NULL, year_id_orig = NULL)]

      fwrite(stage_1_ratio, file.path(share_model_dir, "mrbrt_coverage_ratio_custom_stage_1", paste0(vr, "_mrbrt_coverage_ratio_custom_stage_1.csv")))
      fwrite(stage_1_ratio, file.path(root_cs1_to_model, paste0(vr, "_mrbrt_coverage_ratio_custom_stage_1.csv")))
    }

  } else {

    stop('this option doesnt work')

  }
}





message("Shifting ratio vaccines to YSI framework.")



df <- copy(df_for_ysi)
df[, year_id_orig := year_id]
df[me_name %in% ratios_cs1, cv_intro := pmax(cv_intro, year_start)]
df[me_name %in% ratios_cs1, cv_intro_years := year_id - cv_intro]

df <- df[cv_intro_years >= 0]
df[me_name %in% ratios_cs1, year_id := year_start + cv_intro_years]


df <- df[, `:=` (cv_intro = NULL, cv_intro_years = NULL, stockout = NULL, cv_stockout = NULL)]
df[is.na(sample_size),sample_size:=0]

message(" -> Save me-specific data for models, again! with ysi framework!")


for (ratio_name in ratios_cs1) {
   message(ratio_name)
   df.sub <- df[me_name == ratio_name, ] %>% copy
   setnames(df.sub, "data", "val")  
   df.sub[!is.na(variance), sample_size := 0]  
   fwrite(df.sub, file.path(to_model_dir, paste0(ratio_name, ".csv")), row.names=FALSE, na="")
   fwrite(df.sub, file.path(share_model_dir, "to_model", paste0(ratio_name, ".csv")), row.names = FALSE, na = "")
}







if (just_dpt_cond) mes_to_launch <- "vacc_dpt12_cond"
if (straight_only) mes_to_launch <- c("vacc_dpt1", "vacc_dpt3", "vacc_mcv1", "vacc_bcg", "vacc_polio3")
if (just_dpt3) mes_to_launch <- "vacc_dpt3"
if (dpt_timeliness_only) mes_to_launch <- "vacc_dpt3_timeliness_ratio"
if (polio3_only) mes_to_launch <- "vacc_polio3"
if (ratios_only) mes_to_launch <- ratios
if (just_mcv1) mes_to_launch <- "vacc_mcv1"
if (just_bcg) mes_to_launch <- "vacc_bcg"
if (just_rotac_ratio) mes_to_launch <- "vacc_rotac_dpt3_ratio"
if (just_hib3_ratio) mes_to_launch <- "vacc_hib3_dpt3_ratio"
if (just_hepb3_ratio) mes_to_launch <- "vacc_hepb3_dpt3_ratio"
if (bias_only) mes_to_launch <- NULL
if (mcv_rcv_ratios_only) mes_to_launch <- c("vacc_mcv1", "vacc_mcv2_mcv1_ratio", "vacc_rcv1_mcv1_ratio")
if (just_dpt1_dev) mes_to_launch  <- c("vacc_dpt1", "vacc_dpt3", "vacc_dpt3_dpt1_ratio", "vacc_dpt12_cond")

if (is.null(mes_to_launch)) stop("All done - You specified to only run through prep code to produce new bias results - no ST-GPR models launched.")

if (run_via_bundle) {
  
  source(file.path(code_root, "FILEPATH/upload_model_xw.r"))
  df[, c("cv_id", "cv_stockout_ratio") := NULL]
  df[!is.na(variance), sample_size := 0]  

  
  bundle_uploads <- setdiff(mes_to_launch, c("vacc_bcg", "vacc_polio3"))
  if (length(bundle_uploads) > 0) {
    for (i in bundle_uploads) {
      print(paste0("Preparing to upload ", i, " bundle"))
      upload_model_xw(data=df, me=i, data_date=date, bundle_map=me.db, path_to_version_dir=bundle.version.dir,
                      release_id=release_id, note=NOTES, bias=FALSE)
    }
  }
}







message("Launching coverage models for \n  ", paste(mes_to_launch, collapse = "\n  "))


RUNS <- launch_coverage(
   ref_data_repo       = ref_data_repo,
   to_model_dir        = to_model_dir,
   to_model_cv_dir     = to_model_cv_dir,
   root_cs1            = root_cs1_to_model,
   date                = date,
   n_draws             = n_draws_coverage,
   run_via_bundle      = run_via_bundle,
   bundle.version.dir  = bundle.version.dir, 
   mes_to_launch       = mes_to_launch,
   mes_straight        = c(straight_models, "vacc_dpt12_cond", "vacc_dpt3_dpt1_ratio"),
   mes_ratio           = c(ratios_cs1),
   NOTES               = NOTES,
   proj                = proj,
   mark_best           = 0,
   nparallel           = 50,
   rel_id              = release_id,
   fhs_run_TF          = fhs_run_TF,
   location_set_id     = location_set_id,
   year_start          = year_start,
   year_end            = year_end
)


if (FALSE){
  log <- read_vc_logs(log_type = "stgpr_runs")
  RUNS <- log$run_id
  source("FILEPATH/public.R")
}
if(length(RUNS)) record_stgpr_model_status(root = to_model_dir, stgpr = stgpr)

if(commit_run_logs){
   git_commit_repo(
      repo_root        = ref_data_repo
      , add_files      = c(
         "gbd_model/bias_run_log.csv"
         , "gbd_model/dropout_run_log.csv"
         , "gbd_model/stockout_run_log.csv"
         , "gbd_model/vaccination_run_log.csv"
      )
      , commit_message = paste("automated prep_exp update - bumping stgpr xxxx_run_log.csv histories -", date)
      , dryrun         = FALSE
   )
}

assert_files_exist(
  fpaths_expected = file.path("FILEPATH", RUNS, "model_complete.csv")
  , report_path   = file.path(to_model_dir, "log_stgpr_coverage_model_report.yaml")
)

message("Done.")
