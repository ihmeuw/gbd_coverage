









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





library(data.table)
library(magrittr)


username <- Sys.info()[["user"]]
'%!in%'  <- function(x,y)!('%in%'(x,y))
source(paste0("FILEPATH", username, "FILEPATH/init.r"))

R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), verbose = T)

load_pipeline_config(step = "compare_data_versions", config_path = config_path)

exact_mes <-
  c(
    "vacc_bcg",
    "vacc_dpt1",
    "vacc_dpt3",
    "vacc_hepb3",
    "vacc_hib3",
    "vacc_mcv1",
    "vacc_mcv2",
    "vacc_pcv3",
    "vacc_polio3",
    "vacc_rcv1",
    "vacc_rotac"
  )
mes_to_compare <-
  c(
    exact_mes,
    "vacc_dpt12_cond",
    "vacc_dpt3_timeliness_ratio"
  )


to_model_dir_data <- outdir 

file_stage_stub <- paste(unlist(purrr::map(strsplit(c(file_new_, file_old_), "_"), 1)), collapse = "_vs_")
file_stage_stub <- gsub("\\.rds", "", file_stage_stub)

to_model_dir_data <- file.path(to_model_dir_data, paste0(file_stage_stub, "_", date_old_))
dir.create(path = to_model_dir_data, showWarnings = FALSE, recursive = TRUE)


cat(paste0(date_new_,"/",file_new_," vs ",date_old_,"/",file_old_," data comparison"),
    file = file.path(to_model_dir_data, "data_comparison_dates_and_files.txt"))






assign_val_to_na <- function(x, val, hard_stop = FALSE){
  
  type_x   <- typeof(x)
  type_val <- typeof(val)
  
  msg <- paste0("x and val must share a type:
    x: ", type_x,"
  val: ", type_val)
  
  if(type_x != type_val) {
    if(hard_stop) {
      stop (msg)
    } else {
      warning (msg)
    }
  }
  
  x[is.na(x)] <- val
  return(x)
}


remove_non_dropped <- function(me, dt, dt_new) {
  
  
  
  dt_me     <- dt[me_name==me]
  dt_new_me <- dt_new[me_name==me]
  dt_me[nid %in% unique(dt_new_me$nid), remove := 1L]
  dt_me[, remove := assign_val_to_na(remove, 0L)]
  return(dt_me)
  
}

pct_diff <- function(new_value, old_value, round_digits){
  return(round(((new_value - old_value) / old_value ) * 100, round_digits))
}

prop_diff <- function(new_value, old_value){
  return((new_value - old_value) / old_value )
}


validate_data_comparison_inputs <- function(date_new  = date_new_,
                                            cycle_new = cycle_new_,
                                            date_old  = date_old_,
                                            cycle_old = cycle_old_,
                                            file_new  = file_new_,
                                            file_old  = file_old_,
                                            cols      = cols_to_compare) {
  
  valid_files <-
    c(
      "vaccination.rds",
      "01_rbind_survey_lit_admin.rds",
      "02_after_decider_swaps.rds",
      "02.5_pre_dpt1_imputation.rds",
      "03_pre_bias_adj.rds",
      "04_post_bias_adj.rds",
      "05_ratios_and_intros_applied.rds"
    )
  
  if (file_new %!in% valid_files) {
    stop(paste0("invalid file_new_: ", file_new, " , must be one of "), paste0(valid_files, collapse = ", "))
  }
  
  if (file_old %!in% valid_files) {
    stop(paste0("invalid file_old_: ", file_old, " , must be one of "), paste0(valid_files, collapse = ", "))
  }
  
  new_path <- file.path(data_root, "exp/to_model", cycle_new, date_new)
  old_path <- file.path(data_root, "exp/to_model", cycle_old, date_old)
  
  if(!dir.exists(new_path)) {
    stop("Invalid cycle_old_ and date_old_: ", new_path, " does not exist")
  }
  
  if(!dir.exists(old_path)) {
    stop("Invalid cycle_old_ and date_old_: ", old_path, " does not exist")
  }
  
  
  
  
  
  fnames_cv_drop <-
    c(
      "01_rbind_survey_lit_admin.rds",
      "02_after_decider_swaps.rds",
      "02.5_pre_dpt1_imputation.rds"
    )
  drop_cv <- ifelse(file_new_ %in% fnames_cv_drop | file_old_ %in% fnames_cv_drop,
                    TRUE,
                    FALSE) 
  
  return(drop_cv)
}


create_data_comparisons <- function(
    date_new  = date_new_,
    cycle_new = cycle_new_,
    date_old  = date_old_,
    cycle_old = cycle_old_,
    file_new  = file_new_,
    file_old  = file_old_,
    mes       = mes_to_compare,
    drop_cv   = drop_cv_,
    round_digits 
) {
  
  if(!file_new == file_old) warning("Attempting non-identical file comparison - results may be buggy.\n",
                                    file_new, "\n",
                                    file_old)
  
  
  message("Reading in and subsetting the new and old data versions")
  data_new <- readRDS(file.path(data_root, "exp/to_model", cycle_new, date_new, file_new))
  data_old <- readRDS(file.path(data_root, "exp/to_model", cycle_old, date_old, file_old))
  
  
  if("fake_data" %in% names(data_new) & "fake_data" %in% names(data_old)) {
    data_new <- data_new[is.na(fake_data) & (!is.na(data) | !is.na(cv_outlier)), ]
    data_old <- data_old[is.na(fake_data) & (!is.na(data) | !is.na(cv_outlier)), ]
  } 
  
  
  varnames_to_merge <- c(
    "nid",
    "ihme_loc_id",
    "me_name",
    "year_start",
    "year_end",
    "year_id",
    "age_year",
    "survey_name",
    "survey_module",
    "cv_admin",
    "file_path",
    "age_end"
  )
  
  varnames_to_set  <- c(
    "data",
    "cv_outlier",
    "sample_size",
    "variance",
    "cv_survey",
    "cv_lit"
  )
  
  varnames_avail <- unique(c(names(data_new), names(data_old)))
  varnames_to_merge <- varnames_to_merge[varnames_to_merge %in% varnames_avail]
  varnames_to_set <- varnames_to_set[varnames_to_set %in% varnames_avail]
  if(drop_cv) {
    
    varnames_to_set <- setdiff(varnames_to_set, c("cv_id", "cv_outlier"))
  }
  
  
  
  data_new[, new := 1L]
  data_old[, old := 1L]
  cols_new <- c(varnames_to_merge, varnames_to_set, "new")
  cols_old <- c(varnames_to_merge, varnames_to_set, "old")
  
  data_new <- data_new[, ..cols_new][me_name %in% mes]
  data_old <- data_old[, ..cols_old][me_name %in% mes]
  varnames_set_new <- paste0(varnames_to_set, "_new")
  varnames_set_old <- paste0(varnames_to_set, "_old")
  setnames(data_new, varnames_to_set, varnames_set_new, skip_absent = TRUE)
  setnames(data_old, varnames_to_set, varnames_set_old, skip_absent = TRUE)
  merge_cols <- varnames_to_merge
  
  
  message("Merging data versions together")
  data_cp <- merge(data_new, data_old, by = merge_cols, all.x = T, all.y = T)
  data_cp[, new := assign_val_to_na(new, 0L)]
  data_cp[, old := assign_val_to_na(old, 0L)]
  
  
  message("Making the comparisons")
  if (drop_cv) {
    data_cp <- data_cp[!(is.na(data_new) & is.na(data_old))]
  } else {
    data_cp <- data_cp[!(is.na(data_new) & is.na(data_old) & is.na(cv_outlier_new) & is.na(cv_outlier_old))]
  }
  
  
  data_cp_all <- copy(data_cp)
  
  
  data_cp_same <- data_cp[new == 1L & old == 1L]
  
  
  data_new_only <- data_cp[new == 1L & old == 0L]  
  data_old_only <- data_cp[new == 0L & old == 1L]
  
  
  data_old_only <- lapply(mes[mes != "vacc_dpt3_timeliness_ratio"], function(x) {
    remove_non_dropped(me = x, dt = data_old_only, dt_new = data_new)
  }) %>% rbindlist()
  
  data_old_only <- data_old_only[remove == 0L]
  
  data_cp_same[, data_pct_diff := pct_diff(data_new, data_old, round_digits)]
  data_cp_same[, sample_size_pct_diff := pct_diff(sample_size_new, sample_size_old, round_digits)]
  data_cp_same[, variance_pct_diff := pct_diff(variance_new, variance_old, round_digits)]
  if (!drop_cv) {
    data_cp_same[, cv_outlier_pct_diff := pct_diff(cv_outlier_new, cv_outlier_old, round_digits)]
  }
  data_cp_same[, changed_type := cv_lit_new != cv_lit_old]
  data_cp_same[, changed_type := assign_val_to_na(changed_type, FALSE)]
  
  colorder <- c(
    varnames_to_merge,
    
    "cv_outlier_new",
    "cv_outlier_old",
    "cv_outlier_pct_diff",
    "data_new",
    "data_old",
    "data_pct_diff",
    "sample_size_new",
    "sample_size_old",
    "sample_size_pct_diff",
    "variance_new",
    "variance_old",
    "variance_pct_diff",
    "cv_survey_new",
    "cv_survey_old",
    "cv_lit_new",
    "cv_lit_old",
    "changed_type",
    "new",
    "old"
  )
  
  drop_cv_colnames <- c(
    "cv_id",
    "cv_outlier_new",
    "cv_outlier_old",
    "cv_outlier_pct_diff"
  )
  
  if(drop_cv) colorder <- colorder[colorder %!in% drop_cv_colnames]
  
  setcolorder(data_cp_same, colorder)
  colorder_new <- intersect(colorder, names(data_new_only))
  colorder_old <- intersect(colorder, names(data_old_only))
  setcolorder(data_new_only, colorder_new)
  setcolorder(data_old_only, colorder_old)
  
  
  message("Summarizing NIDs gained/lost")
  nid_list <- list(
    new       = data_new_only[, .(nid = unique(nid), N_per_loc = uniqueN(nid)), by = ihme_loc_id]
    , dropped = data_old_only[, .(nid = unique(nid), N_per_loc = uniqueN(nid)), by = ihme_loc_id]
  )
  
  lapply(nid_list, setorderv, cols = c("ihme_loc_id"), order = 1)
  lapply(nid_list, setorderv, cols = c("N_per_loc"), order = -1)
  nid_fnames <- c("nids_new", "nids_dropped")
  names(nid_list) <- nid_fnames
  
  message("Done")
  
  
  ret_list <- list(
    data_cp_same                 = data_cp_same
    , data_new_only              = data_new_only
    , data_old_only              = data_old_only
    , nids_new                   = nid_list$nids_new
    , nids_dropped               = nid_list$nids_dropped
    
    , dt_diff_data             = data_cp_same[abs(data_pct_diff) > 0, ][, `:=`(perc0 = ifelse(abs(data_pct_diff) > 0, TRUE, FALSE),
                                                                               perc1 = ifelse(abs(data_pct_diff) > 1, TRUE, FALSE),
                                                                               perc5 = ifelse(abs(data_pct_diff) > 5, TRUE, FALSE))]
    , dt_diff_sample_size = data_cp_same[abs(sample_size_pct_diff) > 0, ][, `:=`(perc0 = ifelse(abs(sample_size_pct_diff) > 0, TRUE, FALSE),
                                                                                 perc1 = ifelse(abs(sample_size_pct_diff) > 1, TRUE, FALSE),
                                                                                 perc5 = ifelse(abs(sample_size_pct_diff) > 5, TRUE, FALSE))]
    , dt_diff_variance = data_cp_same[abs(variance_pct_diff) > 0, ][, `:=`(perc0 = ifelse(abs(variance_pct_diff) > 0, TRUE, FALSE),
                                                                           perc1 = ifelse(abs(variance_pct_diff) > 1, TRUE, FALSE),
                                                                           perc5 = ifelse(abs(variance_pct_diff) > 5, TRUE, FALSE))]
    , merge_cols = merge_cols
  )
  
  if(!drop_cv) {
    ret_list$dt_diff_outlier <- data_cp_same[abs(cv_outlier_pct_diff) > 0, ][, `:=`(perc0 = ifelse(abs(cv_outlier_pct_diff) > 0, TRUE, FALSE),
                                                                                    perc1 = ifelse(abs(cv_outlier_pct_diff) > 1, TRUE, FALSE),
                                                                                    perc5 = ifelse(abs(cv_outlier_pct_diff) > 5, TRUE, FALSE))]
  }
  return(ret_list)
}










drop_cv_  <- validate_data_comparison_inputs()
round_digits <- 6L 
data_list <- create_data_comparisons(round_digits = round_digits)



saveRDS(data_list[["data_cp_same"]], file.path(to_model_dir_data, "1_same_new_and_old_data.rds"))
saveRDS(data_list[["data_new_only"]], file.path(to_model_dir_data, "2_new_data_or_cohorts.rds"))
saveRDS(data_list[["data_old_only"]], file.path(to_model_dir_data, "3_old_or_dropped_data.rds"))

for(item in names(data_list)){
  if (item %in% c("data_cp_same", "data_new_only", "data_old_only", "merge_cols")) next
  fwrite(data_list[[item]], file.path(to_model_dir_data, paste0(item, ".csv")))
}




new_data            <- nrow(data_list[["data_new_only"]])
dropped_data        <- nrow(data_list[["data_old_only"]])
changed_data        <- nrow(data_list[["data_cp_same"]][abs(data_pct_diff) > 0])
changed_sample_size <- nrow(data_list[["data_cp_same"]][abs(sample_size_pct_diff) > 0])
changed_variance    <- nrow(data_list[["data_cp_same"]][abs(variance_pct_diff) > 0])
changed_cv_outlier  <- if (drop_cv_) 0 else nrow(data_list[["data_cp_same"]][abs(cv_outlier_pct_diff) > 0])
changed_type        <- nrow(data_list[["data_cp_same"]][abs(changed_type) > 0])


merge_col_msg <- paste(data_list$merge_cols, collapse = ", ")
message_list <- list(
  a  = paste0("Comparing ", date_new_, "/", file_new_, " from ", cycle_new_, " with ", date_old_, "/", file_old_, " from ", cycle_old_),
  b  = paste0("Merged by: ", merge_col_msg),
  b2 = paste0("Data comparison path: ", to_model_dir_data),
  c  = paste0("There were ", new_data, " row(s) of data added in the new version, see 2_new_data_or_cohorts.csv"),
  d  = paste0("There were ", dropped_data, " row(s) of data dropped in the new version, see 3_old_or_dropped_data.csv"),
  e  = paste0("Of the rows present in both versions (percent differences rounded to ", round_digits, " decimal places): "),
  f  = paste0("    ", changed_data, " row(s) had different values in the data column"),
  g  = paste0("    ", changed_sample_size, " row(s) had different values in the sample_size column"),
  g2 = paste0("    ", changed_variance, " row(s) had different values in the variance column"),
  h  = paste0("    ", changed_cv_outlier, " row(s) had different values in the cv_outlier column"),
  i  = paste0("    ", changed_type, " row(s) had different values in the cv_lit column (changed from report to microdata or vice versa)"),
  j  = paste0("See diff columns in 1_same_new_and_old_data.csv for more info"),
  newline = paste0("----------------------------------------------------------\n")
)

report_path <- file.path(to_model_dir_data, "reporting.txt")
report_file <- file(report_path, open = "a+b")
sink(file = report_file, type = "message")
invisible(sapply(message_list, message))
sink(type = "message")
close(report_file)
Sys.chmod(report_path, mode = "0777")

invisible(sapply(message_list, message))
message("Done.")



