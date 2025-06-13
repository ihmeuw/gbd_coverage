






































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

start_time <- proc.time()




r_lib_team_dir <- "FILEPATH"
library(data.table)


source(file.path(code_root, "init.r"))
'%!in%'  <- function(x,y)!('%in%'(x,y))

R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), verbose = FALSE)

load_pipeline_config(step = "compare_gbd_lbd_inputs", config_path = config_path)

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
metadata <- se$build_metadata_shell(code_root = code_root)
save_config(
  step        = "compare_gbd_lbd_inputs",
  config_path = config_path,
  save_root   = outdir,
  add_list    = list(metadata = metadata)
)

message("\n    Saving results to: ", outdir, "\n")

lbd_root <- ifelse(
  read_lbd_from_archive
  , file.path(lbd_root, "archive", lbd_archive_date)
  , lbd_root
)



to_model_dir <- file.path(to_model_root, gbd_date)
message("\nReading vaccination.rds from: ",  to_model_dir)
gbd <- readRDS(file.path(to_model_dir, "vaccination.rds"))

message("\nReading LBD data for: ")
all_mes <- c(resampled, not_resampled) 
names(all_mes) <- all_mes

lbd_data_list <- lapply(all_mes, function(me){
  message("-- ", me)
  res <- ifelse(me %in% resampled, "resampled", "not_resampled")
  lbd_filepath <- file.path(lbd_root, res, paste0(me, "_cov.csv"))
  if(!file.exists(lbd_filepath)) {
    message("lbd file for ", me, ": ", lbd_filepath, " not found, skipping")
  }
  fread(lbd_filepath)
})



lbd_countries <- c()
diff_holder <- list()
year_holder <- list()

message("\nComparing LBD vs. GBD data")

for(me in all_mes) {
  
  message("-- ", me)
  

  if (me == "bcg1") {
    gbd_me <- "vacc_bcg"
  } else {
    gbd_me <- paste0("vacc_", me)
  }

  if(me %in% names(lbd_data_list)) lbd <- copy(lbd_data_list[[me]])
  
  if (me == "dpt12_cond") {
    setnames(lbd, "dpt12_cond", "dpt12_cond_cov")
  }

  lbd <- lbd[(age_bin > 3 | (!is.na(age_bin_agg) & age_bin_agg != "1:3")) & year >= 2000]
  lbd_countries <- unique(c(lbd_countries, lbd$country))
  lbd_sub <- lbd[, .(ss_lbd = sum(N*weight, na.rm = TRUE),
                     data_lbd = weighted.mean((get(paste0(me, "_cov")) / N), N*weight)),
                 by = c("svy_id", "country", "year")]

  gbd_sub <- gbd[me_name == gbd_me & year_id >= 2000]
  setnames(gbd_sub, c("data", "sample_size"), c("data_gbd", "ss_gbd"))

  merge_df <- merge(gbd_sub, lbd_sub, by.x = c("nid", "ihme_loc_id", "year_id"), by.y = c("svy_id", "country", "year"))

  merge_df <- merge_df[, .(nid, ihme_loc_id, year_id, ss_gbd, ss_lbd, data_gbd, data_lbd)]

  merge_df[, ss_diff := ss_lbd - ss_gbd]
  merge_df[, data_diff := data_lbd - data_gbd]
  merge_df[, me_name := me]

  diff_holder[[me]] <- merge_df

  
  gbd_year_sub <- gbd_sub[!is.na(data_gbd)]
  gbd_year_sub[cv_survey == 0, type := "admin"]
  gbd_year_sub[cv_survey == 1 & cv_lit == 1, type := "report"]
  gbd_year_sub[cv_survey == 1 & cv_lit == 0, type := "microdata"]
  gbd_year_sub <- gbd_year_sub[, .(nid, ihme_loc_id, year_id, type)]
  gbd_year_sub[, gbd := 1]
  gbd_year_sub[, country := substr(ihme_loc_id, 1, 3)]
  gbd_year_sub <- gbd_year_sub[country %in% lbd_countries]
  gbd_year_sub <- unique(gbd_year_sub[, .(nid, country, year_id, type, gbd)])

  lbd_year_sub <- unique(lbd[, .(svy_id, country, year, type)])
  lbd_year_sub[, lbd := 1]

  gbd_nids <- unique(gbd_year_sub$nid)
  lbd_nids <- unique(lbd_year_sub$svy_id)
  nids_to_keep <- intersect(gbd_nids, lbd_nids)

  year_merge <- merge(gbd_year_sub, lbd_year_sub, all = TRUE, by.x = c("nid", "country", "year_id"), by.y = c("svy_id", "country", "year"))
  year_merge <- year_merge[nid %in% nids_to_keep]
  year_merge[, me_name := me]

  setnames(year_merge, c("type.x", "type.y"), c("gbd_type", "lbd_type"))
  year_merge <- year_merge[(gbd == 1 & is.na(lbd)) | (is.na(gbd) & lbd == 1),]

  year_holder[[me]] <- year_merge
}

message("\n")
message("Writing comparisons to disk")

comparison_df <- rbindlist(diff_holder)
fwrite(comparison_df, paste0(outdir, "/lbd_gbd_comparison.csv"))

year_df <- rbindlist(year_holder)
year_df <- year_df[is.na(gbd) | is.na(lbd)]
fwrite(year_df, paste0(outdir, "/lbd_gbd_year_comparison.csv"))



message("\nProcessing NIDs in GBD, but not in LBD")

raw_lit <- fread(raw_lit_path)
outlier <- fread(outlier_path)

problem_rows <- list()

for(me in all_mes) {
  
  message("-- ", me)
  
  if (me == "bcg1") {
    gbd_me <- "vacc_bcg"
  } else {
    gbd_me <- paste0("vacc_", me)
  }

  if (me == "dpt12_cond") lbd_me <- me else lbd_me <- paste0(me,'_cov')

  
  gbd_sub <- gbd[me_name == gbd_me & is.na(cv_outlier)]
  gbd_sub[, country := substr(ihme_loc_id, 1, 3)]
  gbd_sub <- gbd_sub[country %in% lbd_countries]

  if(me %in% names(lbd_data_list)) lbd_sub <- copy(lbd_data_list[[me]])

  lbd_sub <- lbd_sub[(age_bin > 3 | (!is.na(age_bin_agg) & age_bin_agg != "1:3")) & (!is.na(get(lbd_me)))]

  
  
  gbd_nids <- unique(gbd_sub[cv_admin == 0 & year_id >= 2000,]$nid)
  missing_nids <-  gbd_nids[gbd_nids %!in% unique(lbd_sub$svy_id)]

  for (n in missing_nids) {
    
    gbd_subnats_present <- FALSE
    
    lbd_subnats_present <- FALSE
    
    lbd_nid_missing <- FALSE
    
    lbd_file_processing_error <- FALSE
    
    lbd_tab_no_rows <- FALSE
    
    unknown_lbd_error <- FALSE

    sub <- gbd_sub[nid == n]
    sub_report <- sub[cv_lit == 1]
    sub_survey <- sub[cv_lit == 0]

    
    
    if (nrow(sub_report) > 0) {
      
      raw_lit_sub <- raw_lit[nid == n & !is.na(get(gbd_me))]

      
      
      if (any(grepl("[A-Z]+_\\d+", raw_lit_sub$location_name_short_ihme_loc_id, perl=TRUE))) {
        gbd_subnats_present <- TRUE
      }

      if (any(!is.na(raw_lit_sub$location_code))) {
        lbd_subnats_present <- TRUE
      }

      
      if(!gbd_subnats_present & !lbd_subnats_present & nrow(sub_survey) == 0) {
        next
      }
    }

    
    
    
    if (nrow(sub_survey) > 0) {
      outlier_sub <- outlier[nid == n & me_name == gbd_me]
      outlier_all <- outlier[nid == n & me_name == ""]

      
      if (nrow(outlier_all) > 0) {
        next
      }

      lbd_tab_path <- paste0(lbd_tab_root, n, ".csv")
      if (nrow(outlier_sub) > 0) {
        if (any(outlier_sub$lbd == 0)) {
          

          lbd_nid_missing <- TRUE
          if(!file.exists(lbd_tab_path)) {
            lbd_file_processing_error <- TRUE
          }
        }
      }
      if(file.exists(lbd_tab_path)) {
        lbd_tab_file <- fread(lbd_tab_path)
        if (nrow(lbd_tab_file) == 0) {
          lbd_tab_no_rows <- TRUE
        }
      }
    }

    if (!any(gbd_subnats_present, lbd_subnats_present, lbd_nid_missing, lbd_file_processing_error, lbd_tab_no_rows)) {
      unknown_lbd_error <- TRUE
    }

    svy <- paste0(n, "_", me)
    country <- unique(sub$country)
    problem_rows[[svy]] <-  data.table(
      nid                       = n,
      me_name                   = me,
      country                   = country,
      gbd_subnats_present       = gbd_subnats_present,
      lbd_subnats_present       = lbd_subnats_present,
      lbd_nid_missing           = lbd_nid_missing,
      lbd_file_processing_error = lbd_file_processing_error,
      lbd_tab_no_rows           = lbd_tab_no_rows,
      unknown_lbd_error         = unknown_lbd_error
    )
  }
}

message("\n")
message("Writing comparisons to disk")

lbd_df <- rbindlist(problem_rows)
fwrite(lbd_df, paste0(outdir, "/missing_lbd_nids.csv"))




message("\nChecking for NIDs in LBD, but not in GBD")

tab_lit <- readRDS(tab_lit_path)

problem_rows <- list()
for(me in all_mes) {
  
  message("-- ", me)
  
  if (me == "bcg1") {
    gbd_me <- "vacc_bcg"
  } else {
    gbd_me <- paste0("vacc_", me)
  }

  if (me == "dpt12_cond") lbd_me <- me else lbd_me <- paste0(me,'_cov')

  
  gbd_sub <- gbd[me_name == gbd_me]
  gbd_sub[, country := substr(ihme_loc_id, 1, 3)]

  if(me %in% names(lbd_data_list)) lbd_sub <- copy(lbd_data_list[[me]])


  if (me=='dpt12_cond') lbd_me <- me else paste0(me,'_cov')
  lbd_sub <- lbd_sub[(age_bin > 3 | (!is.na(age_bin_agg) & age_bin_agg != "1:3")) & (!is.na(get(lbd_me)))]

  
  lbd_nids <- unique(lbd_sub$svy_id)
  missing_nids <- lbd_nids[lbd_nids %!in% unique(gbd_sub$nid)]

  for (n in missing_nids) {
    
    not_outliered <- FALSE
    
    other_gbd_mes <- FALSE
    
    gbd_file_tabulation_error <- FALSE
    
    present_in_raw_lit <- FALSE
    
    present_in_tab_lit <- FALSE
    
    small_sample_sizes <- FALSE
    
    pweight_missing <- FALSE
    
    unknown_gbd_error <- FALSE

    outlier_sub <- outlier[nid == n & me_name == gbd_me]
    outlier_all <- outlier[nid == n & me_name == ""]
    outlier_problem <- outlier_sub[gbd != 1]

    if (nrow(outlier_all) > 0) {
      next
    }

    if (nrow(outlier_problem) > 1 | nrow(outlier_sub) == 0) {
      not_outliered <- TRUE
    }

    other_mes <- gbd[me_name != gbd_me]
    if (nrow(other_mes) > 1) {
      other_gbd_mes <- TRUE
    }

    lbd_n <- lbd_sub[svy_id == n,]
    if ("microdata" %in% unique(lbd_n$type)) {
      gbd_tab_path <- paste0(gbd_tab_root, n, ".csv")
      if(!file.exists(gbd_tab_path)) {
        gbd_file_tabulation_error <- TRUE
        gbd_proc_path <- paste0(gbd_proc_root, n, ".csv")
        if(file.exists(gbd_proc_path)){
          processed <- fread(paste0(gbd_proc_path))
          if(!('pweight' %in% names(processed))) pweight_missing <- TRUE
        }
      } else{
        tabbed <- fread(paste0(gbd_tab_path))
        if(!(any(tabbed[me_name== gbd_me & age_bin > 3]$sample_size > 20))) small_sample_sizes <- TRUE
      }
    }

    if ("report" %in% unique(lbd_n$type) & me != "dpt12_cond") {
      raw_lit_sub <- raw_lit[nid == n & !is.na(get(gbd_me))]
      if (nrow(raw_lit_sub) > 0) {
        present_in_raw_lit <- TRUE
      }

      tab_lit_sub <- tab_lit[nid == n & me_name == gbd_me]
      if (nrow(tab_lit_sub) > 1) {
        present_in_tab_lit <- TRUE
      }
    }

    if (!any(not_outliered, gbd_file_tabulation_error,
             present_in_raw_lit, present_in_tab_lit)) {
      unknown_gbd_error <- TRUE
    }

    svy <- paste0(n, "_", me)
    country <- unique(lbd_n$country)
    problem_rows[[svy]] <-  data.table(
      nid                       = n,
      me_name                   = me,
      country                   = country,
      not_outliered             = not_outliered,
      other_gbd_mes             = other_gbd_mes,
      gbd_file_tabulation_error = gbd_file_tabulation_error,
      present_in_raw_lit        = present_in_raw_lit,
      present_in_tab_lit        = present_in_tab_lit,
      small_sample_sizes        = small_sample_sizes,
      pweight_missing           = pweight_missing,
      unknown_gbd_error         = unknown_gbd_error
    )
  }
}

message("\n")
message("Writing comparisons to disk")
message("\n")

gbd_df <- rbindlist(problem_rows)
gbd_df <- gbd_df[nid != 1,]
gbd_df <- gbd_df[pweight_missing == FALSE & small_sample_sizes == FALSE]
gbd_df[, `:=` (pweight_missing = NULL, small_sample_sizes = NULL)]
fwrite(gbd_df, paste0(outdir, "/missing_gbd_nids.csv"))

message(format((proc.time() - start_time)[3]/60, digits=3), " minutes")
message("Done!")
