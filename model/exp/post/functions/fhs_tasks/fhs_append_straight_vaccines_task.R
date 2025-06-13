


















#' @param code_root [chr] path to local code repo 
#' @param run_date [chr] e.g. "2024_07_11"
#' @param year_end_gbd [int] year end for GBD draws e.g. 2024
#' @param fhs_path_dpt3 [chr] Full path to FHS dpt3 data
#' @param fhs_path_mcv1 [chr] Full path to FHS mcv1 data
#' @param n_cores [int] number of cores to use for parallel processing e.g. jobmon task resources
#' @param results_root [chr] path to the modeled_dir 
#' @param draws_read_root [chr] path to read draws from
#' @param draws_save_root [chr] path to save draws to
#' @param fhs_release_id [int] 
#' @param fhs_location_set_id [int]
#' @param release_id_gbd [int]
#' @param location_set_id_gbd [int]

#' @return [none] side write draw files to disk
#' @export

#' @examples
fhs_append_straight_vaccines_task <- function(
    code_root,
    results_root,
    run_date,
    year_end_gbd,
    draws_read_root,
    draws_save_root,
    fhs_path_dpt3,
    fhs_path_mcv1,
    fhs_release_id,
    fhs_location_set_id,
    release_id_gbd,
    location_set_id_gbd,
    n_cores
) {
  
  
  
  library(parallel)
  source("FILEPATH/get_location_metadata.R")
  source(file.path(code_root, "FILEPATH/read_nc.r"))
  output_fhs_root <- file.path(results_root, "fhs")
  me_names        <- c("vacc_dpt3", "vacc_mcv1")
  dir.create(output_fhs_root, showWarnings = FALSE, recursive = TRUE)
  
  
  
  locs_fhs    <- get_location_metadata(location_set_id = fhs_location_set_id, release_id = fhs_release_id)[level >= 3, ]
  locs_gbd    <- get_location_metadata(location_set_id = location_set_id_gbd, release_id = release_id_gbd)[level >= 3, ]
  me_db       <- fread(file.path(code_root, "reference/me_db.csv"))
  covar_id_dt <- me_db[me_name %in% me_names, .(me_name, covariate_id)]
  
  
  
  fhs_list_raw <- lapply(c(fhs_path_dpt3, fhs_path_mcv1), function(path){
    return(as.data.table(read_nc(path))[scenario == 0, ])
  })
  names(fhs_list_raw) <- me_names
  
  
  message("Reading GBD best draws from: ", draws_read_root)
  vc_list_raw <- lapply(me_names, function(me_name){
    me_files <- list.files(file.path(draws_read_root, me_name), pattern = "\\d*.csv", full.names = TRUE)
    DT <- rbindlist(parallel::mclapply(me_files, fread, mc.cores = n_cores))
    
    return(DT[year_id <= year_end_gbd])
  })
  names(vc_list_raw) <- me_names
  
  stopifnot("draw" %in% names(fhs_list_raw$vacc_dpt3))
  stopifnot("draw_0" %in% names(vc_list_raw$vacc_dpt3))
  
  fhs_n_draws <- length(unique(fhs_list_raw$vacc_dpt3$draw))
  vc_n_draws  <- length(grep("draw_", names(vc_list_raw$vacc_dpt3), value = TRUE))
  
  
  vc_list <- parallel::mclapply(
    mc.cores    = n_cores,
    vc_list_raw,
    fhs_draws_upsample_downsample,
    fhs_n_draws = fhs_n_draws,
    vc_n_draws  = vc_n_draws
  )
  
  vc_list <- parallel::mclapply(
    mc.cores = n_cores, 
    vc_list, 
    function(x){
      x[, scenario := 0]
      x[, age_group_id := 22]
      x[, measure_id := 18]
      x[, run_id := NULL]
      setcolorder(x, "scenario")
      return(x)
    })
  
  vc_col_order <- names(vc_list$vacc_dpt3)
  
  fhs_list <- parallel::mclapply(
    mc.cores = n_cores, 
    seq_along(fhs_list_raw), 
    function(idx){
      .me_name <- names(fhs_list_raw[idx])
      x <- fhs_list_raw[[idx]]
      
      x <- x[year_id > year_end_gbd, ]
      
      x <- as.data.table(tidyr::pivot_wider(x, names_from = "draw", values_from = "value", names_prefix = "draw_"))
      
      x[, age_group_id := 22]
      x[, measure_id := 18]
      x[, me_name := .me_name]
      x <- merge(x, covar_id_dt, by = "me_name", all.x = TRUE)
      x[, me_name := NULL]
      
      setcolorder(x, vc_col_order)
      return(x)
    })
  names(fhs_list) <- me_names
  
  
  loc_ids_super_nat <- locations[level < 3, location_id]
  fhs_list <- lapply(fhs_list, function(x) return(x[!location_id %in% loc_ids_super_nat, ]))
  vc_list  <- lapply(vc_list, function(x) return(x[!location_id %in% loc_ids_super_nat, ]))
  
  
  loc_ids_miss_fhs <- anti_intersect(fhs_list$vacc_dpt3$location_id, fhs_list$vacc_mcv1$location_id)
  loc_ids_miss_vc <- anti_intersect(vc_list$vacc_dpt3$location_id, vc_list$vacc_mcv1$location_id)
  if(length(loc_ids_miss_fhs)) stop("FHS files have mis-matched location_ids: ", toString(loc_ids_miss_fhs))
  if(length(loc_ids_miss_vc)) stop("VC files have mis-matched location_ids: ", toString(loc_ids_miss_vc))
  
  
  loc_ids_fhs_no_vc <- setdiff(fhs_list$vacc_dpt3$location_id, vc_list$vacc_dpt3$location_id) 
  loc_ids_common    <- intersect(fhs_list$vacc_dpt3$location_id, vc_list$vacc_dpt3$location_id)
  
  dt_loc_diff_fhs_not_gbd <- locs_fhs[location_id %in% loc_ids_fhs_no_vc, .(location_id, ihme_loc_id, path_to_top_parent)]
  dt_loc_diff_hierarchy <- fsetdiff(
    locs_gbd[location_id %in% loc_ids_common, .(location_id, ihme_loc_id, path_to_top_parent)]
    , locs_fhs[location_id %in% loc_ids_common, .(location_id, ihme_loc_id, path_to_top_parent)]
  )
  
  
  fwrite(dt_loc_diff_fhs_not_gbd, file.path(output_fhs_root, "loc_diff_fhs_not_gbd.csv"))
  fwrite(dt_loc_diff_hierarchy, file.path(output_fhs_root, "loc_diff_hierarchy.csv"))
  
  dpt3 <- rbind(vc_list$vacc_dpt3, fhs_list$vacc_dpt3)
  mcv1 <- rbind(vc_list$vacc_mcv1, fhs_list$vacc_mcv1)
  
  
  setcolorder(mcv1, names(dpt3))
  
  
  assert_data_schema(dpt3)
  assert_data_schema(mcv1)
  
  vc_list <- list(
    vacc_dpt3 = dpt3,
    vacc_mcv1 = mcv1
  )
  varnames_drop  <- "scenario"
  vc_list <- lapply(vc_list, function(x) return( x[, c(varnames_drop) :=  NULL]))
  
  
  
  
  
  varnames_draws <- grep("draw_", names(dpt3), value = TRUE)
  varnames_ids   <- setdiff(names(vc_list$vacc_dpt3), varnames_draws)
  
  vc_list <- lapply(vc_list, function(dtvc){
    
    unsquare_list <- assert_square(dtvc, varnames_ids, hard_stop = FALSE, verbose = FALSE)
    square_append_dt <- unsquare_list$missing_rows
    square_append_dt[, (varnames_draws) := 0.5]
    dtvc <- rbind(dtvc, square_append_dt)
    setorderv(dtvc, c("location_id", "year_id"))
    
    assert_square(dtvc, varnames_ids, hard_stop = FALSE, verbose = FALSE)
    return(dtvc)
  })
  
  
  message("Saving VC + FHS appended draws to ", root)
  
  for(me in me_names){
    message(" -- ", me)
    save_draws_general(df = vc_list[[me]], me = me, root = draws_save_root)
  }
  
  
}
