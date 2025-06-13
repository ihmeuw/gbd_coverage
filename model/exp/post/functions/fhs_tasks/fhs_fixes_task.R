







#' @param code_root 
#' @param results_root 
#' @param draws_read_root 
#' @param draws_save_root 
#' @param run_date 
#' @param year_end_gbd 
#' @param fhs_release_id 
#' @param fhs_location_set_id 
#' @param release_id_gbd 
#' @param location_set_id_gbd 

#' @return
#' @export

#' @examples
fhs_fixes_task <- function(
    code_root,
    results_root,
    draws_read_root,
    draws_save_root,
    run_date,
    year_end_gbd,
    fhs_release_id,
    fhs_location_set_id,
    release_id_gbd,
    location_set_id_gbd
    
){ 
  
  
  
  
  source("FILEPATH/get_population.R")
  source("FILEPATH/get_location_metadata.R")
  output_fhs_root <- file.path(results_root, "fhs")
  
  draws_root_dv <- file.path(draws_root_gbdxx, run_date)
  
  me_names_straight <- c("vacc_dpt3", "vacc_mcv1"); names(me_names_straight) <- me_names_straight
  
  
  
  dt_loc_diff_fhs_not_gbd <-  fread(file.path(output_fhs_root, "loc_diff_fhs_not_gbd.csv"))
  dt_loc_diff_hierarchy <- fread(file.path(output_fhs_root, "loc_diff_hierarchy.csv"))
  
  if(!all(dt_loc_diff_fhs_not_gbd$location_id == 44858)) stop("ETH SNNP - something wrong, investigate.  fhs_fixes supports only aggregation to 44858.")
  if(!all(dt_loc_diff_hierarchy$ihme_loc_id %like% "CHN")) stop("CHN - investigate. fhx_fixes expects only hierarchy differences for China.")
  
  pop_dt_fhs <- get_population(release_id = fhs_release_id, location_set_id = fhs_location_set_id, year_id = "all", location_id = "all", forecasted_pop = TRUE)
  pop_dt_gbd <- get_population(release_id = release_id_gbd, location_set_id = location_set_id_gbd, year_id = "all", location_id = "all")
  loc_dt_gbd <- get_location_metadata(release_id = release_id_gbd, location_set_id = location_set_id_gbd)
  loc_dt_fhs <- get_location_metadata(release_id = fhs_release_id, location_set_id = fhs_location_set_id)
  
  
  
  
  
  
  
  
  
  
  
  message("Aggregating SNNP provinces in GBD to FHS location")
  
  
  loc_dt_gbd[ihme_loc_id %like% "ETH_", .(location_name, location_id)]
  loc_dt_fhs[ihme_loc_id %like% "ETH_", .(location_name, location_id)]
  
  loc_id_snnpwsswr <- 44858L
  loc_ids_snnp <- c(94364L, 95069L, 60908L)
  pop_snnp <- pop_proportion_calc(pop = pop_dt_gbd, location_ids = loc_ids_snnp, by_vars = "year_id")
  
  
  fhs_list <- lapply(me_names_straight, function(me){
    dt <- fread(file.path(draws_read_root, me, paste0(loc_id_snnpwsswr, ".csv")))
    dt <- dt[year_id > year_end_gbd]
  })
  
  
  
  for(me in me_names_straight){
    dt <- rbindlist(
      lapply(loc_ids_snnp, function(loc_id) fread(file.path(draws_read_root, me, paste0(loc_id, ".csv"))))
    )
    dt              <- dt[year_id <= year_end_gbd]
    varnames_draw   <- grep("draw_", names(dt), value = TRUE)
    varnames_ids    <- setdiff(names(dt), varnames_draw)
    varnames_agg_by <- setdiff(varnames_ids, "location_id")
    dt              <- melt(dt, id.vars = varnames_ids, variable.name = "draw_num")
    dt              <- merge(dt, pop_snnp[, .(location_id, year_id, pop_prop)], by = c("location_id", "year_id"), all.x=TRUE)
    dt[, value := value * pop_prop, by = c(varnames_agg_by, "draw_num")]
    dt_agg          <- dt[, .(value = sum(value)), by = c(varnames_agg_by, "draw_num")]
    dt_agg[, location_id := loc_id_snnpwsswr]
    dt_w <- dcast(dt_agg, ... ~ draw_num, value.var = "value")
    dt_w <- rbind(dt_w, fhs_list[[me]])
    fwrite(dt_w, file.path(draws_save_root, me, paste0(loc_id_snnpwsswr, ".csv")))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  message("\nRemoving files not in FHS hierarchy from :", draws_read_root)
  
  
  loc_ids_fhs       <- loc_dt_fhs[level >= 3, location_id]
  success_list      <- lapply(me_names_straight, function(me){
    message(" -- ", me)
    
    draw_dir_me     <- file.path(draws_read_root, me)
    fnames_disk     <- list.files(draw_dir_me, pattern = ".csv$", full.names = FALSE)
    loc_ids_disk    <- as.integer(sub("\\.csv$", "", fnames_disk))
    loc_ids_rm      <- setdiff(loc_ids_disk, loc_ids_fhs)
    if(length(loc_ids_rm)) file.remove(file.path(draws_read_root, me, paste0(loc_ids_rm, ".csv")))
    
    
    fnames_disk     <- list.files(draw_dir_me, pattern = ".csv$", full.names = FALSE)
    loc_ids_disk    <- as.integer(sub("\\.csv$", "", fnames_disk))
    loc_ids_missing <- setdiff(loc_ids_fhs, loc_ids_disk)
    if(length(loc_ids_missing)) stop("\nMissing some required FHS locations in: ", draw_dir_me, "\n",
                                     toString(loc_ids_missing))
  })
  
  if(any(unlist(success_list) == FALSE)){
    stop("Failed to remove some files.")
  }
}
