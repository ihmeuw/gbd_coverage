



#' @param output_root [chr] path out cross-validation output folder
#' @param dt_nids_all [data.table] with columns [location_id, year_id, me_name,

#' @param locations [data.table] IHME location hierarhcy
#' @param draws_root_gbdxx [chr] path to GBD round draws root folder
#' @param PARAMS [list] cross validation metrics script params
#' @param vaccine_names [chr] e.g. "vacc_dtp3_dtp1_ratio"
#' @param fname_suffix [chr] e.g. "straight" - suffix to add to ouput file -


#' @return
#' @export

#' @examples
load_and_save_gbd_draws <- function(output_root, dt_nids_all, locations, draws_root_gbdxx, PARAMS, vaccine_names, fname_suffix) {
   
   temp_outdir <- file.path(output_root, "temp_gbd_best_draws")
   se$make_directory(temp_outdir)
   
   dt_nids_all     <- merge(dt_nids_all, locations[, .(location_id, ihme_loc_id)], by = "ihme_loc_id", all.x = TRUE)
   loc_ids_to_load <- unique(dt_nids_all$location_id)
   draws_root_best <- file.path(draws_root_gbdxx, sprintf("%s_covidimputed", PARAMS$dv_gbd_best))
   
   for (vacc_name in vaccine_names) {
      message(vacc_name)
      dir_draws <- file.path(draws_root_best, vacc_name)
      draws     <- pbmclapply(loc_ids_to_load, function(loc_id){
         x <- se$read_file(file.path(dir_draws, sprintf("%s.csv", loc_id)))
         x[, me_name := vacc_name]
         x[, ..vars_keep_draws]
      }) %>% rbindlist(fill = TRUE)
      
      draws <- melt(draws, id.vars = c("location_id", "year_id", "me_name"), variable.name = "draw", value.name = "value")
      draws[, draw := defactor_vector(draw)]
      
      draws <- merge(
         dt_nids_all[me_name %in% vacc_name, .(location_id, year_id, me_name, nid, data)]
         , draws
         , by = c("location_id", "year_id", "me_name")
         , all.x = TRUE
      )
      
      if (any(is.na(draws))) stop("NA values in draws")
      message("--saving draws of GBD best for: ", vacc_name)
      se$save_file(draws, file.path(temp_outdir, sprintf("draws_coverage_and_nids_%s.rds", vacc_name)))
   }
   
   message("Combining all vaccine_names into one object")
   draws_all <- lapply(vaccine_names, function(vacc_name){
      se$read_file(file.path(temp_outdir, sprintf("draws_coverage_and_nids_%s.rds", vacc_name)))
   }) 
   draws_all <- rbindlist(draws_all)
   
   
   se$save_file(draws_all, file.path(output_root, sprintf("draws_coverage_and_nids_gbd_best_%s.rds", fname_suffix)), verbose = TRUE)
   
   unlink(temp_outdir, recursive = TRUE)
}