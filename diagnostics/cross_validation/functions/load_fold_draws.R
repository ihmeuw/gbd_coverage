

#' @param dt_nid_list [list] of data.tables with columns [location_id, year_id,

#' @param PATHS [list] of paths to draws for each fold
#' @param fold [chr] `fold_names` element
#' @param vaccine_names [chr] names of vaccines to load e.g. c("vacc_mcv1", "vacc_dtp1")
#' @param vars_keep [chr] variable names to keep in the (wide) draws

#' @return [data.table] draws (long) with columns [location_id, year_id, me_name, nid, data, draw, value]
load_fold_draws <- function(dt_nid_list, PATHS, fold, vaccine_names, vars_keep) {
   dt_nid <- dt_nid_list[[fold]]
   
   loc_ids_to_load <- unique(dt_nid$location_id)
   
   dirs_vacc <- file.path(PATHS$roots_draws[[fold]], vaccine_names)
   names(dirs_vacc) <- vaccine_names
   
   fpaths <- lapply(dirs_vacc, function(root){
      file.path(root, sprintf("%s.csv", loc_ids_to_load))
   })
   
   draws <- purrr::imap(fpaths, function(vacc_paths, vacc_name){
      
      message("--", vacc_name)
      
      mclapply(vacc_paths, function(vacc_path){
         dt <- se$read_file(vacc_path)
         dt[, me_name := vacc_name]
         return(dt[, ..vars_keep])
      }) %>% rbindlist()
   }) %>% rbindlist()
   
   draws <- melt(draws, id.vars = c("location_id", "year_id", "me_name"), variable.name = "draw", value.name = "value")
   draws[, draw := defactor_vector(draw)]
   
   draws <- merge(dt_nid[me_name %in% vaccine_names, .(location_id, year_id, me_name, nid, data)]
                  , draws
                  , by = c("location_id", "year_id", "me_name")
                  , all.x = TRUE)
   if (any(is.na(draws))) stop("NA values in draws")
   return(draws)
}