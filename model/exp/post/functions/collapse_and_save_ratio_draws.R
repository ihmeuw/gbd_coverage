

#' @param vacc_ratio_name [chr] e.g. vacc_hepb3_dpt3_ratio
#' @param gbd_cycle [chr] e.g. "gbd2023" - from paths.csv
#' @param date [chr] pipelin run_date version

#' @return [data.table] draws read, bound
#' @export

#' @import parallel, data.table
read_ratio_draws <- function(vacc_ratio_name, gbd_cycle, date) {
  
  path  <- paste0("FILEPATH", gbd_cycle, "/", date, "/", vacc_ratio_name)
  files <- list.files(path, full.names=TRUE) 
  
  if (length(files)==0) stop(paste0("No draws for this run_id (", vacc_ratio_name, ")"))
  
  df  <- rbindlist(
    mclapply(files, fread, mc.cores=10) 
    , use.names=TRUE
  )
  key <- c("location_id", "year_id", "age_group_id", "sex_id")
  setkeyv(df, cols=key)
  df  <- unique(df)
  
  return(df)
  
}



#' @param ratios [chr] e.g. vacc_hepb3_dpt3_ratio
#' @param gbd_cycle [chr] e.g. "gbd2023" - from paths.csv
#' @param date date [chr] pipelin run_date version

#' @return [stderr] user message, write to disk
#' @export
collapse_and_save_ratio_draws <- function(ratios, gbd_cycle, date, results_root){
  
  for (ratio in ratios) {
    message(ratio)
    
    me_ratio    <- read_ratio_draws(vacc_ratio_name = ratio, gbd_cycle = gbd_cycle, date = date)
    me_collapse <- collapse_draws(me_ratio)
    me_collapse <- me_collapse[, me_name := ratio]
    
    saveRDS(me_collapse, file=paste0(results_root, "/", ratio, ".rds"))
    message(paste0(("  --saved")))
  }
}