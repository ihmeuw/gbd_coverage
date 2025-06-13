





#' @param DT [data.table] with columns location_id, me_name, year_id, population, coverage
#' @param year_id_start [int] start year
#' @param year_id_end [int] end year

#' @return [data.table] with columns location_id, me_name, coverage_start, coverage_end, aroc, window
calculate_aroc <- function(
      DT,
      year_id_start,
      year_id_end
) {
   stopifnot(all(c("location_id", "me_name", "year_id", "population", "coverage", "super_region_name") %in% colnames(DT)))
   
   
   
   varnames_keep <- c("me_name", "location_id", "year_id", "population", "coverage", "super_region_name")
   
   DT_start <- DT[year_id %in% year_id_start, ..varnames_keep]
   DT_end   <- DT[year_id %in% year_id_end, ..varnames_keep]
   DT_start[, year := "start"]
   DT_end[, year := "end"]
   
   DT_start <- dcast(DT_start, location_id + me_name + super_region_name ~ year, value.var = c("coverage", "population"))
   DT_end   <- dcast(DT_end, location_id + me_name + super_region_name ~ year, value.var = c("coverage", "population"))
   
   DT_window <- merge(DT_start, DT_end, by = c("location_id", "me_name", "super_region_name"))
   DT_window[, aroc := log(coverage_end / coverage_start) / (year_id_end - year_id_start)]
   DT_window[!is.finite(aroc), aroc := 0] 
   DT_window[, window := paste0(year_id_start, "_", year_id_end)]
   
   return(DT_window)
}






#' @param DT [data.table] with columns location_id, me_name, year_id, population, coverage
#' @param window_year_map [data.table] with columns year_id_start, year_id_end

#' @return [list] of data.tables with columns location_id, me_name, coverage_start, coverage_end, aroc, window
calculate_aroc_list <- function(DT, window_year_map){
   message("Calulating AROC for:\n", prt_multiline(window_year_map))
   purrr::map2(
      window_year_map$year_id_start
      , window_year_map$year_id_end
      , function(x, y){
         calculate_aroc(DT = DT, year_id_start = x, year_id_end = y)
      }
   )
}




calc_aroc_vec <- function(coverage_start, coverage_end, year_diff){
   return(log(coverage_end / coverage_start) / year_diff)
}

flag_over_90 <- function(x){
   return(ifelse(x >= 0.9, 1, 0))
}
