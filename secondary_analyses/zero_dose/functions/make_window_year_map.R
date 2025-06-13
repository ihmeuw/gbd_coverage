

#' @param window_year_ids [int] simple integer vector of years

#' @return [data.table] with columns year_id_start, year_id_end

#' @examples








make_window_year_map <- function(window_year_ids){
   window_map_dt <- rbindlist(
      lapply(seq_along(window_year_ids), function(idx){
         if(is.na(window_year_ids[idx + 1])) {
            NULL
         } else {
            data.table(year_id_start = window_year_ids[idx], 
                       year_id_end   = window_year_ids[idx + 1])
         }
      })
   )
   
   return(window_map_dt)
   
}
