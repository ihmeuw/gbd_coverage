




#' @param data_table [data.table] square, long format
#' @param key_colnames [chr] require "year_id"; vector of column names that uniquely ID data rows
#' @param years_needed [int] vector of all required years to be in data

#' @return [data.table] square, long format, carry last value forward for all years_needed
carry_last_value_forward <- function(data_table,
                                     key_colnames,
                                     years_needed
){
   
   dt <- copy(data_table)
   
   if(! "year_id" %in% names(dt)) stop("data_table must contain a 'year_id' column")
   key_ids <- 
      lapply(key_colnames, function(col, dt){
         id_vals <- unique(dt[[col]])
      },
      dt = dt)
   names(key_ids) <- key_colnames
   
   assertable::assert_ids(dt, key_ids)
   
   years_have    <- unique(dt$year_id)
   year_have_max <- max(years_have)
   year_need_max <- max(years_needed)
   years_tofill  <- setdiff(years_needed, years_have)
   
   if(year_need_max < year_have_max){
      warning("max year needed is less than max year in data.
            year_have_max = ", year_have_max, "
            year_need_max = ", year_need_max)
   }
   
   if(any(years_tofill < year_have_max)){
      stop("May not backfill years - this function only for carry forward. 
         Years to fill: ", paste(years_tofill, collapse = ", "))
   }
   
   fill_colnames          <- setdiff(names(dt), c(key_colnames))
   key_ids_tofill         <- copy(key_ids)
   key_ids_tofill$year_id <- years_tofill
   
   grid_tofill <- as.data.table(
      expand.grid(
         stringsAsFactors = FALSE
         , key_ids_tofill
      )
   )
   
   dt_year_to_copy <- dt[year_id == year_have_max]
   varnames_merge  <- setdiff(names(key_ids_tofill), "year_id")
   grid_filled     <- merge(grid_tofill, dt_year_to_copy[, -"year_id"], by = varnames_merge, all.x = TRUE)
   dt_filled       <- rbind(dt, grid_filled, fill = FALSE)
   setorderv(dt_filled, key_colnames)
   
   return(dt_filled)
   
}

