



fsm <- list(
   
   
   
   
   #' @param df [data.table] `df` from prep_exp
   #' @param require_at_least_one [lgl] default FALSE - require at least one data source attribution
   #' @param hard_stop [lgl] default TRUE - stop if assertion fails, otherwise warn
   
   #' @return [none] stop if assertion fails, otherwise warn
   #' @export
   
   #' @examples
   assert_data_source_type = function(df, require_at_least_one = FALSE, hard_stop = TRUE){
      
      
      varnames_sources <- c("cv_admin", "cv_survey", "cv_lit")
      varnames_req <- c("ihme_loc_id", "year_id", "me_name", varnames_sources)
      if(!all(varnames_req %in% names(df))) stop("Not all required varnames present in df: ", 
                                                 toString(setdiff(varnames_req, names(df))))
      
      
      chk <- df[, ..varnames_req]
      chk[, n_sources := apply(.SD, 1, sum, na.rm = TRUE), .SDcols = varnames_sources]
      
      
      fail_list <- list(
         no_sources = chk[n_sources == 0, ]
         , multi_sources = chk[n_sources > 1, ]
      )
      
      
      if(!require_at_least_one) fail_list$no_sources <- NULL
      
      
      failure_modes <- unlist(lapply(fail_list, nrow) > 0)
      if(any(failure_modes)){
         if(hard_stop){
            stop("Data source assertion failed:\n", prt_multiline(failure_modes))
         } else {
            warning("Data source assertion failed:\n", prt_multiline(failure_modes))
         }
      }
   }
)