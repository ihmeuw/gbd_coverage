hardcode_sample_size <- function(df, vacc.lit.raw, ihme_locs){
   
   
   valid_locs <- c(
      "NGA"
   )
   stopifnot(all(ihme_locs %in% valid_locs))
   varnames_req <- c("nid", "cv_admin", "cv_survey", "cv_lit", "ihme_loc_id", "sample_size", "year_id")
   stopifnot(all(varnames_req %in% names(df)))
   
   fsm$assert_data_source_type(df = df, require_at_least_one = TRUE, hard_stop = TRUE)
   
   message("-- Hardcode - assigning sample size to locations: ", toString(ihme_locs))
   
   
   dt_lit_raw <- suppressWarnings(as.data.table(readr::read_csv(vacc.lit.raw, show_col_types = FALSE)))
   
   
   
   
   
   
   
   
   ss_nga <- as.integer(dt_lit_raw[location_name_short_ihme_loc_id == "Nigeria|NGA" & nid == 564903, sample_size]) 
   stopifnot(length(ss_nga) == 1)
   message("-- NGA nid 564903 sample size is ", ss_nga)
   df[ihme_loc_id == "NGA" & nid == 564903, sample_size := ss_nga]
   
   
   
   
   return(df)
}