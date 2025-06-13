



#' @param df 
#' @param ihme_locs 

#' @return
#' @export

#' @examples
hardcode_nid_203321_to_admin <- function(df, ihme_locs) {
   
   varnames_req <- c("nid", "cv_admin", "cv_survey", "cv_lit", "ihme_loc_id")
   stopifnot(all(varnames_req %in% names(df)))
   
   fsm$assert_data_source_type(df = df, require_at_least_one = TRUE, hard_stop = TRUE)
   
   message("-- Hardcode - assigning nid 203321 to locations: ", toString(ihme_locs))
   
   
   
   df[cv_admin == 1 & ihme_loc_id %in% ihme_locs, nid := 203321]
   
   return(df)
   
}