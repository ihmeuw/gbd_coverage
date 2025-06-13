




#' @param df [data.table] vaccine coverage table from prep_exp
#' @param ihme_locs_to_modify [chr] vector of ihme_loc_ids with modified data

#' @return [data.table] modified vaccine coverage table
#' @export

#' @examples
modify_data_for_modeling <- function(df, ihme_locs_to_modify){
   
   valid_ihme_locs <- c(
      "PSE"
   )
   
   if(!all(ihme_locs_to_modify %in% valid_ihme_locs)){
      stop("Invalid location(s) specified: ", toString(valid_ihme_locs))
   }
   
   message("Modifying data for modeling:")
   
   message("--PSE - multiplying coverage data by 0.75 for all antigens from 3 months of 0 coverage due to conflict.")
   
   df[ihme_loc_id == "PSE" & year_id == 2023 & nid == 203321, 
      data := data * 0.75]
   
   return(df)
}