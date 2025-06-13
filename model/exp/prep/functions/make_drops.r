#' @title Make drops

#' @description Drop rows from input data. Takes the full df and a subsetted version with 



#' @param df (data.table) survey data being prepped by prep_exp.r
#' @param drop_df (data.table) subsetted df by get_drops() containing rows to drop

#' @return df with drop_df rows removed from it

#' @concept integrating_decider_decisions
make_drops <- function(df, drop_df) {   
  
  
  drop_df[, match := paste(nid, me_name, age_year, sep="_")]
  df <- df[, match := paste(nid, me_name, age_year, sep="_")]
  
  
  df <- df[match %!in% unique(drop_df$match)]
  return(df)
}