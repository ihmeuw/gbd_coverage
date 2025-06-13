#' @title Get Swaps

#' @description Identify rows to swap microdata for report data with decision 



#' @param each_nid (int) nid to drop
#' @param decisions (data.table) csv of decisions for handling specific nids
#' @param df (data.table) survey data being prepped by prep_exp.r

#' @return subsetted df to each_nid marked for swapping in decisions

#' @concept integrating_decider_decisions
get_swaps <- function(each_nid, decisions, df) {
  
  
  checking <- decisions[nid == each_nid]  
  checking <- checking[decision == "use_report_extractions"]  
  
  
  mini.df     <- data.table()
  for (me in unique(checking$me_name)) {
    
    rm.df     <- df[nid==each_nid & me_name==me]  
    
    mini.df   <- rbind(mini.df, rm.df)
  }
  
  
  return(mini.df)
}