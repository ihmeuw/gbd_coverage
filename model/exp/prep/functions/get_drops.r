#' @title Get drops

#' @description Make dt of microdata rows to remove from input data. Uses the decisions table



#' @param each_nid (int) nid to drop
#' @param decisions (data.table) csv of decisions for handling specific nids
#' @param df (data.table) survey data being prepped by prep_exp.r

#' @return subsetted df to each_nid marked for dropping in decisions

#' @concept integrating_decider_decisions
get_drops <- function(each_nid, decisions, df) {
  
  
  checking <- decisions[nid == each_nid]  
  checking <- checking[decision == "drop"]  
  
  
  mini.df     <- data.table()
  for (me in unique(checking$me_name)) {
    
    rm.df     <- df[nid==each_nid & me_name==me]  
    
    mini.df   <- rbind(mini.df, rm.df)
  }
  
  
  return(mini.df)
}