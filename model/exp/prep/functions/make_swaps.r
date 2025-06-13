#' @title Make Swaps

#' @description Using the 'swap_df' df of identified rows to modify, actually make the swap!

#' @param df (data.table) survey data being prepped by prep_exp.r
#' @param swap_df (data.table) subsetted df by get_swaps() containing rows to swap
#' @param tabs (data.table) tabular data to use in the swap
#' @param decisions (data.table) csv of decisions for handling specific nids

#' @return df with swap_df rows changed to report data

#' @concept integrating_decider_decisions
make_swaps <- function(df, swap_df, tabs, decisions) { 
  
  
  swap_df[, match := paste(nid, me_name, age_year, sep="_")]
  df <- df[, match := paste(nid, me_name, age_year, sep="_")]
  df <- df[match %!in% unique(swap_df$match)]
  
  
  tabs <- tabs[, match := paste(nid, me_name, sep="_")]
  tabs <- tabs[, cv_lit := 1]
  swap_df[, match := paste(nid, me_name, sep="_")]
  tabs_to_add <- tabs[match %in% unique(swap_df$match)]
  df <- rbind(df, tabs_to_add, fill=TRUE)  
  df <- df[, match := NULL]   
  
  
  add.df <- decisions[decision == "use_report_extractions" & !is.na(report) & is.na(tabulation)]
  add.df <- add.df[, match := paste(nid, me_name, report, sep="_")]
  
  df     <- df[survey_name=="UNICEF_MICS", match := paste(nid, me_name, data, sep="_")] 
  add.df <- add.df[!match %in% unique(df$match)]
  df     <- df[, match := NULL] 
  tabs   <- tabs[, match := paste(nid, me_name, data, sep="_")]  
  tabs_to_add_2 <- tabs[match %in% unique(add.df$match)]
  df     <- rbind(df, tabs_to_add_2, fill=TRUE)
  df     <- df[, match := NULL]
  
  return(df)
}