#' @title Create id

#' @description Create unique identifier by survey and check for duplicates

#' @param df (data.table) data being prepped by prep_exp.r
#' @param varnames_unique_id (chr) variables to use to create unique id 



#' @return df with an additional cv_id column

#' @concept data_cleaning_functions
create_id <- function(df, varnames_unique_id) {
  
  
  df[, survey_name_orig := survey_name]
  df[nid==203321, survey_name := "WHO/UNICEF Official Country-Reported Data"]
  
  
  cols.id <- varnames_unique_id  
  cols.id <- cols.id[cols.id %in% names(df)]
  
  
  df <- df[, cv_id := .GRP, by=cols.id] 
  
  
  df <- df[, dupe := lapply(.SD, function(x) length(x) - 1), .SDcols='cv_id', by=c('cv_id', 'me_name')]
  
  df[, cv_id_2 := paste0(me_name, cv_id)]  
  df <- df[!duplicated(df$cv_id_2, fromLast=TRUE), ]
  df <- df[, dupe := lapply(.SD, function(x) length(x) - 1), .SDcols='cv_id', by=c('cv_id', 'me_name')]
  
  ndupe <- nrow(df[which(dupe > 0)])
  if (ndupe > 0) {
    print(paste0("Dropping duplicates by cv_id: ", ndupe, " rows"))
    df <- df[!which(dupe > 0)]
  }
  
  if (max(df$dupe) > 0) stop("Duplicates in meta")
  df$dupe <- NULL
  
  
  df[nid==203321, survey_name := survey_name_orig]
  df <- df[, survey_name_orig := NULL]
  
  
  return(df)
  
}