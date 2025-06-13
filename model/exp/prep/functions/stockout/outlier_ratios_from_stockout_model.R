





#' @param df_stockout [data.table] data.table of stockouts for ratio vaccines
#' @param fpath_outliers_stockout_ratio_model [chr] path to reference file of points to treat as normal data

#' @return [data.table] `df_stockout` with select outliers removed
#' @export

#' @examples
outlier_from_stockout_ratio_model <- function(df_stockout, fpath_outliers_stockout_ratio_model){
   
   varnames_og   <- names(df_stockout)
   varnames_keys <- c("me_name", "ihme_loc_id", "year_id", "nid")
   
   dto <- fread(fpath_outliers_stockout_ratio_model) 
   
   df_stockout <- merge(x = df_stockout, y = dto, by = varnames_keys, all.x = TRUE)
   df_stockout[outlier_stockout == TRUE, who_stockout := FALSE]
   
   return(df_stockout[, ..varnames_og])
}