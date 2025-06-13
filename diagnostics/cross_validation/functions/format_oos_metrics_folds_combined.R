







#' @param oos_list [list] of data.tables with columns [me_name,

#' @param varname_metric [chr] name of the metric column in the data.tables e.g.


#' @return [data.table] with columns [me_name, fold1, fold2, ..., foldN,

format_oos_metrics_folds_combined <- function(oos_list, varname_metric) {
   dt <- copy(oos_list[["folds"]])
   setnames(dt, varname_metric, "combined_folds")
   if ("gbd_best" %in% names(oos_list)){
      vars_in_sample <- c("me_name", "in_sample")
      dt_gbd <- copy(oos_list[["gbd_best"]])
      setnames(dt_gbd, varname_metric, "in_sample")
      dt <- merge(dt, dt_gbd[, ..vars_in_sample], by = "me_name", all.x = TRUE)
      
      dt[, in_sample := sprintf("%.4f", in_sample)]
   }
      
   dt[, combined_folds := sprintf("%.4f", combined_folds)]
   
   setcolorder(dt, c("me_name", "in_sample", "combined_folds"))
   dt[]
   return(dt)
}