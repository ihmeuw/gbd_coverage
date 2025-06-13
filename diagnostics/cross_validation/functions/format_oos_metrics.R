



#' @param oos_list [list] of data.tables with columns [me_name, fold, metric]
#' @param fold_names [chr] names of folds 
#' @param varname_metric [chr] name of the metric column in the data.tables e.g. 'rmse'

#' @return [data.table] with columns [me_name, fold1, fold2, ..., foldN, folds_mean, folds_sd, (in_sample)]
format_oos_metrics <- function(oos_list, fold_names, varname_metric) {
   dt <- rbindlist(oos_list[fold_names])
   dt <- dcast(dt, me_name ~ fold, value.var = varname_metric)
   dt[, folds_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = fold_names]
   dt[, folds_sd := sd(.SD), .SDcols = fold_names, by = me_name]
   vars_in_sample <- c("me_name", varname_metric)
   if ("gbd_best" %in% names(oos_list)){
      dt <- merge(dt, oos_list[["gbd_best"]][, ..vars_in_sample], by = "me_name", all.x = TRUE)
      setnames(dt, varname_metric, "in_sample")
      
      dt[, in_sample := sprintf("%.4f", in_sample)]
   }
   
   dt[, folds_mean_sd := sprintf("%.4f (%.4f)", folds_mean, folds_sd)]
   dt[]
   return(dt)
}