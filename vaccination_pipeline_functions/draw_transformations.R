

#' @param DT [data.frame]

#' @return
#' @export

#' @examples
find_ids <- function(DT){
   stopifnot(is.data.table(DT))
   dput(grep("draw|value", colnames(DT), value = TRUE, invert = TRUE))
}








#' @param DT [data.table] input draws
#' @param id_cols [character] columns to keep as is
#' @param verbose [lgl] print debug messages?

#' @return [data.frame] draws in long format
#' @export

#' @examples
draws_wide_to_long <- function(DT, id_cols, verbose = FALSE){
   stopifnot(data.table::is.data.table(DT))
   
   vars_draws = grep("^draw_", colnames(DT), value = TRUE)
   stopifnot(length(vars_draws) > 0)
   
   id_cols_absent <- setdiff(id_cols, colnames(DT))
   if(length(id_cols_absent)>0){
      stop(paste0("id_cols absent in DT: ", toString(id_cols_absent)))
   }
   
   if(verbose == TRUE) message("wide to long - draw columns, e.g. : ", toString(sort(vars_draws[1:5])))
   
   DT <- tidyr::pivot_longer(data = DT, cols = all_of(vars_draws), names_to = "draw", values_to = "value") %>%
      dplyr::mutate(draw = as.integer(sub("^draw_", "", draw))) %>%
      data.table::as.data.table()
   
   
   data.table::setorderv(DT, id_cols)
      
   return(DT)
}



#' @param DT [data.table] input draws
#' @param id_cols [character] columns to keep as is
#' @param verbose [lgl] print debug messages?

#' @return [data.frame] draws in wide format
#' @export

#' @examples
draws_long_to_wide <- function(DT, id_cols, verbose = FALSE){
   
   stopifnot(data.table::is.data.table(DT))
   
   stopifnot("draw" %in% colnames(DT))
   stopifnot("value" %in% colnames(DT))
   
   id_cols_absent <- setdiff(id_cols, colnames(DT))
   if(length(id_cols_absent)>0){
      stop(paste0("id_cols absent in DT: ", toString(id_cols_absent)))
   }
   names_prefix <- "draw_"
   
   if(verbose == TRUE) {
      message("id_cols: ", toString(id_cols))
      message("long to wide - draw columns to e.g. : ", toString(paste0(names_prefix, unique(sort(DT$draw[1:5])))))
   }
   
   DT <- tidyr::pivot_wider(DT, id_cols = all_of(id_cols), names_from = "draw", values_from = "value", names_prefix = names_prefix)  %>%
      data.table::as.data.table()
   

   data.table::setorderv(DT, id_cols)
   
   return(DT)
}



#' @param DT [data.table] input draws

#' @return [data.table] draws in wide format
#' @export

#' @examples
draws_years_to_wide <- function(DT){
   vars_req <- c("year_id", "value")
   stopifnot(all(vars_req %in% colnames(DT)))
   stopifnot(length(unique(DT$year_id)) == 2)
   pivot_wider(DT, names_from = "year_id", values_from = "value", names_prefix = "value_") %>% 
      as.data.table()
}



#' @param DT [data.table] input draws in wide format
#' @param id_cols [character] columns to keep as is
#' @param vars_draws [character] draw columns
#' @param verbose [lgl] print debug messages?
#' @param remove_vars_draws [lgl] remove draw columns?
#' @param fix_mean_zero [lgl] Some sets of draws have only a




#' @return [data.table] mean and 95% CI of draws (columns: mean, lower, upper),

#' @export

#' @examples
draws_to_mean_ci <- function(DT, id_cols, vars_draws = grep("^draw_", colnames(DT), value = TRUE), remove_vars_draws = TRUE, fix_mean_zero = FALSE, verbose = FALSE){
   stopifnot(data.table::is.data.table(DT))
   
   stopifnot(length(vars_draws) > 0)
   stopifnot(is.logical(remove_vars_draws))
   stopifnot(is.logical(fix_mean_zero))
   stopifnot(is.logical(verbose))
   
   
   id_cols_absent <- setdiff(id_cols, colnames(DT))
   if(length(id_cols_absent)>0){
      stop(paste0("id_cols absent in DT: ", toString(id_cols_absent)))
   }
   
   if(verbose == TRUE) message("draws to mean/95%CI - draw columns, e.g. : ", toString(sort(vars_draws[1:5])))
   
   DT <- data.table::as.data.table(DT)
   DT[, mean := base::rowMeans(.SD), .SDcols = vars_draws]
   DT[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = vars_draws]
   DT[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = vars_draws]
   
   
   
   if(fix_mean_zero == TRUE){
      DT[mean > 0 & upper == 0, mean := 0]
      DT[mean > 0 & lower == 0, lower := 0]
   }
   if(remove_vars_draws == TRUE) DT[, c(vars_draws) := NULL]
   
   data.table::setorderv(DT, id_cols)
   
   return(DT)
}






#' @param DT [data.table] a table of (wide) draws
#' @param yr_vec [int] 2 years to compare
#' @param id_cols [character] columns to keep as id columns

#' @return [data.table] a table of mean and 95% CI for the difference between

#' @export

#' @examples
draws_year_diff <- function(DT, yr_vec, id_cols){
   stopifnot(length(yr_vec) == 2)
   yr_vec <- sort(yr_vec)
   id_cols_noyrs <- setdiff(id_cols, "year_id")
   X <- DT[year_id %in% yr_vec] %>% 
      draws_wide_to_long(id_cols = id_cols) %>% 
      pivot_wider(names_from = year_id, values_from = "value", names_prefix = "value_") %>%
      as.data.table()
   
   X[, value := get(paste0("value_", yr_vec[2])) - get(paste0("value_", yr_vec[1]))]
   keep_vars <- c(id_cols_noyrs, "value", "draw")
   X <- X[, ..keep_vars]
   X[, years := paste0(yr_vec[1], "_", yr_vec[2])]
   id_cols_yrs <- c(id_cols_noyrs, "years")
   X <- draws_long_to_wide(X, id_cols = id_cols_yrs)
   return(draws_to_mean_ci(X, id_cols = id_cols_yrs))
}
