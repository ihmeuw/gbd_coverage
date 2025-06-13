#' @title Make ratios




#' @param df (data.table) data being prepped by prep_exp.r
#' @param me (str) ratio vaccine (comes from `ratios` object of prep_exp.r config)
#' @param ratios_using_bias_adjusted_data (chr) Do not use `cv_admin_orig` for any ratios that are calculated using bias adjusted data

#' @return df with newly calculated values for given me

#' @concept ratio_functions
make_ratios <- function(df, me, ratios_using_bias_adjusted_data) {
  
  
  
  vaccs <- unlist(strsplit(me, "_"))[2:3]
  num   <- paste0("vacc_", vaccs[1])
  denom <- paste0("vacc_", vaccs[2])
  cols  <- c("ihme_loc_id", "year_id", "age_group_id", "sex_id", "nid", "cv_id", "cv_admin_orig", "data", "variance", "sample_size", "cv_admin")  
  
  
  df.num     <- df[me_name == num,   ..cols]
  df.num$x   <- "num"
  df.denom   <- df[me_name == denom, ..cols] 
  df.denom$x <- "denom"
  
  
  
  
  
  
  if(!me %in% ratios_using_bias_adjusted_data){
     df.num   <- df.num[!is.na(cv_admin_orig), data := cv_admin_orig]
     df.denom <- df.denom[!is.na(cv_admin_orig), data := cv_admin_orig]
  }
  df.num$cv_admin_orig   <- NULL
  df.denom$cv_admin_orig <- NULL
  cols <- setdiff(cols, c("cv_admin_orig"))  
  
  df.r <- rbind(df.num, df.denom) %>% 
    dcast.data.table(. , 
                     ihme_loc_id + year_id + age_group_id + sex_id + nid + cv_id + cv_admin ~ x, 
                     value.var = c("data", "variance", "sample_size"))  
  
  
  df.r <- df.r[, data := data_num / data_denom]
  df.r <- df.r[, data := ifelse(data >= 1, 0.999, data)] 
  df.r <- df.r[!is.na(data)]
  df.r <- df.r[, variance := data_num ^ 2 / data_denom ^ 2 * (variance_num / data_num ^ 2 + variance_denom / data_denom ^ 2)] 
  df.r <- df.r[, sample_size := data * (1 - pmin(data, 0.999)) / variance] 
  
  
  df.r <- df.r[, ..cols]
  df.r <- df.r[, me_name := me]
  
  return(df.r)
}