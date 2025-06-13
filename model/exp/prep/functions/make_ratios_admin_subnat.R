






#' @param df [data.table] main data.table of vaccine coverage data points in

#' @param me [chr] a `df$me_name` e.g. "vacc_dpt3_dpt1_ratio"
#' @param loc_table [data.table] a data.table of locations, with columns

#' @param ratios_using_bias_adjusted_data [chr] Do not use `cv_admin_orig` for


#' @return
#' @export

#' @examples
make_ratios_admin_subnat <- function(df, me, loc_table, ratios_using_bias_adjusted_data){
   mes_with_imputed_data <- c("vacc_dpt3_dpt1_ratio")
   if(!me %in% mes_with_imputed_data){
      stop("final `me` must have imputed data - currently allowed: ", toString(mes_with_imputed_data))
   }
   nid_imputed <- switch(
      me 
      , vacc_dpt3_dpt1_ratio = 451398 
      , "nid_not_found" 
   )
   if(nid_imputed == "nid_not_found"){
      stop("No imputed nid found for `me` ", me, ".  Please update this function's `switch()`.")
   }
   
   
   vaccs <- unlist(strsplit(me, "_"))[2:3]
   num   <- paste0("vacc_", vaccs[1])
   denom <- paste0("vacc_", vaccs[2])
   
   
   df_sub <- merge(df, loc_table[, .(ihme_loc_id, level)], by = "ihme_loc_id")[level > 3]
   df_admin <- df_sub[me_name %in% c(num, denom) & cv_admin == 1]
   varnames_match <- c("ihme_loc_id", "year_id", "age_group_id", "age_end", "survey_name")
   df_admin <- df_admin[, match_id := .GRP, by = varnames_match]
   cols <- c("ihme_loc_id", "year_id", "age_group_id", "sex_id", "nid", "cv_admin_orig", "data", "variance", "sample_size", "cv_admin", "nid_orig", "match_id")
   
   
   df.num     <- df_admin[me_name == num,   ..cols]
   df.num$x   <- "num"
   df.denom   <- df_admin[me_name == denom, ..cols] 
   df.denom$x <- "denom"
   
   
   
   if(!me %in% ratios_using_bias_adjusted_data){
      df.num                 <- df.num[!is.na(cv_admin_orig), data := cv_admin_orig]
      df.denom               <- df.denom[!is.na(cv_admin_orig), data := cv_admin_orig]
   }
   df.num$cv_admin_orig   <- NULL
   df.denom$cv_admin_orig <- NULL
   cols                   <- setdiff(cols, c("cv_admin_orig"))  
   df.r <-
      rbind(df.num, df.denom) %>% 
         
      dcast.data.table(. ,
                       ihme_loc_id + year_id + age_group_id + sex_id  + cv_admin + match_id ~ x, 
                       value.var = c("data", "variance", "sample_size", "nid_orig"))
   
   
   df.r <- df.r[, data := data_num / data_denom]
   df.r <- df.r[, data := ifelse(data >= 1, 0.999, data)] 
   df.r <- df.r[!is.na(data)]
   df.r <- df.r[, variance := data_num ^ 2 / data_denom ^ 2 * (variance_num / data_num ^ 2 + variance_denom / data_denom ^ 2)] 
   df.r <- df.r[, sample_size := data * (1 - pmin(data, 0.999)) / variance] 
   
   
   varnames_remove <- c(
      "match_id"
      , "nid_orig"
   )
   
   varnames_keep <- c(
      setdiff(cols, varnames_remove),
      "nid_orig_num")
   
   df.r[, nid := nid_imputed]
   
   df.r[nid_orig_num == nid_orig_denom, nid := nid_orig_num]
   
   df.r <- df.r[, ..varnames_keep]
   df.r <- df.r[, me_name := me]
   
}