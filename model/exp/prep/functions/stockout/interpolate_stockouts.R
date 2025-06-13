












interpolate_stockouts <- function(df, interpolated_ihme_locs, to_model_dir){

   valid_ihme_locs <- c(
      "SDN"
   )
   
   if(!all(interpolated_ihme_locs %in% valid_ihme_locs)){
      stop("interpolated_ihme_locs must be a subset of valid_ihme_locs: ", toString(valid_ihme_locs))
   }
      
   
   if("SDN" %in% interpolated_ihme_locs){
      message("SDN - interpolating stockouts - all antigens, 2022")
      sdn <- df[ihme_loc_id == "SDN" & year_id %in% 2021:2023]
      
      sdn[is.na(nid), cv_admin := 1]
      
      sdn <- sdn[cv_admin == 1]
      
      df_min_sdn <- fsetdiff(df, sdn)
      setcolorder(sdn, c("id", "ihme_loc_id", "location_id", "me_name", "year_id", "cv_admin", "cv_survey", "cv_lit", "cv_stockout_ratio", "cv_intro", "cv_intro_years", "stockout"))
      setorderv(sdn, c("ihme_loc_id", "me_name", "year_id"))
      
      
      sdn <- sdn[!duplicated(sdn)]
      sdn <- sdn[!is.na(me_name)]
      non_square_list <- assert_square(sdn, id_varnames = c("ihme_loc_id", "me_name", "year_id"), hard_stop = FALSE, verbose = FALSE)
      
      if(nrow(non_square_list$duplicated_rows)) stop("SDN still has duplicated rows - inspect")
      dt_missing <- non_square_list$missing_rows
      
      if(any(c(2021, 2023) %in% dt_missing$year_id)) {
         warning("\nSDN has missing boundary years - inspect\n", prt_multiline(dt_missing))
      }
      
      
      
      sdn_non_interp <- sdn[me_name %in% dt_missing$me_name]
      sdn_interp <- fsetdiff(sdn, sdn_non_interp)
      
      
      fill_grid <- CJ(ihme_loc_id = "SDN", me_name = unique(sdn_interp$me_name), year_id = 2021:2023)
      sdn_interp <- merge(sdn_interp, fill_grid, by = c("ihme_loc_id", "me_name", "year_id"), all = TRUE)
      sdn_interp[, `:=`(
         id = paste0(ihme_loc_id, year_id, me_name)
         , cv_admin = 1
         , location_id = 522
         
         , nid = 203321 
      )]
      sdn_interp <- clean_data(sdn_interp)
      
      sdn_interp[, `:=`(
         cv_stockout_ratio = zoo::na.approx(cv_stockout_ratio, na.rm = FALSE) 
         , cv_intro = zoo::na.fill(cv_intro, fill = "extend", na.rm = FALSE)
         , cv_intro_years = zoo::na.approx(cv_intro_years, na.rm = FALSE)
      ), 
      by = .(ihme_loc_id, me_name)
      ]
      sdn <- rbind(sdn_interp, sdn_non_interp)
      sdn[!is.na(cv_stockout_ratio), stockout := 1]
      
      
      
      fwrite(sdn_interp, file.path(to_model_dir, "reference/stockouts_interpolated.csv"))
      
      df <- rbind(df_min_sdn, sdn)
   }
   
   return(df)
}