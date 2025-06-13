#' @title Produce Stockout Diagnostics

#' @description The stockout (or "shock") model is used to assess the strength of the stockout.





#' @param to_model_dir (str) absolute filepath to the model run directory
#' @param date (str) date of model run (typically YYYY-MM-DD)
#' @param COVID_years (int) e.g. 2020:2022

#' @return NULL

#' @import data.table ggplot2

#' @concept stockout_functions
produce_stockout_diagnostics <- function(
      to_model_dir,
      fname_stockout_dx,
      stgpr_predictions_path,
      stgpr_ratios_predictions_path,
      stgpr_exact_matches_path,
      stgpr_exact_ratios_matches_path,
      reported_stockouts_path,
      date,
      locs,
      COVID_years
) {
   


  
  
  stgpr_prediction   <- fread(stgpr_predictions_path)
  stgpr_ratios       <- fread(stgpr_ratios_predictions_path)
  reported_stockouts <- readRDS(reported_stockouts_path)
  stgpr_prediction   <- rbind(stgpr_prediction, stgpr_ratios)
   

  
  stgpr_prediction    <- stgpr_prediction[, .(ihme_loc_id, year_id, me_name, data, stgpr = pred_stgpr)]
  stgpr_prediction$me <- gsub("[0-9]+", "", stgpr_prediction$me_name)
  reported_stockouts$reported_stockout <- TRUE

  
  stgpr_prediction  <- merge(reported_stockouts, 
                             stgpr_prediction[, .(ihme_loc_id, year_id, me, me_name, data, stgpr)],
                             by  = c("ihme_loc_id", "year_id", "me"), 
                             all = TRUE)

  stgpr_prediction[reported_stockout == TRUE, who_stockout := data]
  stgpr_prediction$reported_stockout <- NULL

  setnames(stgpr_prediction, "data", "admin")
  
  
  em <- fread(stgpr_exact_matches_path)
  em_ratios <- fread(stgpr_exact_ratios_matches_path)
  em <- rbind(em, em_ratios, fill = TRUE)
  em <- em[, .(ihme_loc_id, year_id, me_name, exact_match = cv_stockout_ratio)]
  
  stgpr_prediction <- merge(
     stgpr_prediction,
     em,
     by = c("ihme_loc_id", "year_id", "me_name"),
     all = TRUE
  )
  
  
  
  stgpr_prediction[!is.na(exact_match), exact_match := admin]

  
  sto_long <- melt.data.table(stgpr_prediction,
                          id.vars         = c("ihme_loc_id", "year_id", "me_name"),
                          measure.vars    = c("admin", "who_stockout", "stgpr", "exact_match"),
                          variable.name   = "type", 
                          variable.factor = FALSE, 
                          value.name      = "value")

  
  sto_long[year_id %in% COVID_years & type == "admin", type := "who_stockout"]

  
  sto_long <- merge(sto_long, 
                locs[, .(ihme_loc_id, location_name, level)], 
                all.x = TRUE, 
                by    = c("ihme_loc_id"))

  
  sto_long <- sto_long[level == 3, ]

  
  locations <- sto_long[type == "who_stockout", unique(location_name)]

  pdf(file = fname_stockout_dx, width = 16, height = 12, pointsize = 12)

  for(location in locations) {

    
    ihme_loc <- sto_long[location_name == location, unique(ihme_loc_id)]
    message(paste0("-- "), location, " (", ihme_loc, ")")

    
    gg <- ggplot(data = sto_long[location_name == location], mapping = aes(x = year_id, y = value, color = type)) +
      geom_line(data = sto_long[location_name == location & type %!in% c("who_stockout", "admin", "exact_match")], alpha = .5) +
      geom_point(data = sto_long[location_name == location & type %in% c("who_stockout", "admin")], alpha = .5) +
      
      geom_point(data = sto_long[location_name == location & type %in% c("exact_match")], alpha = .5, shape = 3, color = "black") +
      
      
      scale_color_manual(breaks = c("admin", "who_stockout", "stgpr", "exact_match"),
                         values = c("
       
       
      theme_bw() +
      ggtitle(paste0(location, " (", ihme_loc, ")")) +
      labs(caption = date) +
      facet_wrap(~ me_name)

    plot(gg)
  
  }

  
  dev.off()

}






