#' @title Split location data

#' @description Splitting pre-2014 India Andhra Pradesh data into AP+Telangana to account





#' @param all_data (data.table) data being prepped by prep_exp.r
#' @param locations (data.table) output of get_location_data
#' @param fhs_run_TF (lgl) if this is an FHS run - you may need to split data differently - this can change year over year

#' @note requires GBD get_population function

#' @return all_data with India admin split accounted for

#' @concept split_and_save_functions
split_loc_data <- function(all_data, locations, fhs_run_TF) {

  if(gbd_cycle != 'gbd2023' | !release_id %in% c(16,33)) stop('split_loc_data is speced out for gbd2023 release 16, may not work for later rounds / releases.')

  
  
  ap_loc_id <- "IND_4841"
  t_loc_id  <- "IND_4871"
  ap_to_split <- all_data[ihme_loc_id==ap_loc_id & year_id <= 2014]  
  ap_nids <- unique(ap_to_split$nid)
  already_split <- unique(all_data[nid %in% ap_nids & ihme_loc_id %in% t_loc_id]$nid)
  ap_to_split <- ap_to_split[me_name %in% admin.vacc & !nid %in% already_split]  
  t_to_split <- copy(ap_to_split)
  t_to_split[, ihme_loc_id := t_loc_id]
  both_to_split <- rbind(ap_to_split, t_to_split)

  mini_pop <- get_population(age_group_id = 22, location_id = c(4841, 4871), sex_id = 3, year_id = 1980:2014,release_id=release_id)
  mini_pop_combined <- mini_pop[, lapply(.SD,sum),by = .(year_id, age_group_id, sex_id, run_id), .SDcols = "population"]
  setnames(mini_pop_combined, "population", "population_combined")
  mini_pop <- merge(mini_pop, mini_pop_combined[,.(year_id, population_combined)], by="year_id", all.x=T)
  mini_pop[, pop_proportion := population / population_combined]
  mini_pop <- merge(mini_pop, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)

  both_to_split <- merge(both_to_split, mini_pop[,.(year_id, ihme_loc_id, pop_proportion)], by=c("year_id", "ihme_loc_id"), all.x=T)

  
  both_to_split[, sample_size := sample_size * pop_proportion]

  
  both_to_split[, variance := data * (1 - data) / sample_size]
  both_to_split[, standard_error := NA]
  both_to_split$standard_error <- as.numeric(both_to_split$standard_error)

  
  both_to_split[, pop_proportion := NULL]

  
  all_data <- all_data[!(ihme_loc_id==ap_loc_id & me_name %in% admin.vacc & nid %in% unique(ap_to_split$nid))]
  all_data <- rbind(all_data, both_to_split)
  
  
  if(!fhs_run_TF){
    snnp_loc_id_old <- "ETH_44858"
    snnp_loc_id_new <- "ETH_95069"
    sid_loc_id  <- "ETH_60908"
    sw_loc_id <- 'ETH_94364'
    snnp_to_split <- all_data[ihme_loc_id==snnp_loc_id_old]  
    snnp_nids <- unique(snnp_to_split$nid)
    already_split <- unique(all_data[nid %in% snnp_nids & ihme_loc_id %in% c(sid_loc_id, sw_loc_id,snnp_loc_id_new)]$nid)
    snnp_to_split <- snnp_to_split[me_name %in% admin.vacc & !(nid %in% already_split)]  
    sid_to_split  <- copy(snnp_to_split)
    sw_to_split <- copy(snnp_to_split)
    sid_to_split[, ihme_loc_id := sid_loc_id]
    sw_to_split[, ihme_loc_id := sw_loc_id]
    snnp_to_split[,ihme_loc_id := snnp_loc_id_new]
    both_to_split <- rbind(snnp_to_split, sid_to_split)
    both_to_split <- rbind(both_to_split, sw_to_split)
    
    mini_pop <- get_population(age_group_id = 22, location_id = as.numeric(gsub('ETH_','', unique(both_to_split$ihme_loc_id))),
                               sex_id = 3, year_id = year_start:year_end,release_id=release_id)
    mini_pop_combined <- mini_pop[, lapply(.SD,sum),by = .(year_id, age_group_id, sex_id, run_id), .SDcols = "population"]
    setnames(mini_pop_combined, "population", "population_combined")
    mini_pop <- merge(mini_pop, mini_pop_combined[,.(year_id, population_combined)], by="year_id", all.x=T)
    mini_pop[, pop_proportion := population / population_combined]
    mini_pop <- merge(mini_pop, locations[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
    
    
    
    both_to_split <- merge(both_to_split, mini_pop[,.(year_id, ihme_loc_id, pop_proportion)], by=c("year_id", "ihme_loc_id"), all.x=T)
    
    
    both_to_split[, sample_size := sample_size * pop_proportion]
    
    
    both_to_split[, variance := data * (1 - data) / sample_size]
    both_to_split[, standard_error := NA]
    both_to_split$standard_error <- as.numeric(both_to_split$standard_error)
    
    
    both_to_split[, pop_proportion := NULL]
    
     all_data <- all_data[!(ihme_loc_id==snnp_loc_id_old 
    )]
    all_data <- rbind(all_data, both_to_split)
  } else {
    
    if(format(Sys.Date(), "%Y") == 2025) {
      stop("Ensure bypassing SNNP data split is still appropriate to match the FHS hierarchy.")
    }
  }
  
  
  return(all_data)

}
