


















require(fasterize, lib.loc = "FILEPATH") 
require(sf, lib.loc = "FILEPATH" )       


suppressMessages(invisible(library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))))
suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))


build_simple_raster_pop_fast <- function(subset_shape, field="ADM0_CODE") {

  
  master_pop <- brick('FILEPATH/WorldPop_total_global_stack.tif')

  
  rasters_of_interest <- c(paste0("worldpop_total_1y_", c(2000, 2005, 2010, 2015), "_00_00.tif"))
  raster_paths_all <- list.files(path = "FILEPATH",
                                 pattern = ".tif$", full.names = TRUE)
  raster_paths     <- raster_paths_all[grepl(paste(rasters_of_interest, collapse = "|"), raster_paths_all)]

  
  master_pop <- do.call(brick, lapply(raster_paths, raster))

  
  cropped_pop <- crop(master_pop, extent(subset_shape), snap="out")

  
  initial_raster <- fasterize::fasterize(st_as_sf(subset_shape), cropped_pop[[1]], field = field)

  if(length(subset(subset_shape, subset = !(get(field) %in% unique(initial_raster)))) != 0) {
    rasterized_shape <- merge(fasterize::fasterize(st_as_sf(subset(subset_shape, !(get(field) %in% unique(initial_raster)))), cropped_pop[[1]], field = field), initial_raster)
  }
  if(length(subset(subset_shape, !(get(field) %in% unique(initial_raster)))) == 0) {
    rasterized_shape <- initial_raster
  }

  masked_pop <- raster::mask(x=cropped_pop, mask=rasterized_shape)

  raster_list <- list()
  raster_list[['simple_raster']] <- rasterized_shape
  raster_list[['pop_raster']]    <- masked_pop

  return(raster_list)
}


load_pop_raster <- function(shapefile_version = 'current') {

  gaul_list         <- get_adm0_codes('all', shapefile_version = shapefile_version)
  use_1k_popraster  <-  TRUE

  
  if(!exists("popraster")){

    
    simple_polygon <- suppressMessages(suppressWarnings(load_simple_polygon(gaul_list = gaul_list, buffer=0.4, subset_only=TRUE,
                                                                            shapefile_version = shapefile_version)))
    
    raster_list <- suppressMessages(suppressWarnings(build_simple_raster_pop_fast(simple_polygon$subset_shape)))

    
    if (use_1k_popraster){
      popraster <- disaggregate(raster_list[['pop_raster']], 5)
    } else {
      popraster <-  raster_list[['pop_raster']]
    }
  }

  
  saveRDS(popraster, "FILEPATH/popraster.Rds")

  
  return(popraster)
}




disaggregate_and_resample <- function(nid, resample_polys = FALSE, vaccine_stems){

  
  
  if (resample_polys == TRUE){
    cat(paste0("======================================================\n||   DISAGGREGATE & RESAMPLE: ", nid, "\n||-- Load data\n"))
  } else {
    cat(paste0("======================================================\n||   DISAGGREGATE: ", nid, "\n||-- Load data\n"))
  }

  
  df_tabulated <- load_nid(nid, folder="tabulated")

  
  if(resample_polys == TRUE){
    data_drop_table <- fread(file.path(extraction_root, "FILEPATH", paste0(nid, ".csv")))
    data_drop_table[, n_after_resample := as.integer(n_after_resample)]
  }

  
  if (length(vaccine_stems) == 1) {
    if (vaccine_stems == "all") {
      vax_info              <- fread("FILEPATH/vaccine_info_lookup.csv")
      indicators_in_data    <- unique(df_tabulated$me_name)
      vaccines_in_data      <- indicators_in_data[!grepl("ratio", indicators_in_data)]
      ratios_in_data        <- indicators_in_data[grepl("ratio", indicators_in_data)]
      vaccine_stems_in_data <- vax_info[vaccine %in% gsub("vacc_", "", vaccines_in_data) & multi_vaccine == FALSE, unique(vaccine_stem)]
      ratios_stems_in_data  <- gsub("vacc_", "", ratios_in_data)
      vaccine_stems         <- c(vaccine_stems_in_data, ratios_stems_in_data)
    }
  }

  
  df_tabulated[, latitude := as.numeric(latitude)]
  df_tabulated[, longitude := as.numeric(longitude)]
  df_tabulated[, survey_name := gsub("/", "_", survey_name)]

  
  for (n in c("SSW", "N_obs")){
    if (!n %in% names(df_tabulated)) {
      df_tabulated[ ,(n) := NA]
    }
  }

  

  if (max(df_tabulated$year_end) >= 2000){

    
    if (resample_polys == TRUE){
      cat("||-- Disaggregate and Resample\n")
    } else {
      cat("||-- Disaggregate\n")
    }

    
    if("mcv" %in% vaccine_stems) {
      vaccine_stems <- c("mcv", vaccine_stems[which(vaccine_stems != "mcv")])
    }

    for (stem in vaccine_stems){

      
      cat(paste0("||---- ", stem, "\n"))

      
      df_input <- copy(df_tabulated)

      
      dose_vars <- unique(df_input$me_name)[grepl(stem, unique(df_input$me_name))]

      
      if (!grepl("ratio", stem)) {
        dose_vars <- dose_vars[!grepl("ratio", dose_vars)]
      }

      if (length(dose_vars) > 0) {

        
        df_input <- df_input[me_name %in% dose_vars, ]

        
        df_input <- data.table(data.table::dcast(df_input, svy_id + country + point + latitude + longitude + location_code +
                                                   shapefile + survey_name + year_start + year_end + age_year + age_bin + age_bin_agg + age_bin_agg_id +
                                                   year_id + psu + weight ~ me_name, sum, value.var = c("value", "N", "N_obs", "SSW"), fill = NA))

        
        val_vars <- paste0("value_", dose_vars)
        setnames(df_input, val_vars, dose_vars)

        if (nrow(df_input) == 0) {
          log_empty_dataset(nid, date, script = "03", step = 1, object_name = paste0("df_input (", stem, ")"))
        }


        

        
        if(stem == "dpt"){

          
          names(df_input)[names(df_input) == "vacc_dpt1"]       <- "dpt_dose_1"
          names(df_input)[names(df_input) == "vacc_dpt2"]       <- "dpt_dose_2"
          names(df_input)[names(df_input) == "vacc_dpt3"]       <- "dpt_dose_3"
          names(df_input)[names(df_input) == "N_vacc_dpt1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_dpt1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_dpt1"]   <- "SSW"

          
          if ("dpt_dose_2" %in% names(df_input)) {
            df_input[!is.na(dpt_dose_2), dpt_dose_1 := dpt_dose_1 - dpt_dose_2]
          }

          if ("dpt_dose_3" %in% names(df_input)) {
            df_input[!is.na(dpt_dose_3), dpt_dose_2 := dpt_dose_2 - dpt_dose_3]
          }
        }

        if(stem == "pcv"){

          
          names(df_input)[names(df_input) == "vacc_pcv1"]       <- "pcv_dose_1"
          names(df_input)[names(df_input) == "vacc_pcv2"]       <- "pcv_dose_2"
          names(df_input)[names(df_input) == "vacc_pcv3"]       <- "pcv_dose_3"
          names(df_input)[names(df_input) == "N_vacc_pcv1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_pcv1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_pcv1"]   <- "SSW"

          
          df_input[ , pcv_dose_1 := pcv_dose_1 - pcv_dose_2]
          df_input[ , pcv_dose_2 := pcv_dose_2 - pcv_dose_3]
        }

        if(stem == "polio"){

          
          names(df_input)[names(df_input) == "vacc_polio1"]       <- "polio_dose_1"
          names(df_input)[names(df_input) == "vacc_polio2"]       <- "polio_dose_2"
          names(df_input)[names(df_input) == "vacc_polio3"]       <- "polio_dose_3"
          names(df_input)[names(df_input) == "N_vacc_polio1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_polio1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_polio1"]   <- "SSW"

          
          df_input[ , polio_dose_1 := polio_dose_1 - polio_dose_2]
          df_input[ , polio_dose_2 := polio_dose_2 - polio_dose_3]
        }

        if(stem == "hepb"){

          
          names(df_input)[names(df_input) == "vacc_hepb1"]       <- "hepb_dose_1"
          names(df_input)[names(df_input) == "vacc_hepb2"]       <- "hepb_dose_2"
          names(df_input)[names(df_input) == "vacc_hepb3"]       <- "hepb_dose_3"
          names(df_input)[names(df_input) == "N_vacc_hepb1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_hepb1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_hepb1"]   <- "SSW"

          
          df_input[ , hepb_dose_1 := hepb_dose_1 - hepb_dose_2]
          df_input[ , hepb_dose_2 := hepb_dose_2 - hepb_dose_3]
        }

        if(stem == "hib"){

          
          names(df_input)[names(df_input) == "vacc_hib1"]       <- "hib_dose_1"
          names(df_input)[names(df_input) == "vacc_hib2"]       <- "hib_dose_2"
          names(df_input)[names(df_input) == "vacc_hib3"]       <- "hib_dose_3"
          names(df_input)[names(df_input) == "N_vacc_hib1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_hib1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_hib1"]   <- "SSW"

          
          df_input[ , hib_dose_1 := hib_dose_1 - hib_dose_2]
          df_input[ , hib_dose_2 := hib_dose_2 - hib_dose_3]
        }

        if(stem == "bcg"){

          
          names(df_input)[names(df_input) == "vacc_bcg"]       <- "bcg_dose_1"
          names(df_input)[names(df_input) == "N_vacc_bcg"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_bcg"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_bcg"]   <- "SSW"
        }

        if(stem == "yfv"){

          
          names(df_input)[names(df_input) == "vacc_yfv"]       <- "yfv_dose_1"
          names(df_input)[names(df_input) == "N_vacc_yfv"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_yfv"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_yfv"]   <- "SSW"
        }

        if(stem == "mcv"){

          
          names(df_input)[names(df_input) == "vacc_mcv1"]       <- "mcv_dose_1"
          names(df_input)[names(df_input) == "vacc_mcv2"]       <- "mcv_dose_2"
          names(df_input)[names(df_input) == "N_vacc_mcv1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_mcv1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_mcv1"]   <- "SSW"

          
          if ("mcv_dose_2" %in% names(df_input)) {
            df_input[!is.na(mcv_dose_2), mcv_dose_1 := mcv_dose_1 - mcv_dose_2]
          }
        }

        if(stem == "rota"){

          
          names(df_input)[names(df_input) == "vacc_rota1"]       <- "rota_dose_1"
          names(df_input)[names(df_input) == "vacc_rota2"]       <- "rota_dose_2"
          names(df_input)[names(df_input) == "vacc_rota3"]       <- "rota_dose_3"
          names(df_input)[names(df_input) == "vacc_rotac"]       <- "rota_dose_c"

          if("rota_dose_c" %in% names (df_input)) {
            names(df_input)[names(df_input) == "N_vacc_rotac"]     <- "N"
            names(df_input)[names(df_input) == "N_obs_vacc_rotac"] <- "N_obs"
            names(df_input)[names(df_input) == "SSW_vacc_rotac"]   <- "SSW"
          } else {
            names(df_input)[names(df_input) == "N_vacc_rota1"]     <- "N"
            names(df_input)[names(df_input) == "N_obs_vacc_rota1"] <- "N_obs"
            names(df_input)[names(df_input) == "SSW_vacc_rota1"]   <- "SSW"
          }

          
          if ("rota_dose_2" %in% names(df_input)) {
            df_input[ , rota_dose_1 := rota_dose_1 - rota_dose_2]
          }
          if ("rota_dose_3" %in% names(df_input)) {
            df_input[ , rota_dose_2 := rota_dose_2 - rota_dose_3]
          }
        }

        if(stem == "rcv"){

          
          names(df_input)[names(df_input) == "vacc_rcv1"]       <- "rcv_dose_1"
          names(df_input)[names(df_input) == "vacc_rcv2"]       <- "rcv_dose_2"
          names(df_input)[names(df_input) == "N_vacc_rcv1"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_rcv1"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_rcv1"]   <- "SSW"

          
          if ("rcv_dose_2" %in% names(df_input)) {
            df_input[!is.na(rcv_dose_2), rcv_dose_1 := rcv_dose_1 - rcv_dose_2]
          }
        }

        if(stem == "hib3_dpt3_ratio"){
          names(df_input)[names(df_input) == "N_hib3_dpt3_ratio"]      <- "N"
          names(df_input)[names(df_input) == "N_obs_hib3_dpt3_ratio"]  <- "N_obs"
          names(df_input)[names(df_input) == "SSW_hib3_dpt3_ratio"]    <- "SSW"
        }

        if(stem == "hepb3_dpt3_ratio"){
          names(df_input)[names(df_input) == "N_hepb3_dpt3_ratio"]      <- "N"
          names(df_input)[names(df_input) == "N_obs_hepb3_dpt3_ratio"]  <- "N_obs"
          names(df_input)[names(df_input) == "SSW_hepb3_dpt3_ratio"]    <- "SSW"
        }

        if(stem == "mcv2_mcv1_ratio"){
          names(df_input)[names(df_input) == "N_mcv2_mcv1_ratio"]      <- "N"
          names(df_input)[names(df_input) == "N_obs_mcv2_mcv1_ratio"]  <- "N_obs"
          names(df_input)[names(df_input) == "SSW_mcv2_mcv1_ratio"]    <- "SSW"
        }

        if(stem == "pcv3_dpt3_ratio"){
          names(df_input)[names(df_input) == "N_pcv3_dpt3_ratio"]      <- "N"
          names(df_input)[names(df_input) == "N_obs_pcv3_dpt3_ratio"]  <- "N_obs"
          names(df_input)[names(df_input) == "SSW_pcv3_dpt3_ratio"]    <- "SSW"
        }

        if(stem == "rotac_dpt3_ratio"){
          names(df_input)[names(df_input) == "N_rotac_dpt3_ratio"]      <- "N"
          names(df_input)[names(df_input) == "N_obs_rotac_dpt3_ratio"]  <- "N_obs"
          names(df_input)[names(df_input) == "SSW_rotac_dpt3_ratio"]    <- "SSW"
        }

        if(stem == "dpt3_timeliness_ratio"){
          names(df_input)[names(df_input) == "vacc_dpt3_timeliness_ratio"]       <- "dpt3_timeliness_ratio"
          names(df_input)[names(df_input) == "N_vacc_dpt3_timeliness_ratio"]     <- "N"
          names(df_input)[names(df_input) == "N_obs_vacc_dpt3_timeliness_ratio"] <- "N_obs"
          names(df_input)[names(df_input) == "SSW_vacc_dpt3_timeliness_ratio"]   <- "SSW"
        }

        if(stem == "dpt3_dpt1_ratio"){
          names(df_input)[names(df_input) == "N_dpt3_dpt1_ratio"]      <- "N"
          names(df_input)[names(df_input) == "N_obs_dpt3_dpt1_ratio"]  <- "N_obs"
          names(df_input)[names(df_input) == "SSW_dpt3_dpt1_ratio"]    <- "SSW"
        }

        
        if (grepl("ratio", stem)) {
          df_input <- subset(df_input, select = !(names(df_input) %in% c(paste0("N_", dose_vars),
                                                                         paste0("N_obs_", dose_vars),
                                                                         paste0("SSW_", dose_vars))))
        } else {
          df_input <- subset(df_input, select = !(names(df_input) %in% c(dose_vars,
                                                                         paste0("N_", dose_vars),
                                                                         paste0("N_obs_", dose_vars),
                                                                         paste0("SSW_", dose_vars))))
        }


        

        
        if (nrow(df_input[point==0, ]) > 0){
          design_effect_file_path <- "FILEPATH/design_effect_nids.csv"
          de_data   <- fread(design_effect_file_path)    
          svy_id    <- nid                               
          N_sum     <- sum(df_input[point == 0, N])      
          N_obs_sum <- sum(df_input[point == 0, N_obs])  
          de_calc   <- N_sum/N_obs_sum                   

          
          if (paste0(nid, stem) %in% unique(paste0(de_data$nid, de_data$vacc))) {
            de_data[nid == svy_id  & vacc == stem, N := N_sum]
            de_data[nid == svy_id  & vacc == stem, N_obs := N_obs_sum]
            de_data[nid == svy_id  & vacc == stem, de := de_calc]
          } else {
            de_data <- rbind(de_data,
                             data.table("nid"   = svy_id,
                                        "vacc"  = stem,
                                        "N"     = N_sum,
                                        "N_obs" = N_obs_sum,
                                        "de"    = de_calc))
          }

          
          write.csv(de_data, file = design_effect_file_path, row.names=FALSE)
        }

        

        
        if (resample_polys == TRUE) {

          if (nrow(df_input[point == 0]) > 0) {
            
            
            if (!exists("popraster")) {
              if (file.exists("FILEPATH/popraster.Rds")) {
                popraster <- readRDS("FILEPATH/popraster.Rds")
              } else {
                popraster <- load_pop_raster()
              }
            }

            df_pointpoly <- resample_polygons(data = df_input,
                                              cores = 10,
                                              indic = stem,
                                              density = 0.001,
                                              gaul_list = get_adm0_codes('all', shapefile_version = 'current'),
                                              pull_poly_method = "fast",
                                              seed = 1234)
          } else {
            
            df_pointpoly <- df_input
          }

          
          data_drop_table[grepl(stem, antigen), resampled := TRUE]
          data_drop_table[grepl(stem, antigen), n_after_resample := df_pointpoly[, sum(N * weight)]]
          write.csv(data_drop_table, file.path(extraction_root, "FILEPATH", paste0(nid, ".csv")), row.names = FALSE)

          if (nrow(df_pointpoly) == 0) {
            log_empty_dataset(nid, date, script = "03", step = 2, object_name = paste0("df_pointpoly (", stem, ")"))
          }

        } else {
          df_pointpoly <- df_input
        }

        
        
        
        

        
        vax_targets  <- set_target_ages(df_pointpoly, vaccines.=vaccines)
        setnames(vax_targets, "age_cohort", "me_cohort_schedule")

        
        vaccine_name       <- paste0("vacc_", vax_info[vaccine_stem == stem & dose == 1, vaccine])
        me_cohort_schedule <- unique(vax_targets[me_name == vaccine_name, me_cohort_schedule])

        
        df_pointpoly$me_cohort_schedule <- me_cohort_schedule

        

        
        outdir <- ifelse(resample_polys == TRUE,
                         file.path(extraction_root, "03_disaggregated", "resampled", stem),
                         file.path(extraction_root, "03_disaggregated", "not_resampled", stem))
        if (!dir.exists(outdir)) dir.create(outdir)
        write.csv(df_pointpoly, file.path(outdir, paste0(nid, ".csv")), row.names=FALSE)

        
        archive_folder <- format(Sys.time(), "%Y_%m_%d")
        dir.create(file.path(outdir, "Archive", archive_folder), recursive = T)
        write.csv(df_pointpoly, file.path(outdir, "Archive", archive_folder, paste0(nid, ".csv")), row.names=FALSE)

      } else {
        cat(paste0("||------ No dose variables, skipping..\n"))
      }
    }

    
    if (resample_polys == TRUE){
      cat(paste0("||-- Disaggregate & resample complete: ", nid, "\n"))
    } else {
      cat(paste0("||-- Disaggregate complete: ", nid, "\n"))
    }
    cat("******************************************************\n")
  } else {
    cat(paste0("||-- All pre-2000, skipping disaggregate\n"))
    cat("******************************************************\n")
  }
}

