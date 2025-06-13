


















source("FILEPATH/geocodebook_functions.R")


mbg_packages_path <- "FILEPATH/package_list.csv"
package_list      <- fread(mbg_packages_path, header = FALSE)[[1]]
suppressMessages(invisible(lapply(package_list, function(x) {
  message(x)
  library(x, character.only = TRUE)
})))


str_match <- stringr::str_match




add_ratios <- function(data, ratio_antigen_1, ratio_antigen_2) {
  vaccines_in_dataset    <- unique(data$me_name)
  ratio_antigen_1_exists <- ratio_antigen_1 %in% vaccines_in_dataset
  ratio_antigen_2_exists <- ratio_antigen_2 %in% vaccines_in_dataset

  if (ratio_antigen_1_exists & ratio_antigen_2_exists) {

    
    lhs_names <- names(data)[!names(data) %in% c("me_name", "value")]
    lhs       <- paste(lhs_names, collapse = " + ")
    rhs       <- "me_name"
    lhs_rhs   <- paste(c(lhs, rhs), collapse = " ~ ")

    data <- data.table(data.table::dcast(data, eval(parse(text = lhs_rhs))))

    
    ratio_column_name <- gsub("vacc_", "", paste0(ratio_antigen_1, "_", ratio_antigen_2, "_ratio"))
    data[, (ratio_column_name) := numeric() ]
    data[!is.na(get(ratio_antigen_2)) & get(ratio_antigen_2) != 0 & !is.na(get(ratio_antigen_1)), (ratio_column_name) := get(ratio_antigen_1) / get(ratio_antigen_2)]

    
    vax_cols     <- names(data)[grep("vacc|ratio", names(data))]
    id_variables <- names(data)[!names(data) %in% vax_cols]
    data <- melt.data.table(data, id.vars=id_variables, measure.vars = vax_cols, variable.name="me_name", variable.factor = F, value.name = "value")

    
    data <- data[!is.na(value), ]
  }
  return(data)
}




tabulate_lbd <- function(nid, collapse_method="kish") {

  
  
  cat(paste0("======================================================\n||   TABULATE (LBD): ", nid, "\n"))

  
  limited_gps_access_nids <- c('505452')
  
  if (nid %in% limited_gps_access_nids) {
    combined_table <- fread(file.path(extraction_root, "geomatch", paste0(nid, ".csv")))
    geo_keep <- c("nid", "team", "data_type", "iso3", "geospatial_id", "point", "lat", "long", "shapefile", "location_code", "location_name", "admin_level", "survey_series")
    combined_table <- combined_table[, geo_keep, with=F]
    setorder(combined_table, "nid", "team", "data_type", "geospatial_id")
    geo <- append_shapefile_type(combined_table)
  } else {
    geo <- suppressMessages(get_geocodebooks(nids = nid))
  }

  
  cat("||-- Load and Prep Data\n")
  dataset <- prep_for_tabulation(nid, team="lbd", vaccines.=c(vaccines, "rotac", "dpt3_timeliness_ratio"))

  
  
  if (dataset[[1]] != FALSE){
    data            <- dataset[[2]]
    data_drop_table <- dataset[[3]]

    if (nrow(data) == 0) {
      log_empty_dataset(nid, date, script = "02_lbd", step = 1, object_name = "data")
    }

    
    for (variable in c("pweight", "psu", "strata")){
      if (!variable %in% names(data)){
        data[ ,(variable):=NA]
      }
    }

    if(max(data$year_end) >= 2000){  

      
      
      cat("||-- Geomatch Data\n")

      
      data$geospatial_id <- as.character(data$geospatial_id)
      data$ihme_loc_id   <- as.character(data$ihme_loc_id)
      data$ihme_loc_id   <- substr(data$ihme_loc_id, 0, 3)
      setnames(geo, "iso3", "ihme_loc_id")
      geo$ihme_loc_id    <- substr(geo$ihme_loc_id, 0, 3)
      geo$geospatial_id  <- as.character(geo$geospatial_id)

      
      data <- merge(geo, data,
                    by=c("nid", "geospatial_id", "ihme_loc_id"),
                    all.x=FALSE, all.y=TRUE, allow.cartesian=TRUE)

      
      rename_table <- data.table(rbind(c("ihme_loc_id", "country"),
                                       c("nid", "svy_id"),
                                       c("geospatial_id", "geo_id"),
                                       c("lat", "latitude"),
                                       c("long", "longitude"),
                                       c("loc_name1", "admin1_name"),
                                       c("loc_code1", "loc_code_admin1"),
                                       c("admin_level1", "admin_level1"),
                                       c("point", "point")))
      names(rename_table) <- c("old", "new")
      invisible(lapply(1:nrow(rename_table), function(x) check_and_rename_col(data, rename_table$old[x], rename_table$new[x])))
      if (nrow(data) == 0) {
        log_empty_dataset(nid, date, script = "02_lbd", step = 2, object_name = "data")
      }

      

      
      if (class(data$latitude)  != "numeric") data[, latitude := as.numeric(as.character(latitude))]
      if (class(data$longitude) != "numeric") data[, longitude := as.numeric(as.character(longitude))]
      if (class(data$strata)    != "numeric") data[, strata := as.numeric(as.character(strata))]
      if (class(data$pweight)   != "numeric") data[, pweight := as.numeric(as.character(pweight))]
      if ("cluster_id" %in% names(data))      data[, cluster_id := NULL]

      
      data[, country := tstrsplit(country, "_")[1]]

      
      data[shapefile == "", shapefile := NA]

      
      data$shapefile <- as.character(data$shapefile)

      
      shapefile_list       <- unique(data[!is.na(shapefile), shapefile])
      shapefile_dir        <- "FILEPATH"
      shapefiles_available <- list.files(shapefile_dir, ".shp$") %>% gsub(".shp", "", .)
      no_shapefile         <- shapefile_list[!(shapefile_list %in% shapefiles_available)]

      if (length(no_shapefile) > 0) {
        cat("The following shapefiles are missing from the shapefile directory. Associated data will be dropped:\n")
        cat(paste0(paste0("  ", no_shapefile, ".shp"), collapse = " \n"), "\n")
        data <- data[!(shapefile %in% no_shapefile), ]

        if (nrow(data) == 0) {
          log_empty_dataset(nid, date, script = "02_lbd", step = 3, object_name = "data")
        }
      }

      
      geo_missingness <- nrow(data[is.na(latitude) & is.na(shapefile)]) / nrow(data)
      vet_log         <- fread(file.path(extraction_root, "log/details", paste0(nid, ".csv")))
      vet_log[, missingness_geomatch := geo_missingness]
      write.csv(vet_log, file.path(extraction_root, "log/details", paste0(nid, ".csv")), row.names=FALSE)

      
      data[is.na(point) & !is.na(latitude) & !is.na(longitude), point := 1]
      data[is.na(point) & !is.na(shapefile) & !is.na(pweight),  point := 0]

      
      data <- data[!(is.na(latitude) & is.na(shapefile))] 
      data <- data[!(is.na(latitude) & is.na(pweight))]   
      data <- data[!is.na(year_id), ]                     
      data <- data[!is.na(value), ]                       

      
      geo_missingness <- data[, .N, by = me_name]
      geo_missingness <- geo_missingness[me_name %in% paste0("vacc_", data_drop_table$antigen), ]
      geo_missingness[, antigen := gsub("vacc_", "", me_name)]
      data_drop_table <- merge(data_drop_table, geo_missingness[, .(antigen, N)], all.x = TRUE, all.y = FALSE)
      data_drop_table[, n_after_geomatch := N]
      data_drop_table$N <- NULL
      setcolorder(data_drop_table, c("nid", "total", "n_with_age_data", "n_0_59_months", "antigen"))
      write.csv(data_drop_table, file = paste0(extraction_root, "FILEPATH", nid, ".csv"), row.names = FALSE)

      if (nrow(data) == 0) {
        log_empty_dataset(nid, date, script = "02_lbd", step = 4, object_name = "data")
      }

      
      
      cat("||-- Collapse Data\n")

      
      data[, N := 1]

      
      df_point <- data[point == 1]
      df_poly  <- data[point == 0]

      
      if (nrow(df_point) > 0) {
        keep_vars <- c("svy_id", "survey_name", "country", "point", "svy_year", "year_start", "year_end",
                       "location_code", "shapefile",
                       "year_id", "age_year", "age_bin", "age_bin_agg", "age_bin_agg_id",
                       "psu", "latitude", "longitude", "outlier", "me_name", "value", "N")
        df_point <- subset(df_point, select = names(df_point) %in% keep_vars)
        df_point[, N_obs := 1]

        
        df_point <- df_point[, lapply(.SD, sum),
                             by = .(svy_id, survey_name, country, point,
                                    location_code, shapefile,
                                    year_start, year_end, year_id, age_year, age_bin, age_bin_agg, age_bin_agg_id,
                                    psu, latitude, longitude, me_name),
                             .SDcols = c("value", "N", "N_obs")]
      }

      
      if (nrow(df_poly) > 0) {
        if (collapse_method == "kish") {
          
          
          df_poly <- df_poly[, c(list(N_obs = sum(N),
                                      SSW = sum(pweight),
                                      N = sum(pweight) ^ 2 / sum(pweight ^ 2)),
                                 lapply(.SD, FUN = weighted.mean, w = pweight)),
                             by = list(svy_id, survey_name, country, point,
                                       location_code, shapefile,
                                       year_start, year_end, year_id, age_year, age_bin, age_bin_agg, age_bin_agg_id, me_name),
                             .SDcols = "value"]

          
          df_poly[, ("value") := lapply(.SD, '*', N), .SDcols = "value"]

        } else {
          
          stop(paste0("Polygon collapse is only configured with the 'kish' method. '", collapse_method, "' method isn't configured"))
        }
      }

      
      df_pointpoly <- rbind(df_point, df_poly, fill=TRUE)
      if ("age_month" %in% names(df_pointpoly)) df_pointpoly$age_month <- NULL

      
      df_pointpoly$weight <- 1

      if (nrow(df_pointpoly) == 0) {
        log_empty_dataset(nid, date, script = "02_lbd", step = 5, object_name = "df_pointpoly")
      }

      
      
      cat("||-- Add Vaccine Ratios\n")

      df_pointpoly[, value := as.double(value)]

      df_pointpoly <- add_ratios(df_pointpoly, "vacc_mcv2",  "vacc_mcv1")
      df_pointpoly <- add_ratios(df_pointpoly, "vacc_pcv3",  "vacc_dpt3")
      df_pointpoly <- add_ratios(df_pointpoly, "vacc_rotac", "vacc_dpt3")
      df_pointpoly <- add_ratios(df_pointpoly, "vacc_hib3",  "vacc_dpt3")
      df_pointpoly <- add_ratios(df_pointpoly, "vacc_hepb3", "vacc_dpt3")
      df_pointpoly <- add_ratios(df_pointpoly, "vacc_dpt3", "vacc_dpt1")


      
      
      archive_folder <- format(Sys.time(), "%Y_%m_%d")
      dir.create(file.path(extraction_root, "FILEPATH", "Archive", archive_folder), recursive = T)
      write.csv(df_pointpoly, file.path(extraction_root, "FILEPATH", "Archive", archive_folder, paste0(nid, ".csv")), row.names=FALSE)

      
      cat("||-- Save Data\n")
      write.csv(df_pointpoly, paste0(extraction_root, "FILEPATH", nid, ".csv"), row.names=FALSE)
      cat(paste0("||-- Tabulate LBD Complete: ", nid, "\n"))
      cat("******************************************************\n")

    } else {

      cat(paste0(nid, ": All data is pre-2000, skipping tabulation\n"))
    }
  } else {
    cat(paste0(nid, ": Tabulation prep failed\n"))
    cat("\ntab_prep_failed\n", file=file.path(extraction_root, "log/datestamp", date, paste0(nid, ".txt")), append=TRUE)
  }
}


