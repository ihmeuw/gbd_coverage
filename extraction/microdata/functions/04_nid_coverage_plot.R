








gadm_shape_dir <- "FILEPATH"

if(!exists("gadm_ad0")) { gadm_ad0 <- readOGR(paste0(gadm_shape_dir, "gadm36_0.shp")) }
if(!exists("gadm_ad1")) { gadm_ad1 <- readOGR(paste0(gadm_shape_dir, "gadm36_1.shp")) }
if(!exists("gadm_ad2")) { gadm_ad2 <- readOGR(paste0(gadm_shape_dir, "gadm36_2.shp")) }



source("FILEPATH/get_location_metadata.R")
if(!exists("locations")) locations <- get_location_metadata(location_set_id=22, gbd_round_id=6)[level >= 3, ]

plot_nid_coverage <- function(nid,
                              covs = c("mcv1_cov", "dpt3_cov", "dpt1_cov", "pcv3_cov", "hib3_cov", "hepb3_cov", "polio3_cov", 
                                       "rotac_cov", "bcg_cov", "yfv_cov", "rcv1_cov"), 
                              path = "FILEPATH") { 
  
  
  
  
  fn <- paste0(extraction_root, "FILEPATH", nid, ".csv")
  
  if (file.exists(fn)) {
    
    df_pointpoly <- fread(fn)
    if (nrow(df_pointpoly) > 0) {
      
      
      str_match <- stringr::str_match
      
      
      geom_polygon_quiet <- function(...) {suppressMessages(ggplot2::geom_polygon(...))}
      geom_path_quiet    <- function(...) {suppressMessages(ggplot2::geom_path(...))}
      
      log <- fread(file.path(extraction_root, "log/details", paste0(nid, ".csv")))

      
      
      
      
      
      
      df_pointpoly[, latitude := as.numeric(latitude)]
      df_pointpoly[, longitude := as.numeric(longitude)]

      df_pointpoly$survey_name <- gsub("/", "_", df_pointpoly$survey_name)
      
      
      
      max_y <- max(df_pointpoly$year_id)
      df_pointpoly[, year_id := max_y]
      
      for (vax in covs) {
        df <- copy(df_pointpoly)
        vax_prefix <- str_match(vax, "^([a-z]*)[0-9]?_cov")[2]
        if(vax == "rotac_cov"){ vax_prefix <- "rota" }
        vax_dose <- str_match(vax, "^[a-z]*([0-9]?)_cov")[2] %>% as.numeric

        if(vax_prefix %in% c("bcg", "yfv")){ vax_dose <- ""  }
        if(vax_prefix == "rota"){ vax_dose = "c" }
        
        
        out_dir <- paste0(path, "/vacc_", gsub("_cov", "", vax), "/")
        if (!dir.exists(out_dir)) dir.create(out_dir, showWarnings = F, recursive = T)
        
        
        
        dose_vars <- unique(df$me_name)[grepl(vax_prefix, unique(df$me_name))]
        if(vax == "mcv1_cov" & length(dose_vars) > 0){ dose_vars <- "vacc_mcv1" } 
        if (length(dose_vars) > 0) {
          
          
          
          
          df <- df[me_name %in% dose_vars]
          
          idvars <- c(names(df)[!names(df) %in% c("me_name", "value")])
          
          df <- data.table(data.table::dcast(df, svy_id + country + point + latitude + longitude + location_code +
                                               shapefile + survey_name + year_id + psu + weight ~ me_name, sum,value.var = c("value", "N")))
          
          val_vars <- paste0("value_", dose_vars)
          N_vars <- paste0("N_", dose_vars)
          names(df)[names(df) %in% val_vars] <- dose_vars
          names(df)[names(df) %in% N_vars] <- c(rep("drop", length(N_vars)-1), "N")
          if(is.na(vax_dose)) { cov_var <- paste0("vacc_",vax_prefix) } else { cov_var <-  paste0("vacc_",vax_prefix, vax_dose) }
          
          if (cov_var %in% names(df)) {
            
            
            outlier_fn <- "FILEPATH/vaccination.csv"
            outlier <- data.table(read.csv(outlier_fn))
            outlier <- outlier[lbd == 1 & (me_name == "" | me_name %in% dose_vars),] 
            is_outlier <- nid %in% unique(outlier$nid)
            
            df[, tmp := get(cov_var)] 
            
            
            df <- subset(df, select = !(names(df) %in% dose_vars))
            df <- subset(df, select = !(names(df) == "drop"))
            
            setnames(df, "tmp", vax)
            
            
            df <- df[, list(N = sum(N), outcome = sum(get(vax))),
                     by = setdiff(names(df), c("N", vax))]
            
            
            setnames(df, "outcome", vax)
            
            
            df[, eval(vax) := get(vax) / N]
            
            
            df[survey_name == "GLOBAL_FUND_HOUSEHOLD_SURVEY", survey_name := "GLOBAL_FUND"]
            df[survey_name == "ARAB_LEAGUE_PAPFAM", survey_name := "ARAB_LG_PAPFAM"]
            df[survey_name == "MACRO_DHS_SP", survey_name := "MACRO_DHS"]
            df[survey_name == "WB_LSMS_ISA", survey_name := "WB_LSMS"]
            
            if("original_year" %in% names(df)) {
              setnames(df, "original_year", "year")
            }
            setnames(df, "svy_id", "nid")
            
            df[, cluster_id := .I]
            df[location_code == "" | location_code == "
            df[, location_code := as.numeric(location_code)]
            
            
            drop_shapefiles <- c("CRI_Pavas", 
                                 "CRI_Tambor",
                                 "CRI_Ulloa") 
            df <- subset(df, !(shapefile %in% drop_shapefiles))
            
            
            
            
            df[survey_name == "MACRO_DHS", source_label := "Demographic and Health Survey"]
            df[survey_name == "UNICEF_MICS", source_label := "UNICEF Multiple Indicator Cluster Survey"]
            df[survey_name == "WB_CWIQ", source_label := "World Bank Core Welfare Indicator Questionnaire Survey"]
            df[is.na(source_label), source_label := survey_name]
            
            
            nid_table <- unique(subset(df, select = c("nid", "survey_name", "source_label", "country", "year_id")))
            nid_table <- nid_table[order(year_id)]
            nid_table <- nid_table[nrow(nid_table), ]
            
            
            
            
            
            
            
            
            
  
            gadm_ad0_shape <- subset(gadm_ad0, GID_0 == df$country[1])
            gadm_ad1_shape <- subset(gadm_ad1, GID_0 == df$country[1])
            gadm_ad2_shape <- subset(gadm_ad2, GID_0 == df$country[1])
            
            year_list <- 1980:2019 
            
            
            
            vals <- c(1.0,       0.8,        0.6,      0.496094,   0.4,       0.2,       0.164063,  0.000000)
            cols <- c("
            
            
            
            message("Plotting...")
            
            the_nid <- nid_table$nid
            the_source <- nid_table$survey_name
            the_source_label <- nid_table$source_label
            the_year <- nid_table$year_id
            if (paste0("missingness_", vax_prefix) %in% names(log)) {
              mvax <- round(log[ ,get(paste0("missingness_", vax_prefix))], 2)
            } else {
              mvax <- 0
            }
            
            message(paste0(" --> ", the_source_label, ": ", the_year))
            fn <- paste(nid_table$country, the_source, the_year, the_nid, ".png", sep="_")
            
            fn <- paste0(out_dir, substr(fn, 1,nchar(fn)-5) ,substr(fn, nchar(fn)-3,nchar(fn)) )
            png(fn, width = 1500, height = 900)
            
            df_nid <- subset(df, nid == the_nid)
            df_nid_point <- subset(df_nid, !is.na(latitude))
            df_nid_poly <- subset(df_nid, !is.na(location_code))
            
            
            if (nrow(df_nid_poly) > 0) {
              shpfiles <- unique(df_nid_poly$shapefile)
              shape_list <- lapply(shpfiles, function(shpfile) {
                the_spdf <- readRDS(paste0("FILEPATH", shpfile, ".rds"))
                the_df <- df_nid_poly[shapefile == shpfile]
                the_spdf <- subset(the_spdf, GAUL_CODE %in% unique(the_df$location_code))
                the_df <- the_df[, .(vax = weighted.mean(get(vax), w = weight*N)), by = location_code]
                names(the_df)[names(the_df) == "vax"] <- vax
                the_spdf <- merge(the_spdf, the_df, by.x = "GAUL_CODE", by.y = "location_code")
                
                the_spdf@data$id <- rownames(the_spdf@data)
                the_spdf.points = fortify(the_spdf, region="id")
                the_spdf.df <- join(the_spdf.points, the_spdf@data, by="id") %>% as.data.table
                return(the_spdf.df)
              })
              df_plot_poly <- rbindlist(shape_list, fill=T)
            }
            
            
            gg_svy <- ggplot() +
              geom_path_quiet(data = gadm_ad1_shape,
                              aes(x=long, y=lat, group=group),
                              size = 0.2,
                              color = "black") +
              geom_path_quiet(data = gadm_ad0_shape,
                              aes(x=long, y=lat, group=group),
                              size = 1,
                              color = "black")
            
            if (nrow(df_nid_poly) > 0) {
              gg_svy <- gg_svy +
                geom_polygon_quiet(data = df_plot_poly,
                                   aes(x=long, y=lat, group=group, fill = get(vax))) +
                geom_path_quiet(data = df_plot_poly,
                                aes(x=long, y=lat, group=group),
                                size = 0.2, color = "black") +
                labs(fill = paste0(toupper(vax_prefix), vax_dose, " Coverage")) +
                scale_fill_gradientn(colors = cols, values = vals,
                                     breaks = c(0,0.2,0.4,0.6,0.8,1),
                                     limits = c(0,1),
                                     labels = scales::percent)
            }
            
            if (nrow(df_nid_point) > 0) {
              gg_svy <- gg_svy +
                geom_point(data = df_nid_point,
                           aes(x = longitude,
                               y = latitude,
                               size = N,
                               color = get(vax))) +
                labs(color = paste0(toupper(vax_prefix), vax_dose, " Coverage"),
                     size = "Number of Children\nin Cluster") +
                scale_size_area() +
                scale_color_gradientn(colors = cols, values = vals,
                                      breaks = c(0,0.2,0.4,0.6,0.8,1),
                                      limits = c(0,1),
                                      labels = scales::percent)
            }
            
            gg_svy <- gg_svy +
              geom_path_quiet(data = gadm_ad0_shape,
                              aes(x=long, y=lat, group=group),
                              size = 1,
                              color = "black") +
              
              
              theme_void() +
              coord_equal() +
              
              theme(legend.text = element_text(size = 15),
                    legend.title = element_text(size = 18),
                    plot.title = element_text(size = 20),
                    legend.key.height = unit(3,"line"),
                    legend.key.width = unit(2.3, "line"))
            
            g_legend<-function(a.gplot){
              tmp <- ggplot_gtable(ggplot_build(a.gplot))
              leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
              legend <- tmp$grobs[[leg]]
              return(legend)}
            
            
            the_legend <- g_legend(gg_svy)
            
            if(!paste0(vax_prefix, "_card_fraction") %in% names(log)){
              if("card_fraction" %in% names(log)) { cf <- round(log$card_fraction, 2) } else { cf <- "NA" }
            } else {
              cf <- round(log[ ,get(paste0(vax_prefix, "_card_fraction"))], 2)
            }

            text1 = paste(paste0("\nCountry:", "\n\n"),
                          paste0("\nYear:", "\n"),
                          paste0("\nNID:", "\n"),
                          paste0("\nObservations:", "\n"),
                          paste0("\nCard fraction:", "\n"),
                          paste0("\nMissing vaccine:", "\n"),
                          paste0("\nMissing age:", "\n"),
                          paste0("\nMissing geomatch:", "\n"),
                          paste0("\nMissing weight:", "\n"),
                          paste0("\nOutliered:", "\n"))
            
            text2 = paste(paste0("\n", locations[ihme_loc_id==unique(log$ihme_loc_id), location_name_short], "\n(", unique(log$ihme_loc_id), ")", "\n"),
                          paste0("\n", the_year, "\n"),
                          paste0("\n", the_nid, "\n"),
                          paste0("\n", log$sample_size, "\n"),
                          paste0("\n", cf * 100, "%\n"),
                          paste0("\n", mvax * 100, "%\n"),
                          paste0("\n", round(log$missingness_age, 2) * 100, "%\n"),
                          paste0("\n", round(log$missingness_geo, 2) * 100, "%\n"),
                          paste0("\n", round(log$missingness_pweight, 2) * 100, "%\n"),
                          paste0("\n", as.character(is_outlier), "\n"))
            
            text1 <- textGrob(text1, just="left",
                              gp = gpar(fontsize = 16, fontface = "bold", family="mono"))
            
            text2 <- textGrob(text2, just="left",
                              gp = gpar(fontsize = 16, fontface = "bold", family="mono"))
            
            title_grob <- textGrob(paste0(the_source_label, ": ", the_year), gp = gpar(fontsize = 21, fontface = "bold"))
            
            
            gg_svy <- gg_svy + theme(legend.position = "none")
            
            lay <- rbind(c(1,1,1,1,1,1,1,1, 1,1,1,1),
                         c(2,2,2,2,2,2,5,NA,3,4,4,4))
            
            
            plot_all <- arrangeGrob(title_grob, gg_svy, text1, text2, the_legend,
                                    layout_matrix = lay,
                                    heights = c(0.3,5))
            grid.draw(plot_all)
            dev.off()
            
            message(paste0("Plotted ", vax, " in ", fn))
          

          } else {
            message(paste0(nid, ": no ", vax, " data, skipping"))
          }
        } else {
          message(paste0(nid, ": no ", vax, " data, skipping"))
        }
      }
    } else{
      message(paste0(nid, ": no data, skipping"))
    }
    
  } else {
    message(paste0(nid, ": missing file, skipping"))
  }
}