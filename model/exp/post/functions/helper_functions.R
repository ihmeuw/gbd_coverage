








#' @param ad_level 0, 1, or 2
#' @param shapefile_version shapefile version being used
#' @param adm0_list adm0 codes used to subset the shapefile (if applicable)

#' @return a sf object for use with `geom_sf()` and `ggplot()`

prep_ad_shp_for_mapping <- function(ad_level, 
                                    shapefile_version = "2018_08_01",
                                    adm0_list = NULL) {
   message(paste0("Loading admin ", ad_level, " shapefile..."))
   ad_shp <- sf::st_read(get_admin_shapefile(admin_level = ad_level, version = shapefile_version))
   if (!is.null(adm0_list)) ad_shp <- subset(ad_shp, ADM0_CODE %in% adm0_list)
   return(ad_shp)
}





#' @param raster_file file path to raster layer
#' @param raster_obj raster layer (as raster object)
#' @param template Raster* object to use as a template for cropping, masking, etc.

#' @return a data.table for use in ggplot with columns "longitude", "latitude", and "value"

prep_raster_for_mapping <- function(raster_file = NULL, raster_obj = NULL, template = NULL) {
   if (is.null(raster_file) & is.null(raster_obj)) stop("Must specify either raster_file or ras")
   if (is.null(raster_obj)) raster_obj <- raster(raster_file) 
   if (!is.null(template)) {
      raster_obj <- extend(raster_obj, template)
      raster_obj <- crop(raster_obj, template)
      extent(raster_obj) <- alignExtent(extent(raster_obj), template, snap = "near")
      raster_obj <- mask(raster_obj, template)  
   }
   df_ras<- rasterToPoints(raster_obj) %>% as.data.table
   names(df_ras) <- c("longitude", "latitude", "value")
   return(df_ras)
}




#' @param ... additional arguments for `scale_fill_gradientn()`

scale_fill_vaccine <- function(...) {
   
   
   
   
   
   
   
   
   
   
   vals <- c(1.0,       0.8,        0.53,      0.47,      0.4,       0.25,      0.10,      0.000000)
   cols <-  c("#4d004b", "#5378b2",  "#dbecf7", "#feefc3", "#fee191", "#FC8D58", "#b03027", "#711f19")
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   return(scale_fill_gradientn(colors = cols, values = vals, 
                               breaks = c(0,0.2,0.4,0.6,0.8,1),
                               limits = c(0,1),
                               labels = scales::percent,
                               ...))
}




#' @param ... additional arguments for `scale_color_gradientn()`

scale_color_vaccine <- function(...) {
   
   
   
   
   
   vals <- c(1.0,       0.8,        0.53,      0.47,      0.4,       0.25,      0.10,      0.000000)
   c("#4d004b", "#5378b2",  "#dbecf7", "#feefc3", "#fee191", "#FC8D58", "#b03027", "#711f19")
   
   
   return(scale_color_gradientn(colors = cols, values = vals, 
                                breaks = c(0,0.2,0.4,0.6,0.8,1),
                                limits = c(0,1),
                                labels = scales::percent,
                                ...))
}



scale_fill_diff <- function(...) {
   return(scale_fill_gradient2(low="#00441b", high="#40004b",mid="white",midpoint=0))
}




theme_empty <- function(...) {
   theme_empty <- theme_classic(base_size = 16, ...) +      
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(), 
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            text = element_text(family = "Lato"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            plot.caption = element_text(hjust = 0.5))
   return(theme_empty)                                                
}                                                                    















#' @param ind indicator
#' @param ig indicator group
#' @param rd run_date
#' @param shapefile_version shapefile version used for this run
#' @param yl year_list. must match up with run
#' @param plot_title title for the plot
#' @param adm0_list if a vector of adm0 codes is passed here, then the



#' @param legend title title for the legend
#' @param stat statistic to plot (i.e. mean, upper, lower, etc)
#' @param raked used raked raster or not?
#' @param mask_output should the standard population/aridity map be applied?
#' @param lakes_and_rivers should the mask for rivers and lakes be applied?
#' @param ad1_borders draw admin 1 borders?
#' @param ad2_borders draw admin 2 borders?

#' @return a list of ggplot objects, one for each year in `year_list`

map_raster <- function(ind = indicator,
                       ig = indicator_group,
                       rd = run_date,
                       shapefile_version = "2018_08_01",
                       yl = year_list,
                       adm0_list = NULL,
                       plot_title,
                       legend_title,
                       stat = "mean",
                       raked,
                       mask_output = T,
                       lakes_and_rivers = T,
                       ad1_borders = T,
                       ad2_borders = T) {
   
   
   sharedir <- paste0("FILEPATH", ig, "/", ind, "FILEPATH", rd, "/")
   ras_file <- paste0(sharedir, ind, "_", stat, ifelse(raked, "_raked", ""), "_raster.tif")
   
   
   if (!dir.exists(sharedir)) stop("No directory found for this indicator and run date")
   if (!file.exists(ras_file)) stop("Raster file for this statistic and indicator not found")
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- brick(ras_file)
   if (nlayers(ras) != length(year_list)) stop("Length of year list and number of layers in raster brick don't match")
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   gg_list <- lapply(1:length(year_list), function(i) {
      
      yr <- year_list[i]
      df_yr_ras <- prep_raster_for_mapping(raster_obj = ras[[i]])
      
      message(paste0("   ", yr))
      
      gg <- ggplot() +
         geom_raster(data = df_yr_ras,
                     aes(x = longitude, y = latitude, fill = value))
      
      
      if (mask_output){
         gg <- gg + geom_raster(data = df_msk, 
                                aes(x = longitude, y = latitude), fill = "gray80")
      }
      
      if (lakes_and_rivers) {
         gg <- gg + geom_raster(data = df_lks, 
                                aes(x = longitude, y = latitude), fill = "white")
      }
      
      if (ad2_borders) {
         gg <- gg + geom_sf(data = ad2_shp,
                            color = "black",
                            lwd = 0.1,
                            fill = NA)
      }
      
      if (ad1_borders) {
         gg <- gg + geom_sf(data = ad1_shp,
                            color = "black",
                            lwd = 0.3,
                            fill = NA)
      }
      
      
      gg <- gg +
         geom_sf(data = ad0_shp,
                 color = "black",
                 lwd = 0.7,
                 fill = NA) +
         theme_empty() +
         scale_fill_vaccine() +
         coord_sf(datum = NA) +
         labs(fill = legend_title,
              title = plot_title, 
              
              subtitle = yr) +
         theme(title = element_text(size = 32)) +
         theme(legend.text=element_text(size=16))
      
      
      
      
      
      
      
      return(gg)
      
   })
   
   names(gg_list) <- paste0(ind, "_", year_list)
   
   return(gg_list)
   
}




map_raster_gavi <- function(ind = indicator,
                            ig = indicator_group,
                            rd = run_date,
                            shapefile_version = "2018_08_01",
                            yl = year_list,
                            adm0_list = NULL,
                            plot_title,
                            legend_title,
                            stat = "mean",
                            raked,
                            mask_output = T,
                            lakes_and_rivers = T,
                            ad1_borders = T,
                            ad2_borders = T) {
   
   if(indicator=='dpt1_cov'){
      yl = 2017
      year_list=c(2017)
   }
   
   sharedir <- paste0("FILEPATH", ig, "/", ind, "FILEPATH", rd, "/")
   ras_file <- paste0(sharedir, ind, "_", stat, ifelse(raked, "_raked", ""), "_raster.tif")
   if(indicator=='dpt1_cov'){
      ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   }
   
   if (!dir.exists(sharedir)) stop("No directory found for this indicator and run date")
   if (!file.exists(ras_file)) stop("Raster file for this statistic and indicator not found")
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- brick(ras_file)
   if (nlayers(ras) != length(year_list)) stop("Length of year list and number of layers in raster brick don't match")
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   gg_list <- lapply(1:length(year_list), function(i) {
      
      yr <- year_list[i]
      df_yr_ras <- prep_raster_for_mapping(raster_obj = ras[[i]])
      
      message(paste0("   ", yr))
      
      gg <- ggplot() +
         geom_raster(data = df_yr_ras,
                     aes(x = longitude, y = latitude, fill = value))
      
      
      if (mask_output){
         gg <- gg + geom_raster(data = df_msk, 
                                aes(x = longitude, y = latitude), fill = "gray80")
      }
      
      if (lakes_and_rivers) {
         gg <- gg + geom_raster(data = df_lks, 
                                aes(x = longitude, y = latitude), fill = "white")
      }
      
      if (ad2_borders) {
         gg <- gg + geom_sf(data = ad2_shp,
                            color = "black",
                            lwd = 0.1,
                            fill = NA)
      }
      
      if (ad1_borders) {
         gg <- gg + geom_sf(data = ad1_shp,
                            color = "black",
                            lwd = 0.3,
                            fill = NA)
      }
      
      
      gg <- gg +
         geom_sf(data = ad0_shp,
                 color = "grey42",
                 lwd = 0.5,
                 fill = NA) +
         theme_empty() +
         scale_fill_vaccine() +
         coord_sf(datum = NA) +
         labs(fill = legend_title,
              title = paste0('2017 ',plot_title),
              caption = " ") 
      
      
      
      
      
      
      
      
      
      return(gg)
      
   })
   
   names(gg_list) <- paste0(ind, "_", year_list)
   
   return(gg_list)
   
}





map_raster_gavi_dpt1 <- function(ind = indicator,
                                 ig = indicator_group,
                                 rd = run_date,
                                 shapefile_version = "2018_08_01",
                                 adm0_list = NULL,
                                 plot_title,
                                 legend_title,
                                 stat = "mean",
                                 raked,
                                 mask_output = T,
                                 lakes_and_rivers = T,
                                 ad1_borders = T,
                                 ad2_borders = T) {
   
   
   ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- raster(ras_file)
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   df_yr_ras <- prep_raster_for_mapping(raster_obj = ras)
   
   
   gg <- ggplot() +
      geom_raster(data = df_yr_ras,
                  aes(x = longitude, y = latitude, fill = value))
   
   
   if (mask_output){
      gg <- gg + geom_raster(data = df_msk, 
                             aes(x = longitude, y = latitude), fill = "gray80")
   }
   
   if (lakes_and_rivers) {
      gg <- gg + geom_raster(data = df_lks, 
                             aes(x = longitude, y = latitude), fill = "white")
   }
   
   
   
   gg <- gg +
      geom_sf(data = ad0_shp,
              color = "grey42",
              lwd = 0.5,
              fill = NA) +
      theme_empty() +
      scale_fill_vaccine() +
      coord_sf(datum = NA) +
      labs(fill = legend_title,
           title = paste0('2017 ',plot_title),
           caption = " ") 
   
   
   
   
   
   
   
   
   
   return(gg)
   
}










map_raster_gavi_who_scale <- function(ind = indicator,
                                      ig = indicator_group,
                                      rd = run_date,
                                      shapefile_version = "2018_08_01",
                                      yl = year_list,
                                      adm0_list = NULL,
                                      plot_title,
                                      legend_title,
                                      stat = "mean",
                                      raked,
                                      mask_output = T,
                                      lakes_and_rivers = T,
                                      ad1_borders = T,
                                      ad2_borders = T) {
   
   if(indicator=='dpt1_cov'){
      yl = 2017
      year_list=c(2017)
   }
   
   sharedir <- paste0("FILEPATH", ig, "/", ind, "FILEPATH", rd, "/")
   ras_file <- paste0(sharedir, ind, "_", stat, ifelse(raked, "_raked", ""), "_raster.tif")
   if(indicator=='dpt1_cov'){
      ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   }
   
   if (!dir.exists(sharedir)) stop("No directory found for this indicator and run date")
   if (!file.exists(ras_file)) stop("Raster file for this statistic and indicator not found")
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- brick(ras_file)
   if (nlayers(ras) != length(year_list)) stop("Length of year list and number of layers in raster brick don't match")
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   gg_list <- lapply(1:length(year_list), function(i) {
      
      yr <- year_list[i]
      df_yr_ras <- prep_raster_for_mapping(raster_obj = ras[[i]])
      
      
      
      df_yr_ras$bin <- ifelse(0<df_yr_ras$value & df_yr_ras$value<0.50, "0-49%", ifelse(0.5<df_yr_ras$value & df_yr_ras$value<0.80, "50-79%", ifelse(0.8<df_yr_ras$value & df_yr_ras$value<0.9, "80-89%", ifelse(df_yr_ras$value>0.90 & df_yr_ras$value <1, "90-100%", NA))))
      
      dynamic_bins <- sort(unique(df_yr_ras$bin))
      if(sum(is.na(unique(df_yr_ras$bin)))>0){
         dynamic_bins <- c(dynamic_bins, NA)
      }
      dynamic_colors <- rep(NA, length(dynamic_bins))
      for(i in 1:length(dynamic_bins)){
         dynamic_colors[i] <- ifelse(dynamic_bins[i]=="0-49%", "#cc0000", ifelse(dynamic_bins[i]=="50-79%", "#ffff4d", ifelse(dynamic_bins[i]=="80-89%", "#63e963", ifelse(dynamic_bins[i]=="90-100%", "#158415", "grey")) ))
         
      }
      
      message(paste0("   ", yr))
      
      gg <- ggplot() +
         geom_raster(data = df_yr_ras,
                     aes(x = longitude, y = latitude, fill = bin))
      
      
      if (mask_output){
         gg <- gg + geom_raster(data = df_msk, 
                                aes(x = longitude, y = latitude), fill = "gray80")
      }
      
      if (lakes_and_rivers) {
         gg <- gg + geom_raster(data = df_lks, 
                                aes(x = longitude, y = latitude), fill = "white")
      }
      
      if (ad2_borders) {
         gg <- gg + geom_sf(data = ad2_shp,
                            color = "black",
                            lwd = 0.1,
                            fill = NA)
      }
      
      if (ad1_borders) {
         gg <- gg + geom_sf(data = ad1_shp,
                            color = "black",
                            lwd = 0.3,
                            fill = NA)
      }
      
      
      gg <- gg +
         geom_sf(data = ad0_shp,
                 color = "grey42",
                 lwd = 0.5,
                 fill = NA) +
         theme_empty() +
         scale_fill_manual(values=dynamic_colors) +
         coord_sf(datum = NA) +
         labs(fill = legend_title,
              title = paste0('2017 ',plot_title),
              caption = " ") 
      
      
      
      
      
      
      
      
      
      return(gg)
      
   })
   
   names(gg_list) <- paste0(ind, "_", year_list)
   
   return(gg_list)
   
}













map_raster_gavi_dpt1_who_scale <- function(ind = indicator,
                                           ig = indicator_group,
                                           rd = run_date,
                                           shapefile_version = "2018_08_01",
                                           adm0_list = NULL,
                                           plot_title,
                                           legend_title,
                                           stat = "mean",
                                           raked,
                                           mask_output = T,
                                           lakes_and_rivers = T,
                                           ad1_borders = T,
                                           ad2_borders = T) {
   
   
   ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- raster(ras_file)
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   df_yr_ras <- prep_raster_for_mapping(raster_obj = ras)
   
   
   df_yr_ras$bin <- ifelse(0<df_yr_ras$value & df_yr_ras$value<0.50, "0-49%", ifelse(0.5<df_yr_ras$value & df_yr_ras$value<0.80, "50-79%", ifelse(0.8<df_yr_ras$value & df_yr_ras$value<0.9, "80-89%", ifelse(df_yr_ras$value>0.90 & df_yr_ras$value <1, "90-100%", NA))))
   
   dynamic_bins <- sort(unique(df_yr_ras$bin))
   if(sum(is.na(unique(df_yr_ras$bin)))>0){
      dynamic_bins <- c(dynamic_bins, NA)
   }
   dynamic_colors <- rep(NA, length(dynamic_bins))
   for(i in 1:length(dynamic_bins)){
      dynamic_colors[i] <- ifelse(dynamic_bins[i]=="0-49%", "#cc0000", ifelse(dynamic_bins[i]=="50-79%", "#ffff4d", ifelse(dynamic_bins[i]=="80-89%", "#63e963", ifelse(dynamic_bins[i]=="90-100%", "#158415", "grey")) ))
      
   }
   
   
   gg <- ggplot() +
      geom_raster(data = df_yr_ras,
                  aes(x = longitude, y = latitude, fill = bin))
   
   
   if (mask_output){
      gg <- gg + geom_raster(data = df_msk, 
                             aes(x = longitude, y = latitude), fill = "gray80")
   }
   
   if (lakes_and_rivers) {
      gg <- gg + geom_raster(data = df_lks, 
                             aes(x = longitude, y = latitude), fill = "white")
   }
   
   
   
   gg <- gg +
      geom_sf(data = ad0_shp,
              color = "grey42",
              lwd = 0.5,
              fill = NA) +
      theme_empty() +
      scale_fill_manual(values=dynamic_colors) +
      coord_sf(datum = NA) +
      labs(fill = legend_title,
           title = paste0('2017 ',plot_title),
           caption = " ") 
   
   
   
   
   
   
   
   
   
   return(gg)
   
}

















map_raster_gavi_who_scale_gradient <- function(ind = indicator,
                                               ig = indicator_group,
                                               rd = run_date,
                                               shapefile_version = "2018_08_01",
                                               yl = year_list,
                                               adm0_list = NULL,
                                               plot_title,
                                               legend_title,
                                               stat = "mean",
                                               raked,
                                               mask_output = T,
                                               lakes_and_rivers = T,
                                               ad1_borders = T,
                                               ad2_borders = T) {
   
   if(indicator=='dpt1_cov'){
      yl = 2017
      year_list=c(2017)
   }
   
   sharedir <- paste0("FILEPATH", ig, "/", ind, "FILEPATH", rd, "/")
   ras_file <- paste0(sharedir, ind, "_", stat, ifelse(raked, "_raked", ""), "_raster.tif")
   if(indicator=='dpt1_cov'){
      ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   }
   if(indicator=="dpt3_cov"){
      if(adm0_list %in% c(140, 17, 10, 81)){
         yl = 2017
         year_list=c(2017)
         ras_file <- ('FILEPATH/dpt3_cov_mean_raked_2017_2017.tif')
      }
   }
   
   if (!dir.exists(sharedir)) stop("No directory found for this indicator and run date")
   if (!file.exists(ras_file)) stop("Raster file for this statistic and indicator not found")
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   if(adm0_list==105){
      ad0_shp <- read_sf('FILEPATH/IND_full_ad0.shp')
   }  
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- brick(ras_file)
   if (nlayers(ras) != length(year_list)) stop("Length of year list and number of layers in raster brick don't match")
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   gg_list <- lapply(1:length(year_list), function(i) {
      yr <- year_list[i]
      df_yr_ras <- prep_raster_for_mapping(raster_obj = ras[[i]])
      
      
      
      
      message(paste0("   ", yr))
      
      gg <- ggplot() +
         geom_raster(data = df_yr_ras,
                     aes(x = longitude, y = latitude, fill = value))
      
      
      if (mask_output){
         gg <- gg + geom_raster(data = df_msk, 
                                aes(x = longitude, y = latitude), fill = "gray80")
      }
      
      if (lakes_and_rivers) {
         gg <- gg + geom_raster(data = df_lks, 
                                aes(x = longitude, y = latitude), fill = "white")
      }
      
      if (ad2_borders) {
         gg <- gg + geom_sf(data = ad2_shp,
                            color = "black",
                            lwd = 0.1,
                            fill = NA)
      }
      
      if (ad1_borders) {
         gg <- gg + geom_sf(data = ad1_shp,
                            color = "black",
                            lwd = 0.3,
                            fill = NA)
      }
      
      
      
      breaks_NAME <- c(0.25,0.50,0.80,0.90,1.00)
      colors_NAME <- c("#D50032", "#E37110", "#9CA312", "#009639", "#653279")
      
      
      
      gg <- gg +
         geom_sf(data = ad0_shp,
                 color = "grey42",
                 lwd = 0.5,
                 fill = NA) +
         theme_empty() +
         scale_fill_gradientn(colors=colors_NAME, breaks=breaks_NAME, limits=c(0,1)) +
         coord_sf(datum = NA) +
         labs(fill = legend_title,
              title = paste0('2017 ',plot_title),
              caption = " ") 
      
      
      
      
      
      
      
      return(gg)
      
   })
   
   names(gg_list) <- paste0(ind, "_", year_list)
   
   return(gg_list)
   
}










map_raster_gavi_dpt1_who_scale_gradient <- function(ind = indicator,
                                                    ig = indicator_group,
                                                    rd = run_date,
                                                    shapefile_version = "2018_08_01",
                                                    adm0_list = NULL,
                                                    plot_title,
                                                    legend_title,
                                                    stat = "mean",
                                                    raked,
                                                    mask_output = T,
                                                    lakes_and_rivers = T,
                                                    ad1_borders = F,
                                                    ad2_borders = F) {
   
   
   ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   if(adm0_list==105){
      ad0_shp <- read_sf('FILEPATH/IND_full_ad0.shp')
   }  
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- raster(ras_file)
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   df_yr_ras <- prep_raster_for_mapping(raster_obj = ras)
   
   
   
   
   gg <- ggplot() +
      geom_raster(data = df_yr_ras,
                  aes(x = longitude, y = latitude, fill = value))
   
   
   if (mask_output){
      gg <- gg + geom_raster(data = df_msk, 
                             aes(x = longitude, y = latitude), fill = "gray80")
   }
   
   if (lakes_and_rivers) {
      gg <- gg + geom_raster(data = df_lks, 
                             aes(x = longitude, y = latitude), fill = "white")
   }
   
   
   breaks_NAME <- c(0.25,0.50,0.80,0.90,1.00)
   colors_NAME <- c("#D50032", "#E37110", "#9CA312", "#009639", "#653279")
   
   
   gg <- gg +
      geom_sf(data = ad0_shp,
              color = "grey42",
              lwd = 0.5,
              fill = NA) +
      theme_empty() +
      scale_fill_gradientn(colors=colors_NAME, breaks=breaks_NAME, limits=c(0,1)) +
      coord_sf(datum = NA) +
      labs(fill = legend_title,
           title = paste0('2017 ',plot_title),
           caption = " ") 
   
   
   
   
   
   
   
   
   
   return(gg)
   
}







map_raster_gavi_who_scale_gradient_2 <- function(ind = indicator,
                                                 ig = indicator_group,
                                                 rd = run_date,
                                                 shapefile_version = "2018_08_01",
                                                 yl = year_list,
                                                 adm0_list = NULL,
                                                 plot_title,
                                                 legend_title,
                                                 stat = "mean",
                                                 raked,
                                                 mask_output = T,
                                                 lakes_and_rivers = T,
                                                 ad1_borders = T,
                                                 ad2_borders = T) {
   
   if(indicator=='dpt1_cov'){
      yl = 2017
      year_list=c(2017)
   }
   
   sharedir <- paste0("FILEPATH", ig, "/", ind, "FILEPATH", rd, "/")
   ras_file <- paste0(sharedir, ind, "_", stat, ifelse(raked, "_raked", ""), "_raster.tif")
   if(indicator=='dpt1_cov'){
      ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   }
   if(indicator=="dpt3_cov"){
      if(adm0_list %in% c(140, 17, 10, 81)){
         yl = 2017
         year_list=c(2017)
         ras_file <- ('FILEPATH/dpt3_cov_mean_raked_2017_2017.tif')
      }
   }
   
   if (!dir.exists(sharedir)) stop("No directory found for this indicator and run date")
   if (!file.exists(ras_file)) stop("Raster file for this statistic and indicator not found")
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   if(adm0_list==105){
      ad0_shp <- read_sf('FILEPATH/IND_full_ad0.shp')
   }  
   
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- brick(ras_file)
   if (nlayers(ras) != length(year_list)) stop("Length of year list and number of layers in raster brick don't match")
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   gg_list <- lapply(1:length(year_list), function(i) {
      yr <- year_list[i]
      df_yr_ras <- prep_raster_for_mapping(raster_obj = ras[[i]])
      
      
      
      
      message(paste0("   ", yr))
      
      gg <- ggplot() +
         geom_raster(data = df_yr_ras,
                     aes(x = longitude, y = latitude, fill = value))
      
      
      if (mask_output){
         gg <- gg + geom_raster(data = df_msk, 
                                aes(x = longitude, y = latitude), fill = "gray80")
      }
      
      if (lakes_and_rivers) {
         gg <- gg + geom_raster(data = df_lks, 
                                aes(x = longitude, y = latitude), fill = "white")
      }
      
      if (ad2_borders) {
         gg <- gg + geom_sf(data = ad2_shp,
                            color = "black",
                            lwd = 0.1,
                            fill = NA)
      }
      
      if (ad1_borders) {
         gg <- gg + geom_sf(data = ad1_shp,
                            color = "black",
                            lwd = 0.3,
                            fill = NA)
      }
      
      
      
      breaks_NAME <- c(0.50,0.80,0.90,1.00) 
      colors_NAME <- c("#D50032", "#E37110", "#9CA312", "#009639") 
      labs_NAME <- c("50%", "80%", "90%", "100%")
      
      
      
      
      gg <- gg +
         geom_sf(data = ad0_shp,
                 color = "grey42",
                 lwd = 0.5,
                 fill = NA) +
         theme_empty() +
         scale_fill_gradientn(colors=colors_NAME, breaks=breaks_NAME, labels=labs_NAME, limits=c(0,1)) +
         coord_sf(datum = NA) +
         labs(fill = legend_title,
              title = paste0('2017 ',plot_title),
              caption = " ") 
      
      
      
      
      
      
      
      return(gg)
      
   })
   
   names(gg_list) <- paste0(ind, "_", year_list)
   
   return(gg_list)
   
}


map_raster_gavi_dpt1_who_scale_gradient_2 <- function(ind = indicator,
                                                      ig = indicator_group,
                                                      rd = run_date,
                                                      shapefile_version = "2018_08_01",
                                                      adm0_list = NULL,
                                                      plot_title,
                                                      legend_title,
                                                      stat = "mean",
                                                      raked,
                                                      mask_output = T,
                                                      lakes_and_rivers = T,
                                                      ad1_borders = F,
                                                      ad2_borders = F) {
   
   
   ras_file <- ('FILEPATH/dpt1_cov_mean_raked_2017_2017.tif')
   
   
   ad0_shp <- prep_ad_shp_for_mapping(ad_level = 0,
                                      shapefile_version = "2018_08_01", 
                                      adm0_list = adm0_list)
   if(adm0_list==105){
      ad0_shp <- read_sf('FILEPATH/IND_full_ad0.shp')
   }  
   if (ad1_borders) {
      ad1_shp <- prep_ad_shp_for_mapping(ad_level = 1,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   if (ad2_borders) {
      ad2_shp <- prep_ad_shp_for_mapping(ad_level = 2,
                                         shapefile_version = "2018_08_01", 
                                         adm0_list = adm0_list)
   }
   
   
   
   message("Loading raster; cropping and masking")
   ras <- raster(ras_file)
   
   
   ras <- crop(ras, ad0_shp)
   ras <- mask(ras, fasterize(ad0_shp, ras[[1]])) 
   
   
   
   if (lakes_and_rivers) {
      message("Loading lakes/rivers")
      df_lks <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_lakes.tif', 
                                        template = ras[[1]])
   }
   
   if (mask_output) {
      message("Loading mask")
      df_msk <- prep_raster_for_mapping(raster_file = 'FILEPATH/global_mask_master.tif',
                                        template = ras[[1]])
   }  
   
   
   message("Plotting by year:")
   
   df_yr_ras <- prep_raster_for_mapping(raster_obj = ras)
   
   
   
   
   gg <- ggplot() +
      geom_raster(data = df_yr_ras,
                  aes(x = longitude, y = latitude, fill = value))
   
   
   if (mask_output){
      gg <- gg + geom_raster(data = df_msk, 
                             aes(x = longitude, y = latitude), fill = "gray80")
   }
   
   if (lakes_and_rivers) {
      gg <- gg + geom_raster(data = df_lks, 
                             aes(x = longitude, y = latitude), fill = "white")
   }
   
   
   breaks_NAME <- c(0.50,0.80,0.90,1.00) 
   colors_NAME <- c("#D50032", "#E37110", "#9CA312", "#009639") 
   labs_NAME <- c("50%", "80%", "90%", "100%")
   
   
   gg <- gg +
      geom_sf(data = ad0_shp,
              color = "grey42",
              lwd = 0.5,
              fill = NA) +
      theme_empty() +
      scale_fill_gradientn(colors=colors_NAME, breaks=breaks_NAME, labels=labs_NAME, limits=c(0,1)) +
      coord_sf(datum = NA) +
      labs(fill = legend_title,
           title = paste0('2017 ',plot_title),
           caption = " ") 
   
   
   
   
   
   
   
   
   
   return(gg)
   
}


