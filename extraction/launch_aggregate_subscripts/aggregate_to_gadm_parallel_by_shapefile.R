
user <- Sys.info()['user']
code_root <- file.path("FILEPATH", user, "vaccines")
r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {
  se <- asNamespace("INDIVIDUAL_NAME") 
})


if(interactive()){
  j <- 2
  run_date <- '2023_11_15'
  point <- 0
  shapefile <- 'admin2013_1'
} else {
   se$parse_all_named_cli_args(
      required_args = list(
            j             = "integer",
            run_date      = "character",
            point         = "integer",
            shapefile     = "character",
            data_run_date = "character"
         )
   )
}

message(j)
message(run_date)
message(point)
message(shapefile)


user               <- Sys.info()['user']
remote             <- 'origin'
branch             <- 'dev'
pullgit            <- FALSE


commondir      <- sprintf('FILEPATH')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))


message('Loading in required R packages and MBG functions')
suppressMessages(invisible(library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                                 R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))))
suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))
for (p in package_list) {try(library(p, character.only = T))}


source(file.path(getwd(), "FILEPATH", "input_clean.R"))
'%!in%' <- function(x,y)!('%in%'(x,y))
library(sf)
library(stringr)
library(sp)
library(rgeos)
library(raster)
library(fasterize)
require(maptools)
sf::sf_use_s2(FALSE)

release_id=16

countries_to_drop <- c('MDV','MKD','XKO','KAZ','TCA','TUV','SRB','PSE','PRK','WSM','GBR','FJI','BLR',
                       'BIH','KIR','TUR','CHN','ALB')



st_snap_points = function(x, y 
) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}



gadm_px_threshold <- 0.25
gadm_pop_threshold <- 0.25
gadm_threshold <- 0.25



sliver_px_threshold <- 0.01
sliver_pop_threshold <- 0.01





vaccines <- c('pcv','mcv','hepb','hib','rota')
vaccs <- shorts<-c('pcv3','mcv2','hepb3','hib3','rotac')
indicators=c( 'pcv3_cov','mcv2_cov','hepb3_cov','hib3_cov','rotac_cov')
ind_titles <- vacc_names<-c('PCV3','MCV2','HepB3','Hib3','RotaC')





indicator_group='vaccine'
indicator_family = "binomial"
svy_id = "svy_id"
sample_column = "SSW"
subnational_nids = NULL
out_dir <- "FILEPATH"


shapefile_path <- paste0('FILEPATH',shapefile_version,'/lbd_standard_admin_2.shp')
shapefile_path_1 <- paste0('FILEPATH',shapefile_version,'/lbd_standard_admin_1.shp')
rasterize_field <- 'ADM2_CODE'
loc_code_field <- "ADM2_CODE"
loc_title_field <- "ADM2_NAME"
crop_shapefile <- TRUE
crop_shapefile_field <- "ADM0_NAME"


loc <- source("FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id=22, release_id=release_id)
locs <- subset(locs, select = c("location_id", "parent_id", "location_name", "location_name_short", "location_type", "super_region_name", "region_name", "ihme_loc_id"))









admin_shp <- sf::st_read(shapefile_path)
admin_shp_data_orig <- as.data.table(admin_shp)
admin_shp_data <- unique(subset(admin_shp_data_orig, select = c(loc_code_field, loc_title_field)))
setnames(admin_shp_data, c(loc_code_field, loc_title_field), c("std_loc_code", "std_loc_title"))
gaul_to_loc_id <- admin_shp_data






shp_adm1 <- readRDS(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_1.rds"))
shp_adm2 <- readRDS(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_2.rds"))
shp_adm0 <- st_as_sf(readRDS(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_0.rds")))


field <- 'ADM0_NAME'
shp_adm1@data$ADM0_NAME <- as.character(shp_adm1@data$ADM0_NAME)
shp_adm2@data$ADM0_NAME <- as.character(shp_adm2@data$ADM0_NAME)
shp_adm1@data[shp_adm1@data[,field] %like% "d'Ivoire", field] <- "Cote d'Ivoire"
shp_adm2@data[shp_adm2@data[,field] %like% "d'Ivoire", field] <- "Cote d'Ivoire"
shp_adm1@data[shp_adm1@data[,field] == "São Tomé and Príncipe", field] <- "Sao Tome and Principe"
shp_adm2@data[shp_adm2@data[,field] == "São Tomé and Príncipe", field] <- "Sao Tome and Principe"
shp_adm1@data[shp_adm1@data[,field] == "Republic of Congo", field] <- "Congo"
shp_adm2@data[shp_adm2@data[,field] == "Republic of Congo", field] <- "Congo"


shp_adm2<-shp_adm2[,c('ADM2_CODE', 'ADM2_NAME', 'ADM1_CODE', 'ADM1_NAME', 'ADM0_CODE', 'ADM0_NAME')]
shp_adm1<-shp_adm1[,c('ADM1_CODE', 'ADM1_NAME', 'ADM0_CODE', 'ADM0_NAME')]


loc_codes <- get_location_code_mapping(shapefile_version=shapefile_version)



admin_levels<-fread(paste0(out_dir, run_date, '/admin_level_decisions.csv'))
admin_levels[admin_level>2, admin_level:=2]



vaccine <- vaccines[j]
vacc <- short <- vaccs[j]
indicator <- indicators[j]
ind_title <- vacc_name <- ind_titles[j]


vd_out_dir <- paste0(out_dir, '/', run_date, '/', vacc, '/')


  flag <- ''


vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, flag, '.csv'))
vax_data <- vax_data[country %!in% countries_to_drop]


vax_data <- vax_data[age_bin > 3 | (is.na(age_bin) & age_bin_agg != '1:3')]


if(indicator=='mcv2_cov'){
  sched<-readRDS('FILEPATH/vaccine_target.rds')
  sched <- sched[me_name=='vacc_mcv2' & ihme_loc_id %in% unique(vax_data$country)]
  sched<-as.data.table(sched)
  
  
  vax_data <- vax_data[!(country %in% sched[age_cohort>4]$ihme_loc_id)]
  
  
  vax_data[,min_age_bin:=5]
  for(i in 3:4){ 
  vax_data[(country %in% sched[age_cohort==i]$ihme_loc_id), min_age_bin:=i+3]  
  }
  
  
  vax_data <- vax_data[age_bin >= min_age_bin| (is.na(age_bin) & age_bin_agg != '1:3')]
  vax_data[,min_age_bin:=NULL]
}



vax_data <- vax_data[!(svy_id == 452953 & source=='UNICEF_MICS')]


vax_data <- vax_data[!is.na(get(indicator))]


df <- fread(paste0(vd_out_dir, '/', vacc, '_intro_data.csv'))



if(point==1){
  message('now working on point data aggregation')
  point_data <- data.table()
  
  for(i in unique(vax_data[point==1]$svy_id)){
    
    
    message("
    
    input_data <- subset(vax_data[point==1], svy_id == i)
    ctry_nid <- i
    
    
    country_prefix <- unique(input_data$country)
    
    gbd_name <- locs[ihme_loc_id == country_prefix, location_name_short]
    country_name <- crop_shapefile_value <- ADM0_NAME <- gbd_name
    
    
    
 
    
    
    country_code<-suppressMessages(suppressWarnings(get_adm0_codes(country_prefix)))
    
    
    
    
    input_data[SSW==-2147483648, SSW:=NA]
    
    input_data$SSW <- ifelse(is.na(input_data$SSW), input_data$N_obs, input_data$SSW)
    
    
    
    
 
    
    input_data <-      input_data %>%
      rowwise() %>% 
      ungroup() %>% data.table() %>%
      setnames(c(svy_id, sample_column), c("svy_id", "sample_column"))
    
    countries <- input_data$country %>% unique()
    
    
    
    
    country_shp<-admin_shp[admin_shp$ADM0_CODE==country_code,]
    if(nrow(country_shp)==0) stop('mismatch in country codes, fix')
    
    suppressWarnings(suppressMessages(input_admin <-
                                        input_data %>%
                                        st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(country_shp)) %>%
                                        st_join(country_shp)))
    
    
    missing<-input_admin[is.na(input_admin$ADM0_NAME),]
    
    
    input_admin <- input_admin[!is.na(input_admin$ADM0_NAME),]
    
    if(nrow(missing)>0){
      message(paste0('snapping ', nrow(missing), ' points that fell outside the country shapefile'))
      
      snapped_missing<-st_snap_points(missing, country_shp) %>% 
        st_cast('MULTIPOINT') %>% 
        st_as_sf()
      
      
      snapped_missing$row_id  <- missing$row_id
      
      snapped_missing<-  st_join(snapped_missing, country_shp)
      
      
      missing<-as.data.table(missing)
      missing[,geometry:=NULL]
      snapped_missing <- merge(snapped_missing, missing[,list(country, svy_id,point,location_code,shapefile,
                                                              source,year,psu,weight,N,N_obs,sample_column,outlier,region,row_id,prev=get(indicator))], by = c("row_id"))
      snapped_missing<-as.data.table(snapped_missing)
      snapped_missing[,geometry:=NULL]
    } else{
      snapped_missing<- data.table()
    }
    
    
    input_admin<-input_admin %>%
      st_set_geometry(NULL) %>%
      setnames(eval(indicator), "prev") %>%
      as.data.table()
    
    
    input_admin<-rbind(input_admin,snapped_missing,fill=T)
    
    
    
    setnames(input_admin, c(loc_code_field, loc_title_field), c("std_loc_code", "std_loc_title"))
    
    
    input_admin[, std_loc_code := as.numeric(as.character(std_loc_code))]
    
    missing <-
      input_admin %>%
      filter(is.na(std_loc_code)) %>% nrow()
    
    input_admin <- as.data.table(input_admin)
    
    
    if (indicator_family == "binomial") input_admin[, prev := prev / N]
    
    
    
    suppressWarnings(suppressMessages(input_admin0 <-
                                        input_admin %>%
                                        group_by(svy_id, year, source, point, std_loc_code, std_loc_title) %>%
                                        dplyr::summarise(
                                          
                                          outcome = weighted.mean(prev, sample_column),
                                          vax_N = sum(N * weight) 
                                          
                                        ) %>%
                                        ungroup() %>%
                                        data.table()))
    
    
    
    
    
    
    
    input_admin0 <- input_clean(input_admin0)
    
    
    
    if (!is.null(subnational_nids)){
      input_admin0 <- subnational_nid_subset(input_admin0)
    }
    
    new_admin <- input_admin0
    new_nat_tab <- weighted.mean(new_admin$outcome, new_admin$vax_N)
    new_tot_tab <- sum(new_admin$vax_N)
    
    
    setnames(new_admin, "outcome", "vax_prev")
    
    intro <- subset(df, iso3 == country_prefix)
    intro <- intro$intro_yr
    
    new_admin[,yrs_since_intro:=year - intro]
    new_admin[,intro_yr:=intro]
    new_admin[,country := country_prefix]
    new_admin[,shp_level:='ADM2_CODE']
    new_admin[,original_shapefile:=NA]
    new_admin[,std_loc_title:=NULL]
    
    
    
    point_data <- rbind(point_data, new_admin)
    
  } 
}



if(point == 0){
  message('Now moving to polygon data alignment for shapefile ', shapefile)
  
  
  i <- shapefile
  
  input_data <- subset(vax_data[point==0], shapefile == i)  
  if(nrow(input_data)==0)quit('no qualifying input data matches this shapefile',save='no',status=0)
  
  input_data<-merge(input_data, admin_levels, by=c('shapefile','svy_id','country'),all.y=F)
  if(nrow(input_data)==0)stop('input data doesnt match admin levels file--needs to be included there')
  
  try(shp_fast <- fast_load_shapefile(i),silent=T)
  if(!exists('shp_fast')){ shp <- readOGR(paste0('FILEPATH',i,'.shp'))
  }else{
    shp <- copy(shp_fast)
    remove(shp_fast)
  }
  
  
  
  if(i == 'PAN_MICS_161587') shp <- st_read(paste0('FILEPATH', i, '.shp'))
  
  shp<-shp[shp$GAUL_CODE %in% input_data$location_code,]
  
  
  poly_data <- data.table()
  
  for(c in unique(input_data$country)){
    message(c)

    
    c_data <- input_data[country==c]
    
    
    if(unique(c_data$admin_level) < 1) {
      message('this is regional level data, dropping it from dataset')
      if (length(unique(input_data$country)) > 1) {
        next
      } else{
        message("don't use this regional-level shapefile: end")
        quit(save='no',status=0)
      }
    }
    
    
    adm_level <- unique(c_data$admin_level)
    
    
    setnames(c_data, eval(paste0(vacc, '_cov')), "prev")
    c_data[, vax_prev := prev / N]
    c_data[,vax_N:=N]
    
    
    c_data <- c_data[,list(vax_prev=weighted.mean(vax_prev, vax_N),
                           vax_N=sum(vax_N)), 
                     by=c('country', 'svy_id', 'location_code','shapefile','year','admin_level')]
    
    
    country<-loc_codes[ihme_lc_id==c]$loc_nm_sh
    
    c_shp <- shp[shp$GAUL_CODE %in% c_data$location_code,]
    
    c_shp_adm1 <- shp_adm1[shp_adm1$ADM0_NAME==country,]
    c_shp_adm2 <- shp_adm2[shp_adm2$ADM0_NAME==country,]
    
    if(length(c_shp_adm1)==0) {
      country<-loc_codes[ihme_lc_id==c]$loc_name
      c_shp_adm1 <- shp_adm1[shp_adm1$ADM0_NAME==country,]
      c_shp_adm2 <- shp_adm2[shp_adm2$ADM0_NAME==country,]
      if(length(c_shp_adm1)==0) {
        stop('country names arent matched properly')
      }
    }
    
    
    if(adm_level==1){
      c_shp_adm <- copy(c_shp_adm1)
    } else{
      c_shp_adm <-copy(c_shp_adm2)
    }
    remove(c_shp_adm1, c_shp_adm2)
    
    
    gaul_code <- suppressMessages(suppressWarnings(get_adm0_codes(c, shapefile_version = shapefile_version)))
    raster_outputs <- prep_shapes_for_raking(
      raster_agg_factor = 1,
      rake_subnational = F,
      countries_not_to_subnat_rake = NULL,
      reg = c,
      modeling_shapefile_version = shapefile_version,
      raking_shapefile_version = shapefile_version,
      field = "loc_id"
    )
    
    
    simple_raster <- raster_outputs[["simple_raster"]]
    
    simple_polygon <- raster_outputs[["simple_polygon"]]
    
    
    pixel_id <- raster_outputs[["pixel_id"]]
    link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version, raster_agg_factor = 1)
    
    covdt <- load_populations_cov(c, 'total', measure = 'count', simple_polygon, simple_raster, year_list = unique(c_data$year), interval_mo=12, pixel_id = pixel_id, raster_agg_factor = 1)
    
    connector <- get_gbd_locs(rake_subnational = TRUE,
                              reg = c,
                              shapefile_version = shapefile_version)
    
    nat_connector <- get_gbd_locs(rake_subnational = F,
                                  reg = c,
                                  shapefile_version = shapefile_version)
    cell_ids <- link_table[[2]]
    
    link <- prep_link_table(link_table = link_table,
                            simple_raster = simple_raster,
                            pixel_id = pixel_id)
    
    
    
    
    link <- sub_nat_link_merge(rake_subnational=TRUE,
                               link,
                               connector,
                               nat_connector,
                               countries_not_to_subnat_rake=c('PHL','NGA','PAK'))
    
    
    
    c_shp<-st_as_sf(c_shp)
    c_shp_adm<-st_as_sf(c_shp_adm)
    c_shp$matching_code <- 1:nrow(c_shp)
    c_shp_adm$matching_code <- 1:nrow(c_shp_adm)
    
    
    
    gr<-fasterize(c_shp_adm, simple_raster, field='matching_code',background=-9999)
    
    
    gr<-gr+simple_raster*0
    
    
    
    for(y in unique(c_data$year)){
      for(s in unique(c_data[year==y]$svy_id)){
        
        message(y)
        y_data<-c_data[year==y & svy_id==s]
        
        
        shp_data<-merge(c_shp, y_data, by.x='GAUL_CODE', by.y='location_code')
        
        
        cr<-fasterize(shp_data, simple_raster, field='matching_code',background=-9999)
        cr  <- cr + simple_raster*0
        
        
        
        raster_vals<-unique(cr$layer)
        raster_vals<-raster_vals[raster_vals>0]
        
        missing_shapes<-shp_data[shp_data$matching_code %!in% raster_vals,]
        missing_points<-st_centroid(missing_shapes)
        if(nrow(missing_points)>0){
          missing_raster<-rasterize(missing_points, simple_raster, field='matching_code',background=NA)
          
          
          
          
          if(length(na.omit(values(missing_raster))) >0 ){
          
          poly_sr<-st_as_sf(rasterToPolygons(missing_raster))
          poly_sr$shared_pixel_id<- 1:nrow(poly_sr)
          
          st_crs(poly_sr) <- st_crs(missing_points)
          
          
          all_missing<-st_join(missing_points,poly_sr)
          
          
          all_missing <-
            all_missing %>%
            group_by(shared_pixel_id) %>%
            dplyr::summarise(
              vax_prev = weighted.mean(vax_prev, vax_N),
              vax_N = sum(vax_N)
            ) %>%
            ungroup() %>%
            st_cast('POINT')
          
          
          all_missing$matching_code <- (max(shp_data$matching_code) + 1):(max(shp_data$matching_code) + nrow(all_missing)) 
          missing_raster<-rasterize(all_missing, simple_raster, field='matching_code',background=NA)
          
          } 
        }else{
          missing_raster<-copy(simple_raster)
          values(missing_raster)<-NA
        }
        
        
        vax_rast<-fasterize(shp_data, simple_raster, field='vax_prev',background=-9999)
        vax_N_rast<-fasterize(shp_data, simple_raster, field='vax_N',background=-9999)
        
        
        vax_rast <- vax_rast + simple_raster*0
        vax_N_rast <- vax_N_rast + simple_raster*0
        
        
        id_cell_pred<-as.data.table(cbind(as.vector(gr), as.vector(cr), 
                                          
                                          as.vector(vax_rast), as.vector(vax_N_rast), as.vector(missing_raster)))
        
        names(id_cell_pred) <- c('gadm_poly','custom_poly',
                                 
                                 'vax_prev', 'vax_N', 'missing_custom_polys')
        id_cell_pred <- id_cell_pred[!is.na(gadm_poly)]
        
        
        
        id_cell_pred <- prep_cell_pred(cell_pred = id_cell_pred, 
                                       cell_ids  = cell_ids,
                                       pixel_id  = pixel_id,
                                       covdt     = covdt[year==y])
        
        id_cell_pred = merge(link, id_cell_pred, by.x = 'ID', by.y = 'cell_id',allow.cartesian = TRUE)
        
        
        id_cell_pred[vax_prev == -9999, vax_prev := NA]
        id_cell_pred[vax_N == -9999, vax_N := NA]
        id_cell_pred[gadm_poly == -9999, gadm_poly := NA]
        id_cell_pred[custom_poly == -9999, custom_poly := NA]
        
        
        id_cell_pred[,u_id:=1:.N]
        
        
        if(adm_level==1){
          id_cell_pred[,GADM_CODE:=ADM1_CODE]
        }else{
          id_cell_pred[,GADM_CODE:=ADM2_CODE]
        }
        
        
        if(nrow(id_cell_pred[!is.na(missing_custom_polys)])>0){
          message(paste0(length(unique(na.omit(id_cell_pred$missing_custom_polys))), ' missing small polygons identified, adding to custom polys data'))
          for(m in unique(na.omit(id_cell_pred$missing_custom_polys))){
            
            id_cell_pred[missing_custom_polys==m & is.na(custom_poly), custom_poly:=missing_custom_polys]
            id_cell_pred[custom_poly==m, vax_prev:= all_missing[all_missing$matching_code==m,]$vax_prev]
            id_cell_pred[custom_poly==m, vax_N:= all_missing[all_missing$matching_code==m,]$vax_N]
            
            
            if(nrow(id_cell_pred[missing_custom_polys==m & missing_custom_polys!=custom_poly])>0){
              for(u in unique(id_cell_pred[missing_custom_polys==m & missing_custom_polys!=custom_poly]$u_id)){
                
                id_cell_pred[u_id==u, area_fraction:=area_fraction/2]
                new_row <- id_cell_pred[u_id==u]
                new_row[,custom_poly:=missing_custom_polys]
                new_row[,vax_prev:=all_missing[all_missing$matching_code==m,]$vax_prev]
                new_row[,vax_N:=all_missing[all_missing$matching_code==m,]$vax_N]
                id_cell_pred<-rbind(id_cell_pred,new_row)
              }
            }
          }
        }
        
        
        id_cell_pred[,pop:=pop*area_fraction]
        
        
        gadm_px_summary<-as.data.table(table(id_cell_pred$GADM_CODE))
        names(gadm_px_summary)<-c('GADM_CODE','px_count')
        
        custom_px_summary<-as.data.table(table(id_cell_pred$custom_poly))
        names(custom_px_summary)<-c('custom_poly','px_count')
        
        px_summary<-as.data.table(table(id_cell_pred[,list(GADM_CODE,custom_poly)]))
        names(px_summary)<-c('GADM_CODE','custom_poly','px_count')
        
        gadm_pop_summary<-as.data.table(aggregate(pop~GADM_CODE,data=id_cell_pred,FUN='sum'))
        custom_pop_summary<-as.data.table(aggregate(pop~custom_poly,data=id_cell_pred,FUN='sum'))
        pop_summary<-as.data.table(aggregate(pop~GADM_CODE+custom_poly,data=id_cell_pred,FUN='sum'))
        
        
        
        for(p in sort(unique(id_cell_pred$GADM_CODE))){
          
          
          questionable <- FALSE
          max_overlap <- 0
          
          
          total_px_count<-gadm_px_summary[GADM_CODE==p]$px_count
          all_custom_count <-sum(px_summary[GADM_CODE==p]$px_count)
          total_px_overlap <-all_custom_count/total_px_count
          
          
          total_pop<-gadm_pop_summary[GADM_CODE==p]$pop
          all_custom_pop<-sum(pop_summary[GADM_CODE==p]$pop)
          if(total_pop!=0)total_pop_overlap<-all_custom_pop/total_pop else total_pop_overlap <- 0 
          
          
          if(total_px_overlap<gadm_px_threshold & total_pop_overlap<gadm_pop_threshold){
            message(paste0('flagging gadm polygon ', p, ' due to insufficient pixel (',round(total_px_overlap,4),
                           ') & population overlap (',round(total_pop_overlap,4),
                           ') with custom shapes'))
            
            
            
            questionable <- TRUE
          }
          
          
          for(q in as.numeric(unique(px_summary[GADM_CODE==p & px_count>0]$custom_poly))){
            
            
            if( nrow(pop_summary[GADM_CODE==p & custom_poly==q]) == 0 | sum(pop_summary[GADM_CODE==p & custom_poly==q]$pop==0)){
              message(paste0('dropping overlapping custom segment ', q, 
                             'of gadm polygon ', p, 
                             ' due to NAs in population'))
              id_cell_pred <- id_cell_pred[!(GADM_CODE==p & custom_poly==q)|is.na(custom_poly)]
              next
            }
            
            
            custom_px_count<-custom_px_summary[custom_poly==q]$px_count
            custom_pop<-custom_pop_summary[custom_poly==q]$pop
            
            
            sub_px_count<-px_summary[GADM_CODE==p & custom_poly==q]$px_count
            sub_pop_count<-pop_summary[GADM_CODE==p & custom_poly==q]$pop
            px_prop <- sub_px_count/custom_px_count
            pop_prop <- sub_pop_count/custom_pop
            
            if(px_prop<sliver_px_threshold & pop_prop<sliver_pop_threshold){
              message(paste0('dropping overlapping custom segment ', q, 
                             'of gadm polygon ', p, 
                             ' due to insufficient pixel  (',round(px_prop,4),') & population overlap (',round(pop_prop,4),') with custom shapes'))
              id_cell_pred <- id_cell_pred[!(GADM_CODE==p & custom_poly==q)|is.na(custom_poly)]
            }
            
            if(questionable==TRUE){
              max_overlap <- max(max_overlap, px_prop, pop_prop)
            }
            
          }
          
          if(questionable==TRUE & max_overlap < gadm_threshold){
            message(paste0('dropping gadm polygon ', p, ' due to insufficient max overlap (',round(max_overlap,4),
                           ') with custom shapes'))
            id_cell_pred <- id_cell_pred[GADM_CODE!=p]
          }
        }
        
        
        id_cell_pred<- id_cell_pred[!is.na(custom_poly)]
        
        
        gadm_px_summary<-as.data.table(table(id_cell_pred$GADM_CODE))
        names(gadm_px_summary)<-c('GADM_CODE','px_count')
        
        custom_px_summary<-as.data.table(table(id_cell_pred$custom_poly))
        names(custom_px_summary)<-c('custom_poly','px_count')
        
        px_summary<-as.data.table(table(id_cell_pred[,list(GADM_CODE,custom_poly)]))
        names(px_summary)<-c('GADM_CODE','custom_poly','px_count')
        
        gadm_pop_summary<-as.data.table(aggregate(pop~GADM_CODE,data=id_cell_pred,FUN='sum'))
        custom_pop_summary<-as.data.table(aggregate(pop~custom_poly,data=id_cell_pred,FUN='sum'))
        pop_summary<-as.data.table(aggregate(pop~GADM_CODE+custom_poly,data=id_cell_pred,FUN='sum'))
        
        
        for(q in as.numeric(unique(px_summary[px_count>0]$custom_poly))){
          custom_pop<-custom_pop_summary[custom_poly==q]$pop
          id_cell_pred[custom_poly==q, total_custom_pop:=custom_pop]
          id_cell_pred[custom_poly==q, vax_N_new:=(pop/total_custom_pop)*vax_N]
        }
        
        
        admin_agg <- id_cell_pred[,list(vax_prev=weighted.mean(vax_prev, vax_N_new),
                                        vax_N=sum(vax_N_new)), by='GADM_CODE']
        
        
        if(adm_level==1){
          c_shp_adm$GADM_CODE<-as.numeric(as.character(c_shp_adm$ADM1_CODE))
          admin_agg<-merge(admin_agg, c_shp_adm, by='GADM_CODE', all.y=F) 
        }else{
          c_shp_adm$GADM_CODE<-as.numeric(as.character(c_shp_adm$ADM2_CODE))
          admin_agg<-merge(admin_agg, c_shp_adm, by='GADM_CODE', all.y=F) 
        }        
        
        
        
        
        
        intro <- subset(df, iso3 == c)
        intro <- intro$intro_yr
        
        admin_agg[,year:=y]
        admin_agg[,yrs_since_intro:=year - intro]
        admin_agg[,intro_yr:=intro]
        
        
        admin_agg[,country:=c]
        admin_agg[,svy_id:=s]
        admin_agg[,source:=unique(input_data[year==y & country==c & svy_id==s]$source)]
        admin_agg[,original_shapefile:=i]
        admin_agg[,shp_level:=adm_level]
        admin_agg[,point:=0]
        admin_agg[,gadm_poly:=NULL]
        admin_agg[,GADM_CODE:=NULL]
        admin_agg[,geometry:=NULL]
        admin_agg[,geo_id:=NULL]
        admin_agg[,OBJECTID:=NULL]
        
        if(adm_level==1){
          admin_agg[,ADM2_CODE:=NA]
          admin_agg[,ADM2_NAME:=NA]
        }
        
        
        poly_data<-rbind(poly_data, admin_agg)
        
      } 
    } 
  } 
} 


if(point ==1){
  type <- 'point'
  aligned_data <- copy(point_data)
} else{ type <- 'polygon'
aligned_data <- copy(poly_data)
}
write.csv(aligned_data, paste0(vd_out_dir ,vacc,'_data_', run_date,'_', type, '_', shapefile, '.csv'), row.names=F)

message('done!')
