
user <- Sys.info()['user']
code_root <- file.path("FILEPATH", user, "vaccines")
r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {
  se <- asNamespace("INDIVIDUAL_NAME") 
})

library(dplyr)

if(interactive()){
  admin_level_data_decisions_date <- '2024_for_prev_date_testing'
  data_run_date <- '2024-01-03'
} else {
   se$parse_all_named_cli_args(
      required_args = list(
         admin_level_data_decisions_date = "character",
         data_run_date                   = "character"
      )
   )
}

message(admin_level_data_decisions_date)


user               <- Sys.info()['user']
remote             <- 'origin'
branch             <- 'develop'
pullgit            <- FALSE


commondir      <- sprintf('FILEPATH')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))


message('Loading in required R packages and MBG functions')
library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                                 R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))
for (p in package_list) {try(library(p, character.only = T))}



source(file.path(getwd(), "FILEPATH", "input_clean.R"))
source(file.path(getwd(), "FILEPATH", "scale_fill_vaccine.R"))
source(file.path(getwd(), "FILEPATH", "scale_color_vaccine.R"))
'%!in%' <- function(x,y)!('%in%'(x,y))
library(sf)
library(stringr)
library(sp)
library(rgeos)
library(raster)





vaccines <- c('mcv','pcv','hib','hepb','rota'
)
vaccs <- shorts<-c('mcv2', 'pcv3' ,'hib3','hepb3','rotac'
)
indicators=c('mcv2_cov', 'pcv3_cov','hib3_cov','hepb3_cov','rotac_cov'
)
ind_titles <- vacc_names<-c('MCV2', 'PCV3','Hib3','HepB3',"RotaC"
)





indicator_group='vaccine'
indicator_family = "binomial"
svy_id = "svy_id"
sample_column = "SSW"
subnational_nids = NULL
raked <- TRUE
out_dir<-paste0('FILEPATH', admin_level_data_decisions_date,'/')

countries_to_drop <- c('MDV','MKD','XKO','KAZ','TCA','TUV','SRB','PSE','PRK','WSM','GBR','FJI','BLR',
                       'BIH','KIR','TUR','CHN')


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



modeling_regions <- c("vax_name", "vax_essa", "vax_wssa", "vax_sssa", "vax_cssa", "vax_soas", "vax_seas", "vax_eaas", "vax_caeu", "vax_crbn", "vax_ctam", "vax_ansa", "vax_trsa")
reg_table <- lapply(modeling_regions, function(x) {return(data.table(ad0_code = suppressWarnings(suppressMessages(get_adm0_codes(x))), reg = x))}) %>% rbindlist


admin_shp <- sf::st_read(shapefile_path)
admin_shp_data_orig <- as.data.table(admin_shp)
admin_shp_data <- unique(subset(admin_shp_data_orig, select = c(loc_code_field, loc_title_field)))
setnames(admin_shp_data, c(loc_code_field, loc_title_field), c("std_loc_code", "std_loc_title"))
gaul_to_loc_id <- admin_shp_data



shp_adm1 <- readOGR(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_1.shp"))
shp_adm2 <- readOGR(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_2.shp"))


field <- 'ADM0_NAME'
shp_adm1@data$ADM0_NAME <- as.character(shp_adm1@data$ADM0_NAME)
shp_adm2@data$ADM0_NAME <- as.character(shp_adm2@data$ADM0_NAME)
shp_adm1@data[shp_adm1@data[,field] %like% "d'Ivoire", field] <- "Cote d'Ivoire"
shp_adm2@data[shp_adm2@data[,field] %like% "d'Ivoire", field] <- "Cote d'Ivoire"
shp_adm1@data[shp_adm1@data[,field] == "São Tomé and Príncipe", field] <- "Sao Tome and Principe"
shp_adm2@data[shp_adm2@data[,field] == "São Tomé and Príncipe", field] <- "Sao Tome and Principe"
shp_adm1@data[shp_adm1@data[,field] == "Republic of Congo", field] <- "Congo"
shp_adm2@data[shp_adm2@data[,field] == "Republic of Congo", field] <- "Congo"



loc_codes <- get_location_code_mapping(shapefile_version=shapefile_version)

vax_data<- rbindlist(lapply(1:length(vaccines), function(j){
  
  
  
  vaccine <- vaccines[j]
  vacc <- short <- vaccs[j]
  indicator <- indicators[j]
  ind_title <- vacc_name <- ind_titles[j]
  
  
  vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, '.csv'))
  vax_data <- vax_data[point==0 & country %!in% countries_to_drop]
  
  vax_data<-vax_data[,list(country,svy_id,shapefile,location_code)]
  
  return(vax_data)
}))
  
vax_data<-unique(vax_data)


dec<-fread(paste0(out_dir,'/admin_level_decisions_interim.csv'))
dec<-unique(dec[,list(country,svy_id,shapefile,admin_level,dec=1)])

vd<-unique(vax_data[shapefile!='',list(svy_id,shapefile,vax=1,country)])

new<-merge(dec,vd,by=c('svy_id','shapefile','country'),all.y=T)

new<-new[is.na(dec)]


setorderv(new,cols=c('country','shapefile'))

new[,admin_level:=NULL]
new[,dec:=NULL]


outputdir <- paste0('FILEPATH',admin_level_data_decisions_date,'/')

successful <- ""
failed <- ""

success_filename <- "admin_level_mapping_success.txt"
fail_filename <- "admin_level_mapping_fail.txt"

succ_path <- file.path(outputdir, success_filename)
fail_path <- file.path(outputdir, fail_filename)

 
pdf(paste0(out_dir,"/polygons_comparisons_",admin_level_data_decisions_date,".pdf"), width = 14, height = 8)

  for(i_n in 1:length(unique(new$shapefile))){
    
    i=unique(new$shapefile)[i_n]
    message('should be working on .......', i)
    if(i=='') next
    message("
    

    shapefile <- i
    
    
    input_data <- subset(vax_data, shapefile == i)
    input_data <- input_data[svy_id %in% new[shapefile==i]$svy_id]
    
    countries <- unique(input_data$country)
    
    gbd_names <- locs[ihme_loc_id %in% countries, location_name]
    
    reg <- reg_table[ad0_code %in% get_adm0_codes(countries), reg]
    
    
    
    try(shp <- fast_load_shapefile(i),silent=T)
    if(!exists('shp')) shp <- readOGR(paste0('FILEPATH',i,'.shp'))
    
    
    
    if(i == 'PAN_MICS_161587') next 
    
    shp<-shp[shp$GAUL_CODE %in% input_data$location_code,]
    
    
    for(c in unique(input_data$country)){
      
      
      
      message(c)
      
      
      
      c_data <- input_data[country==c]
      
      
      
      c_shp <- shp[shp$GAUL_CODE %in% c_data$location_code,]
      
      country<-loc_codes[ihme_lc_id==c]$loc_nm_sh
      
      
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
      
      
      ca<-st_as_sf(c_shp)
      cb<-st_as_sf(c_shp_adm1)
      cc<-st_as_sf(c_shp_adm2)
      
      
      
  cust<-    ggplot(data=ca, fill='light gray') + 
              geom_sf(size=1)+
              theme_void() + 
              theme(legend.position = 'none')+
              ggtitle(paste('Custom shape: ', i))
      
  adm1<-    ggplot(data=cb, fill='light gray') + 
              geom_sf(size=1)+
              theme_void() + 
              theme(legend.position = 'none')+
              ggtitle('Admin 1 GADM shapes')
      
  adm2<-    ggplot(data=cc, fill='light gray') +
              geom_sf(size=1)+
              theme_void() + 
              theme(legend.position = 'none')+
              ggtitle('Admin 2 GADM shapes')
        
 
  d<-data.table(as.vector(c('custom', 'admin 1', 'admin 2')))
  d$V2 <- as.vector(c(nrow(ca), nrow(cb), nrow(cc)))
  names(d) <-c('shapes','polygon count')
  
  country<-c
  nids<-paste0(unique(c_data$svy_id))
  
  svys<-nids[1]
  
  if(length(nids)>1){
    for(x in 2:length(nids)){
    svys<-paste(svys, nids[x])
    }
  }

  grid.arrange(cust,tableGrob(d, rows=NULL), adm1,adm2, top=paste0(c, ": ",svys), ncol=2)
  
  custom_count <- d[shapes == "custom", "polygon count"]
  admin1_count <- d[shapes == "admin 1", "polygon count"]
  admin2_count <- d[shapes == "admin 2", "polygon count"]
  
  admin1_diff <- abs(custom_count - admin1_count) / custom_count * 100
  admin2_diff <- abs(custom_count - admin2_count) / custom_count * 100
  
  if (admin1_diff <= 20 && admin2_diff <= 20) {
    output_string <- paste0(svys, " ", shapefile, "\r\n")
    failed <- paste0(failed, output_string)
  } else if (admin1_diff <= 20) {
    output_string <- paste0("new[shapefile=='", shapefile, "', admin_level:=1]\r\n")
    successful <- paste0(successful, output_string)
  } else if (admin2_diff <= 20) {
    output_string <- paste0("new[shapefile=='", shapefile, "', admin_level:=2]\r\n")
    successful <- paste0(successful, output_string)
  } else {
    output_string <- paste0(svys, " ", shapefile, "\r\n")
    failed <- paste0(failed, output_string)
  }
    
      }
    remove(shp)
  } 

dev.off()

writeLines(successful, succ_path)

if(nchar(failed) > 0) {
  writeLines(failed, fail_path)
}

message('plots done, go look and make decisions')
