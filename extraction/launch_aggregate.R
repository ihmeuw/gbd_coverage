


user <- Sys.info()['user']

code_root <- file.path("FILEPATH", user, "vaccines")
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), verbose = T)


if (!exists("config_path")) config_path <- file.path(code_root, "pipeline_config.yaml")

load_pipeline_config(step = "launch_aggregate", config_path = config_path)

source(file.path(code_root, "vaccination_pipeline_functions/submit_job.R"))

sf::sf_use_s2(FALSE)

base_dir <- paste0('FILEPATH', user, 'FILEPATH', run_date, '/')

dir.create(base_dir)
dir.create(paste0(base_dir, 'errors/'))
dir.create(paste0(base_dir, 'output/'))

stdout <- paste0(base_dir, "output/%x.o%j")
stderr <- paste0(base_dir, "errors/%x.e%j")

outputdir <- paste0('FILEPATH',run_date,'/')
dir.create(outputdir)


remote             <- 'origin'
branch             <- 'develop'
pullgit            <- FALSE


commondir      <- sprintf('FILEPATH')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))


message('Loading in required R packages and MBG functions')
suppressMessages(invisible(library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                                                 R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))))
suppressMessages(invisible(library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))))
for (p in package_list) {try(library(p, character.only = T))}



source(file.path(getwd(), "FILEPATH", "input_clean.R"))
source(file.path(getwd(), "FILEPATH", "scale_color_vaccine.R"))
source(file.path(getwd(), "FILEPATH", "scale_fill_vaccine.R"))
'%!in%' <- function(x,y)!('%in%'(x,y))
library(sf)
library(stringr)
library(sp)
library(rgeos)
library(raster)
library(fasterize)



vaccines <- c('pcv','mcv','hepb','hib','rota')
vaccs <- shorts<-c('pcv3','mcv2','hepb3','hib3','rotac')
indicators=c( 'pcv3_cov','mcv2_cov','hepb3_cov','hib3_cov','rotac_cov')
ind_titles <- vacc_names<-c('PCV3','MCV2','HepB3','Hib3','RotaC')



countries_to_drop <- c('MDV','MKD','XKO','KAZ','TCA','TUV','SRB','PSE','PRK','WSM','GBR','FJI','BLR',
                       'BIH','KIR','TUR','CHN','ALB')



if(launch_intros){
  wait_on_slurm_job_id(submit_job(
    script_path   = file.path(getwd(), "extraction/launch_aggregate_subscripts", "compile_intro_year_data.R")
    , args_list    = list(data_run_date = run_date)
    , job_name    = "prep_intro_data"
    , archiveTF   = TRUE
    , mem_G       = "20G"
    , threads     = 5
    , runtime_min = 960
    , dry_runTF   = FALSE
    , partition   = queue
    , Account     = project
    , std_out_path= stdout
    , std_err_path= stderr
  ))
}


if(check_alignment){
  
  dec<-fread(paste0('FILEPATH',last_aggregate_run_date,'/admin_level_decisions.csv'))
  dec<-unique(dec[,list(country,svy_id,shapefile,admin_level, dec=1)])
  
  vax_data<-unique(    
    rbindlist(lapply(indicators,function(indicator){
      vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, '.csv'))
      vax_data <- vax_data[country %!in% countries_to_drop]
      
      vax_data<-unique(vax_data[point==0,list(country,shapefile,svy_id)])
      vax_data[!svy_id %in% dec$svy_id]
    }))
  )
  
  vd<-unique(vax_data[shapefile!='',list(svy_id,shapefile,vax=1,country)])
  
  new<-merge(dec,vd,by=c('svy_id','shapefile','country'),all.y=T)
  
  new<-new[is.na(dec)]
  
  
  if(nrow(new)>0){
    setorderv(new,cols=c('country','shapefile'))
    
    new[,admin_level:=NULL]
    new[,dec:=NULL]
    shapefiles <- unique(dec$shapefile)
    
    new_updated<-rbindlist(lapply(unique(new[shapefile %in% shapefiles]$shapefile), function(s){
      countries <- unique(dec[shapefile==s,list(country,admin_level)])
      new_updated <-  merge(new[shapefile==s & country %in% countries$country], countries, by=('country'), all.x=T)
      return(new_updated)
    }))
    
    new_updated[,vax:=NULL]
    new_updated[,dec:=1]
    dec<-rbind(dec, new_updated)
    
    fwrite(dec, paste0(outputdir,'/admin_level_decisions_interim.csv'))
    
    
    new<-merge(new,dec,by=c('country','svy_id','shapefile'),all.x=T)
    new<-new[is.na(admin_level)]
    
    if(nrow(new) > 0){
      wait_on_slurm_job_id(submit_job(
        script_path   = file.path(getwd(), "extraction/launch_aggregate_subscripts", "admin_levels_diagnostic_plots.R")
        , args_list   = list(data_run_date = data_run_date, admin_level_data_decisions_date = run_date, shapefile_version = shapefile_version)
        , job_name    = "make_shapefile_admin_level_diagnostics"
        , archiveTF   = TRUE
        , mem_G       = "50G"
        , threads     = 5
        , runtime_min = 960
        , dry_runTF   = FALSE
        , partition   = queue
        , Account     = project
        , std_out_path= stdout
        , std_err_path= stderr
      ))
      
      success_filename <- "admin_level_mapping_success.txt"
      fail_filename <- "admin_level_mapping_fail.txt"
      
      succ_path <- file.path(outputdir, success_filename)
      fail_path <- file.path(outputdir, fail_filename)
      
      if (file.exists(fail_path)) {
        stop('stop, manually review figures for new shapefiles, make hardcoded decisions manually and write out. then, re-run, skipping this step.')
      }
      if (file.exists(succ_path)) {
        success_lines <- readLines(succ_path)
        for (new_code_line in success_lines) {
          eval(parse(text = new_code_line))
        }
      } else {
        message("Success file does not exist; no mappings were made.")
      }
      
      
      new[, vax := NULL]
      dec<-rbind(dec,new)
      dec[,dec:=NULL]
      
      fwrite(dec, paste0(outputdir, 'admin_level_decisions.csv'))
    } else{
      fwrite(dec, paste0(outputdir,'/admin_level_decisions.csv'))
    }
    
  }else{
    fwrite(dec, paste0(outputdir,'/admin_level_decisions.csv'))
  }
  
}



if(launch_processing==T){
  jobs_wait <- list()
  
  for(i in 1:length(vaccines)){
    
    vaccine <- vaccines[i]
    vacc <- short <- vaccs[i]
    indicator <- indicators[i]
    ind_title <- vacc_name <- ind_titles[i]
    
    flag <- ''
    
    
    vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, flag, '.csv'))
    vax_data <- vax_data[country %!in% countries_to_drop & !is.na(get(indicator))]
    vax_data <- vax_data[age_bin > 3 | (is.na(age_bin) & age_bin_agg != '1:3')]
    
    
    if(launch_point){
      
      jobs_wait = c(jobs_wait, submit_job(
        script_path   = file.path(getwd(), "extraction/launch_aggregate_subscripts", "aggregate_to_gadm_parallel_by_shapefile.R")
        , args_list   = list(j = i, run_date = run_date, point = 1, shapefile = "NA", data_run_date = data_run_date, shapefile_version = shapefile_version)
        , job_name    = paste(vaccines[i], "_alignment_point", sep = "")
        , archiveTF   = TRUE
        , mem_G       = "20G"
        , threads     = 5
        , runtime_min = 960
        , dry_runTF   = FALSE
        , partition   = queue
        , Account     = project
        , std_out_path= stdout
        , std_err_path= stderr
      ))
    }
    if(launch_polys){
      for(s in unique(vax_data[point==0 & shapefile !='']$shapefile)){
        jobs_wait = c(jobs_wait, submit_job(
          script_path   = file.path(getwd(), "extraction/launch_aggregate_subscripts", "aggregate_to_gadm_parallel_by_shapefile.R")
          , args_list   = list(j = i, run_date = run_date, point = 0, shapefile = s, data_run_date = data_run_date, shapefile_version = shapefile_version, pop_release = pop_release, modeling_shapefile_version = modeling_shapefile_version)
          , job_name    = paste(vaccines[i], "_alignment_", s, sep = "")
          , archiveTF   = TRUE
          , mem_G       = "30G"
          , threads     = 5
          , runtime_min = 960
          , dry_runTF   = FALSE
          , partition   = queue
          , Account     = project
          , std_out_path= stdout
          , std_err_path= stderr
        ))
      }
    }
    
  }
  jobs_vec <- unlist(jobs_wait)
  wait_on_slurm_job_id(jobs_vec)

}


if(check_missing){
  missing <- data.table()
  for(i in  1:length(vaccines)){
    
    
    vaccine <- vaccines[i]
    vacc <- short <- vaccs[i]
    indicator <- indicators[i]
    flag <- ''
    
    
    vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, flag, '.csv'))
    vax_data <- vax_data[country %!in% countries_to_drop]
    
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
    } else{
      vax_data <- vax_data[age_bin > 3 | (is.na(age_bin) & age_bin_agg != '1:3')]
    }
    vax_data<-unique(vax_data[,list(country,svy_id,shapefile,point)])
    vax_data <- vax_data[point==0]
    admin_levels<-fread(paste0(outputdir, '/admin_level_decisions.csv'))
    vax_data<-merge(vax_data,admin_levels,by=c('country','svy_id','shapefile'), all.x=T)
    vax_data <- vax_data[admin_level>=1]
    
    
    
    
    for(s in unique(vax_data[point==0 & shapefile !='']$shapefile)){
      if(!file.exists(paste0(outputdir, vacc,'/',vacc,'_data_',run_date,'_polygon_',s,'.csv'))){
        print(paste0(vacc, ': ', s, ' did not write out' ))
        missing <- rbind(missing,data.table(vaccine=vacc,shapefile=s,point=0))
      }
    }
    if(!file.exists(paste0(outputdir, vacc,'/',vacc,'_data_',run_date,'_point_NA.csv'))){
      print(paste0(vacc, ': ', s, ' did not write out' ))
      missing <- rbind(missing,data.table(vaccine=vacc,shapefile=NA,point=1))
    }
    
    
  }
  if(nrow(missing)>0) stop('some jobs broke, investigate')
  
  
  if(nrow(missing)>0){
    for(i in 1:length(vaccines)){
      
      vaccine <- vaccines[i]
      vacc <- short <- vaccs[i]
      indicator <- indicators[i]
      ind_title <- vacc_name <- ind_titles[i]
      
      missing_v <- missing[vaccine==vacc]
      if(nrow(missing_v)==0) next
      
      for(s in missing_v$shapefile){
        submit_job(
          script_path   = file.path(getwd(), "extraction/launch_aggregate_subscripts", "aggregate_to_gadm_parallel_by_shapefile.R")
          , args_list   = list(j = i, run_date = run_date, point = 0, shapefile = s, data_run_date = data_run_date)
          , job_name    = paste(vaccine, "_alignment_", s, "_relaunched", sep = "")
          , archiveTF   = TRUE
          , mem_G       = "100G"
          , threads     = 5
          , runtime_min = 960
          , dry_runTF   = FALSE
          , partition   = queue
          , Account     = project
          , std_out_path= stdout
          , std_err_path= stderr
        )
      }
    }
    
    stop('stop and re-run missing section, debug, repeat until none missing')
  }else{
    print('no broken jobs, can proceed')
  }
}





if(combine_outputs){
  mbg2 <- readRDS(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_2.rds"))
  mbg2 <- as.data.table(mbg2)
 
  for(i in 1:length(vaccines)){
    
    
    vaccine <- vaccines[i]
    vacc <- short <- vaccs[i]
    indicator <- indicators[i]
    ind_title <- vacc_name <- ind_titles[i]
    
    vd_out_dir <- paste0(outputdir, '/', vacc, '/')
    
    
    point_data<-fread(paste0(vd_out_dir ,vacc,'_data_', run_date,'_point_NA.csv'))
    
    
    mbg2$ADM2_CODE <- as.numeric(as.character(mbg2$ADM2_CODE))
    point_data <- merge(point_data[shp_level=='ADM2_CODE'], mbg2, by.x="std_loc_code", by.y="ADM2_CODE")
    
    
    poly_files<-list.files(path = vd_out_dir, pattern = '_polygon_')
    
    poly_data<-data.table()
    for(s in poly_files){
      p<-  fread(paste0(vd_out_dir ,s))
      if(nrow(p)==1 & ncol(p)==1) next 
      p[,c('OBJECTID','loc_id'):=NULL]
      poly_data<-rbind(poly_data, p)
    }
    
    
    point_data[,ADM2_CODE:=std_loc_code]
    point_data[,std_loc_code:=NULL]
    
    point_data[,shp_level:=2]
    point_data[,geo_id:=NULL]
    point_data[,ad2_id:=NULL]
    point_data[,ad0_parent:=NULL]
    point_data[,ad1_parent:=NULL]
    point_data[,loc_id:=NULL]
    
    poly_data[,matching_code:=NULL]
    
    aligned_data<-rbind(poly_data, point_data)
    
    write.csv(aligned_data, paste0(vd_out_dir,vacc,'_data_', run_date, '.csv'), row.names=F)
    
  }
}



if(run_diagnostics){
  
  dec<- fread(paste0(outputdir, 'admin_level_decisions.csv'))
  
  for(i in 1:length(vaccines)){
    
    
    vaccine <- vaccines[i]
    vacc <- short <- vaccs[i]
    indicator <- indicators[i]
    ind_title <- vacc_name <- ind_titles[i]
    
    message(vacc)
    
    flag <- ''
    
    
    vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, flag, '.csv'))
    vax_data <- vax_data[country %!in% countries_to_drop]
    
    
    vax_data<-merge(vax_data, dec, by=c('shapefile','svy_id','country'),all.y=F, all.x=T)
    vax_data <- vax_data[point==1 | admin_level>=1]
    vax_data <- vax_data[year>1999 & (age_bin > 3| is.na(age_bin & age_bin_agg!='1:3'))]
    
    
    if(indicator=='mcv2_cov'){
      sched<-readRDS('FILEPATH/vaccine_target.rds')
      sched <- sched[me_name=='vacc_mcv2' & ihme_loc_id %in% unique(vax_data$country)]
      sched<-as.data.table(sched)
      
      
      vax_data <- vax_data[!(country %in% sched[age_cohort>4]$ihme_loc_id)]
      
      
      vax_data[,min_age_bin:=5]
      for(i in 3:4){ 
        vax_data[(country %in% sched[age_cohort==i]$ihme_loc_id), min_age_bin:=i+3]  
      }
      
      
      vax_data <- vax_data[age_bin >= min_age_bin]
      vax_data[,min_age_bin:=NULL]
    }
    
    vd_out_dir <- paste0(outputdir, '/', vacc, '/')
    
    aligned_data<- fread(paste0(vd_out_dir,vacc,'_data_', run_date, '.csv'))
    aligned_data <- aligned_data[year>1999]
    aligned_data <- aligned_data[country %!in% countries_to_drop]
    
    diag_dir<-paste0(vd_out_dir, 'FILEPATH')
    
    countries<-unique(mbg2$ADM0_NAME)
    
    
    missing_nids <- vector()
    for(d in unique(vax_data$svy_id)){
      if(d %!in% unique(aligned_data$svy_id)){ message(paste0('svy_id ', d, ', point==', unique(vax_data[svy_id==d]$point)==1,
                                                              ', country: ', unique(vax_data[svy_id ==d]$country),
                                                              ' and shapefile ', unique(vax_data[svy_id ==d]$shapefile),
                                                              ' is missing from the aligned data--investigate why'))
        missing_nids <- c(missing_nids, d)
      }
    }
    
    
    mismatching_N <- vector()
    for(p in c(0,1)){
      for(a in unique(aligned_data[point==p]$svy_id)){
        for(s in unique(aligned_data[point==p & svy_id==a]$original_shapefile)){
          original_N<-round(sum(vax_data[point==p & svy_id==a & shapefile==s]$N),5)
          new_N<-round(sum(aligned_data[point==p & svy_id==a & original_shapefile==s]$vax_N),5)
          if(original_N != new_N){
            message(paste0('N changed for svy_id ', a, ', point==', unique(aligned_data[point==p & svy_id==a & original_shapefile==s]$point)==1,
                           ', country: ', unique(vax_data[point==p & svy_id ==a & shapefile==s]$country), ' and shapefile ', s,
                           '--investigate why. original N was ', original_N, ', now it is ', new_N))
            mismatching_N <- c(mismatching_N, a)
          }
        }
      }
    }
  }
}

if(make_plots){
  
  dec<- fread(paste0(outputdir, 'admin_level_decisions.csv'))
  
  shp_adm1 <- readRDS(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_1.rds"))
  shp_adm2 <- readRDS(paste0("FILEPATH",shapefile_version,"/lbd_standard_admin_2.rds"))
  
  
  field <- 'ADM0_NAME'
  shp_adm1@data$ADM0_NAME <- as.character(shp_adm1@data$ADM0_NAME)
  shp_adm2@data$ADM0_NAME <- as.character(shp_adm2@data$ADM0_NAME)
  shp_adm1@data[shp_adm1@data[,field] %like% "d'Ivoire", field] <- "Cote d'Ivoire"
  shp_adm2@data[shp_adm2@data[,field] %like% "d'Ivoire", field] <- "Cote d'Ivoire"
  shp_adm1@data[shp_adm1@data[,field] == "São Tomé and Príncipe", field] <- "Sao Tome and Principe"
  shp_adm2@data[shp_adm2@data[,field] == "São Tomé and Príncipe", field] <- "Sao Tome and Principe"
  shp_adm1@data[shp_adm1@data[,field] == "Republic of Congo", field] <- "Congo"
  shp_adm2@data[shp_adm2@data[,field] == "Republic of Congo", field] <- "Congo"
  
  
  
  shp_adm1<-st_as_sf(shp_adm1)
  shp_adm2<-st_as_sf(shp_adm2)
  
  
  
  
  
  for(i in 1:length(vaccines)){
    
    
    
    
    
    vaccine <- vaccines[i]
    vacc <- short <- vaccs[i]
    indicator <- indicators[i]
    ind_title <- vacc_name <- ind_titles[i]
    
    message(vacc)
    
    flag <- ''
    
    
    vax_data<-fread(paste0('FILEPATH',data_run_date,'FILEPATH', indicator, flag, '.csv'))
    vax_data <- vax_data[country %!in% countries_to_drop]
    vax_data <- vax_data[year>1999 & (age_bin > 3| is.na(age_bin & age_bin_agg!='1:3'))]
    
    
    vax_data<-merge(vax_data, dec, by=c('shapefile','svy_id','country'),all.y=F, all.x=T)
    vax_data <- vax_data[point==1 | admin_level>=1]
    
    
    vd_out_dir <- paste0(outputdir,  '/', vacc, '/')
    
    aligned_data<- fread(paste0(vd_out_dir,vacc,'_data_', run_date, '.csv'))
    
    diag_dir<-paste0(vd_out_dir, 'FILEPATH')
    
    
    dir.create(diag_dir)
    pdf(paste0(diag_dir, vacc, '_point_data_comparisons.pdf'), width = 14, height = 8)
    for(s in unique(aligned_data[point==1]$svy_id)){
      for(y in unique(aligned_data[point==1 & svy_id==s]$year)){
        point_data<-vax_data[point==1 & svy_id==s & year==y]
        gadm_data<-aligned_data[point==1 & svy_id==s & year==y]
        
        
        point_data <-
          point_data %>%
          st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(shp_adm2)) %>%
          setnames(eval(indicator), "vax_prev") %>%
          setnames('N', "vax_N") %>%
          st_join(shp_adm2)
        
        point_data$vax_prev <- point_data$vax_prev / point_data$vax_N
        
        
        
        gadm_data<-merge(shp_adm2, gadm_data, by='ADM2_CODE', all.x=F)
        
        
        
        
        points_plot <- ggplot()+geom_sf(data=point_data,aes(color=vax_prev, size=vax_N/3))+
          geom_sf(data=gadm_data, fill=NA)+
          theme_void()+
          scale_color_vaccine()+
          labs(title='Original points')+
          theme(plot.title = element_text(size=15))+
          guides(size=FALSE)
        
        
        polys_plot <- ggplot()+geom_sf(data=gadm_data,aes(fill=vax_prev))+
          geom_sf(data=gadm_data, fill=NA)+
          theme_void()+
          scale_fill_vaccine()+
          labs(title='GADM polygons')+
          theme(plot.title = element_text(size=15))
        
        grid.arrange(points_plot, polys_plot, top=paste0('NID: ', s, ", ",unique(point_data$country), ', year: ', y), ncol=2)
        
      }
    }
    dev.off()
    
    
    pdf(paste0(diag_dir, vacc, '_polygon_data_comparisons.pdf'), width = 14, height = 8)
    for(j in unique(aligned_data[point==0]$original_shapefile)){
      message(j)
      gadm_data<-aligned_data[point==0 & original_shapefile== j]
      custom_data<-vax_data[shapefile==j]
      
      setnames(custom_data, eval(paste0(vacc, '_cov')), "prev")
      custom_data[, vax_prev := prev / N]
      custom_data[,vax_N:=N]
      custom_data[,N:=NULL]
      custom_data[,prev:=NULL]
      
      
      try(shp_fast <- fast_load_shapefile(j),silent=T)
      if(!exists('shp_fast')){ shp <- readOGR(paste0('FILEPATH',j,'.shp'))  }else{
        shp <- copy(shp_fast)
        remove(shp_fast)
      }
      
      if(j == 'PAN_MICS_161587') next 
      
      shp<-st_as_sf(shp)
      shp<-shp[shp$GAUL_CODE %in% vax_data[shapefile==j]$location_code,]
      
      
      
      
      
      for(s in unique(gadm_data$svy_id)){
        print(s)
        
        if(unique(custom_data[svy_id==s]$admin_level)<2){
          gadm_shp<-copy(shp_adm1)
          code='ADM1_CODE'
        } else{
          gadm_shp <- copy(shp_adm2)
          code='ADM2_CODE'
        }
        
        s_data <- custom_data[svy_id==s]
        new_data <-gadm_data[svy_id==s]
        new_data<-merge(gadm_shp, new_data, by=code, all.x=F)
        s_data<-merge(shp, s_data, by.x='GAUL_CODE', by.y='location_code', all.x=F)
        
        
        
        
        
        for(y in sort(unique(new_data$year))){  
          gadm_plot <- ggplot()+geom_sf(data=new_data[new_data$year==y,],aes(fill=vax_prev))+
            geom_sf(data=new_data[new_data$year==y,], fill=NA)+
            theme_void()+
            scale_fill_vaccine()+
            labs(title='GADM polygons')+
            theme(plot.title = element_text(size=15))
          
          custom_plot <- ggplot()+geom_sf(data=s_data[s_data$year==y,],aes(fill=vax_prev))+
            geom_sf(data=s_data[s_data$year==y,], fill=NA)+
            theme_void()+
            scale_fill_vaccine()+
            labs(title='Original polygons')+
            theme(plot.title = element_text(size=15))
          
          grid.arrange(custom_plot, gadm_plot, top=paste0('NID: ', s, ", ",unique(new_data$country), ', year: ', y), ncol=2)
          
        }
      }
    }
    
    dev.off()
    
    
  } 
}
