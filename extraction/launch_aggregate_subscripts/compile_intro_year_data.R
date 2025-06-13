user <- Sys.info()['user']
code_root <- file.path("FILEPATH", user, "vaccines")
r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {
  se <- asNamespace("INDIVIDUAL_NAME") 
})


if(interactive()){
  data_run_date <- '2023_11_15'
} else {
  se$parse_all_named_cli_args(required_args = list(data_run_date = "character"))
}

message(data_run_date)

library(data.table)

outputdir <- paste0('FILEPATH',data_run_date,'/')
yrs <- readRDS('FILEPATH/vaccine_intro.rds')
yrs[,year_id:=NULL]
yrs[,cv_intro_years:=NULL]
yrs[,cv_outro:=NULL]
yrs <- unique(yrs)

for(vacc in c('hib3','rotac','hepb3',
              'mcv2','pcv3')) {
  
  dir.create(paste0(outputdir, '/',vacc,'/'))

yrs_vacc <- subset(yrs, me_name == paste0("vacc_", vacc))
yrs_vacc[,country:=gsub('_*[0-9]','',ihme_loc_id)]


yrs_vacc_no_subnat <- subset(yrs_vacc,  !grepl("_", ihme_loc_id))
yrs_vacc_subnat <- subset(yrs_vacc,  grepl("_", ihme_loc_id))

yrs_vacc_subnat[,country:=gsub('_*[0-9]','',ihme_loc_id)]
subnat_countries<-table(as.data.table(table(yrs_vacc_subnat[,list(cv_intro,country)]))[N>0]$country)
subnat_countries <- names(subnat_countries[subnat_countries>1])

yrs_vacc_no_subnat <- yrs_vacc_no_subnat[!(country %in% subnat_countries)]
yrs_vacc_subnat <- yrs_vacc_subnat[country %in% subnat_countries]

yrs_vacc<-rbind(yrs_vacc_no_subnat,yrs_vacc_subnat)

yrs_vacc<-yrs_vacc[,list(iso3=ihme_loc_id,gbd_loc=location_id,intro_yr=cv_intro,country)]

yrs_vacc$intro_yr <- ifelse(yrs_vacc$intro_yr == 9999, NA, yrs_vacc$intro_yr)


write.csv(yrs_vacc, paste0(outputdir, '/',vacc,'/', vacc, '_intro_data.csv'))

}

message('done!')