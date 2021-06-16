#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#         Raking China subnational (loc set 5) PCV draws to loc set 4 and 3           #
#                                   8 June 2019                                       #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


########################## SET OBJECTS ################################################
### set run_date
run_date <- date

# #### set antigen
vax_transform <- c('hib3', 'rotac', 'pcv3')
#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#

## Set repo location and indicator group
user               <- "USERNAME"
core_repo          <- paste0('FILEPATH/')
indic_repo         <- paste0('FILEPATH/')
remote             <- 'origin'
branch             <- 'develop'
pullgit            <- FALSE

## sort some directory stuff
commondir      <- sprintf('FILEPATH/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))
package_list <- package_list[package_list!= "tictoc"]  

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')

# source(paste0(core_repo, 'mbg_central/setup.R'))
lapply(package_list, library, character.only=TRUE)
setwd(paste0(core_repo, '/mbg_central'))
for (ii in (list.files(pattern = "functions.R"))) {print(ii); source(ii)}
setwd('FILEPATH')

# Custom load indicator-specific functions
source(paste0(indic_repo,'functions/misc_vaccine_functions.R'))
source('FILEPATH/helper_functions.R')
source('FILEPATH/gbd_logit_raking_function.R')  
'%!in%' <- function(x,y)!('%in%'(x,y))

#### raking settings:
zero_heuristic <- T
iterate <- T
approx_0_1 <- T
MaxJump = 11
MaxIter = 80
FunTol = 1e-5
if_no_gbd <- "return_na"


########################################################################################
############## Step 0: pull populations for weight
########################################################################################

source("FILEPATH/get_outputs.R")
source("FILEPATH/get_ids.R")
source("FILEPATH/location_metadata_functions.R")
source("FILEPATH/get_population.R")

yr <-c(year.est.start:year.est.end)
nyrs <- length(yr)
ctry <- c(491, 492, 493, 494, 495, 496, 497, 498, 499, 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521)

level5_pops  <- get_population(age_group_id = 22, location_id = ctry, year_id = yr, gbd_round_id = gbd_round, sex_id = 3, decomp_step='iterative')
colnames(level5_pops) <- c("age_group_id1",  "location_id", "year", "sex_id", "pop", "run_id1")

level4_pops <- get_population(age_group_id = 22, location_id = c(44533, 354, 361), year_id = yr, gbd_round_id = gbd_round, sex_id = 3, decomp_step='iterative')
colnames(level4_pops) <- c("age_group_id1",  "location_id", "year", "sex_id", "pop", "run_id1")
level4_pops <- level4_pops[order(location_id),]


##############################################################################################
############## Step 1: rake level 3 to level 4 
##############################################################################################

for (vacc in vax_transform) {
  
  message(paste0("Raking: ", vacc))
  
  level3 <- readRDS(paste0('FILEPATH/vacc_',vacc,'_china.rds'))
  rake_to <- data.table(cbind(level3$location_id, level3$year_id, level3$gpr_mean))
  colnames(rake_to) <- c("name", "year", "value")
  rake_to[value < 0.001, value := 0]
  
  level4 <- fread(paste0('FILEPATH/',vacc,'_level4_draws_prepped.csv'))
  level4_extra <- level4
  bad <- c("location_id",  "year_id",      "age_group_id", "sex_id",  "measure_id",   "covariate_id")
  level4 <- level4 %>% dplyr::select(-one_of(bad))
  
  raked_4_to_3 <- gbd_raking_level4(rake_targets=rake_to, gbd_loc_id=6, cell_pred=level4, nyears=nyrs, year_list=c(yr), population=c(level4_pops$pop))
  
  raked_44533_draws <- raked_4_to_3[87:129,]  
  raked_44533_means <- rowMeans(raked_44533_draws)
  
  
  ##############################################################################################
  ############## Step 2: rake level 5 to level 4 
  ##############################################################################################
  # read in and format china level 4 means
  name <- rep(44533, nyrs)
  rake_to <- data.table(cbind(name, yr, raked_44533_means))  
  colnames(rake_to) <- c("name", "year", "value")
  rake_to[value < 0.001, value := 0]
  
  # read in china level 5 draws
  level5 <- fread(paste0('FILEPATH/',vacc,'_level5_draws_prepped.csv'))
  level5_extra <- level5
  bad <- c("location_id",  "year_id",      "age_group_id", "sex_id",  "measure_id",   "covariate_id")
  level5 <- level5 %>% dplyr::select(-one_of(bad))
  
  raked_5_to_44533 <- gbd_raking(rake_targets=rake_to, gbd_loc_id=44533, cell_pred=level5, nyears=nyrs, year_list=c(yr), population=level5_pops$pop)
  
  
  ##############################################################################################
  ############## Step 3: format draws back to orignal format and save
  ##############################################################################################
  
  add_back_level5 <- level5_extra %>% dplyr::select(one_of(bad))
  add_back_level4 <- level4_extra %>% dplyr::select(one_of(bad))
  
  raked_level4 <- cbind(add_back_level4, raked_4_to_3)
  raked_level5 <- cbind(add_back_level5, raked_5_to_44533)
  
  fwrite(raked_level4, paste0('FILEPATH/',vacc,'_draws_raked_level4.csv'))
  fwrite(raked_level5, paste0('FILEPATH/',vacc,'_draws_raked_level5.csv'))
  
  message(paste0(" -- Done raking: ", vacc))
}

