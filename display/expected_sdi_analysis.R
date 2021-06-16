#----HEADER-------------------------------------------------------------------------------------------------------------
# Author : USERNAME
# Date:    September 2020
# Purpose: Re-create expected vaccine coverage given SDI analysis 
#***********************************************************************************************************************

#----SETUP--------------------------------------------------------------------------------------------------------------
### clear workspace
rm(list=ls())

### set os flexibility
username <- Sys.info()[["user"]]
os <- .Platform$OS.type
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "FILEPATH/"
  h_root <- "~/"
  central_lib <- "FILEPATH/"
} else {
  j_root <- "FILEPATH/"
  h_root <- "FILEPATH/"
  central_lib <- "FILEPATH/"
}


### load packages, install if missing
lib.loc <- paste0(h_root,"R/",R.Version()$platform,"/",R.Version()$major,".",R.Version()$minor)
dir.create(lib.loc,recursive=T, showWarnings = F)
.libPaths(c(lib.loc,.libPaths()))

packages <- c("data.table","magrittr","ggplot2", "msm", "dplyr", "gridExtra", "ggpubr", "wCorr", "dplyr", "matrixStats", "msm","argparse")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

# MR-BRT
library(mrbrt001, lib.loc = "FILEPATH/")

# Logit functions
logit <- function(x) {
  log(x/(1-x))
}
invlogit <- function(x) {
  exp(x)/(1+exp(x))
}


### source functions and global objects
source(paste0("FILEPATH/init.r"))
source(file.path(central_lib, "FILEPATH/get_location_metadata.R"))
source(file.path(central_lib, "FILEPATH/get_covariate_estimates.R"))

decomp_step <- "iterative"
locations   <- get_location_metadata(location_set_id=22, gbd_round_id=gbd_round, decomp_step=decomp_step)
date        <- format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d")

### estimate version
model_version <- "2021-02-11-covidfree"  
gbd_cycle     <- "gbd2020"  
vaccines      <- c("vacc_dpt1", "vacc_dpt3", "vacc_mcv1", "vacc_polio3")  

nsamples <- 1000L  

### misc. root paths
temp_root <- "FILEPATH/coverage"
#***********************************************************************************************************************


#----DATA PREP----------------------------------------------------------------------------------------------------------
### prep input data
# estimates
est <- lapply(paste0(data_root, "/exp/modeled/", gbd_cycle, "/", model_version, "/", vaccines, ".rds"),
                        readRDS) %>% rbindlist(., fill=TRUE) %>% unique
est <- est[,.(location_id, year_id, me_name, gpr_mean, gpr_lower, gpr_upper)] 
# sdi covariate
sdi <- get_covariate_estimates(covariate_id = 881, location_id = unique(locations[level==3]$location_id), decomp_step = decomp_step)
sdi <- sdi[,.(location_id, location_name, year_id, mean_value)]
# merge
all <- merge(est, sdi, by=c("location_id", "year_id"), all.x=T)
setnames(all, "mean_value", "sdi")

### clean data object
# subset down to nationals only
all <- merge(all, locations[,.(location_id, level)], by="location_id", all.x=T)[level==3]
# only based on locations where vaccine is introduced  
all <- all[gpr_mean != 0]  
# transform into logit space
all[, logit_coverage_mean := logit(gpr_mean)]
all[, coverage_sd := ((gpr_upper-gpr_lower)/3.92)]  
all[, logit_coverage_se := deltamethod(~log(x1/(1-x1)), gpr_mean, coverage_sd^2), by = c("me_name", "year_id", "location_id")] 
# limit analysis to 1980-2019 
all <- all[year_id < 2020]
#***********************************************************************************************************************



#----RUN MODELS---------------------------------------------------------------------------------------------------------
for (vaccine in vaccines) {

  message(paste0("Running expected coverage given model for ", vaccine))

  out.dir <- file.path(temp_root, "FILEPATH", model_version)
  ifelse(!dir.exists(out.dir), dir.create(out.dir), FALSE)

  # subset data to vaccine
  dt <- all[me_name==vaccine]

  # Set MR-BRT data format
  dat_1 <- MRData()
  dat_1$load_df(
    data = dt,
    col_obs = "logit_coverage_mean",
    col_obs_se = "logit_coverage_se",
    col_covs = list("sdi"),
    col_study_id = "location_id"
  )

  n_knots=5
  start=0.1
  end=0.9
  min_dist=0.1


  ### actual reproduction attempt
  knots_samples <- utils$sample_knots(
    num_intervals = 4L, 
    knot_bounds = matrix(rep(c(start, end), each=n_knots-2), ncol=2),  

    interval_sizes = rbind(c(min_dist, 1.), c(min_dist, 1.), c(min_dist, 1.),c(min_dist, 1.)),  

    num_samples = 20L
  )

  ensemble_cov_model1 <- LinearCovModel(
    alt_cov = "sdi",
    use_spline = TRUE,
    spline_knots = array(c(0,0.2,0.4,0.6,1)),  
    spline_degree = 3L,
    spline_knots_type = 'frequency',
    prior_spline_monotonicity = "increasing",  
    prior_gamma_gaussian = array(c(1e-7, 1)),  
    spline_r_linear = TRUE,  
    spline_l_linear = TRUE  


  model <- MRBeRT(data = dat_1,  
                  ensemble_cov_model = ensemble_cov_model1,
                  ensemble_knots = knots_samples,
                  cov_models = list(LinearCovModel("intercept",  
                                                   use_re=TRUE)),  
                  inlier_pct = 0.9)  




  model$fit_model(inner_print_level = 5L, inner_max_iter = 10L) 

  df_pred3 <- data.frame(sdi = seq(0, 1, by = 0.01))

  global_pred <- expand.grid(location_id = unique(dt$location_id),  
                             sdi = seq(0,1,0.01)) %>% as.data.table()

  dat_pred3 <- MRData()

  dat_pred3$load_df(
    data = global_pred,
    col_covs=list('sdi')
  )



  # uncertainty from global model
  n_samples <- nsamples  
  samples2 <- model$sample_soln(sample_size = n_samples)

  draws2 <- model$create_draws(
    data = dat_pred3,
    beta_samples = samples2[[1]],
    gamma_samples = samples2[[2]],
    random_study = FALSE  
    )


  # this predicts a weighted average from the spline ensemble
  pred5a <- model$predict(data = dat_pred3, predict_for_study = FALSE)
  global_pred$pred5a <- pred5a



  global_pred$pred_lo <- apply(draws2, 1, function(x) quantile(x, 0.025)) 
  global_pred$pred_hi <- apply(draws2, 1, function(x) quantile(x, 0.975)) 



  # quick reference plot (rewrite of above in ggplot)
  ribbon_data <- global_pred[,.(sdi, pred5a, pred_lo, pred_hi)] %>% unique
  plot <- ggplot(data=global_pred, aes(x=sdi, y=invlogit(pred5a))) +
    geom_point(data=dt, aes(x=sdi, y=gpr_mean), alpha=0.15) +
    theme_bw() +
    geom_line(data=global_pred, aes(x=sdi, y=invlogit(pred5a)), colour="maroon") +
    geom_ribbon(data=ribbon_data, aes(x=sdi, ymin=invlogit(pred_lo), ymax=invlogit(pred_hi)), alpha=0.4, fill="pink") +
    scale_y_continuous(breaks=seq(0,1,.1)) +
    labs(title = paste(vaccine, model_version))

  pdf(file.path(out.dir, paste0(vaccine, "_expected_coverage.pdf")), width=10, height=6)

  print(plot)

  dev.off()
  
  system(paste0("convert -density 300 ", file.path(out.dir, paste0(vaccine, "_expected_coverage.pdf")), file.path(out.dir, paste0(vaccine, "_expected_coverage.png"))))


  # exponentiate and re-name columns
  global_pred <- global_pred[, `:=` (mean=invlogit(pred5a), lower=invlogit(pred_lo), upper=invlogit(pred_hi),
                                     pred5a=NULL, pred_lo=NULL, pred_hi=NULL,
                                     location_id=NULL)] %>% unique
  # save!
  fwrite(global_pred, paste0(out.dir, "/", vaccine, ".csv"), row.names=F)
  message(paste0(" --All done with ", vaccine))


}
