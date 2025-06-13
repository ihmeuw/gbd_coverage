





start_time <- Sys.time()

if(interactive()){
  
  
  
  
  
  
  
  
  
  
  
  
  date_version     = "2024_11_13"
  vaccine_ratio    = "vacc_mcv2_mcv1_ratio"
  
  release_id       = 33
  outputdir        = file.path("FILEPATH", date_version, vaccine_ratio)
  to_model_dir     = file.path("FILEPATH", date_version)
  to_model_cs1_dir = file.path(to_model_dir, "coverage_ratio_custom_stage_1", vaccine_ratio)
  mrbrt_env_path   = "FILEPATH"
  fhs_run_TF       = FALSE
  fhs_year_end     = 2100
  
} else {
  
  library(argparse)
  
  parser <- ArgumentParser()
  
  parser$add_argument("--vaccine_ratio",    help = "Which vaccine ratio are you modeling?", type="character")
  
  parser$add_argument("--release_id",       help = "Which release is the input data coming from?", type="character")
  parser$add_argument("--outputdir",        help = "legacy central output folder", type="character")
  parser$add_argument("--to_model_dir",     help = "date_versioned output folder", type="character")
  parser$add_argument("--to_model_cs1_dir", help = "date_versioned subfolder for custom stage 1 modeled results by vaccine-ratio", type="character")
  parser$add_argument("--mrbrt_env_path",   help = "path FILEPATH", type="character")
  parser$add_argument("--fhs_run_TF",       help = "is this run for FHS?", type="logical")
  parser$add_argument("--fhs_year_end",     help = "Last year to forecast (config param)", type="integer")
  
  
  
  args          <- parser$parse_args()
  list2env(args, environment()); rm(args)
  
  vaccine_ratio <- as.character(vaccine_ratio)
  
  release_id    <- as.numeric(release_id)
  fhs_year_end <- as.integer(fhs_year_end)
}


message(vaccine_ratio)

message(release_id)
message(outputdir)
message(to_model_dir)
message(to_model_cs1_dir)
message(mrbrt_env_path)


library(reticulate)

reticulate::use_python(mrbrt_env_path)
mr <- import("mrtool")
cw <- import("crosswalk")


library(ggplot2)
library(data.table)
library(lme4)
library(splines)
library(magrittr)
library(arrow)
library(Metrics)
library(car)







samples <- 500L
theta_global <- 20


invlogit <- function(x) {
  exp(x)/(1+exp(x))
}




source("FILEPATH/get_covariate_estimates.R")
source('FILEPATH/get_location_metadata.R')




locs <- get_location_metadata(location_set_id=22, release_id=release_id)
loc_id_map <- locs[, .(ihme_loc_id, location_id)]



message("Loading data")
df_mrbrt          <- fread(paste0(to_model_dir,"/",vaccine_ratio,".csv"))
custom_covariates <- fread(paste0(to_model_dir,"FILEPATH",vaccine_ratio, "_cv_covariates.csv"))

message("Merging with locations")
df_mrbrt          <- merge(df_mrbrt, loc_id_map)
custom_covariates <- merge(custom_covariates, loc_id_map)

message("Merging data with covariates")
df_mrbrt       <- merge(df_mrbrt, custom_covariates, by = c('location_id','ihme_loc_id','year_id','age_group_id','sex_id'), all = TRUE)

df_mrbrt <- merge(df_mrbrt, subset(locs, select = c("location_id", "location_name","level","parent_id")),by = 'location_id')
df_mrbrt[,natl_ihme_loc_id := substr(ihme_loc_id, 1, 3)] 



message("Adding intro years")

df_mrbrt[is.na(sample_size) | sample_size <= 0, sample_size := 100]
df_mrbrt[, stderr := sqrt(val * (1 - val) / sample_size)]
df_mrbrt[nid == 203321, stderr := mean(df_mrbrt[nid == 203321]$stderr, na.rm = TRUE)]


df_mrbrt[cv_intro_1 == 1, intro_year := year_id]
df_mrbrt[, intro_year := min(intro_year, na.rm = TRUE), by = location_id]
df_mrbrt[is.na(intro_year), intro_year := 1980]


df_mrbrt[, years_since_intro := year_id - intro_year]
df_mrbrt[years_since_intro < 0, years_since_intro := NA]


df_mrbrt[val >= 0.999, val := 0.999]


df_mrbrt_fit <- df_mrbrt[!is.na(df_mrbrt$val) & !is.na(years_since_intro)]

message("Linear to logit transformation")

transforms<-cw$utils$linear_to_logit(mean = array(df_mrbrt_fit$val), 
                                     sd   = array(df_mrbrt_fit$stderr))
df_mrbrt_fit$val <- transforms[[1]]
df_mrbrt_fit$stderr <- 0.5 


if(fhs_run_TF){
  df_mrbrt[year_id == fhs_year_end, val := logit(0.999)]
  df_mrbrt[year_id == fhs_year_end, variance := 0.5]
}

knots_range <- max(df_mrbrt_fit[!is.na(val)]$years_since_intro)
if(is.na(knots_range)) knots_range <- max(na.omit(df_mrbrt_fit$year_id)) - 1979 
knots <- c(0,
           5/knots_range, 
           0.5,
           0.95,
           1)

if(fhs_run_TF){
  fhs_knot_scale_factor <- fhs_year_end - 1980 
  knots                 <- c(knots * (knots_range / fhs_knot_scale_factor))
  knots[5]              <- 1
} 


message("Fitting global MRBRT model")


dat_global <- mr$MRData()
dat_global$load_df(
  data = df_mrbrt_fit[level==3],
  col_obs = "val",
  col_obs_se = "stderr",
  col_covs = list(
    "years_since_intro", "cv_stockout"),
  col_study_id = "location_id" 
)


spmg_sd_values <- c(rep(Inf,3), 0.05)
if(fhs_run_TF) spmg_sd_values <- rep(Inf,4)


global_model <- mr$MRBRT(
  data = dat_global,
  cov_models = list(
    mr$LinearCovModel("intercept", use_re = TRUE),
    mr$LinearCovModel("cv_stockout"),
    mr$LinearCovModel(
      alt_cov = "years_since_intro",
      use_spline = TRUE,
      prior_spline_monotonicity = "increasing",
      spline_knots_type = "domain",
      spline_knots = array(knots),
      spline_r_linear = TRUE,
      prior_spline_convexity = "concave",
      prior_spline_maxder_gaussian = rbind(c(rep(0,4)), spmg_sd_values)
    )
  ),
  inlier_pct = 0.9
)

if(fhs_run_TF){

dat_0 <- mr$MRData()
dat_0$load_df(
  data = df_mrbrt[!is.na(val)],
  col_covs = list(
    "years_since_intro", "cv_stockout")
)
global_model$attach_data(dat_0)
}

global_model$fit_model(inner_max_iter = 1000L)




message("Predicting from global MRBRT model")

df_mrbrt[is.na(years_since_intro) & (is.na(intro_year)), years_since_intro:=0]

dat_global_pred <- mr$MRData()

dat_global_pred$load_df(
  data = df_mrbrt,
  col_covs = list(
    "years_since_intro", "cv_stockout")
)


df_mrbrt$preds <- invlogit(global_model$predict(data=dat_global_pred))




mrbrt_global_model <- list("MRBRT - global", "NA", global_model, df_mrbrt) 
names(mrbrt_global_model) <- c("name", "formula", "model", "preds")


message("Fitting country-specific MRBRT models")
samples2 <- global_model$sample_soln(sample_size = as.integer(samples))


draws2 <- global_model$create_draws(
  data = dat_global_pred,
  beta_samples = samples2[[1]],
  gamma_samples = samples2[[2]],
  random_study = FALSE)
draws2 <- cbind(df_mrbrt, draws2)
saveRDS(object = draws2, file = file.path(to_model_cs1_dir, "draws2.rds"))

samples_global_model <- samples2[[1]]
sds_global_model <- apply(samples_global_model, 2, sd)
country_model_beta_prior <- rbind(global_model$beta_soln, sds_global_model * theta_global)
if(country_model_beta_prior[1,2]>0) stop('stockout global beta is >0, need to stop and review')

path_global_priors <- file.path(outputdir, paste0("mrbrt_custom_stage_1_", vaccine_ratio, "_global_priors.csv"))
path_global_priors_to_model <- file.path(to_model_cs1_dir, paste0("mrbrt_custom_stage_1_", vaccine_ratio, "_global_priors.csv"))
message("Writing global priors to: ", path_global_priors)
fwrite(rbind(country_model_beta_prior,theta_global), path_global_priors)
message("Writing global priors to: ", path_global_priors_to_model)
fwrite(rbind(country_model_beta_prior,theta_global), path_global_priors_to_model)


message("Grabbing stockout beta value for covid adjustments later")
df_mrbrt$stockout_beta <- global_model$beta_soln[2]
df_mrbrt$spline_1_beta <- global_model$beta_soln[3]
df_mrbrt$spline_2_beta <- global_model$beta_soln[4]
df_mrbrt$spline_3_beta <- global_model$beta_soln[5]
df_mrbrt$spline_4_beta <- global_model$beta_soln[6]
df_mrbrt$spline_5_beta <- global_model$beta_soln[7]



message("Running country-specific MRBRT models")

country_models <- lapply(unique(df_mrbrt_fit[level==3]$location_id), function(loc_id) {
  
  message(loc_id)
  country <- unique(df_mrbrt[location_id==loc_id]$natl_ihme_loc_id)
  if(length(country)>1)stop('multiple ihme_loc_ids attributed here, stop and check')
  
  df_mrbrt_fit_country <- subset(df_mrbrt_fit, location_id == loc_id)
  if(nrow(df_mrbrt_fit_country) < 2) {
    message(" -- only one row of data! returning NULL")
    return(NULL)
  }
  
  dat_country <- mr$MRData()
  dat_country$load_df(
    data = df_mrbrt_fit_country,
    col_obs = "val",
    col_obs_se = "stderr",
    col_covs = list(
      "years_since_intro", "cv_stockout")
  )
  
  country_model <- mr$MRBRT(
    data = dat_country,
    cov_models = list(
      mr$LinearCovModel("intercept", use_re = FALSE,
                        prior_beta_gaussian = country_model_beta_prior[,1]),
      mr$LinearCovModel("cv_stockout", use_re = FALSE,
                        prior_beta_gaussian = country_model_beta_prior[,2]),
      mr$LinearCovModel("years_since_intro", 
                        use_spline = TRUE,
                        prior_spline_monotonicity = "increasing", 
                        spline_knots_type = "domain", 
                        spline_knots = array(knots),
                        spline_r_linear = TRUE,
                        prior_spline_convexity = "concave",
                        prior_beta_gaussian = country_model_beta_prior[,3:length(country_model_beta_prior[1,])],
                        prior_spline_maxder_gaussian = rbind(c(rep(0,4)), spmg_sd_values)
      )
    ),
    inlier_pct = 1
  )
  
  
  dat_1 <- mr$MRData()
  dat_1$load_df(
    data = df_mrbrt[!is.na(val)],
    col_obs = "val",
    col_obs_se = "stderr",
    col_covs = list(
      "years_since_intro", "cv_stockout"),
    col_study_id = "location_id"
  )
  country_model$attach_data(dat_1)
  
  country_model$fit_model(inner_max_iter = 1000L)
  
  
  
  dat_country_pred <- mr$MRData()
  
  df_mrbrt_country <- df_mrbrt[natl_ihme_loc_id==country] 
  
  dat_country_pred$load_df(
    data = df_mrbrt_country,
    col_covs = list(
      "years_since_intro", "cv_stockout")
  )
  
  df_mrbrt_country$preds <- invlogit(country_model$predict(data=dat_country_pred))
  
  
  df_mrbrt_country$country_stockout_beta <- country_model$beta_soln[2]
  df_mrbrt_country$country_spline_1_beta <- country_model$beta_soln[3]
  df_mrbrt_country$country_spline_2_beta <- country_model$beta_soln[4]
  df_mrbrt_country$country_spline_3_beta <- country_model$beta_soln[5]
  df_mrbrt_country$country_spline_4_beta <- country_model$beta_soln[6]
  df_mrbrt_country$country_spline_5_beta <- country_model$beta_soln[7]
  
  mrbrt_country_model <- list("MRBRT - country", "NA", country_model, df_mrbrt_country) 
  names(mrbrt_country_model) <- c("name", "formula", "model", "preds")
  
  
  return(mrbrt_country_model)
  
})


message("Combining country-specific MRBRT models into a single set of predictions")

simplified_preds <- lapply(1:length(country_models), function(i) {
  
  if (is.null((country_models[[i]]))) {
    return(NULL)
  } else {
    preds <- subset(country_models[[i]]$preds, select = c("year_id", "location_id", "preds","country_stockout_beta",
                                                          "country_spline_1_beta","country_spline_2_beta","country_spline_3_beta",
                                                          "country_spline_4_beta","country_spline_5_beta"
    ))
    setnames(preds, "preds", "country_preds")
    return(preds)
  }
  
})


simplified_preds <- simplified_preds[lengths(simplified_preds) != 0]
simplified_preds <- rbindlist(simplified_preds)




message("Combining country-specific MRBRT predictions with global MRBRT results")

df_mrbrt_country_preds <- copy(df_mrbrt)
df_mrbrt_country_preds <- merge(df_mrbrt_country_preds, simplified_preds, by = c("location_id", "year_id"), all.x = TRUE)


df_mrbrt_country_preds[!is.na(country_preds), preds := country_preds]
df_mrbrt_country_preds[!is.na(country_stockout_beta), stockout_beta := country_stockout_beta]
df_mrbrt_country_preds[!is.na(country_spline_1_beta), spline_1_beta := country_spline_1_beta]
df_mrbrt_country_preds[!is.na(country_spline_2_beta), spline_2_beta := country_spline_2_beta]
df_mrbrt_country_preds[!is.na(country_spline_3_beta), spline_3_beta := country_spline_3_beta]
df_mrbrt_country_preds[!is.na(country_spline_4_beta), spline_4_beta := country_spline_4_beta]
df_mrbrt_country_preds[!is.na(country_spline_5_beta), spline_5_beta := country_spline_5_beta]

df_mrbrt_betas <- unique(df_mrbrt_country_preds[,list(year_id,location_id,ihme_loc_id,natl_ihme_loc_id,
                                                      stockout_beta,
                                                      spline_1_beta,spline_2_beta,spline_3_beta,spline_4_beta,spline_5_beta
)])
df_mrbrt_country_preds[,stockout_beta:=NULL]
df_mrbrt_country_preds[,country_stockout_beta:=NULL]



message("Writing results to central output: ", outputdir)
fwrite(df_mrbrt_betas,file.path(outputdir, paste0("mrbrt_custom_stage_1_", vaccine_ratio, "_betas.csv")))
fwrite(df_mrbrt_country_preds,file.path(outputdir, paste0("mrbrt_custom_stage_1_", vaccine_ratio, "_results.csv")))

message("Writing results to date_version folder: ", to_model_cs1_dir)
fwrite(df_mrbrt_betas,file.path(to_model_cs1_dir, paste0("mrbrt_custom_stage_1_", vaccine_ratio, "_betas.csv")))
fwrite(df_mrbrt_country_preds,file.path(to_model_cs1_dir, paste0("mrbrt_custom_stage_1_", vaccine_ratio, "_results.csv")))

end_time <- Sys.time()
message(end_time - start_time)


message("All done! Returning to prep_exp")