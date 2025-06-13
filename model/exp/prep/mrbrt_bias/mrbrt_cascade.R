









if(interactive()){
  
  
  
  
  run_date          = "2024_03_22_post_dpt1"
  code_root         = file.path("FILEPATH", Sys.info()[["user"]], "vaccines/init.r")
  plot_mrbrt_bias   = FALSE
  cov_id            = as.integer(1099)
  work_dir          = file.path("FILEPATH", run_date)
  to_model_bias_dir = file.path("FILEPATH", run_date, "admin_bias")
  me_name_i         = c("vacc_mcv1", "vacc_dpt3", "vacc_bcg", "vacc_polio3", "vacc_dpt1")[1]
  
} else {
   
   
   
   message("Starting arg-parser.")
   r_lib_team_dir <- "FILEPATH"
   withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
   se$parse_all_named_cli_args(
      required_args = list(
         run_date            = "character"
         , code_root         = "character"
         , plot_mrbrt_bias   = "logical"
         , cov_id            = "integer"
         , work_dir          = "character"
         , to_model_bias_dir = "character"
         , me_name_i         = "character"
      )
   )
   
}




if(!cov_id %in% c(881, 1099)) stop("MRBRT bias model only defined for covariate_ids 881 and 1099")

start_time <- Sys.time()


username <- Sys.info()[["user"]]

if (Sys.info()["sysname"] == "Linux") {
  j_root <- "FILEPATH"
  h_root <- "~/"
  central_lib <- "FILEPATH"
} else {
  j_root <- "J:/"
  h_root <- "H:/"
  central_lib <- "K:/libraries/"
}


lib.loc <- paste0(h_root,"R/",R.Version()$platform,"/",R.Version()$major,".",R.Version()$minor)
dir.create(lib.loc, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib.loc, .libPaths()))

packages <- c(
  "parallel",
  "data.table",
  "magrittr",
  "ggplot2",
  "msm",
  "dplyr",
  "gridExtra",
  "ggpubr",
  "wCorr",
  "dplyr",
  "matrixStats",
  "msm",
  "argparse",
  "reticulate"
)

for(p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

Sys.setenv("RETICULATE_PYTHON" = "FILEPATH") 
library(reticulate)
reticulate::use_python("FILEPATH")
mr <- import("mrtool")
cw <- import("crosswalk")



Sys.setenv("RETICULATE_PYTHON" = "FILEPATH")
library(mrbrt003, lib.loc = "FILEPATH")
source(file.path(code_root, "init.r"))
source(file.path(central_lib, "FILEPATH/get_location_metadata.R"))
source(file.path(central_lib, "FILEPATH/get_covariate_estimates.R"))


hierarchy <- get_location_metadata(location_set_id = 22, release_id = release_id)
nats      <- hierarchy[level == 3, location_id]

log_trans      <- TRUE  
pre_outlier    <- TRUE  
date           <- as.character(run_date)
bias_data_date <- copy(date)

n_draws   <- 100  
draw_cols <- paste0("draw_", 0:(n_draws - 1))



bias_data_path <- file.path(data_root, "FILEPATH", gbd_cycle, bias_data_date, "admin_bias")  






cov <- get_covariate_estimates(covariate_id = cov_id, location_id = unique(hierarchy[level >= 3]$location_id), release_id = release_id)
cov <- cov[ , .(location_id, year_id, mean_value)]
if (cov_id == 1099) cov[, mean_value := mean_value / 100]





model_dir              <- file.path(work_dir, me_name_i)
to_model_bias_dir_vacc <- file.path(to_model_bias_dir, me_name_i)
dir.create(model_dir,              showWarnings = FALSE, recursive = TRUE)
dir.create(to_model_bias_dir_vacc, showWarnings = FALSE, recursive = TRUE)


if (pre_outlier) {
   dt <- readRDS(file.path(bias_data_path, "bias_correction_data_pairs_pre_outlier.rds"))[me_name==me_name_i] %>% setnames(., "data", "val")
   
} else {
   dt <- fread(file.path(bias_data_path, paste0(me_name_i, ".csv")))
}


dt <- dt[, .GRP, by=names(dt)]
setnames(dt, "GRP", "row_id")



if (log_trans) {
   
   dt[ , log_val    := log(val)]
   dt[ , log_val_se := deltamethod(~ log(x1), val, variance), by = c("row_id", "location_id", "year_id")]  
   dt[ , weight     := 1 / log_val_se ^ 2]
   dt[ , lower      := exp(log(val) - 1.96 * log_val_se)]
   dt[ , upper      := exp(log(val) + 1.96 * log_val_se)]
}


dt <- merge(dt, cov, by = c("location_id", "year_id"), all.x = TRUE)[!is.na(mean_value)]


fwrite(dt, file = file.path(model_dir, paste0("input_data_", me_name_i, "_raw.csv")))
fwrite(dt, file = file.path(to_model_bias_dir_vacc, paste0("input_data_", me_name_i, "_raw.csv")))





dt[, nat_ihme_loc_id := substr(ihme_loc_id, 0, 3)]
parents <- copy(hierarchy[level == 3, .(location_id, ihme_loc_id)]) %>% 
   setnames(.,
            c("ihme_loc_id", "location_id"),
            c("nat_ihme_loc_id", "nat_id"))

dt <- merge(dt, parents, by = "nat_ihme_loc_id", all.x = T)
fwrite(dt, file = file.path(model_dir, paste0("input_data_", me_name_i, "_subs_as_nats.csv")))
fwrite(dt, file = file.path(to_model_bias_dir_vacc, paste0("input_data_", me_name_i, "_subs_as_nats.csv")))





message(paste0("Running Global model for bias_", me_name_i))

dat_1 <- MRData()
dat_1$load_df(
   data     = dt,  col_obs = "log_val", col_obs_se = "log_val_se",
   col_covs = list("mean_value"), col_study_id = "location_id" 
)

if (cov_id == 881) {
   model <- MRBRT(data       = dat_1,
                  cov_models = list(LinearCovModel("mean_value",
                                                   use_spline = TRUE,
                                                   use_re = FALSE,
                                                   
                                                   spline_knots_type = "frequency",
                                                   spline_knots = array(c(0, 0.2, 0.4, 0.6, 1)),  
                                                   spline_r_linear = TRUE,  
                                                   spline_l_linear = TRUE,  
                                                   prior_gamma_gaussian = array(c(0, 1e-5)),  
                                                   prior_spline_maxder_gaussian = rbind(c(0, 0, 0, 0), c(Inf, Inf, Inf, 1e-3)))),
                  inlier_pct = 0.9)  
} else if (cov_id == 1099) {
   model <- MRBRT(data       = dat_1,
                  cov_models = list(LinearCovModel("mean_value",
                                                   use_spline = TRUE,
                                                   use_re = FALSE,
                                                   
                                                   spline_knots_type = "frequency",
                                                   spline_knots = array(c(0, 0.25, 0.5, 0.75, 1)),  
                                                   spline_r_linear = TRUE,  
                                                   spline_l_linear = TRUE,  
                                                   prior_gamma_gaussian = array(c(0, 1e-5)),  
                                                   prior_spline_maxder_gaussian = rbind(c(0, 0, 0, 0), c(Inf, Inf, Inf, 1e-3)))),
                  inlier_pct = 0.9)  
   
}  

data_temp <- MRData(covs = list(mean_value = array(seq(0,1,0.01))))

model$attach_data(data_temp)


model$fit_model()



global_pred <- expand.grid(location_id = unique(dt$location_id),  
                           mean_value = seq(0,1,0.01)) %>% as.data.table()

dat_pred <- MRData()

dat_pred$load_df(
   data = global_pred,
   col_covs=list("mean_value") 
)



n_samples <- 100L  
samples2  <- model$sample_soln(sample_size = n_samples)

draws2 <- model$create_draws(
   data = dat_pred,
   beta_samples = samples2[[1]],
   gamma_samples = samples2[[2]],
   random_study = FALSE )

global_pred$pred <- model$predict(dat_pred, predict_for_study = FALSE, sort_by_data_id = TRUE) %>% exp()  

global_pred$pred_lo <- apply(draws2, 1, function(x) quantile(x, 0.025)) %>% exp()
global_pred$pred_hi <- apply(draws2, 1, function(x) quantile(x, 0.975)) %>% exp()


global_pred <- merge(global_pred, hierarchy[,.(location_id, location_name)])
global_pred <- merge(global_pred,
                     dt[, .(location_id, mean_value_obs = mean_value, obs = exp(log_val), lower, upper)],
                     allow.cartesian = T)
fwrite(global_pred, file.path(model_dir, "global_pred.csv"), row.names=FALSE)
fwrite(global_pred, file.path(to_model_bias_dir_vacc, "global_pred.csv"), row.names=FALSE)


df_mod2 <- cbind(model$data$to_df(), data.frame(w=model$w_soln)) %>% as.data.table %>% setnames(., "study_id", "location_id")  



if (plot_mrbrt_bias) {
   
   bias_data <- global_pred[,.(location_id, location_name, mean_value_obs, obs)] %>% unique
   plot_dt   <- copy(dt) %>% setnames(., "val", "obs")
   bias_data <- merge(bias_data, plot_dt[,.(location_id, year_id, obs, weight, log_val_se)], by=c("location_id", "obs"), all=T)
   df_trimmed <- copy(df_mod2) %>% setnames(., c("obs", "mean_value"), c("log_obs", "mean_value_obs"))
   bias_data[, log_obs := log(obs)]
   bias_data <- merge(bias_data, df_trimmed, by=c("location_id", "log_obs", "mean_value_obs"), all=T)
   fwrite(bias_data, file.path(model_dir, "plot_bias_data.csv"), row.names=FALSE)
   fwrite(bias_data, file.path(to_model_bias_dir_vacc, "plot_bias_data.csv"), row.names=FALSE)
   
   ribbon_data <- global_pred[,.(mean_value, pred, pred_lo, pred_hi)] %>% unique
   
   
   eek <- ggplot(data=global_pred, aes(x=mean_value, y=log(pred))) +
      geom_line() +
      geom_hline(yintercept=0, alpha=0.25, color="blue") +
      geom_ribbon(data=ribbon_data, aes(x=mean_value, ymin=log(pred_lo), ymax=log(pred_hi)), alpha=0.5, fill="pink") +
      theme_bw()
   
   
   
   eek2 <- eek +
      geom_point(data=bias_data, aes(x=mean_value_obs,
                                     y=log(obs),
                                     size=1/(log_val_se^2),
                                     colour=as.factor(w)), alpha=0.15) +  
      labs(title=paste0("log(bias_", me_name_i, ") vs HAQ"))
   
   
   
   
   pdf(file.path(to_model_bias_dir_vacc, paste0("log_pred_vs_", cov_id, "_global.pdf")), width = 10, height = 7)
   print(eek2)
   dev.off()
   
}






samples_model <- mrbrt003::core$other_sampling$sample_simple_lme_beta(sample_size = 1000L, model = model)
sds_model     <- apply(samples_model, 2, sd)

model_locs <- unique(dt$location_id) 

loc_pred <- data.table()
cov_pred <- data.table()

for(loc_id in model_locs){
   
   message(loc_id)
   
   
   if(grepl("vacc", me_name_i)){  
      theta <- 3  
   }else{
      theta <- 3
   }
   
   dt_loc <- dt[location_id == loc_id]  
   
   dat_2 <- MRData()
   dat_2$load_df(
      data = dt_loc,  col_obs = "log_val", col_obs_se = "log_val_se",
      col_covs = list("mean_value"), col_study_id = "location_id" 
   )
   
   mod2_beta_prior <- rbind(model$beta_soln, sds_model * theta)
   
   mod2_max_der_prior <- rbind(rep(0, ncol(mod2_beta_prior) - 1),
                               c(rep(Inf, ncol(mod2_beta_prior) -2), 0.1))  
   
   model_2 <- MRBRT(data = dat_2,
                    cov_models = list(LinearCovModel("mean_value",
                                                     use_spline = TRUE,
                                                     use_re = FALSE,
                                                     
                                                     spline_r_linear = TRUE,
                                                     spline_l_linear = TRUE,
                                                     spline_knots_type = "frequency",
                                                     spline_knots = array(c(0, 0.25, 0.5, 0.75, 1)),
                                                     prior_beta_gaussian = mod2_beta_prior,
                                                     prior_spline_maxder_gaussian = rbind(c(0, 0, 0, 0), c(Inf, Inf, Inf, 0.1)) 
                    )))  
   
   model_2$attach_data(data_temp)
   
   model_2$fit_model()
   
   
   
   
   dt_pred2 <- expand.grid(location_id = loc_id,  
                           mean_value = seq(0,1,0.01)) %>% as.data.table()
   
   dat_pred2 <- MRData()
   
   dat_pred2$load_df(
      data = dt_pred2,
      col_covs=list("mean_value"),
      col_study_id = "location_id"  
   )
   
   dt_pred2$pred <- model_2$predict(dat_pred2, predict_for_study = TRUE, sort_by_data_id = TRUE) %>% exp()
   
   dt_pred2 <- merge(dt_pred2, hierarchy[,.(location_id, location_name)])
   dt_pred2 <- merge(dt_pred2, dt_loc[, .(location_id, mean_value_obs = mean_value, obs = exp(log_val),   
                                          lower, upper)],
                     allow.cartesian = T)
   
   loc_pred <- rbind(loc_pred, dt_pred2)
   
   
   
   dt_pred3 <- cov[location_id == loc_id] 
   
   dat_pred3 <- MRData()
   
   dat_pred3$load_df(
      data = dt_pred3,
      col_covs=list("mean_value"),
      col_study_id = "location_id"  
   )
   
   dt_pred3$pred <- model_2$predict(dat_pred3, predict_for_study = TRUE, sort_by_data_id = TRUE) %>% exp()
   
   
   cov_pred <- rbind(cov_pred, dt_pred3)
   
   
}





if (plot_mrbrt_bias) {
   
   sub_pred <- copy(cov_pred)
   sub_pred <- merge(sub_pred, hierarchy[,c("location_id", "location_name", "ihme_loc_id", "level")], by="location_id", all.x=T)
   
   
   eek_base <- ggplot(data=global_pred, aes(x=mean_value, y=log(pred))) +
      geom_line() +
      geom_hline(yintercept=0, alpha=0.25, color="blue") +
      theme_bw()
   
   
   sub_plot <- function(loc) {
      
      eek_sub <- eek_base +
         geom_line(data=sub_pred[location_name==loc], aes(x=mean_value, y=log(pred)), color="forestgreen") +
         geom_point(data=bias_data[location_name==loc], aes(x=mean_value_obs,
                                                            y=log_obs,
                                                            size=1/(log_val_se^2),
                                                            colour=as.factor(w)), alpha=0.15) +
         labs(title=paste0("log(bias_", me_name_i, ") vs HAQ: ", loc))
   }
   
   
   
   locs <- unique(sub_pred[level==3]$location_name)
   locs <- sort(locs)
   comparison_plots <- lapply(locs, sub_plot)
   names(comparison_plots) <- locs
   
   file_name <- paste0("log_pred_vs_", cov_id, "_by_loc")
   
   
   pdf(file = paste0(to_model_bias_dir_vacc, "/", file_name, ".pdf"),
       onefile = T,
       height = 7,
       width = 10)
   
   for (loc in locs) {
      message(paste0("  Subnat ", me_name_i, " plot loc ", loc))
      print(comparison_plots[[loc]])
   }
   
   dev.off()
   
   
}





global_locs <- setdiff(hierarchy[level >= 3, location_id], model_locs)  

cov_pred_global <- cov[location_id %in% global_locs]

dat_pred4 <- MRData()

dat_pred4$load_df(
   data         = cov_pred_global,
   col_covs     = list("mean_value"),
   col_study_id = "location_id"
)

cov_pred_global$pred <- model$predict(dat_pred4, predict_for_study = FALSE, sort_by_data_id = TRUE) %>% exp()

cov_pred <- rbind(cov_pred, cov_pred_global)  


cov_pred[, me_name := me_name_i]
write.csv(cov_pred, paste0(model_dir, "/mrbrt_", me_name_i, "_results.csv"), row.names = FALSE)
write.csv(cov_pred, paste0(to_model_bias_dir_vacc, "/mrbrt_", me_name_i, "_results.csv"), row.names = FALSE)


message("lobstr::mem_used() = ", capture.output(lobstr::mem_used()))
run_time <- capture.output(round(Sys.time() - start_time, 2))
done_message <- paste("MRBRT bias cascade done - runtime =", run_time)
message(done_message)

writeLines(done_message, file.path(to_model_bias_dir_vacc, "done.txt"))
