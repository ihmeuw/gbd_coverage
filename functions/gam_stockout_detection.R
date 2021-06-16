#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    August 2019
# Purpose: Stockout detection function based on original admin data points as reported in the JRF
#***********************************************************************************************************************

#----Source the function

# Function to modify
stockout_eval <- function(vacc_name,
                          iso,
                          methods = c("arima", "loess", "sma", "gam"),
                          logit_transform = TRUE,
                          verbose = FALSE,
                          adjusted_admin = NULL) {

  if (verbose) message(paste0(vacc_name, " | ", iso))

  if (adjusted_admin) {
    a <- copy(df)
    # then make 'a' look like 'a' when using unadjusted amin
    a <- a[nid==203321 & !is.na(data)]
    who.cols <- c("ihme_loc_id", "year_id", "me_name", "data", "cv_admin", "nid", "survey_name")
    a <- a[, who.cols, with=F]
  } else {
    a <- copy(who.admin)
  }
  a$year_id <- as.numeric(as.character(a$year_id))
  # Subset admin data to specified me_name and ihme_loc_id
  a <- a[me_name==vacc_name & ihme_loc_id==iso]
  # sort ascending and assign each row id to loop through
  a <- a[order(year_id)]
  a <- a[, count := seq(1:nrow(a))]

  ## Logit functions
  logit <- function(x) {
    log(x/(1-x))
  }

  invlogit <- function(x) {
    exp(x)/(1+exp(x))
  }


  if (nrow(a)==0) {

    return(a)

  } else if (!is.na(max(a$count)) & max(a$count) > 2) {

    # Function to return interpolated values for both Loess and Arima methods

    predicted <- lapply(min(a$year_id):max(a$year_id), function(y) {
      if (verbose) message(paste0("--- ", y))

      year_list <- min(a$year_id):max(a$year_id)

      # remove point to predict (note: can predict for first or last point with arima)
      # structure is a little different: assign NA
      subdata   <- copy(a)

      # Fill in missing years between the first & last year of the time series
      if (length(year_list[!(year_list %in% subdata$year_id)]) > 0) {
        missing_years <- data.table(ihme_loc_id = unique(subdata$ihme_loc_id),
                                    me_name  = unique(subdata$me_name),
                                    cv_admin  = unique(subdata$cv_admin),
                                    survey_name  = unique(subdata$survey_name),
                                    year_id = year_list[!(year_list %in% subdata$year_id)],
                                    data = NA)
        subdata <- rbind(subdata, missing_years, fill = T, use.names = T)
      }

      subdata <- subdata[order(year_id)]
      subdata[, count := .I]

      subdata[, data_remove_one := data]
      subdata[year_id == y, data_remove_one := NA]

      if (logit_transform == TRUE) {

        # Logit transform
        # First, ensure that no zeros or ones
        subdata[data_remove_one <= 0, data_remove_one := 0.001]
        subdata[data_remove_one >= 1, data_remove_one := 0.999]

        # Then transform
        subdata[, data_remove_one := logit(data_remove_one)]

      }


      # Simple GAM ---------------------------------------------------------------------
      if ("gam" %in% methods) {

        if (length(unique(subdata$data_remove_one[!is.na(subdata$data_remove_one)])) == 1) {
          # Catch edge case where all data is identical and GAM can't fit
          # Just return the single value
          pred_gam <- unique(subdata$data_remove_one[!is.na(subdata$data_remove_one)])
          se_gam <- NA
        } else {

          # Fit a GAM with error catching
          max_k <- min(10, nrow(subdata[!is.na(data_remove_one)])) # Can't have higher k than covariates
          sub_gam <- try(mgcv::gam(data_remove_one ~ s(year_id, k=max_k), data = subdata, method = "REML"), silent = TRUE)
          fit_gam <- try(predict(sub_gam, data.frame(year_id = y), se.fit = TRUE), silent = TRUE)

          if (!("try-error" %in% class(sub_gam)) & !("try-error" %in% class(fit_gam))) {
            pred_gam <- fit_gam$fit
            se_gam <- fit_gam$se.fit
          } else {
            pred_gam <- NA
            se_gam <- NA
          }
        }
      }

      # Arima then Kalman Smoothing approach -------------------------------------------
      if ("arima" %in% methods) {
        # run an auto arima model on all points except 'y', which is set to NA
        sub_arima <-  forecast::auto.arima(subdata$data_remove_one,
                                           stepwise = TRUE,
                                           approximation=TRUE,
                                           seasonal = TRUE)

        # Use Kalman smoothing to generate prediction for missing value
        # This uses the state space form of the auto-fitted arima model and essentially interpolates
        # Code adapted from ADDRESS

        ks <- KalmanSmooth(subdata$data_remove_one, sub_arima$model)

        pred_arima <- sub_arima$model$Z %*% ks$smooth[which(year_list == y),]
        se_arima <- NA 
      }

      # Simple moving average approach -------------------------------------------------
      if ("sma" %in% methods) {

        sub_sma <- smooth::sma(subdata$data_remove_one, h=10)

        pred_sma <- sub_sma$fitted[which(year_list==y)]
        se_sma <- NA 

      }


      # Loess approach -----------------------------------------------------------------
      if ("loess" %in% methods) {
        # Try bandwidth of 0.5
        sub_loess <- loess(data_remove_one ~ year_id, subdata, span = 0.5)

        # Predict now for point left out!
        sub_pred  <- try(predict(sub_loess, newdata=data.frame(year_id = y), se = TRUE), silent = TRUE)

        if ("try-error" %in% class(sub_pred)) {
          failed <- TRUE
        } else if (is.na(as.numeric(sub_pred$se.fit[1])) | is.na(as.numeric(sub_pred$se.fit[1]))) {
          failed <- TRUE
        } else if (sub_pred$se.fit[1]==Inf | is.nan(sub_pred$se.fit[1])) {
          failed <- TRUE
        } else {
          failed <- FALSE
        }

        # Retry with wider bandwidth if original loess / prediction failed
        if (failed == TRUE) {

          # print("Re-predicting with span = 1 because not enough data to not use it all")
          sub_loess <- loess(data_remove_one ~ year_id, subdata, span = 1)
          sub_pred  <- try(predict(sub_loess, newdata=data.frame(year_id = y), se = TRUE), silent = TRUE)

        }

        if ("try-error" %in% class(sub_pred)) {
          # Failed in prediction for second attempt, return NAs
          pred_loess <- NA
          se_loess <- NA
        } else if (is.na(as.numeric(sub_pred$se.fit[1])) | is.na(as.numeric(sub_pred$se.fit[1])) | sub_pred$se.fit[1]==Inf | is.nan(sub_pred$se.fit[1])) {
          # Returning NAs, i.e. if first or last data point, return NAs
          pred_loess <- NA
          se_loess <- NA
        } else {
          pred_loess <- sub_pred$fit[1]
          se_loess <- sub_pred$se.fit[1]
        }
      }

      return_df <- data.table(year_id = y)

      # Need to figure out how to back-transform SEs out of logit space

      if ("arima" %in% methods) {
        pred_arima <- as.numeric(pred_arima)
        if (logit_transform == TRUE) pred_arima <- invlogit(pred_arima)
        return_df <- cbind(return_df,
                           data.table(pred_fitted_arima = pred_arima,
                                      se_arima = as.numeric(se_arima)))
      }

      if ("loess" %in% methods) {
        pred_loess <- as.numeric(pred_loess)
        if (logit_transform == TRUE) pred_loess <- invlogit(pred_loess)
        return_df <- cbind(return_df,
                           data.table(pred_fitted_loess = pred_loess,
                                      se_loess = as.numeric(se_loess)))
      }

      if ("sma" %in% methods) {
        pred_sma <- as.numeric(pred_sma)
        if (logit_transform == TRUE) pred_sma <- invlogit(pred_sma)
        return_df <- cbind(return_df,
                           data.table(pred_fitted_sma = pred_sma,
                                      se_sma = as.numeric(se_sma)))
      }

      if ("gam" %in% methods) {
        pred_gam <- as.numeric(pred_gam)
        if (logit_transform == TRUE) pred_gam <- invlogit(pred_gam)
        return_df <- cbind(return_df,
                           data.table(pred_fitted_gam = pred_gam,
                                      se_gam= as.numeric(se_gam)))
      }

      return(return_df)

    }) %>% rbindlist

    # Add the interpolated values on for the years for which data was present
    a <- merge(a, predicted, by="year_id", all.x = T, all.y = F)

    if ("loess" %in% methods) a[, diff_loess_normal := data - pred_fitted_loess]
    if ("arima" %in% methods) a[, diff_arima_normal := data - pred_fitted_arima]
    if ("sma" %in% methods) a[, diff_sma_normal := data - pred_fitted_sma]
    if ("gam" %in% methods) a[, diff_gam_normal := data - pred_fitted_gam]

    if ("loess" %in% methods) a[, diff_loess := invlogit(logit(pred_fitted_loess) - logit(data))]
    if ("arima" %in% methods) a[, diff_arima := invlogit(logit(pred_fitted_arima) - logit(data))]
    if ("sma" %in% methods) a[, diff_sma := invlogit(logit(pred_fitted_sma) - logit(data))]
    if ("gam" %in% methods) a[, diff_gam := invlogit(logit(pred_fitted_gam) - logit(data))]


    diff_cols <- paste0("diff_", methods)

    a_subset <- a[complete.cases(subset(a, select = diff_cols)),]

    if ("loess" %in% methods) a[, rmse_loess := sqrt(mean(a_subset$diff_loess^2))]
    if ("arima" %in% methods) a[, rmse_arima := sqrt(mean(a_subset$diff_arima^2))]
    if ("sma" %in% methods) a[, rmse_sma := sqrt(mean(a_subset$diff_sma^2))]
    if ("gam" %in% methods) a[, rmse_gam := sqrt(mean(a_subset$diff_gam^2))]

    return(a)

  } else {
    return(a)
  }
}
