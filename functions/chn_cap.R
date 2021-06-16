# Functions to shift distributions in logit space in order to conform to theoretical maximums from stock data

# Author: USERNAME spring 2019, updated by USERNAME spring 2020


#rm(list = ls())
lapply(c("data.table", "ggplot2", "tidyr", "dplyr", "magrittr", "jsonlite", "rio"), library, character.only = T)

########################################################################################################
# Function to transform distribution in logit space to agree with a maximum value from stock data

#' stock_limit()
#'
#' Function to transform distribution of vaccine coverage draws in logit space to agree with a maximum value
#' from stock data. The idea here is to calculate the theoretical maximum coverage assuming that there is no
#' dropout and no wastage, then find some factor (`k`) that can be used to transform the distribution of
#' vaccine coverage draws obtained from the ST-GPR model so that a given quantile of the distribution is located
#' at that theoretical maximum coverage line. For instance, if the stock data implies that vaccine coverage
#' can be no higher than 10%, one might select the 97.5th %ile of draws as the target. This function will
#' shift the distribution in logit space such that the 97.5th %ile of the distribution is at 10%.
#'
#' In essence:
#' logit(target quantile of ST-GPR distribution) + k = logit(max coverage per stock data)
#'
#' The function is currently written to work only with stock data from China. Note that:
#'
#' For PCV3, stock data is not available for 2019 at the time that this function was written, so the value of `k`
#' from 2018 is used instead. This implies that the relationship between ST-GPR and stock data will be similar in 2019
#' as it is in 2019. As a new PCV-13 formulation was introduced in 2017, there isn't enough data to estimate either
#' stock in 2019 or `k` in 2019 using a model-based approach, so we considered 2018 `k` to be our best prior.
#' 
#' For rotac, several assumptions are made. First, both the Lanzhou lamb rotavirus vaccine and Rotateq are included in this
#' stock data set. Rotateq is given in a 3-dose series, all under the age of 1, so (total doses in stock data / pop) / 3
#' was used to calculate the theoretical maximum number of children immunized under the age of 1 with Rotateq. For the Lanzhou
#' lamb rotavirus vaccine, a schedule of "annual doses from 2mo-3y" is included. To continue the assumption of no wastage,
#' no dropout,  (total doses in stock data / pop) / 4 was used, assuming that 1/4 of doses go to < 1 year olds, and "complete
#' immunization" at 1 year of age is just one dose.  This is a somewhat crude assumption but as we don't have additional data
#' to understand differential age patterns in receipt of vaccination, was used.
#'
#' Second, for rotac, the vaccine has been in use since 2000 but we only have stock data for 2007-2018. The relationship between `k` and
#' year is roughly linear, so `k` was linearly extrapolated back to 2000 and for 2019. This assumes that the relationship between ST-GPR
#' modeled coverage and max coverage from stock data (in logit space) has been changing constantly over time.
#'
#'
#' Other limitations / areas for future work:
#' - Does not currently incorporate population uncertainty. This could be approached by creating 1000 draws of `k` using population draws
#' - Does not incorporate uncertainty in `k` from the linear model / prediction framework. This could also be addressed by
#'   switching to draws of `k` rather than a single scalar, and propogating uncertainty in these draws
#' - Every year of new data added needs to be manually set up in the innards of this function, and the end_year needs to be changed
#'   to match the final data-year available.
#'
#' Last, note that we will need to update the calls to the central functions with each gbd round as they are hard-coded to GBD 2019.
#' This has been modified in GBD 2020 to be more gbd_cycle-flexible (gbd_cycle as specified in the sourced `init.r` script) (NG)
#'
#' @param gbd_round GBD round for which vaccine draws should be pulled. "gbd_2019", etc.
#' @param vaccine vaccine of interest. "pcv3", "rotac", etc.
#' @param run_date ST-GPR run-date to use to pull vaccine draws
#' @param loc_name location name of interest. For now "China" only. Could adapt to use loc_id instead
#' @param quantile_max quantile of the ST-GPR-produced distribution of coverage to calibrate to the theoretical max from stock data.
#'                     default is 97.5th %ile since this appears to give reasonable results (using the maximum draw shifts the mean
#'                     down very far when right-tailed)
#'
#' @returns A list with two items
#'          1) `k_table`: a table containing all of the major components of this transformation:
#'             a) `location_id`, `year_id`, `age_group_id`, `sex_id`, `covariate_id` for each row
#'             b) `quant_max`: the original value of the quantile of interest from ST-GPR draws (ie 97.5th %ile of original ST-GPR results)
#'             c) `mean`: the original mean coverage estimate for that year
#'             d) `lower`: the original lower (2.5th %ile) coverage bound
#'             e) `upper`: the original upper (97.5th %ile) coverage bound
#'             f) `stock_max_n`: the theoretical max number of children vaccinated per stock data
#'             g) `population`: population of under-1 year old children from GBD
#'             h) `stock_max_cvg`: the theoretical max coverage from stock data (`stock_max_n` / `population`)
#'             i) `k`: final value of k used to transform draws
#'             j) `k_pred`: if applicable, predicted values of k, ie from a linear model
#'             k) `k_original`: if applicable, original value of k (for reference)
#'             l) `mean_trans`: mean coverage estimate after transformation
#'             m) `lower_trans`: lower (2.5th %ile) coverage bound after transformation
#'             n) `upper_trans`: upper (97.5th %ile) coverage bound after transformation
#'          2) `trans_draws`: a draw object containing the transformed draws



stock_limit <- function(gbd_cycle,
                        vaccine,
                        run_date,
                        loc_name,
                        quantile_max = 0.975, 
                        last_data_yr, 
                        last_est_yr) {

  # 0. Setup and consistency checks

  # Temporary check - China only for now
  if (loc_name != "China") stop("Only set up to handle China for now")

  # Load locations table
  source("FILEPATH/get_location_metadata.R")
  locations <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step="iterative")

  # Grab the relevant location_id for `loc_names`
  loc_id <- subset(locations, location_name %in% loc_name)$location_id


  # 1. Define functions ##################################################

  logit <- function(x) {
    log(x/(1-x))
  }

  invlogit <- function(x) {
    exp(x)/(1+exp(x))
  }


  # 2. Load data sources #################################################

  # A. Vaccine draws -----------------------------------------------------

  vax_draws <- fread(paste0("FILEPATH/", loc_id, ".csv"))

  # B. Population data ---------------------------------------------------

  source("FILEPATH/get_population.R")

  population <- get_population(gbd_round_id=gbd_round,
                               age_group_id=28, # Loading 0-1 year olds, to correspond with GBD estimates
                               sex_id=3,  # Both sexes
                               location_id=loc_id,
                               year_id=unique(vax_draws$year_id),
                               decomp_step="iterative")

  population <- subset(population, select = c("location_id","year_id", "population"))

  # C. Stock data --------------------------------------------------------

  # Read in the stock data file, and integrate new data year per cycle
  df_stock <- as.data.table(rio::import("FILEPATH/PCVPPVHPVDTP comboHibRota 2007-2018.xlsx", skip=1))
  df_stock2 <- as.data.table(rio::import("FILEPATH/PCV_PPV_HPV_DTP_COMBOHIBROTA_Linksbridge_2019.xlsx", skip=1))
  
  df_stock <- merge(df_stock, df_stock2, by=c("Category", "Product_EN", "Company_EN"), all.x=T, all.y=T)  

  # Subset to vaccines of interest and convert to theoretical max number vaccinated per year

  if (vaccine == "pcv3") {

    df_stock <- subset(df_stock, Category == "PCV Total", select = c("Category", as.character(c(2007:last_data_yr))))

    # Transform long
    df_stock <- melt(df_stock, 
                     id.vars = "Category", 
                     measure_vars = as.character(c(2007:last_data_yr)))
    setnames(df_stock, c("variable", "value"), c("year_id", "stock_doses"))
    df_stock$year_id <- as.numeric(as.character(df_stock$year_id)) # convert from factor to numeric

    # Assume that NA indicates zero doses 
    df_stock[is.na(stock_doses), stock_doses := 0]

    # Calculate the theoretical maximum children vaccinated
    df_stock[, stock_max_n := stock_doses / 3]

    # Subset to columns of interest
    df_stock <- subset(df_stock, select = c("year_id", "stock_max_n"))

  } else if (vaccine == "rotac") {

    df_stock <- subset(df_stock, Product_EN %in% c("Rotavirus(5V)", "Rotavirus"), select = c("Product_EN", "Company_EN", as.character(c(2007:last_data_yr))))

    # Transform long
    df_stock <- melt(df_stock,
                     id.vars = c("Product_EN", "Company_EN"),
                     measure.vars = as.character(c(2007:last_data_yr)))
    setnames(df_stock, c("variable", "value"), c("year_id", "stock_doses"))
    df_stock$year_id <- as.numeric(as.character(df_stock$year_id)) # convert from factor to numeric

    # Assume that NA indicates zero doses 
    df_stock[is.na(stock_doses), stock_doses := 0]

    # Calculate the theoretical maximum children vaccinated
    # Note: this assumes that for the lamb rota vaccine by LIBP there is no dropout and no wastage
    df_stock[Product_EN == "Rotavirus(5V)", stock_max_n := stock_doses / 3] # 3 dose series for RotaTeq
    df_stock[Product_EN == "Rotavirus", stock_max_n := stock_doses / 4] # Annual series at 2 mo - 3 years for LIBP lamb rota vaccine = 4 doses

    # Collapse to a single year
    df_stock <- df_stock[, .(stock_max_n = sum(stock_max_n)), by = year_id]

  } else if (vaccine == "hib3") {
    
    df_stock <- subset(df_stock, Category == "Hib Total", select = c("Category", as.character(c(2007:last_data_yr))))
    
    # Transform long
    df_stock <- melt(df_stock, 
                     id.vars = "Category", 
                     measure_vars = as.character(c(2007:last_data_yr)))
    setnames(df_stock, c("variable", "value"), c("year_id", "stock_doses"))
    df_stock$year_id <- as.numeric(as.character(df_stock$year_id)) # convert from factor to numeric
    
    # Assume that NA indicates zero doses 
    df_stock[is.na(stock_doses), stock_doses := 0]
    
    # Calculate the theoretical maximum children vaccinated
    df_stock[, stock_max_n := stock_doses / 3]
    
    # Subset to columns of interest
    df_stock <- subset(df_stock, select = c("year_id", "stock_max_n"))
    
  } else {
    
    stop("Only currently set up to handle pcv3 and rotac, adding in hib3 for GBD 2020")
    
  }

  # 3. Create a table for calculation of k to use in logit-space distribution shifts

  # A. First, quantiles of draws -----------------------------------------

  # For convenience, figure out which are the draw columns from our vaccine draw set
  draw_cols <- names(vax_draws)[grepl("draw_*", names(vax_draws))]
  non_draw_cols <- names(vax_draws)[!grepl("draw_*", names(vax_draws))]

  # Start to build `k_table`, which will hold all of the loc-year specific calibration information
  # Calculate the mean & quantile of interest to calibrate to the maximum stock coverage
  k_table <- vax_draws[, .(quant_max = matrixStats::rowQuantiles(as.matrix(.SD), probs = quantile_max),
                           mean = rowMeans(.SD),
                           lower = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025),
                           upper = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975)),
                       .SDcols = draw_cols,
                       by = non_draw_cols]

  # B. Next, merge on population and stock data --------------------------

  # Merge the stock data
  k_table <- merge(k_table, df_stock, all.x = T)

  # Merge the population data
  k_table <- merge(k_table, subset(population, select = c("year_id", "population"), all.x = T))

  # C. Calculate k to shift distributions  -------------------------------

  # Calculate maximum possible coverage from stock data
  k_table[, stock_max_cvg := stock_max_n / population]

  # Calculate k for each year with stock data
  k_table[, k := logit(stock_max_cvg) - logit(quant_max)]

  # D. Some logical checks

  df_stgpr_no_stock <- subset(k_table, stock_max_cvg == 0 & quant_max > 0)
  if (nrow(df_stgpr_no_stock) > 0) {
    message("The following years have non-zero coverage estimates from ST-GPR but zero estimates from stock data: check intro years?")
    message("Note that coverage in these years will be set to zero in the stock data transformation")
    print(df_stgpr_no_stock)
  }

  df_stock_no_stgpr <- subset(k_table, stock_max_cvg > 0 & quant_max == 0)
  if (nrow(df_stock_no_stgpr) > 0) {
    message("The following years have non-zero coverage estimates from stock data but zero estimates from ST-GPR: need to check intro years!")
    print(df_stock_no_stgpr)
    stop("Check intro years prior to continuing")
  }

  # E. Fill in missing values of k for years with no stock data available

  # For PCV, assume that 2019 will have the same relationship between ST-GPR estimates and eventual stock data as 2018
  # A new PCV13 vaccine is being phased in since 2017 so not reasonable to use the overall trend over time
  if (vaccine == "pcv3") {

    k_table[, k_original := k]
    k_table[year_id %in% (last_data_yr+1):year.est.end]$k <- k_table[year_id == last_data_yr]$k  

  }

  if (vaccine == "rotac") {
    # Trend in k vs year is relatively linear - use a linear regression to estimate k in years without stock data
    k_table[, k_original := k]
    k_table[year_id %in% (last_data_yr+1):year.est.end]$k <- k_table[year_id == last_data_yr]$k  
    k_table[year_id %in% 2000:2006]$k <- k_table[year_id == 2007]$k  

  }
  
  if (vaccine == "hib3") {
    
    k_table[, k_original := k]
    k_table[year_id %in% (last_data_yr+1):year.est.end]$k <- k_table[year_id == last_data_yr]$k  
    k_table[year_id %in% 2000:2006]$k <- k_table[year_id == 2007]$k  
    
  }


  # F. Transform the draws object ----------------------------------------

  # Transform all draws for which we have `k`
  years_to_transform <- unique(k_table[!is.na(k)]$year_id)

  #Create a copy of the vaccine draws object (only years that will be transformed)
  trans_draws <- copy(vax_draws)
  trans_draws <- subset(trans_draws, year_id %in% years_to_transform)

  # merge on k values
  trans_draws <- merge(trans_draws, subset(k_table, select = c("year_id", "k")), by = "year_id")

  # apply the transformation at the draw level
  trans_draws[, (draw_cols) := lapply(.SD, function(x) {invlogit(logit(x) + trans_draws[['k']])}), .SDcols = draw_cols]

  # Check to ensure that this worked
  trans_draws_summary <- trans_draws[, .(mean_trans = rowMeans(.SD),
                                         quant_trans = matrixStats::rowQuantiles(as.matrix(.SD), prob = quantile_max),
                                         lower_trans = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025),
                                         upper_trans = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975)),
                                     .SDcols = draw_cols,
                                     by = non_draw_cols]

  # Add this to k_table
  k_table <- merge(k_table, trans_draws_summary, by=non_draw_cols, all.x=T)

  # Last, create a final draws object that substitutes the transformed draws
  trans_draws <- rbind(subset(vax_draws, !(year_id %in% years_to_transform)),
                       subset(trans_draws, select = names(trans_draws)[names(trans_draws) != "k"]),
                       use.names = TRUE)

  trans_draws <- trans_draws[order(year_id),]

  # Check to ensure same dimensions and all years represented
  if (!all.equal(dim(trans_draws), dim(vax_draws))){
    stop("Dimensions of transformed & original draws not the same - need to debug!")
  }

  if (!all.equal(subset(trans_draws, select = non_draw_cols), subset(vax_draws, select = non_draw_cols))) {
    stop("The non-draw columns of the original & transformed draws are not equal - need to debug!")
  }

  # Plot the output
  plot.root <- file.path(data_root, "exp/modeled", gbd_cycle, date, "china_transformation_plots")
  ifelse(!dir.exists(plot.root), dir.create(plot.root, recursive=TRUE), FALSE)
  if (vaccine=="pcv3") vaccine_disp <- "PCV3" else if (vaccine=="hib3") vaccine_disp <- "Hib3" else if (vaccine=="rotac") vaccine_disp <- "RotaC"

  png(file = paste0(plot.root, "/china_transformation_", vaccine, ".png"),
      width = 8,
      height = 6,
      units = "in",
      res = 600)
  ggplot(data = k_table) +
    geom_ribbon(aes(x=year_id, ymin = lower, ymax = upper), fill = "#e41a1c", alpha = 0.2) +
    geom_line(aes(x=year_id, y = mean), color = "#e41a1c") +
    geom_ribbon(aes(x=year_id, ymin = lower_trans, ymax = upper_trans), fill = "#377eb8", alpha = 0.2) +
    geom_line(aes(x=year_id, y = mean_trans), color = "#377eb8") +
    geom_point(aes(x=year_id, y = stock_max_cvg), color = "#4daf4a") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    theme_bw() +
    labs(title = paste0(vaccine_disp, " coverage: China"),
         subtitle = "Calibration to stock data",
         caption = "Red: pre-calibration estimates from ST-GPR\nBlue: post-calibration estimates\nGreen: theoretical maximum coverage from stock data",
         y = paste0(vaccine_disp, " coverage"),
         x = "Year")
  dev.off()
  
  # Done
  return(list(k_table = k_table,
              trans_draws = trans_draws))

}
