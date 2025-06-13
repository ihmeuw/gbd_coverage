




lapply(c("data.table", "ggplot2", "tidyr", "dplyr", "magrittr", "jsonlite", "rio"), library, character.only = T)





















































#' @param gbd_round GBD round for which vaccine draws should be pulled. "gbd_2019", etc.
#' @param vaccine vaccine of interest. "pcv3", "rotac", etc.
#' @param run_date ST-GPR run-date to use to pull vaccine draws
#' @param loc_name location name of interest. For now "China" only. Could adapt to use loc_id instead
#' @param quantile_max quantile of the ST-GPR-produced distribution of coverage to calibrate to the theoretical max from stock data.



#' @returns A list with two items



















stock_limit <- function(gbd_cycle,
                        vaccine,
                        run_date,
                        loc_name,
                        quantile_max = 0.975, 
                        last_data_yr, 
                        last_est_yr,
                        plot_output = TRUE) {

  

  
  if (loc_name != "China") stop("Only set up to handle China for now")

  
  source("FILEPATH/get_location_metadata.R")
  locations <- get_location_metadata(location_set_id=location_set_id, release_id=release_id)

  
  loc_id <- subset(locations, location_name %in% loc_name)$location_id


  

  logit <- function(x) {
    log(x/(1-x))
  }

  invlogit <- function(x) {
    exp(x)/(1+exp(x))
  }


  

  
  
  
  vax_draws <- fread(paste0("FILEPATH", gbd_cycle, "/", run_date, "/vacc_", vaccine, "/", loc_id, ".csv"))
  
  
  

  source("FILEPATH/get_population.R")

  population <- get_population(
    age_group_id    = 28,  
    sex_id          = 3,         
    location_id     = loc_id,
    year_id         = unique(vax_draws$year_id),
    release_id      = release_id,
    location_set_id = location_set_id,
    forecasted_pop  = fhs_run_TF
  )
  
  population <- subset(population, select = c("location_id","year_id", "population"))

  
  

  

  
  df_stock <- as.data.table(rio::import("/home/j/DATA/Incoming Data/CHN/SUBNATIONAL_VACCINATION_COVERAGE/PCVPPVHPVDTP comboHibRota 2007-2018.xlsx", skip=1))
  
  df_stock3 <- as.data.table(rio::import("/home/j/DATA/Incoming Data/CHN/SUBNATIONAL_VACCINATION_COVERAGE/20210409 PCV_PPV_HPV_DTP_COMBOHIBROTA_Linksbridge_2020.xlsx", skip=1))  
  
  df_stock <- merge(df_stock, df_stock3, by=c("Category", "Product_EN", "Company_EN"), all.x=T, all.y=T)  

  

  if (vaccine == "pcv3") {

    df_stock <- subset(df_stock, Category == "PCV Total", select = c("Category", as.character(c(2007:last_data_yr))))

    
    df_stock <- melt(df_stock, 
                     id.vars = "Category", 
                     measure_vars = as.character(c(2007:last_data_yr)))
    setnames(df_stock, c("variable", "value"), c("year_id", "stock_doses"))
    df_stock$year_id <- as.numeric(as.character(df_stock$year_id)) 

    
    df_stock[is.na(stock_doses), stock_doses := 0]

    
    df_stock[, stock_max_n := stock_doses / 3]

    
    df_stock <- subset(df_stock, select = c("year_id", "stock_max_n"))

  } else if (vaccine == "rotac") {

    df_stock <- subset(df_stock, Product_EN %in% c("Rotavirus(5V)", "Rotavirus"), select = c("Product_EN", "Company_EN", as.character(c(2007:last_data_yr))))

    
    df_stock <- melt(df_stock,
                     id.vars = c("Product_EN", "Company_EN"),
                     measure.vars = as.character(c(2007:last_data_yr)))
    setnames(df_stock, c("variable", "value"), c("year_id", "stock_doses"))
    df_stock$year_id <- as.numeric(as.character(df_stock$year_id)) 

    
    df_stock[is.na(stock_doses), stock_doses := 0]

    
    
    df_stock[Product_EN == "Rotavirus(5V)", stock_max_n := stock_doses / 3]  
    df_stock[Product_EN == "Rotavirus", stock_max_n := stock_doses / 4]      

    
    df_stock <- df_stock[, .(stock_max_n = sum(stock_max_n)), by = year_id]

  } else if (vaccine == "hib3") {
    
    df_stock <- subset(df_stock, Category == "Hib Total", select = c("Category", as.character(c(2007:last_data_yr))))
    
    
    df_stock <- melt(df_stock, 
                     id.vars = "Category", 
                     measure_vars = as.character(c(2007:last_data_yr)))
    setnames(df_stock, c("variable", "value"), c("year_id", "stock_doses"))
    df_stock$year_id <- as.numeric(as.character(df_stock$year_id)) 
    
    
    df_stock[is.na(stock_doses), stock_doses := 0]
    
    
    df_stock[, stock_max_n := stock_doses / 3]
    
    
    df_stock <- subset(df_stock, select = c("year_id", "stock_max_n"))
    
  } else {
    
    stop("Only currently set up to handle pcv3 and rotac, also hib3 for GBD 2020+")
    
  }

  

  

  
  draw_cols     <- names(vax_draws)[grepl("draw_*", names(vax_draws))]
  non_draw_cols <- names(vax_draws)[!grepl("draw_*", names(vax_draws))]

  
  
  k_table <- vax_draws[, .(quant_max = matrixStats::rowQuantiles(as.matrix(.SD), probs = quantile_max),
                           mean = rowMeans(.SD),
                           lower = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025),
                           upper = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975)),
                       .SDcols = draw_cols,
                       by = non_draw_cols]
  
  

  
  k_table <- merge(k_table, df_stock, all.x = T)
  
  
  k_table <- merge(k_table, subset(population, select = c("year_id", "population"), all.x = TRUE))
  
  

  
  k_table[, stock_max_cvg := stock_max_n / population]
  
  
  
  k_table[, k := logit(stock_max_cvg) - logit(quant_max)]
  
  
  
  
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

  

  
  
  if (vaccine == "pcv3") {

    k_table[, k_original := k]
    k_table[year_id %in% (last_data_yr+1):last_est_yr]$k <- k_table[year_id == last_data_yr]$k  
  }

  if (vaccine == "rotac") {
    
    

    
    
    
    
    
    

    k_table[, k_original := k]
    k_table[year_id %in% (last_data_yr+1):last_est_yr]$k <- k_table[year_id == last_data_yr]$k  
    k_table[year_id %in% 2000:2006]$k <- k_table[year_id == 2007]$k  
    
  }
  
  if (vaccine == "hib3") {
    
    k_table[, k_original := k]
    k_table[year_id %in% (last_data_yr+1):last_est_yr]$k <- k_table[year_id == last_data_yr]$k  
    k_table[year_id %in% 2000:2006]$k <- k_table[year_id == 2007]$k  
  }

  

  
  
  
  
  
  
  
  

  

  
  years_to_transform               <- unique(k_table[!is.na(k)]$year_id)
  
  
  trans_draws               <- copy(vax_draws)
  trans_draws               <- subset(trans_draws, year_id %in% years_to_transform)
  
  
  trans_draws               <- merge(trans_draws, subset(k_table, select = c("year_id", "k")), by = "year_id")
  
  
  trans_draws[, (draw_cols) := lapply(.SD, function(x) {invlogit(logit(x) + trans_draws[['k']])}), .SDcols = draw_cols]
  
  
  trans_draws_summary <- trans_draws[, .(mean_trans = rowMeans(.SD),
                                         quant_trans = matrixStats::rowQuantiles(as.matrix(.SD), prob = quantile_max),
                                         lower_trans = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025),
                                         upper_trans = matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975)),
                                     .SDcols = draw_cols,
                                     by = non_draw_cols]
  
  
  k_table <- merge(k_table, trans_draws_summary, by=non_draw_cols, all.x=T)
  
  
  trans_draws <- rbind(subset(vax_draws, !(year_id %in% years_to_transform)),
                       subset(trans_draws, select = names(trans_draws)[names(trans_draws) != "k"]),
                       use.names = TRUE)
  
  trans_draws <- trans_draws[order(year_id),]
  
  
  

  
  if (!all.equal(dim(trans_draws), dim(vax_draws))){
    stop("Dimensions of transformed & original draws not the same - need to debug!")
  }

  if (!all.equal(subset(trans_draws, select = non_draw_cols), subset(vax_draws, select = non_draw_cols))) {
    stop("The non-draw columns of the original & transformed draws are not equal - need to debug!")
  }

  
  if(plot_output) {
    library(ggplot2)
    plot.root <- file.path(data_root, "exp/modeled", gbd_cycle, date, "china_transformation_plots")
    ifelse(!dir.exists(plot.root), dir.create(plot.root, recursive=TRUE), FALSE)
    
    vaccine_disp <- switch(
      vaccine,
      pcv3 = "PCV3",
      hib3 = "Hib3",
      rotac = "RotaC"
    )
    
    png(file = paste0(plot.root, "/china_transformation_", vaccine, ".png"),
        width = 8,
        height = 6,
        units = "in",
        res = 600)
    p <- ggplot(data = k_table) +
      geom_ribbon(aes(x=year_id, ymin = lower, ymax = upper), fill = "
      geom_line(aes(x=year_id, y = mean), color = "
      geom_ribbon(aes(x=year_id, ymin = lower_trans, ymax = upper_trans), fill = "
      geom_line(aes(x=year_id, y = mean_trans), color = "
      geom_point(aes(x=year_id, y = stock_max_cvg), color = "
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      theme_bw() +
      labs(title = paste0(vaccine_disp, " coverage: China"),
           subtitle = "Calibration to stock data",
           caption = "Red: pre-calibration estimates from ST-GPR\nBlue: post-calibration estimates\nGreen: theoretical maximum coverage from stock data",
           y = paste0(vaccine_disp, " coverage"),
           x = "Year")
    print(p)
    dev.off()
  }
  
  
  return(list(k_table = k_table,
              trans_draws = trans_draws))
}



