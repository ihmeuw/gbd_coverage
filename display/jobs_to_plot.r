#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    Feb 2018
# Purpose: Create time series plots and maps of custom ST-GPR output (national level for time series, all subnationals for maps)
# Run:     source("FILEPATH/jobs_to_plot.r", echo=TRUE)
#***********************************************************************************************************************


#----SETUP--------------------------------------------------------------------------------------------------------------
### load packages
source("FILEPATH/load_packages.R")
load_packages(c("proto", "findpython", "getopt", "argparse", "magrittr", "ggplot2", "data.table", "grid", "scales"))

### setup arguments
parser <- ArgumentParser()
parser$add_argument("--data_path", help="Path to dataset to plot", type="character")
parser$add_argument("--output_path", help="Path to PDF output", type="character")
parser$add_argument("--loc_id", help="Location to parallelize", type="integer")
parser$add_argument("--add_regions", help="Add region data or not?", type="character")
parser$add_argument("--add_outliers", help="Add data outliers from db or not?", type="character")
parser$add_argument("--add_wuenic", help="Add WUENIC estimates for comparison or not?", type="character")
parser$add_argument("--add_mbg", help="Add MBG aggregated estimates or not?", type="character")
parser$add_argument("--y_axis", help="Should the y-axis be constrained to 0-1?", type="character")
parser$add_argument("--all_vaccines", help="Should we plot all 11 vaccines?", type="character")
parser$add_argument("--compare_version", help="Date to compare run to", type="character")
parser$add_argument("--compare_second", help="Second GBD date to compare run to", type="character")
parser$add_argument("--data_compare", help="Do you want to evaluate new data alongside the model?", type="character")  

### save job args as objects
args         <- parser$parse_args()
list2env(args, environment()); rm(args)
add_regions  <- add_regions %>% as.logical
add_outliers <- add_outliers %>% as.logical
add_wuenic   <- add_wuenic %>% as.logical
add_mbg      <- add_mbg %>% as.logical
y_axis       <- y_axis %>% as.logical
all_vaccines <- all_vaccines %>% as.logical
data_compare <- data_compare %>% as.logical 
if (compare_version=="NULL") compare_version <- NULL
if (compare_second=="NULL") compare_second <- NULL
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### read data
print(data_path)
full_dt <- readRDS(data_path)

### figure out x axis
x_min <- 1980 
x_max <- max(full_dt$year_id)
source("FILEPATH/helpers.R")

facet_var    <- "me_name"
x_axis_title <- "Year"
y_axis_title <- "Proportion vaccinated"

### find region and national of country
region <- full_dt[location_id==loc_id, region_id] %>% unique
nat    <- full_dt[location_id==loc_id, national_id] %>% unique
lname  <- full_dt[location_id==loc_id, location_name] %>% unique
liso3  <- full_dt[location_id==loc_id, ihme_loc_id] %>% unique
if ("cv_intro" %in% colnames(full_dt)) {
  intro_dt <- full_dt[location_id==loc_id, .(cv_intro, me_name)] %>% unique
  intro_dt[cv_intro==9999, cv_intro := NA]
  missing_mes <- unique(full_dt$me_name)[!unique(full_dt$me_name) %in% intro_dt]
  intro_dt <- rbind(intro_dt, data.table(me_name=missing_mes, cv_intro=rep(NA, length(missing_mes))), fill=TRUE)
}
if (is.na(nat)) {  
  subnat <- FALSE
} else {
  subnat <- TRUE
}
#***********************************************************************************************************************


#----PREP PLOT----------------------------------------------------------------------------------------------------------
if (all_vaccines) pdf(paste0(output_path), width=21.5, h=5.5) else pdf(paste0(output_path), width=16, h=5.5) 
for (sname in unique(full_dt$sex_name)) {
  
  ### initialize needed data tables
  region_dt <- full_dt[region_id==region & sex_name==sname & location_id != loc_id & year_id >= x_min & year_id <= x_max, ]
  nat_dt    <- full_dt[sex_name==sname & location_id==nat & year_id >= x_min & year_id <= x_max, ]  
  dt        <- full_dt[sex_name==sname & location_id==loc_id & year_id >= x_min & year_id <= x_max, ]
  
  ### data count - what to plot?
  if ("data" %in% names(dt)) {
    count <- nrow(dt[!is.na(data), ])
    r_count <- nrow(region_dt[!is.na(data), ]) 
    n_count <- nrow(nat_dt[!is.na(data), ]) 
  } else {
    count <- 0
    r_count <- 0
    n_count <- 0
  }
  
  ### how much new data to plot?  
  if (data_compare) {
    new_count <- nrow(dt[!is.na(new_data), ])
  } else {
    new_count <- 0
  }
 
  ### set axis range
  maxgpr <- quantile(dt[!is.na(gpr_upper), gpr_upper], 0.999, na.rm=TRUE)
  if (nrow(dt[!is.na(data), ]) > 0) {
    maxdata <- quantile(dt[!is.na(data), data], 0.999, na.rm=TRUE)
    y_max <- ifelse(maxgpr > maxdata, maxgpr, maxdata)
  } else {
    y_max <- maxgpr
  }
  mingpr <- quantile(dt[!is.na(gpr_lower), gpr_lower], 0.001, na.rm=TRUE)
  if (nrow(dt[!is.na(data), ]) > 0) {
    mindata <- quantile(dt[!is.na(data), data], 0.001, na.rm=TRUE)
    y_min <- ifelse(mingpr < mindata, mingpr, mindata)
  } else {
    y_min <- mingpr
  }
  if (y_axis) {
    y_max <- 1.02
    y_min <- 0
  }
  #***********************************************************************************************************************
  
  
  #----PLOT---------------------------------------------------------------------------------------------------------------
  ### make plot
  plot <- ggplot(data=dt, aes(x=year_id, y=y_max)) +
    geom_blank() # set to blank before any lines/data are checked
  
  ### add GPR uncertainty
  plot <- plot + geom_ribbon(data=dt[!is.na(gpr_lower), ], aes(ymin=gpr_lower, ymax=gpr_upper), fill="lightgreen", alpha=0.2)
  
  ### add region/nation data points
  if (add_regions) {
    if (r_count > 0)  plot <- plot + geom_point(data=region_dt, aes(y=data, colour="Region data"), na.rm=TRUE, alpha=0.5) 
    if (subnat & n_count > 0) plot <- plot + geom_point(data=nat_dt, aes(y=data, colour="National data"), na.rm=TRUE, alpha=0.5)
  }
  
  ### add outliered points
  if (("cv_outlier" %in% names(dt)) & add_outliers==TRUE) {
    if (nrow(dt[!is.na(cv_outlier), ]) > 0) {
      plot <- plot +
        geom_errorbar(data=dt[cv_admin != 1, ], aes(ymax=cv_outlier + (1.96 * sqrt(variance)), ymin=cv_outlier - (1.96 * sqrt(variance)), colour="Outlier"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt, aes(y=cv_outlier, colour="Outlier", shape=source_type), alpha=0.5, na.rm=TRUE)
    }
  }
  
  ### add original admin data points
  if (("cv_admin_orig" %in% names(dt))) { 
    if (nrow(dt[!is.na(cv_admin_orig) & !is.na(data), ]) > 0) { 
      plot <- plot + geom_point(data=dt[!is.na(cv_admin_orig) & !is.na(data), ], aes(y=cv_admin_orig, colour="Original Country-reported"), alpha=0.6, na.rm=TRUE, fill=NA, shape=1) 
    } 
  }
  

  ### add location data points
  if (count > 0) {
    if (data_compare & new_count > 0) { 
      # plot non-new data
      plot <- plot +
        geom_errorbar(data=dt[cv_admin != 1, ], aes(ymax=data + (1.96 * sqrt(variance)), ymin=data - (1.96 * sqrt(variance)), colour="Data"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt, aes(y=data, colour="Data", shape=source_type), na.rm=TRUE, alpha=0.9)  
      # plot new_data
      plot <- plot +
        geom_errorbar(data=dt[new_data == 1, ], aes(ymax=data + (1.96 * sqrt(variance)), ymin=data - (1.96 * sqrt(variance)), colour="New data"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt, aes(y=data[new_data==1], colour="New data", shape=source_type), na.rm=TRUE, alpha=0.5)  
      } else {
      plot <- plot +
        geom_errorbar(data=dt[cv_admin != 1, ], aes(ymax=data + (1.96 * sqrt(variance)), ymin=data - (1.96 * sqrt(variance)), colour="Data"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt, aes(y=data, colour="Data", shape=source_type), na.rm=TRUE, alpha=0.9)  
      }
  }
 
  
    ### add comparison model
  if ("gpr_mean_compare" %in% names(dt) & !is.null(compare_version)) {
    plot <- plot + 
      geom_line(aes(y=gpr_mean_compare, colour=compare_version), alpha=0.8)
  }
  
  ### add second GBD comparison model  
  if ("gpr_mean_compare_more" %in% names(dt) & !is.null(compare_second)) {
    plot <- plot +
      geom_line(aes(y=gpr_mean_compare_more, colour=compare_second), alpha=0.8)
  }
  
  ### add GPR line
  plot <- plot + 
    geom_line(data=dt[!is.na(gpr_mean), ], aes(y=gpr_mean, colour="GBD Estimate"))
  
  ### add WUENIC estimates
  if (add_wuenic & "coverage_wuenic" %in% names(dt)) { if (nrow(dt[!is.na(coverage_wuenic), ]) > 0) {
    plot <- plot + geom_line(data=dt[!is.na(coverage_wuenic), ], aes(y=coverage_wuenic, colour="WUENIC"), alpha=0.8)
  } }
  
  ### add intro years
  add_intro_years <- TRUE
  if (add_intro_years) {
    plot <- plot + geom_vline(data=intro_dt, aes(xintercept=cv_intro), colour="black", linetype="dotted", alpha=0.8, na.rm=TRUE)
  }
  
  ### color and shape scales
  lims <- c("Original Country-reported")
  if (add_outliers) lims <- c(lims, "Outlier")
  lims <- c(lims, "Data", "GBD Estimate")
  if (add_mbg & "mean_mbg" %in% names(dt)) lims <- c(lims, "MBG Estimate")
  if (add_wuenic & "coverage_wuenic" %in% names(dt)) lims <- c(lims, "WUENIC")
  if (!is.null(compare_version) & "gpr_mean_compare" %in% names(dt)) lims <- c(lims, compare_version) else compare_version <- "NULL"
  if (!is.null(compare_second) & "gpr_mean_compare_more" %in% names(dt)) lims <- c(lims, compare_second) else compare_second <- "NULL"
  if (data_compare) lims <- c(lims, "New data") 

  vals <- c("#F2465A", "#1E90FF", "violet", "lightgreen", "black", "gray38", "gray70", 
            "#FF99CC", "#99CCFF", "#3399FF", "#218944", "#FF6600", "#ba7ff9", "#7a0000", 
            "Red")
  names(vals) <- c("Linear", "Space-time", "National GPR", "GPR Unraked", "Data", "National data", "Region data",
                   "Original Country-reported", "Outlier", "WUENIC", "GBD Estimate", compare_version, "MBG Estimate", compare_second, 
                   "New data")
  plot <- plot + scale_colour_manual(values = vals,
                                     limits = lims)
  plot <- plot +
    scale_shape_manual(values=c(19, 18, 17, 12, 15, 1),
                       limits=c("Country-reported", "DHS", "MICS", "Other microdata", "Other tabulation", "Imputed"),
                       labels=c("Country-reported", "DHS", "MICS", "Other microdata", "Other tabulation", "Imputed")) +
    guides(colour = guide_legend(override.aes=list(size=3), nrow=1),
           shape  = guide_legend(override.aes=list(size=3), nrow=1))
  
  ### set options
  if (all_vaccines) facets <- 6 else facets <- 4
  plot <- plot + labs(title=paste0(lname, " (", liso3, ")")) + 
    xlab(x_axis_title) +
    ylab(y_axis_title) + facet_wrap(as.formula(paste("~", facet_var)), ncol=facets)
  if (y_axis) {
    plot <- plot + coord_cartesian(ylim=c(y_min, y_max)) + scale_x_continuous(limits=c(x_min, x_max+1), expand=c(0, 0))  
  } else {
    plot <- plot + scale_x_continuous(limits=c(x_min, x_max+1), expand=c(0, 0)) 
  }
  
  ### set theme
  plot <- plot + theme_bw() + 
    theme(axis.title=element_text(),
          plot.title=element_text(face="bold", size=14, hjust=0.5),
          strip.text=element_text(size=10, face="bold"),
          strip.background=element_blank(),
          axis.text=element_text(size=8),
          legend.position="bottom",
          legend.title=element_blank(),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.text=element_text(size=11),
          panel.spacing.x = unit(6, "mm") #,
          )
  
  ### print! 
  print(plot)
}

### close connection
dev.off() 
#***********************************************************************************************************************