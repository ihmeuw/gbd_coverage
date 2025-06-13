





r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
  message("Starting arg-parser.")
  se$parse_all_named_cli_args(
    required_args = list(
      data_path            = "character"
      , temp_root          = "character"
      , plot_parent_root   = "character"
      
      , add_regions        = "logical"
      , add_outliers       = "logical"
      , add_wuenic         = "logical"
      , y_axis             = "logical"
      , all_vaccines       = "logical"
      , data_compare       = "logical"
      , code_root          = "character"
      , save_legend        = "logical"
      
      , compare_version    = NA
      , compare_second     = NA
      , custom_subset_mes  = NA
      , custom_data_path   = NA
    )
  )
  idx <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
} else {
  if(!is.na(Sys.getenv()[['CODE_ROOT']])){ 
    code_root <- Sys.getenv()[['CODE_ROOT']]
  } else {
    code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
  }
}

if(interactive()){
  data_path          = "FILEPATH/plot_gpr_temp.rds"
  output_path        = "FILEPATH/25318.pdf"
  plot_parent_root   = "FILEPATH"
  temp_root          = "FILEPATH"
  loc_id             = 25318
  add_regions        = FALSE
  add_outliers       = TRUE
  add_wuenic         = FALSE
  y_axis             = TRUE
  all_vaccines       = TRUE
  compare_version    = "NULL_covidimputed"
  compare_second     = "NULL_covidimputed"
  data_compare       = FALSE
  custom_subset_mes  = NULL
  custom_data_path   = NULL
  save_legend        = FALSE
  idx                = 1
} 




source("FILEPATH/load_packages.R")
load_packages(c("proto", "findpython", "getopt", "argparse", "magrittr", "ggplot2", "data.table", "grid", "scales"))

if (!is.null(compare_version) && compare_version %like% "NULL") compare_version <- NULL
if (!is.null(compare_second)  && compare_second  %like% "NULL") compare_second  <- NULL

launch_grid <- readRDS(file = file.path(plot_parent_root, "launch_grid.rds"))
loc_id      <- as.integer(launch_grid[idx]$location_id)
if(length(loc_id) != 1) stop("loc_id must be a single integer ", toString(loc_id))
output_path <- file.path(temp_root, paste0(loc_id, ".pdf"))

message("Plotting ", se$prt_multiline(launch_grid[location_id == loc_id]))



message(data_path)
full_dt <- readRDS(data_path)


x_min <- 1980
x_max <- max(full_dt$year_id)
source("FILEPATH/helpers.R")

facet_var    <- "me_name"
x_axis_title <- "Year"
y_axis_title <- "Proportion vaccinated"


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







if (!is.null(custom_subset_mes)){
  custom_subset_mes <- unlist(strsplit(custom_subset_mes, ","))
  if(!facet_var == "me_name") stop("If using custom_subset_mes - plot dimensions do not yet support faceting by: ", facet_var, "\n -- custom mes: ", toString(custom_subset_mes))
  facet_ncols       <- ceiling(length(custom_subset_mes) / 2)
  custom_nrow       <- ceiling(length(custom_subset_mes) / facet_ncols)
  custom_wdth       <- 4 * facet_ncols
  custom_ht         <- 2.25 * custom_nrow
  scaling_factor    <- 10 / custom_wdth
  pdf_w             <- custom_wdth * scaling_factor
  pdf_h             <- custom_ht * scaling_factor
  
  legend_nrow <- ceiling(16 / (custom_wdth * scaling_factor))
} else if (all_vaccines) {
  pdf_w <- 21.5; pdf_h <- 5.5; legend_nrow <- 2; facet_ncols <- 6
} else {
  pdf_w <- 16; pdf_h <- 5.5; legend_nrow <- 2; facet_ncols <- 4
}

cairo_pdf(output_path, width = pdf_w, height = pdf_h)

for (sname in unique(full_dt$sex_name)) {
  
  

  
  region_dt <- full_dt[region_id==region & sex_name==sname & location_id != loc_id & year_id >= x_min & year_id <= x_max, ]
  nat_dt    <- full_dt[sex_name==sname & location_id==nat & year_id >= x_min & year_id <= x_max, ]
  dt        <- full_dt[sex_name==sname & location_id==loc_id & year_id >= x_min & year_id <= x_max, ]

  
  if ("data" %in% names(dt)) {
    count <- nrow(dt[!is.na(data), ])
    r_count <- nrow(region_dt[!is.na(data), ])
    n_count <- nrow(nat_dt[!is.na(data), ])
  } else {
    count <- 0
    r_count <- 0
    n_count <- 0
  }

  
  if (data_compare) {
    new_count <- nrow(dt[!is.na(new_data), ])
    new_survey_count <- nrow(dt[!is.na(new_data) & cv_admin != 1, ])
  } else {
    new_count <- 0
  }

  
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
  


  
  
  plot <- ggplot(data=dt, aes(x=year_id, y=y_max)) +
    geom_blank() 

  
  plot <- plot + geom_ribbon(data=dt[!is.na(gpr_lower), ], aes(ymin=gpr_lower, ymax=gpr_upper), fill="lightgreen", alpha=0.2)

  
  if (add_regions) {
    if (r_count > 0)  plot <- plot + geom_point(data=region_dt, aes(y=data, colour="Region data"), na.rm=TRUE, alpha=0.5) 
    if (subnat & n_count > 0) plot <- plot + geom_point(data=nat_dt, aes(y=data, colour="National data"), na.rm=TRUE, alpha=0.5) 
  }

  
  if (("cv_outlier" %in% names(dt)) & add_outliers==TRUE) {
    if (nrow(dt[!is.na(cv_outlier), ]) > 0) {
      plot <- plot +
        geom_errorbar(data=dt[cv_admin != 1, ], aes(ymax=cv_outlier + (1.96 * sqrt(variance)), ymin=cv_outlier - (1.96 * sqrt(variance)), colour="Outlier"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt, aes(y=cv_outlier, colour="Outlier", shape=source_type), alpha=0.5, na.rm=TRUE)
    }
  }

  
  if (("cv_admin_orig" %in% names(dt))) {
    if (nrow(dt[!is.na(cv_admin_orig) & !is.na(data), ]) > 0) {
      plot <- plot + geom_point(data=dt[!is.na(cv_admin_orig) & !is.na(data) & old_data==0, ], aes(y=cv_admin_orig, colour="Original Country-reported"), alpha=0.6, na.rm=TRUE, fill=NA, shape=1)
    }
  }


  
  if (count > 0) {
    if (data_compare) {
      
      plot <- plot +
        geom_point(data=dt[old_data==1,], aes(y=data, colour="Old data", shape=source_type), na.rm=TRUE, alpha=0.5)  

      plot <- plot +
        geom_errorbar(data=dt[cv_admin != 1 & old_data==0, ], aes(ymax=data + (1.96 * sqrt(variance)), ymin=data - (1.96 * sqrt(variance)), colour="Data"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt[old_data==0], aes(y=data, colour="Data", shape=source_type), na.rm=TRUE, alpha=0.9)

      
      if (new_survey_count > 0) {
        plot <- plot +
          geom_errorbar(data=dt[new_data == 1 & cv_admin != 1, ], aes(ymax=data + (1.96 * sqrt(variance)), ymin=data - (1.96 * sqrt(variance)), colour="New data"), width=0, alpha=0.5, na.rm=TRUE)
      }

      if (new_count > 0) {
      plot <- plot +
        geom_point(data=dt[new_data==1], aes(y=data, colour="New data", shape=source_type), na.rm=TRUE, alpha=0.8)  
      }
      } else {
      plot <- plot +
        geom_errorbar(data=dt[cv_admin != 1, ], aes(ymax=data + (1.96 * sqrt(variance)), ymin=data - (1.96 * sqrt(variance)), colour="Data"), width=0, alpha=0.5, na.rm=TRUE) +
        geom_point(data=dt, aes(y=data, colour="Data", shape=source_type), na.rm=TRUE, alpha=0.9)

      }
  }




  
  if ("gpr_mean_compare" %in% names(dt) & !is.null(compare_version)) {
    plot <- plot +
      geom_line(aes(y=gpr_mean_compare, colour=compare_version), alpha=0.8)
  }

  
  if ("gpr_mean_compare_more" %in% names(dt) & !is.null(compare_second)) {
    plot <- plot +
      geom_line(aes(y=gpr_mean_compare_more, colour=compare_second), alpha=0.8)
  }

  
  plot <- plot +
    geom_line(data=dt[!is.na(gpr_mean), ], aes(y=gpr_mean, colour="GBD Estimate")) 

  if(!is.null(custom_data_path)){
    stop("Adding custom data requires editing `jobs_to_plot.r`")
    message("\nAdding custom data from: ", custom_data_path, "\n -- Written 2024-10-18\n")
    dtcust <- fread(custom_data_path)
    locations <- fread(file.path(dirname(data_path), "locations.csv"))
    .ihme_loc_id <- unlist(lapply(strsplit(dtcust$location_name_short_ihme_loc_id, "\\|"), function(x){x[2]}))
    dtcust[, ihme_loc_id := .ihme_loc_id]
    dtcust <- merge(dtcust, locations, by = "ihme_loc_id", all.x = TRUE)
    dtcust[, location_name := lancet_label]
    me_db <- fread(file.path(code_root, "reference/me_db.csv"))
    custom_me_names <- me_db[display_name %in% custom_subset_mes, me_name]
    
    
    custom_me_names_coded <- sub("dpt", "pent", custom_me_names)
    
    vars_keep <- c("year_start", "location_id", "ihme_loc_id", "location_name", custom_me_names_coded)
    dtsub <- dtcust[nid == 564903 & location_id == loc_id, ..vars_keep]
    setnames(dtsub, custom_me_names_coded, custom_subset_mes)
    library(tidyr)
    library(dplyr)
    dtsub <- as.data.table(
      pivot_longer(dtsub, cols = all_of(custom_subset_mes), names_to = "me_name", values_to = "data")
    )
    
    dtsub[, year_id := year_start - 1]
    dtsub[, data := as.double(data)/100]
    
    plot <- plot + geom_point(data=dtsub, aes(y=data, colour="New Survey Data"), na.rm=TRUE, alpha=0.9, shape = 8, color = "red", size = 3) +
      labs(caption = "New survey shown in red")
  }

  
  if (add_wuenic & "coverage_wuenic" %in% names(dt)) { if (nrow(dt[!is.na(coverage_wuenic), ]) > 0) {
    plot <- plot + geom_line(data=dt[!is.na(coverage_wuenic), ], aes(y=coverage_wuenic, colour="WUENIC"), alpha=0.8)
  } }

  
  add_intro_years <- TRUE
  if (add_intro_years) {
    plot <- plot + geom_vline(data=intro_dt, aes(xintercept=cv_intro), colour="black", linetype="dotted", alpha=0.8, na.rm=TRUE)
  }

  
  lims <- c("Original Country-reported")
  if (add_outliers) lims <- c(lims, "Outlier")
  lims <- c(lims, "Data", "GBD Estimate")
  if (add_wuenic & "coverage_wuenic" %in% names(dt)) lims <- c(lims, "WUENIC")
  if (!is.null(compare_version) & "gpr_mean_compare" %in% names(dt)) lims <- c(lims, compare_version) else compare_version <- "NULL"
  if (!is.null(compare_second) & "gpr_mean_compare_more" %in% names(dt)) lims <- c(lims, compare_second) else compare_second <- "NULL"
  if (data_compare) lims <- c(lims, "New data", "Old data")

  vals <- c(
    "Linear"                    = "
    "Space-time"                = "
    "National GPR"              = "violet",
    "GPR Unraked"               = "lightgreen",
    "Data"                      = "black",
    "National data"             = "gray38",
    "Region data"               = "gray70",
    "Original Country-reported" = "
    "Outlier"                   = "
    "WUENIC"                    = "
    "GBD Estimate"              = "
    "compare_version"           = "
    "MBG Estimate"              = "
    "compare_second"            = "
    "New data"                  = "red",
    "Old data"                  = "
  )
  
  names(vals)[names(vals) == "compare_version"] <- compare_version
  names(vals)[names(vals) == "compare_second"] <- compare_second
  
  plot <- plot + scale_colour_manual(values = vals,
                                     limits = lims)
  plot <- plot +
    scale_shape_manual(values=c(19, 18, 17, 12, 15, 1),
                       limits=c("Country-reported", "DHS", "MICS", "Other microdata", "Other tabulation", "Imputed"),
                       labels=c("Country-reported", "DHS", "MICS", "Other microdata", "Other tabulation", "Imputed")) +
    guides(colour = guide_legend(override.aes=list(size=3), nrow=legend_nrow),
           shape  = guide_legend(override.aes=list(size=3), nrow=legend_nrow))

  plot <- plot + labs(title=paste0(lname, " (", liso3, ")")) + 
    xlab(x_axis_title) +
    ylab(y_axis_title) + facet_wrap(as.formula(paste("~", facet_var)), ncol=facet_ncols)
  if (y_axis) {
    plot <- plot + coord_cartesian(ylim=c(y_min, y_max)) + scale_x_continuous(limits=c(x_min, x_max+1), expand=c(0, 0))  
  } else {
    plot <- plot + scale_x_continuous(limits=c(x_min, x_max+1), expand=c(0, 0))
  }

  
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
          panel.spacing.x = unit(6, "mm") 
          
          )
  
  
  
  
  legend_path <- file.path(plot_parent_root, "legend_complete.rds")
  if(save_legend == TRUE) {
    plot_legend <- ggpubr::get_legend(plot)
    saveRDS(plot_legend, legend_path)
  }
  if (!file.exists(legend_path)) {
    stop(
      "Plotting requires user to save a completed legend to disk.
      - see diagnostics/plot_vaccines/jobs_to_plot.r for details"
    )
  }
  plot_legend    <- readRDS(legend_path)
  plot_no_legend <- plot + theme(legend.position = "none")
  grid::grid.newpage()
  grid::grid.draw(
    gridExtra::arrangeGrob(
      plot_no_legend,
      plot_legend,
      layout_matrix = matrix(c(1, 2), nrow = 2, byrow = TRUE),
      heights = c(0.9, 0.1)
    )
  )
  
  
}


dev.off()

message("Done.")