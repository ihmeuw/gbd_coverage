# Code to create global, super-regional, and regional aggregates with uncertainty -------------------------
# Plotting example at bottom ------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------

########################################################################################################################
# SECTION 1: Prep 
########################################################################################################################


username <- Sys.info()[["user"]]
source(paste0("FILEPATH/init.r"))
library(ggrepel)

### 0. Custom settings
model_version <- "2021-02-11"  
decomp_step <- "iterative"

superregion_exception <- FALSE  
prep_to_share <- FALSE
save_draws <- TRUE  
world_bank_agg <- FALSE   
introduced_locs_only <- FALSE
under_1s  <- 28  


### 1. Pull locs
source("FILEPATH/get_location_metadata.R")
locs <- get_location_metadata(location_set_id=location_set_id, gbd_round_id=gbd_round, decomp_step=decomp_step)  

most_detailed_locs <- locs[most_detailed == 1]

### 2. Pull pops
source('FILEPATH/get_population.R')
populations <- get_population(location_id = unique(locs$location_id), sex_id="3", year_id = "-1", age_group_id=c(under_1s, 238), single_year_age = TRUE, gbd_round_id=gbd_round, decomp_step = decomp_step)
# add a populations col denoting if 0-11mo (cohort 1) or 12-23mo (cohort 2) for location-specific target population aggregations
populations[age_group_id==under_1s, age_cohort := 1]  
populations[age_group_id==238, age_cohort := 2] 


### 3. Pull schedule (MCV1, MCV2, RCV1)
schedule <- readRDS(file.path(ref_data_repo, "vaccine_target.rds"))
schedule <- merge(schedule, locs[,.(ihme_loc_id, location_id)], by="ihme_loc_id", all.x=T)[, ihme_loc_id := NULL]
schedule[age_cohort > 2, age_cohort := 2]

########################################################################################################################
# SECTION 2: Make function
########################################################################################################################


# Function to create global, super-regional, and regional aggregates for a given vaccine

make_vaccine_aggregate <- function(vaccine, gbd_round, run_date, locs = most_detailed_locs, pops = populations, verbose = F, 
                                   superregion_exception = F, world_bank_agg = F, introduced_locs_only = F, sched = schedule) {
  
  if (verbose) message(paste0("\nVaccine: ", vaccine))
  
  # 1. Pull draws
  # Helper function to load draws and combine for all locations
  load_draws <- function(me, loc_ids, gbd_rd, rd) {
    
    draw_dir <- paste0("FILEPATH/", me)
    
    draws <- rbindlist(lapply(loc_ids, function(l) fread(paste0(draw_dir, "/", l, ".csv"))), fill = TRUE)
    
    # If there are missing measure_id or covariate_id, fill in
    if (("measure_id" %in% names(draws)) & ("covariate_id" %in% names(draws))) {
      if (nrow(draws[is.na(measure_id)]) + nrow(draws[is.na(covariate_id)]) > 0) {
        m_id <- unique(draws[!is.na(measure_id)]$measure_id)
        c_id <- unique(draws[!is.na(covariate_id)]$covariate_id)
        
        draws[is.na(measure_id), measure_id := m_id]
        draws[is.na(covariate_id), covariate_id := c_id]
      }
    }
    
    return(draws)
  }
  
  if (verbose) message("  -- Loading draws")
  all_draws <- load_draws(me = vaccine, loc_ids = unique(locs$location_id), gbd_rd = gbd_round, rd = run_date)
  
  ### 2. Merge on population info 
  if (verbose) message("  -- Merging populations")
  if (vaccine %in% c("vacc_mcv1", "vacc_mcv2", "vacc_rcv1")) {
    # merge in pops by mcv-sched-specific age groups
    vax_sched <- sched[me_name==vaccine][, me_name := NULL]
    all_draws <- merge(all_draws, vax_sched, by="location_id", all.x=T)
    all_draws[is.na(age_cohort), age_cohort := 2]
    all_draws <- merge(all_draws, subset(pops, select = c("location_id", "year_id", "sex_id", "population", "age_cohort")), all.x = T, all.y = F, by = c("location_id", "year_id", "sex_id", "age_cohort"))
    all_draws <- all_draws[, age_cohort := NULL]
    
  } else {
    pops <- pops[age_cohort==1][, age_cohort := NULL]
    all_draws <- merge(all_draws, subset(pops, select = c("location_id", "year_id", "sex_id", "population")), all.x = T, all.y = F, by = c("location_id", "year_id", "sex_id"))
    
  }
  
  
  ### 3. Merge on location info
  if (verbose) message("  -- Merging locations")
  all_draws <- merge(all_draws, locs, by = "location_id", all.x = T, all.y = F)
  
  ### 4. Calculate draws of aggregates - number of children vaccinated (by draw) and pop 
  if (verbose) message("  -- Converting to counts")
  draw_cols <- names(all_draws)[grepl("draw_[0-9]*", names(all_draws))]
  non_draw_cols <- setdiff(names(all_draws), draw_cols)
  
  ### 5. Convert to counts (both coverage & population)
  all_draws[, (draw_cols) := .SD * population, .SDcols = draw_cols]
  
  ### 6. Aggregate counts of children vaccinated & population by geography and year
  if (verbose) message("  -- Collapsing by geography")
  global_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "sex_id", "age_group_id", "covariate_id"), .SDcols = c(draw_cols, "population")]
  super_region_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "sex_id", "age_group_id", "covariate_id", "super_region_id", "super_region_name"), .SDcols = c(draw_cols, "population")]
  region_draws <- all_draws[, lapply(.SD, sum), by = c("year_id", "sex_id", "age_group_id", "covariate_id", "super_region_id", "super_region_name", "region_id", "region_name"), .SDcols = c(draw_cols, "population")]
  
  if (save_draws) {
    message("  -- Saving geographic counts draw objects")
    counts_dir <- file.path(agg_dir, "count_draws")
    if (!dir.exists(counts_dir)) dir.create(counts_dir)
    global_draws[, me_name := vaccine]
    fwrite(global_draws, file=paste0(counts_dir, "/", vaccine, "_global_draws.csv"))
    super_region_draws[, me_name := vaccine]
    fwrite(super_region_draws, file=paste0(counts_dir, "/", vaccine, "_super_region_draws.csv"))
    region_draws[, me_name := vaccine]
    fwrite(region_draws, file=paste0(counts_dir, "/", vaccine, "_region_draws.csv"))

  }
  
  
  ### 7. Create draws of percentages (coverage) by year & aggregate location, then calculate mean/upper/lower across draws
  convert_and_collapse <- function(df, global=FALSE, super_region=FALSE) {
    
    # Get draw cols and non draw cols
    dcols <- names(df)[grepl("draw_[0-9]*", names(df))]
    ndcols <- setdiff(names(df), dcols)
    
    # Convert from counts to proportions
    df[, (draw_cols) := .SD / population, .SDcols = draw_cols]
    
    if (save_draws & global) {  
      cov_dir <- file.path(agg_dir, "coverage_draws")
      if (!dir.exists(cov_dir)) dir.create(cov_dir)
      global_save <- copy(df) %>% .[, me_name := vaccine]
      fwrite(global_save, file=paste0(cov_dir, "/", vaccine, "_global_cov_draws.csv"))
    }
    
    if (save_draws & super_region) {  
      cov_dir <- file.path(agg_dir, "coverage_draws")
      if (!dir.exists(cov_dir)) dir.create(cov_dir)
      global_save <- copy(df) %>% .[, me_name := vaccine]
      fwrite(global_save, file=paste0(cov_dir, "/", vaccine, "_super_region_cov_draws.csv"))
    }
    
    
    df[, mean := rowMeans(.SD), .SDcols = dcols]
    df[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = dcols]
    df[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = dcols]
    
    keep_cols <- names(df)[!(names(df) %in% dcols)]
    df <- subset(df, select = keep_cols)
    
    return(df)
  }
  
  if (verbose) message("  -- Collapsing to mean and upper/lower bounds")
  global_df <- convert_and_collapse(global_draws, global=TRUE)
  super_region_df <- convert_and_collapse(super_region_draws, super_region=TRUE)
  region_df <- convert_and_collapse(region_draws)
  
  ### 8. Add some additional information and return the multi-level aggregates
  global_df[, level := "global"]
  super_region_df[, level := "super_region"]
  region_df[, level := "region"]
  
  
    output_df <- rbindlist(list(global_df, super_region_df, region_df), fill = TRUE)
  
  
  output_df[, me_name := vaccine]  
  
  ### 9. save csv for future analyses
  message("  -- Saving aggregated output file to agg_dir")
  fwrite(output_df, file=paste0(agg_dir, "/", vaccine, ".csv"))
  
  

  message("All done")
    
  
  return(output_df)
}


########################################################################################################################
# SECTION 3: Call function 
########################################################################################################################


# Make directory to save output in same folder as mean results
results.root <- paste0(root, "/data/exp/modeled/", gbd_cycle, "/", model_version)  
agg_dir <- file.path(results.root, "aggregate_summaries")
ifelse(!dir.exists(agg_dir), dir.create(agg_dir), FALSE)

# Load all vaccines of interest
vaccines <- c("vacc_dpt1", "vacc_dpt3", "vacc_mcv1", "vacc_pcv3", "vacc_hib3", "vacc_hepb3", 
              "vacc_mcv2", "vacc_polio3", "vacc_rotac", "vacc_bcg", "vacc_rcv1") 

df_vax <- lapply(vaccines, 
                 function(v){
                   make_vaccine_aggregate(vaccine = v, 
                                          run_date = model_version,  
                                          gbd_round = gbd_cycle,  
                                          locs = most_detailed_locs, 
                                          pops = populations, 
                                          verbose = TRUE, 
                                          superregion_exception = superregion_exception, 
                                          world_bank_agg = world_bank_agg, 
                                          introduced_locs_only = introduced_locs_only)
                   
                 }) %>% rbindlist



########################################################################################################################
# SECTION 5: PLOT function output (based just on regular geographic aggregation)
########################################################################################################################
vaccines <- c("vacc_dpt1", "vacc_dpt3", "vacc_mcv1", "vacc_pcv3", "vacc_hib3", "vacc_hepb3",
              "vacc_mcv2", "vacc_polio3", "vacc_rotac", "vacc_bcg", "vacc_rcv1")
df_vax <- lapply(paste0(data_root, "/exp/modeled/", gbd_cycle, "/", model_version, "/aggregate_summaries/", vaccines, ".csv"),
                        fread) %>% rbindlist(., fill=TRUE) %>% unique

if (gbd_cycle=="gbd2020") {
  year_end <- 2019 
  df_vax <- df_vax[year_id <= year_end]
}

df_plot <- subset(df_vax, level == "global")  
df_plot[me_name == "vacc_dpt1", label := "DTP1"]
df_plot[me_name == "vacc_dpt3", label := "DTP3"]
df_plot[me_name == "vacc_hepb3", label := "HepB3"]
df_plot[me_name == "vacc_hib3", label := "Hib3"]
df_plot[me_name == "vacc_mcv1", label := "MCV1"]
df_plot[me_name == "vacc_mcv2", label := "MCV2"]
df_plot[me_name == "vacc_pcv3", label := "PCV3"]
df_plot[me_name == "vacc_polio3", label := "Pol3"]
df_plot[me_name == "vacc_rotac", label := "RotaC"]
df_plot[me_name == "vacc_rcv1", label := "RCV1"]  

df_plot <- df_plot[!is.na(label)]

cols <- c("#8C021C", "#72451C", "#CCA606", "#334403", "#187933", "#064742", "#006E8B", "#331D49", "#5C2266")
cols2 <- c("#E58606","#5D69B1","#52BCA3","#99C945","#CC61B0","#24796C","#DAA51B","#2F8AC4","#764E9F","#ED645A","#CC3A8E","#A5AA99")
cols3 <- c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")
cols_select <- c("#671f90", "#25919a") 



### PUBLICATION GLOBAL TIME SERIES
plot.root <- file.path("FILEPATH/paper", model_version)  
ifelse (!dir.exists(plot.root), dir.create(plot.root, recursive = TRUE), FALSE)

cols3 <- c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99")
cols4 <- c("#82B1E5","#0571EC", 
           "#c3710c","#F2B701", 
           "#E7073F","#891238", 
           "#80BA5A","#66169F", 
           "#5C5751", "#307404") 

ifelse(!dir.exists(file.path(plot.root, "global_time_series")), dir.create(file.path(plot.root, "global_time_series")), FALSE)

pdf(file = paste0(plot.root, "/FILEPATH/vaccines_global_to_", year_end, ".pdf"),
    width = 8,
    height = 5)

ggplot(data = df_plot, 
       aes(x = year_id, 
           y = mean,
           ymin = lower,
           ymax = upper,
           group = label)) +
  
  geom_hline(aes(yintercept=0.9), linetype="dotted") +
  annotate("text", x = 1980, y = 0.9, label = "GVAP Target", vjust = 1.5, hjust = 0, size=5) +
  
  geom_ribbon(alpha = 0.15, aes(fill = label)) +
  geom_line(alpha = 0.95, aes(color = label)) +
  
  coord_cartesian(ylim=c(-0.05, 1.05), xlim=c(1979, 2020), expand=TRUE) +

  theme_minimal() + 
  theme(axis.title=element_text(),
        strip.text=element_text(size=12, face ="bold"),
        strip.background=element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size = 11)
  ) +

  labs(x = "Year", y = "Coverage", color = "Vaccine", fill = "Vaccine", 
       title = "Figure 1. Global vaccine coverage by vaccine, 1980 to 2019.") +  
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1) %>% round(., 2), 
                     labels = scales::percent, 
                     limits = c(0,1), 
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = c(seq(1980, 2015, by = 5), as.numeric(year_end))) +
  scale_color_manual(values = cols4) +
  scale_fill_manual(values = cols4) #+
  

dev.off()




### SR FACETS PLOT 
df_plot <- subset(df_vax, level == "super_region")  
df_plot[me_name == "vacc_dpt1", label := "DTP1"]
df_plot[me_name == "vacc_dpt3", label := "DTP3"]
df_plot[me_name == "vacc_hepb3", label := "HepB3"]
df_plot[me_name == "vacc_hib3", label := "Hib3"]
df_plot[me_name == "vacc_mcv1", label := "MCV1"]
df_plot[me_name == "vacc_mcv2", label := "MCV2"]
df_plot[me_name == "vacc_pcv3", label := "PCV3"]
df_plot[me_name == "vacc_polio3", label := "Pol3"]
df_plot[me_name == "vacc_rcv1", label := "RCV1"]
df_plot[me_name == "vacc_rotac", label := "RotaC"]

df_plot <- df_plot[!is.na(label)]

# add in lancet_label
df_plot[, location_name := super_region_name]
df_plot <- merge(df_plot, locs[,.(location_name, lancet_label)], by="location_name", all.x=T)

# plotting directory
ifelse(!dir.exists(file.path(plot.root, "FILEPATH")), dir.create(file.path(plot.root, "FILEPATH"), recursive=TRUE), FALSE)

# plot
## png:
png(file = paste0(plot.root, "/FILEPATH/vaccines_SR_to_", year_end, "_lancet_label.png"),
    width = 13,
    height = 8,
    units = "in",
    res = 600)

ggplot(data = df_plot, 
       aes(x = year_id, 
           y = mean,
           ymin = lower,
           ymax = upper,
           group = label)) +
  
  geom_hline(aes(yintercept=0.9), linetype="dotted") +
  annotate("text", x = 1980, y = 0.9, label = "GVAP Target", vjust = 1.5, hjust = 0, size=4) +
  
  geom_ribbon(alpha = 0.1, aes(fill = label)) +
  geom_line(alpha = 0.9, aes(color = label)) +
  
  coord_cartesian(ylim=c(-0.05, 1.05), xlim=c(1979, 2020), expand=TRUE) +
  
  theme_minimal() + 
  theme(axis.title=element_text(),
        plot.title=element_text(face="bold",size=18, hjust = 0.5),
        strip.text=element_text(size=10, face ="bold"),
        strip.background=element_blank(),
        axis.text.x = element_text(size = 10.5, angle = 45),
        axis.text.y = element_text(size = 10.5),
        axis.title.x = element_text(size=10.5),
        axis.title.y = element_text(size=10.5),
        legend.title = element_text(size = 10.5)
  ) +
  
  labs(x = "Year", y = "Coverage", color = "Vaccine", fill = "Vaccine") +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1) %>% round(., 2), 
                     labels = scales::percent, 
                     limits = c(0,1), 
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = c(seq(1980, 2015, by = 5), as.numeric(year_end))) +
  scale_color_manual(values = cols4) +
  scale_fill_manual(values = cols4) +
  
  facet_wrap(~lancet_label)  

dev.off()


## pdf:
pdf(file = paste0(plot.root, "/FILEPATH/vaccines_SR_to_", year_end, "_lancet_label.pdf"),
    width = 13,
    height = 8)

ggplot(data = df_plot, 
       aes(x = year_id, 
           y = mean,
           ymin = lower,
           ymax = upper,
           group = label)) +
  
  geom_hline(aes(yintercept=0.9), linetype="dotted") +
  annotate("text", x = 1980, y = 0.9, label = "GVAP Target", vjust = 1.5, hjust = 0, size=4) +
  
  geom_ribbon(alpha = 0.1, aes(fill = label)) +
  geom_line(alpha = 0.9, aes(color = label)) +
  
  coord_cartesian(ylim=c(-0.05, 1.05), xlim=c(1979, 2020), expand=TRUE) +
  
  theme_minimal() + 
  theme(axis.title=element_text(),
        plot.title=element_text(face="bold",size=18, hjust = 0.5),
        strip.text=element_text(size=10, face ="bold"),
        strip.background=element_blank(),
        axis.text.x = element_text(size = 10.5, angle = 45),
        axis.text.y = element_text(size = 10.5),
        axis.title.x = element_text(size=10.5),
        axis.title.y = element_text(size=10.5),
        legend.title = element_text(size = 10.5)
  ) +
  
  labs(x = "Year", y = "Coverage", color = "Vaccine", fill = "Vaccine") +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1) %>% round(., 2), 
                     labels = scales::percent, 
                     limits = c(0,1), 
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = c(seq(1980, 2015, by = 5), as.numeric(year_end))) +
  scale_color_manual(values = cols4) +
  scale_fill_manual(values = cols4) +
  
  facet_wrap(~lancet_label)  

dev.off()
