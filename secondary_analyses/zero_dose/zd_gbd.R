






r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
   
   message("Starting arg-parser.")
   se$parse_all_named_cli_args(required_args = list(
      code_root = "character"
   ))
} else {
   
   if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
      code_root <- Sys.getenv()['CODE_ROOT']
   } else {
      code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
   }
}






source(file.path(code_root, "init.r"))
source(file.path(code_root, "FILEPATH/params_zd_gbd.R"))


library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(colorspace)
library(ggridges)
library(scales)
library(ggrepel)
library(tidyr)
library(vmTools, lib.loc = paths$r_lib_team_dir) 
library(lemon, lib.loc = paths$r_lib_team_dir)
library(pals, lib.loc = paths$r_lib_team_dir)
library(rcartocolor, lib.loc = paths$r_lib_team_dir) 
library(ggh4x)


R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"), recursive = FALSE, modifiedOnly = FALSE)
R.utils::sourceDirectory(file.path(code_root, "FILEPATH"), recursive = FALSE, modifiedOnly = FALSE)
R.utils::sourceDirectory(file.path(code_root, "FILEPATH"), recursive = FALSE, modifiedOnly = FALSE)



source(file.path(cc_r_lib_root, "get_population.R"))


output_root <- file.path(secondary_analyses_root, "zero_dose_gbd")
output_dv   <- se$get_new_output_dv(output_root, "today")
if(exists("dv_reconnect", where = params) && !is.null(params$dv_reconnect)) output_dv <- dv_reconnect
output_dir  <- file.path(output_root, output_dv)
slt         <- SLT$new(user_root_list = list(output_root = output_root), user_central_log_root = output_root)
slt$create_date_version_folders_with_logs(date_version = output_dv)


message("Output directory: ", output_dir)



{
   
   tables <- list(
      locs  = get_location_metadata(
         release_id        = release_id
         , location_set_id = location_set_id
      )
      , pop_forecast = get_population(
         age_group_id      = params$age_group_id_under_1
         , location_id     = "all"
         , location_set_id = params$location_set_id_forecast
         , release_id      = params$release_id_forecast
         , year_id         = year_start:params$zd_year_end
         , sex_id          = params$sex_id_both
         , forecasted_pop  = TRUE
      )
      
   )
   
   pop_map <- tables$pop_forecast[, .(location_id, year_id, population)]
   loc_map <- tables$locs[, .(location_id, ihme_loc_id, level, path_to_top_parent, most_detailed, super_region_name, location_name_short)]
   
   se$save_file(pop_map, file.path(output_dir, "pop_map.csv"))
   se$save_file(loc_map, file.path(output_dir, "loc_map.csv"))
   
   
   metadata <- c(
      se$build_metadata_shell(code_root = code_root)
      , list(
         init_list    = init_list
         , params     = params
         , paths      = paths
         , output_dir = output_dir
      )
   )
   se$save_file(metadata, f_path = se$increment_file_version(file.path(output_dir, "metadata.yaml")))
   se$save_file(tables,   f_path = file.path(output_dir, "tables.rds"))
   
   
   
   
   aes_map_super_reg_long <- c(
      `Global`                                           = "black", 
      `Central Europe, Eastern Europe, and Central Asia` = "
      `High-income`                                      = "
      `Latin America and Caribbean`                      = "
      `North Africa and Middle East`                     = "
      `South Asia`                                       = "
      `Southeast Asia, East Asia, and Oceania`           = "
      `Sub-Saharan Africa`                               = "
   )
   super_region_factor_levels_long <- names(aes_map_super_reg_long)
   
   aes_map_super_reg <- setNames(
      aes_map_super_reg_long,
      c("Global", "CEECA", "HI", "LAC", "NAMENA", "SA", "SEAEO", "SSA")
   )
   super_region_factor_levels <- names(aes_map_super_reg)
   
}



{
   
   dpt1 <- read_file(paths$dpt1_best)
   
   dpt1 <- merge(dpt1, loc_map, by = c("location_id"), all.x = TRUE)
   dpt1 <- merge(dpt1, pop_map, by = c("location_id", "year_id"), all.x = TRUE)
   dpt1[, age_group_id := 28] 
   
   dpt1 <- dpt1[level %in% c(0, 3), ] 
   dpt1 <- dpt1[year_id <= params$zd_year_current, ]
   
   setnames(
      dpt1
      , c("gpr_mean", "gpr_lower", "gpr_upper")
      , c("coverage", "lower", "upper")
   )
   
   vars_keep <- c(
      "me_name",
      "location_id",
      "year_id",
      "population",
      "age_group_id",
      "coverage",
      "lower",
      "upper",
      "sex_id",
      "level"
   )
   
   dpt1 <- dpt1[, ..vars_keep]
   
   
   message("Binding on global aggregates")
   
   dpt1agg <- se$read_file(paths$dpt1_aggregates)[year_id <= params$zd_year_current]
   dpt1agg[, level := as.integer(
      case_when(
         level == "global" ~ 0L
         , level == "super_region" ~ 1L
         , level == "region" ~ 2L
         , .default = NA_integer_
      )
   )]
   stopifnot(nrow(dpt1agg[is.na(location_id)])==0)
   setnames(dpt1agg, c("mean"), c("coverage"))
   sq_chk <- assert_square(dpt1agg, id_varnames = c("year_id", "location_id"))
   
   dpt1 <- dpt1[!location_id %in% dpt1agg$location_id, ]
   dpt1 <- rbind(dpt1, dpt1agg[, ..vars_keep])
   setorderv(dpt1, c("location_id", "year_id"))
   stopifnot(all(complete.cases(dpt1)))
   sq_chk <- assert_square(dpt1, id_varnames = c("year_id", "location_id"))
}






{
   
   vars_coverage <- c("coverage", "lower", "upper")
   suffixes      <- c("", "_l", "_u") 
   suffixes_zd   <- c("", "_u", "_l") 
   vars_n_vacc   <- paste0("n_vacc", suffixes)
   vars_n_zd     <- paste0("n_zd", suffixes_zd)
   dpt1[, c(vars_n_vacc) := lapply(.SD, function(x) {population * x}), .SDcols = vars_coverage]
   dpt1[, c(vars_n_zd)   := lapply(.SD, function(x) {population - x}), .SDcols = vars_n_vacc]
   
   
   if(params$run_section_flat_pop_counterfactual) dpt1_cf <- copy(dpt1)
   
   
   if(FALSE){
      
      zd_nats_2019 <- dpt1[level == 3 & year_id == 2019, sum(n_zd)]
      zd_glob_2019 <- dpt1[level == 0 & year_id == 2019, n_zd]
      
      
      (zd_nats_2019 - zd_glob_2019) / zd_glob_2019 
   }
   
   
}




{  
   
   
   vars_n_zd_target   <- paste0("n_zd_target", suffixes_zd)
   vars_targ_2030     <- paste0("coverage_target_2030", suffixes)
   vars_n_vacc_target <- paste0("n_vacc_target", suffixes)
   vars_aroc          <- paste0("aroc", suffixes)
   
   zd_2019 <- dpt1[year_id == 2019L, ]
   
   zd_2019[, c(vars_n_zd_target) := lapply(.SD, function(x) {x * 0.50}), .SDcols = vars_n_zd]
   zd_2019 <- merge(zd_2019, pop_map[year_id == params$zd_year_end, .(location_id, population_2030 = population)], by = c("location_id"))
   
   zd_2019[, c(vars_targ_2030) := lapply(.SD, function(x) {(population_2030 - x) / population_2030}), .SDcols = vars_n_zd_target]
   
   zd_2019[, c(vars_n_vacc_target) := lapply(.SD, function(x) {population_2030 - x}), .SDcols = vars_n_zd_target]
   
   
   zd_2019[, c(vars_aroc) := lapply(.SD, function(x) {log(x / coverage) / (2030 - 2019)}), .SDcols = vars_targ_2030]
   
   
   vars_keep <- c(
      "me_name"
      , "location_id"
      , "year_id"
      , "sex_id"
      , "age_group_id"
      , "population_2030"
      , vars_targ_2030
      , vars_n_vacc_target
      , vars_n_zd_target
   )
   
   zd_2030 <- zd_2019[, ..vars_keep]
   
   setnames(
      zd_2030
      , c(
         "population_2030"
         , vars_targ_2030
         , vars_n_vacc_target
         , vars_n_zd_target
      )
      , c(
         "population"
         , vars_coverage
         , vars_n_vacc
         , vars_n_zd
      )
   )
   
   zd_2030[, year_id := 2030L]
   
   vars_shared <- intersect(names(dpt1), names(zd_2030))
   dpt1        <- rbind(dpt1[, ..vars_shared], zd_2030[, ..vars_shared])
   setorderv(dpt1, c("location_id", "year_id"))
   sq_chk      <- assert_square(dpt1, id_varnames = c("year_id", "location_id"))
   
   
   
   
   
   dpt1 <- merge(dpt1, loc_map[, .(location_id, super_region_name)], by = c("location_id"), all.x = TRUE)
   dpt1[location_id == 1, super_region_name := "Global"]
   
   se$save_file(
      merge(dpt1, loc_map[, .(location_id, ihme_loc_id, location_name_short)], by = "location_id", all.x = TRUE)
      , file.path(output_dir, "dpt1_past_plus_2030_zd_target.csv")
   )
   
   
   dpt1_maps    <- copy(dpt1[location_id %in% loc_map[level >= 3, location_id], ]) 
   dpt1_maps_sr <- copy(dpt1)
   dpt1_tl      <- copy(dpt1)
   
}


if(params$run_section_flat_pop_counterfactual){
   
   
   
   
   zd_2019_cf <- dpt1_cf[year_id == 2019L, ]
   
   zd_2019_cf[, c(vars_n_zd_target) := lapply(.SD, function(x) {x * 0.50}), .SDcols = vars_n_zd]
   
   zd_2019_cf <- merge(zd_2019_cf, pop_map[year_id == 2023, .(location_id, population_2030 = population)], by = c("location_id"))
   
   zd_2019_cf[, c(vars_targ_2030) := lapply(.SD, function(x) {(population_2030 - x) / population_2030}), .SDcols = vars_n_zd_target]
   
   zd_2019_cf[, c(vars_n_vacc_target) := lapply(.SD, function(x) {population_2030 - x}), .SDcols = vars_n_zd_target]
   
   
   zd_2019_cf[, c(vars_aroc) := lapply(.SD, function(x) {log(x / coverage) / (2030 - 2019)}), .SDcols = vars_targ_2030]
   zd_2030_cf <- zd_2019_cf[, ..vars_keep]
   setnames(
      zd_2030_cf
      , c(
         "population_2030"
         , vars_targ_2030
         , vars_n_vacc_target
         , vars_n_zd_target
      )
      , c(
         "population"
         , vars_coverage
         , vars_n_vacc
         , vars_n_zd
      )
   )
   zd_2030_cf[, year_id := 2030L]
   
   
   
   dpt1_cf        <- rbind(dpt1_cf[, ..vars_shared], zd_2030_cf[, ..vars_shared])
   setorderv(dpt1_cf, c("location_id", "year_id"))
   dpt1_cf <- merge(dpt1_cf, loc_map[, .(location_id, super_region_name)], by = c("location_id"), all.x = TRUE)
   dpt1_cf[location_id == 1, super_region_name := "Global"]
   sq_chk      <- assert_square(dpt1_cf, id_varnames = c("year_id", "location_id"))
   
   
   
   
   
   dpt1_cf_aroc <- calculate_aroc(dpt1_cf, year_id_start = 2023, year_id_end = 2030)
   
   
   se$save_file(dpt1_cf_aroc, file.path(output_dir, "zd_counterfactual_2023_flat_pop.csv"))
}














if(params$run_section_exploratory_analysis){
   
   
   
   
   
   
   
   
   
   window_year_ids_7y <- union(params$year_posts_past, params$year_posts_future)
   
   window_year_map_7y <- make_window_year_map(window_year_ids_7y)
   aroc_list_7y       <- calculate_aroc_list(DT = dpt1, window_year_map = window_year_map_7y)
   
   
   
   
   
   {
      dt_aroc_plot <- rbindlist(aroc_list_7y)
      
      dt_aroc_plot[, population_change := population_end - population_start]
      dt_aroc_plot[, pop_prop_change := population_change / population_start]
      
      dt_aroc_plot <- merge(
         x = dt_aroc_plot
         
         , y = loc_map[, .(location_id, location_name_short, level)]
         , by = "location_id"
         , all.x = TRUE
      )
      
      dt_aroc_plot[, aroc := 100 * aroc]
      dt_aroc_plot[, pop_perc_change := 100 * pop_prop_change]
      dt_aroc_plot[!is.finite(aroc), aroc := 0]
      
      assert_data_schema(dt_aroc_plot)
      
      
      
      dt_aroc_plot[, super_region_name_long := super_region_name]
      for(i in 1:length(aes_map_super_reg)){
         dt_aroc_plot[super_region_name == names(aes_map_super_reg_long)[i], super_region_name := names(aes_map_super_reg)[i]]
      }
      
      
      dt_glb <- dt_aroc_plot[level == 0]
      dt_nat <- dt_aroc_plot[level == 3]
   }
   
   
   
   {
      
      p_scatter_start_end <- ggplot(dt_nat, aes(x = coverage_start, y = coverage_end, color = factor(super_region_name, levels = super_region_factor_levels))) +
         geom_point(alpha = 0.3) +
         scale_color_manual(values = aes_map_super_reg) +
         geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
         facet_grid(super_region_name ~ window ) +
         
         theme_minimal() +
         labs(
            x = "Coverage Start",
            y = "Coverage End",
            title = "Change in coverage",
            subtitle = "End vs. Start of 7-year time window",
            color = "Super Region",
            size = "Population % Change",
            caption = "Populations: IHME"
         ) +
         theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
            , strip.text.y.left = element_text(angle = 0)
            , strip.text.y.right = element_text(angle = 0)
         )
      
      
      
      
      p_scatter_start_aroc <- ggplot(dt_nat, aes(x = coverage_start, y = aroc, color = factor(super_region_name, levels = super_region_factor_levels))) +
         geom_point(alpha = 0.3) +
         scale_color_manual(values = aes_map_super_reg) +
         geom_hline(yintercept = 0, linetype = "dashed") +
         facet_grid(window ~ super_region_name) +
         theme_minimal() +
         labs(
            x = "Coverage Start",
            y = "Annualized Rate of Change (AROC)",
            title = "Annualized Rate of Change (AROC) in Coverage vs. Starting Coverage",
            color = "Super Region",
            size = "Population % Change",
            caption = "Populations: IHME"
         ) +
         theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
            , strip.text.y.left = element_text(angle = 0)
            , strip.text.y.right = element_text(angle = 0)
         )
      
      
      p_scatter_aroc_pop_change <- ggplot(dt_nat, aes(x = pop_perc_change, y = aroc, color = factor(super_region_name, levels = super_region_factor_levels))) +
         geom_point(alpha = 0.3) +
         scale_color_manual(values = aes_map_super_reg) +
         geom_vline(xintercept = 0, linetype = "dashed") +
         geom_hline(yintercept = 0, linetype = "dashed") +
         facet_wrap(~window) +
         theme_minimal() +
         labs(
            x = "Population % Change",
            y = "Annualized Rate of Change (AROC)",
            title = "Annualized Rate of Change (AROC) in Coverage vs. Population % Change",
            color = "Super Region",
            caption = "Populations: IHME"
         ) +
         theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
         )
      
      
      
      
      p_box_whisker_aroc <- ggplot(dt_nat[location_id != 1], aes(x = super_region_name, y = aroc, fill = factor(super_region_name, levels = super_region_factor_levels))) +
         geom_boxplot() +
         scale_fill_manual(values = aes_map_super_reg) +
         
         geom_hline(aes(yintercept = aroc), data = dt_glb[location_id == 1]) +
         geom_label(aes(label = round(aroc, 3)), data = dt_glb[location_id == 1], nudge_y = 1, fill = alpha("white", 0.5)) +
         theme_minimal() +
         facet_wrap(~window) +
         scale_y_continuous(limits = c(-5, 15)) +
         labs(
            x = "Super Region",
            y = "Annualized Rate of Change (AROC)",
            title = "Annualized Rate of Change (AROC) in Coverage",
            subtitle = "Horizontal line is Global AROC",
            color = "Super Region",
            size = "Population % Change",
            caption = "Populations: IHME"
         ) +
         theme(
            legend.position = "none"
            , axis.text.x = element_text(angle = 45, hjust = 1)
         )
      
      
      
      
      p_density_aroc <- 
         ggplot(dt_nat[location_id != 1], aes(x = aroc, fill = factor(super_region_name, levels = super_region_factor_levels))) +
         geom_density(alpha = 0.5) +
         scale_fill_manual(values = aes_map_super_reg) +
         geom_vline(aes(xintercept = 0)) +
         theme_minimal() +
         scale_x_continuous(limits = c(-5, 5)) +
         facet_wrap(~window, scales = "free_y") +
         labs(
            x = "Annualized Rate of Change (AROC)",
            y = "Density",
            title = "Density of Annualized Rate of Change (AROC) in Coverage",
            subtitle = "Vertical line is Global AROC",
            color = "Super Region",
            fill = "Super Region",
            size = "Population % Change",
            caption = "Populations: IHME"
         ) +
         theme(
            
         )
      
      
      
      
      
      
      
      
      
      
      p_ridgeline_aroc <- 
         ggplot(dt_nat[location_id != 1], aes(x = aroc, y = super_region_name, fill = factor(super_region_name, levels = super_region_factor_levels))) +
         geom_density_ridges(alpha = 0.5) +
         scale_fill_manual(values = aes_map_super_reg) +
         geom_vline(aes(xintercept = 0)) +
         theme_minimal() +
         facet_wrap(~window, scales = "free_y") +
         scale_x_continuous(limits = c(-5, 5)) +
         labs(
            x = "Annualized Rate of Change (AROC)",
            y = "Super Region",
            title = "Ridgeline of Annualized Rate of Change (AROC) in Coverage",
            subtitle = "Vertical line is Global AROC",
            color = "Super Region",
            fill = "Super Region",
            size = "Population % Change",
            caption = "Populations: IHME"
         ) +
         theme(
            legend.position = "none"
         )
      
      p_densities <- wrap_plots(
         p_density_aroc,
         p_ridgeline_aroc,
         guides = "collect", 
         ncol = 2
      )
      
      
      p_sup_reg <- wrap_plots(
         p_scatter_start_end,
         
         
         p_box_whisker_aroc,
         p_density_aroc,
         p_ridgeline_aroc,
         guides = "collect",
         ncol = 2
      )
      
      
      ggsave(
         file.path(output_dir, "7yr_window_sup_reg.png")
         , p_sup_reg
         , width  = 20
         , height = 12
      )
      
   }
   
}









if(params$run_section_pub_timelines_barstacks){
   
   message("Running publication timeline/barstack plots")
   
   
   
   
   
   
   
   
   
   
   
   year_id_plot_left   <- 1974
   year_id_plot_right  <- 2030
   
   barchart_year_start <- 1980 - 1
   barchart_year_end   <- 2030 + 1
   
   dpt1_tl_sr <- merge(dpt1, loc_map[, .(location_id, level)], by = "location_id", all.x = TRUE)
   dpt1_tl_sr <- dpt1_tl_sr[level %in% 0:1]
   
   dpt1_tl_sr[, coverage_pop_weighted := coverage * 100]
   
   
   dt_sr_zd_targets <- copy(dpt1_tl_sr)
   sr_targets       <- dt_sr_zd_targets[year_id == 2030, .(super_region_name, target_2030 = coverage_pop_weighted)]
   dt_sr_zd_targets <- merge(dt_sr_zd_targets, sr_targets, all.x = TRUE, by = c("super_region_name"))
   
   dt_sr_zd_targets[, above_target := coverage_pop_weighted > target_2030]
   dt_sr_zd_targets[year_id == 2030, above_target := NA]
   dt_sr_zd_targets[above_target == TRUE, ]
   
   dt_sr_zd_targets[, within_1pct := (coverage_pop_weighted + 1) > target_2030]
   dt_sr_zd_targets[year_id == 2030, within_1pct := NA]
   dt_sr_zd_targets[within_1pct == TRUE, ]
   fwrite(dt_sr_zd_targets, file.path(output_dir, "dt_sr_zd_targets.csv"))
   
   
   
   dpt1_tl_sr_fct <- copy(dpt1_tl_sr)[, super_region_name := factor(super_region_name, levels = super_region_factor_levels_long)]
   
   
   top_million <- as.integer(substr(max(dpt1_tl_sr$n_zd), 1,1)) + 1
   
   
   p_sr_timeline_benchmarks_coverage <-
     dpt1_tl_sr_fct %>%
     ggplot(aes(x = year_id, y = coverage_pop_weighted, ymin = lower * 100, ymax = upper * 100, color = super_region_name)) +
     
     geom_ribbon(
       aes(fill = super_region_name) ,  
       alpha = 0.3,  
       color = NA,
       data = dpt1_tl_sr_fct %>% filter(year_id >= 1980 & year_id <= 2023),  
       show.legend = TRUE) +
     plot_benchmark_timeline(
       DT                 = dpt1_tl_sr_fct,
       year_id_plot_left  = year_id_plot_left,
       year_id_plot_right = year_id_plot_right,
       params             = params,
       milestone_y_adjust = 0,
       sr_label_varname   = "coverage_pop_weighted",
       
       title              = "",
       y_lab              = "DTP1 Coverage (%)"
     ) +
     geom_hline(data = dpt1_tl_sr_fct[super_region_name == "Global" & year_id == 2030], aes(yintercept = coverage_pop_weighted), linetype = "dotted", alpha = 0.8) +
     scale_y_continuous(
       limits = c(0, 100),
       breaks = seq(0, 100, by = 20),                
       minor_breaks = seq(0, 100, by = 10),          
       expand = c(0, 0)
     ) +
     
     guides(
       color = guide_legend(
         nrow = 2,
         override.aes = list(
           size = 5,
           shape = 15
         )
       )
     ) +
     theme_border_axes_and_ticks() +
     theme(
       
       axis.text.x = element_text(size = 12),
       axis.text.y = element_text(size = 12),
       axis.title.x = element_text(size = 14),
       axis.title.y = element_text(size = 14),
       panel.grid.major.y = element_line(color = "grey85"),
       panel.grid.minor.y = element_line(color = "grey90")
     )
   
   
   
   p_sr_barstack <-
      dpt1_tl_sr[super_region_name != "Global"] %>%
      ggplot() +
      plot_sr_barstack_base(
         DT                       = dpt1_tl_sr
         , barchart_year_start    = barchart_year_start
         , barchart_year_end      = barchart_year_end
         , aes_map_super_reg_long = aes_map_super_reg_long
         , y_lab                  = "Zero-Dose Counts"
         
         , title                  = ""
         , top_million            = top_million
      ) +
      geom_segment(
         data = dpt1_tl_sr[super_region_name == "Global" & year_id == 2030]
         , aes(x = 1979.6, xend = 2030.4, y = n_zd, yend = n_zd, linetype = "50% Zero Dose Reduction Target")
         , color = "gray10"
         , alpha = 0.8
         
         , show.legend = TRUE
      ) + 
      scale_linetype_manual(name = "IA2030 Goal", values = "dotted") +
      
      guides(fill = guide_none()) +
      theme_border_axes_and_ticks() +
      theme(
       
       axis.text.x = element_text(size = 12),
       axis.text.y = element_text(size = 12),
       axis.title.x = element_text(size = 14), 
       axis.title.y = element_text(size = 14)
     )
   
   
   
   
   
   scale_y_bar_breaks <- list(
      
      "Central Europe, Eastern Europe, and Central Asia" = seq(0, max(dpt1_tl_sr[super_region_name == "Central Europe, Eastern Europe, and Central Asia", n_zd]), 1e6)
      , "High-income"                                    = seq(0, max(dpt1_tl_sr[super_region_name == "High-income", n_zd]), 1e6) 
      , "Latin America and Caribbean"                    = seq(0, max(dpt1_tl_sr[super_region_name == "Latin America and Caribbean", n_zd]), 2e6)
      , "North Africa and Middle East"                   = seq(0, max(dpt1_tl_sr[super_region_name == "North Africa and Middle East", n_zd]), 2e6)
      , "South Asia"                                     = seq(0, max(dpt1_tl_sr[super_region_name == "South Asia", n_zd]), 8e6)
      , "Southeast Asia, East Asia, and Oceania"         = seq(0, max(dpt1_tl_sr[super_region_name == "Southeast Asia, East Asia, and Oceania", n_zd]), 5e6)
      , "Sub-Saharan Africa"                             = seq(0, max(dpt1_tl_sr[super_region_name == "Sub-Saharan Africa"]$n_zd), 4e6)
   )
   
   scale_y_bar_breaks <- lapply(scale_y_bar_breaks, function(x) c(x, x[2] + x[length(x)]))
   
   
   sr_bump_by_one <- c(
      "Central Europe, Eastern Europe, and Central Asia"
      , "High-income"
      
   )
   scale_y_bar_breaks[sr_bump_by_one] <- lapply(scale_y_bar_breaks[sr_bump_by_one], function(x) c(x, x[2] + x[length(x)]))
   
   stopifnot(all(sapply(scale_y_bar_breaks, length) == 4))
   
   scales_y_bar_facet <- list(
      
      "Central Europe, Eastern Europe, and Central Asia" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["Central Europe, Eastern Europe, and Central Asia"]]
         , limits = c(0, max(scale_y_bar_breaks[["Central Europe, Eastern Europe, and Central Asia"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
      , "High-income" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["High-income"]]
         , limits = c(0, max(scale_y_bar_breaks[["High-income"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
      , "Latin America and Caribbean" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["Latin America and Caribbean"]]
         , limits = c(0, max(scale_y_bar_breaks[["Latin America and Caribbean"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
      , "North Africa and Middle East" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["North Africa and Middle East"]]
         , limits = c(0, max(scale_y_bar_breaks[["North Africa and Middle East"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
      , "South Asia" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["South Asia"]]
         , limits = c(0, max(scale_y_bar_breaks[["South Asia"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
      , "Southeast Asia, East Asia, and Oceania" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["Southeast Asia, East Asia, and Oceania"]]
         , limits = c(0, max(scale_y_bar_breaks[["Southeast Asia, East Asia, and Oceania"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
      , "Sub-Saharan Africa" = scale_y_continuous(
         
         
         breaks = scale_y_bar_breaks[["Sub-Saharan Africa"]]
         , limits = c(0, max(scale_y_bar_breaks[["Sub-Saharan Africa"]]))
         , labels = unit_format(unit = "M", scale = 1e-6)
         , expand = c(0, 0)
      )
   )
   
   p_sr_barstack_facet <-
      dpt1_tl_sr[super_region_name != "Global"] %>%
      ggplot(aes(x = year_id, y = n_zd, fill = super_region_name)) +
      plot_sr_barstack_base(
         DT                       = dpt1_tl_sr[super_region_name != "Global"]
         , barchart_year_start    = barchart_year_start
         , barchart_year_end      = barchart_year_end
         , aes_map_super_reg_long = aes_map_super_reg_long
         , y_lab                  = NULL
         , title                  = "Zero-Dose Counts"
         , top_million            = top_million
      ) +
      
      
      
      
      
      lemon::facet_rep_wrap(
         ~ super_region_name,
         scales = "free_y",
         ncol = 1
      ) +
      ggh4x::facetted_pos_scales(
         y = scales_y_bar_facet
      ) +
      lemon::coord_capped_cart(bottom='both', xlim=c(1980, 2030)) +
      geom_segment(
         data       = dpt1_tl_sr[super_region_name != "Global" & year_id == 2030]
         , aes(x    = 1979.6, xend = 2030.4, y = n_zd, yend = n_zd)
         , color    = "gray20"
         , alpha    = 0.8
         , linetype = "dotted"
      ) +
      theme(
         strip.text.x      = element_blank()
         , legend.position = "none"
         
         , strip.placement = "outside"
         , strip.text      = element_text(size = 10)
      ) + 
      theme_border_axes_and_ticks()
   
   
   
   p_sr_tl_coverage_facet <-
      dpt1_tl_sr[super_region_name != "Global"] %>% 
      ggplot(aes(x = year_id, y = coverage_pop_weighted, ymin = lower * 100, ymax = upper * 100, color = super_region_name)) +
      geom_line(data = dpt1_tl_sr[super_region_name != "Global" & year_id <= 2023], linewidth = 1, linetype = "solid", alpha = 0.8) +
     
     geom_ribbon(
       aes(fill = super_region_name) ,  
       alpha = 0.3,  
       color = NA,     
       data = dpt1_tl_sr[super_region_name != "Global" & year_id <= 2023],
       show.legend = TRUE) +
     
      
      geom_point(data = dpt1_tl_sr[super_region_name != "Global" & year_id == 2030], shape = 18, size = 2, alpha = 0.8) + 
      
      geom_hline(data = dpt1_tl_sr[super_region_name != "Global" & year_id == 2030], aes(yintercept = coverage_pop_weighted), linetype = "dotted", alpha = 0.8) +
      theme_minimal() +
      labs(
         x = "Year",
         , y = NULL
         , title = "DTP1 Coverage (%)"
         , color = "Super Region"
         , fill = "Super Region"
      ) +
      scale_color_manual(values = aes_map_super_reg_long) +
      scale_fill_manual(values = aes_map_super_reg_long) +
      scale_x_continuous(
         limits         = c(1980, 2031) 
         , breaks       = unique(c(seq(1980, 2030, 5)))
         , minor_breaks = NULL
         , expand       = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 20),                
        minor_breaks = seq(0, 100, by = 10),          
        expand = c(0, 0)
      ) +
      
      
      lemon::facet_rep_wrap(
         ~ super_region_name,
         scales             = "free_y",
         ncol               = 1,
         strip.position     = "left",
         repeat.tick.labels = "left",
         labeller           = label_wrap_gen(width = 18) 
      ) +
      
      theme(
         legend.position   = "none"
         
         , strip.placement = "outside"
         , strip.text      = element_text(size = 10)
      ) + 
      theme_border_axes_and_ticks()+
      theme(
       panel.grid.major.y = element_line(color = "grey85"),
       panel.grid.minor.y = element_line(color = "grey90")
     )
   
   
   
   
   
   layout <- 
      "
   EEECD
   AAACD
   AAACD
   AAACD
   BBBCD
   BBBCD
   BBBCD
   "
   p_sr_timeline_barstack_publication <-
      free(p_sr_timeline_benchmarks_coverage) +
      free(p_sr_barstack) +
      free(p_sr_tl_coverage_facet) +
      free(p_sr_barstack_facet) +
      guide_area() +
      plot_layout(
         design = layout
         , guides = "collect"
      ) & 
      theme(
         
         
         
         
         
         legend.title = element_text(size = 12)
         , legend.text = element_text(size = 10)
      )
   
   ggsave(
     file.path(output_dir, "Figure 2.pdf")
     , p_sr_timeline_barstack_publication
     , width  = 16
     , height = 11
     , units = "in"        
     , device = cairo_pdf
   )
   
   ggsave(
      file.path(output_dir, "sr_timeline_barstack_publication.png")
      , p_sr_timeline_barstack_publication
      , width  = 16
      , height = 11
   )
   
}






if(params$run_section_CM_zd_sup_reg_timelines){
   
   message("Running super region zero-dose count plots.")
   
   
   color_scale_g_plus_sr <- c(
      "Global"                                           = "black",
      "Southeast Asia, East Asia, and Oceania"           = "red",
      "Central Europe, Eastern Europe, and Central Asia" = "blue",
      "High-income"                                      = "darkgreen",
      "Latin America and Caribbean"                      = "orange",
      "North Africa and Middle East"                     = "purple",
      "South Asia"                                       = "turquoise4", 
      "Sub-Saharan Africa"                               = "deeppink4"
   )
   
   
   
   
   
   
   
   
   
   
   
   zd_sr <- se$read_file(paths$dpt1_aggregates)
   zd_sr <- zd_sr[level %in% c("global", "super_region")]
   setnames(
      zd_sr
      , c("mean")
      , c("coverage")
   )
   zd_sr[, n_vacc := population * coverage]
   zd_sr[, n_zd := population - n_vacc]
   
   
   zd_sr[super_region_name == "", super_region_name := "Global"]
   
   zd_sr <- zd_sr[, .(n_zd = sum(n_zd)), by = c("year_id", "super_region_name")]
   
   
   zd_sr[, super_region_name := factor(super_region_name, levels = names(color_scale_g_plus_sr))]
   
   
   top_million <- as.integer(substr(max(zd_sr$n_zd), 1,1)) + 1
   
   p_zd_sr_plus_g <-
      ggplot(zd_sr, aes(x = year_id, y = n_zd, color = super_region_name)) +
      geom_line(linewidth = 1, alpha = 0.7) +
      scale_color_manual(values = color_scale_g_plus_sr) +
      theme_minimal() +
      scale_y_continuous(breaks = seq(0, max(zd_sr$n_zd, top_million * 10^7), 1e7), labels = unit_format(unit = "M", scale = 1e-6)) +
      geom_vline(xintercept = params$zd_year_current, linetype = "dotted") +
      annotate("text", x = params$zd_year_current, y = 0, label = "2023", vjust = 1.5, hjust = 0, angle = 90) +
      labs(
         x = "Year",
         y = "Zero Dose Children",
         color = "Super Region",
         title = "Global + Super Regions"
      ) +
      theme(legend.position = "none")
   
   
   p_zd_sr <- 
      p_zd_sr_plus_g %+% zd_sr[super_region_name != "Global"] +
      theme(legend.position = "none") +
      labs(
         title = "Super Regions",
      )
   
   
   p_zd_sr_facet <- 
      p_zd_sr_plus_g %+% zd_sr[super_region_name != "Global"] +
      facet_wrap(~super_region_name, scales = "free_y") +
      scale_y_continuous(
         breaks = c(
            1e6
            , 2e6 
            , 3e6 
            , seq(0, max(zd_sr$n_zd, top_million * 10^7), 5e6)
         )
         , minor_breaks = NULL
         , labels = unit_format(unit = "M", scale = 1e-6)
      ) +
      labs(
         title = "",
      ) +
      theme(
         strip.text.x = element_text(size = 10)
      )
   
   
   
   p <- (p_zd_sr_plus_g + p_zd_sr) / p_zd_sr_facet +
      theme(
         legend.position = "none"
         , legend.text = element_text(size = 10)
      ) +
      plot_annotation(
         title = "Zero Dose Children",
         theme = theme(
            plot.title = element_text(size = 18)
         )
      ) 
   
   print(p)
   
   ggsave(file.path(output_dir, "zd_children_by_super_region.png"), p, width = 13, height = 9)
   
   zd_sr_display <- zd_sr[, n_zd := round(n_zd, 0)]
   zd_sr_w <- dcast(zd_sr_display, year_id ~ super_region_name, value.var = "n_zd")
   se$save_file(zd_sr_w, file.path(output_dir, "zd_children_by_super_region.csv"))
}









if(params$run_section_maps){
   
   message("Starting Maps")
   
   {
      library(sp)
      library(raster)
      library(sf)
      library(showtext)
      library(Cairo)
      library(ggpattern)
      source(file.path(paths$code_root_vaccine_mbg, "FILEPATH/scale_fill_vaccine.R"))
      
      shp_adm0_0 <- sf::read_sf(dsn = file.path(paths$shapefile_vc_lbd_root,'lbd_standard_admin_0.shp'))
      
      
      
      disputed <- sf::read_sf(dsn = file.path(paths$shapefile_vc_lbd_root,'lbd_disputed_mask.shp'))
      disputed_outline <- disputed %>% 
         st_as_sf() %>% 
         sf::st_cast("MULTILINESTRING") %>%   
         sf::st_coordinates() %>%  
         as.data.table() %>% 
         setnames(c("long", "lat", "piece", "id")) %>%  
         mutate(group = paste0(id, ".", piece))
      
      
      
      
      setnames(shp_adm0_0, "loc_id", "location_id")
      
      countries_to_mask <- c(
      )
      
      
      countries_to_remove <- c(
         "Antarctica"
      )
      shp_adm0_0 <- shp_adm0_0[!shp_adm0_0$ADM0_NAME %in% countries_to_remove, ]
      nrow(shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_remove, ])
      
      year_list     <- 2000:2023
      vaccines      <- c('dpt1_cov')
      vaccines_nice <- c('DTP 1st dose')
      vacc_nice     <- vaccines_nice[1]
      vaccines_gbd  <- c("vacc_dpt1")
      
      
      
      
      
      year_ids_future   <- c(2023L, 2030L)
      window_map_future <- make_window_year_map(year_ids_future)
      list_aroc_future  <- calculate_aroc_list(DT = dpt1_maps, window_year_map = window_map_future)
      
      list_aroc_future  <- lapply(list_aroc_future, function(x) x[, aroc := pmax(aroc, 0)])
      dt_aroc_future    <- rbindlist(list_aroc_future)
      
      
      se$save_file(dt_aroc_future, file.path(output_dir, "dt_aroc_future.csv"))
   }
   
   
   
   
   
   if(params$run_section_maps_pub_past_percentiles_benchmarks){
      
      
      
      {
         title_stub <- "Future AROC as a percentile of past global AROC distribution"
         subtitle_stub <- "Future AROC required to achieve national 50% reduction in zero-dose children by 2030"
         neg_label <-  "Negative"
         neg_color <- "aquamarine"
         
         quantile_bins <- 100
         plot_breaks <- find_breaks(range = c(0,100), by_x = 10) 
         
         
         
         
         carot_tealrose <- c("
         
         
         
         
         carto_earth <- c("
         
         
         
         
         carto_armyrose <- c("
         
         
         
         
         
         
         plot_colors_percentiles <- c(
            colorRampPalette(c( "blue", "lightblue"))(5), 
            colorRampPalette(c("pink", "red"))(5)         
         )
         
         carto_low_vals <- rev(carto_pal(n = 5, "Teal"))
         carto_high_vals <- carto_pal(n = 5, "SunsetDark") 
         
         carto_high_vals <- c("
         plot_colors_percentiles <- c(
            carto_low_vals
            , carto_high_vals
         )
         
         
         year_list_rolling_benchmarks <- list(
            
            
            
            seq(2000, 2019-7, 1)
            
            
            
         )
         window_list_rolling_benchmarks <- lapply(year_list_rolling_benchmarks, function(yrs) {
            data.table(year_id_start = yrs, year_id_end = yrs + 7)
         })
      }
      
      for(window_map_past_7_years in window_list_rolling_benchmarks){
         
         
         
         
         {
            .window_start_year <- min(window_map_past_7_years$year_id_start)
            .window_end_year   <- max(window_map_past_7_years$year_id_end)
            window_display     <- glue::glue("{.window_start_year} to {.window_end_year}")
            
            window_fname_rolling_benchmarks <- glue::glue("{.window_start_year}_{.window_end_year}")
            
            
            message("\nMapping rolling EPI benchmark period windows for: ", window_display, "\n")
            
            list_aroc_past_7_year_rolling <- calculate_aroc_list(DT = dpt1_maps, window_year_map = window_map_past_7_years)
            
            
            dt_aroc_past_7_year_rolling   <- rbindlist(list_aroc_past_7_year_rolling)
            
            
            
            
            
            if(params$map_remove_neg_past_aroc){
               message("-- Removing negatives from past distribution.")
               dt_aroc_past_7_year_rolling[aroc < 0, aroc := NA]
            }
            past_negs_comment <- ifelse(params$map_remove_neg_past_aroc, "(past negatives removed)", "")
            past_negs_fname_stub <- ifelse(params$map_remove_neg_past_aroc, "_negs_removed", "")
            
            
            dt_aroc_past_7_year_rolling <- merge(
               dt_aroc_past_7_year_rolling
               , dt_aroc_future[, .(
                  location_id
                  , aroc_future = aroc
                  
               )]
               , by = "location_id"
               , all.x = TRUE
            )
            
            
            message("-- global quantiles")
            
            global_percentiles       <- quantile(dt_aroc_past_7_year_rolling$aroc, na.rm = TRUE, probs = seq(0,1,1/quantile_bins))
            global_percentiles_hmisc <- Hmisc::wtd.quantile(dt_aroc_past_7_year_rolling$aroc, weights = dt_aroc_past_7_year_rolling$population_start, probs = seq(0,1,1/quantile_bins))
            global_percentiles_hmisc <- round(global_percentiles_hmisc, 7) 
            stopifnot(all(diff(order(round(global_percentiles_hmisc, 7))))==1)
            
            global_quartiles <- quantile(dt_aroc_past_7_year_rolling$aroc, na.rm = TRUE)
            global_quartiles_hmisc <- Hmisc::wtd.quantile(dt_aroc_past_7_year_rolling$aroc, weights = dt_aroc_past_7_year_rolling$population_start)
            dt_quartiles <- data.table(
               quartile = c("min", "Q1", "median", "Q3", "max")
               , global_quartiles
               , global_quartiles_hmisc
            )
            
            dt_percentiles <- as.data.table(cbind(global_percentiles, global_percentiles_hmisc), keep.rownames = TRUE)
            setnames(dt_percentiles, "rn", "percentile")
            
            dt_aroc_past_7_year_rolling[, percentile_global       := findInterval(aroc_future,     global_percentiles,     all.inside = TRUE)]
            dt_aroc_past_7_year_rolling[, percentile_global_hmisc := findInterval(aroc_future,     global_percentiles_hmisc, all.inside = TRUE)]
            dt_aroc_past_7_year_rolling[, percentile_global_past  := findInterval(aroc,            global_percentiles, all.inside = TRUE)]
            dt_aroc_past_7_year_rolling[, percentile_global_hmisc_past  := findInterval(aroc,      global_percentiles_hmisc, all.inside = TRUE)]
            
            dt_aroc_past_7_year_rolling <- merge(dt_aroc_past_7_year_rolling, loc_map[, .(location_id, ihme_loc_id, location_name_short)])
            
            setcolorder(dt_aroc_past_7_year_rolling, c("me_name", "location_id", "ihme_loc_id", "location_name_short"))
            fwrite(dt_quartiles, file.path(output_dir, glue::glue("global_quartiles_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.csv")))
            fwrite(dt_percentiles, file.path(output_dir, glue::glue("global_percentiles_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.csv")))
            fwrite(dt_aroc_past_7_year_rolling, file.path(output_dir, glue::glue("dt_aroc_past_7_year_rolling_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.csv")))
            
         }
         
         
         
         
         {
            message("Panel A")
            
            dt_pp_diff <- 
               dpt1_maps[year_id %in% c(2023, 2030), .(location_id, me_name, year_id, coverage)] %>% 
               pivot_wider(names_from = year_id, values_from = coverage, names_prefix = "coverage_") %>% 
               as.data.table()
            dt_pp_diff[, coverage_diff := (coverage_2030 - coverage_2023) * 100]
            dt_pp_diff[coverage_diff < 0, coverage_diff := NA]
            
            pp_range <- range(dt_pp_diff$coverage_diff, na.rm = TRUE)
            
            
            
            
            pp_breaks <- c(seq(0, 25, 5), max(pp_range))
            pp_breaks_labels <- c(
               neg_label 
               , paste0(pp_breaks[-length(pp_breaks)], "-", pp_breaks[-1], "%")
            )
            pp_breaks_labels[length(pp_breaks_labels)] <- "25%+"
            
            dt_pp_diff$coverage_diff_bin <- base::cut(
               x      = dt_pp_diff$coverage_diff
               , breaks = pp_breaks
               , labels = pp_breaks_labels[2:length(pp_breaks_labels)]
            )
            dt_pp_diff[is.na(coverage_diff_bin), coverage_diff_bin := neg_label]
            dt_pp_diff$coverage_diff_bin <- factor(dt_pp_diff$coverage_diff_bin, levels = pp_breaks_labels)
            dt_panel_a <- merge(dt_pp_diff, loc_map, by = "location_id", all.x = TRUE)
            setorderv(dt_panel_a, c("location_id"))
            se$save_file(dt_panel_a, file.path(output_dir, glue::glue("dt_panel_a_{window_fname_rolling_benchmarks}.csv")))
            
            dt_pp_diff_summary <- dt_pp_diff[, .N, by = coverage_diff_bin]
            dt_pp_diff_summary[, N_perc := round(N / (sum(N) - 18) * 100, 1)]
            setorderv(dt_pp_diff_summary, "coverage_diff_bin")
            dt_pp_diff_summary
            se$save_file(dt_pp_diff_summary, file.path(output_dir, glue::glue("summary_panel_a_{window_fname_rolling_benchmarks}.csv")))
            
            dt_pp_diff2 <- merge(dt_pp_diff, zd_2030[, .(location_id, year_id, n_zd)], all.x = TRUE)
            dt_pp_diff2_summary <- dt_pp_diff2[, .(n_zd = sum(n_zd)), by = coverage_diff_bin]
            dt_pp_diff2_summary[, n_zd_pct := round(n_zd / sum(n_zd) * 100, 1)]
            setorderv(dt_pp_diff2_summary, "coverage_diff_bin")
            se$save_file(dt_pp_diff2_summary, file.path(output_dir, glue::glue("summary_panel_a_zd_{window_fname_rolling_benchmarks}.csv")))
            
            
            loc_ids_hi_cov_diff <- dt_pp_diff[coverage_diff > 10, location_id]
            n_zd_hi_cov_diff <- dpt1_maps[location_id %in% loc_ids_hi_cov_diff & year_id == 2030, sum(n_zd)]
            n_zd_all <- dpt1_maps[year_id == 2030, sum(n_zd)]
            n_zd_hi_cov_diff / n_zd_all
            
            
            pp_plot_colors <- c(
               neg_color 
               , RColorBrewer::brewer.pal(length(pp_breaks[pp_breaks>0]), "YlOrRd")
               
            )
            
            
            c("aquamarine", "
            
            pp_plot_colors <- 
               c("aquamarine", "
            
            names(pp_plot_colors) <- pp_breaks_labels
            
            
            dt_pp_diff_plot <- merge(shp_adm0_0, dt_pp_diff, by = "location_id", all.x = TRUE)
            
            p_pp_diff <-
               ggplot() +
               geom_sf(
                  data = dt_pp_diff_plot,
                  aes(fill = coverage_diff_bin),
                  color = 'black',
                  linewidth = 0.1
               ) +
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,], fill = 'gray70', color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_pp_diff_plot[is.na(dt_pp_diff_plot$coverage_diff_bin) ,], fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               plot_disputed_outline(disputed_outline = disputed_outline) +
               
               
               theme_zd_maps_manual_fill(plot_colors = pp_plot_colors, scale_name = "Percentage Points") +
               
               labs(
                  title = glue::glue("DTP1 Coverage Percentage Point Increase Required 2023 to 2030")
                  , subtitle = glue::glue("")
               ) 
            
            CairoPNG(
               file.path(output_dir, 
                         paste0(
                            
                            glue::glue("panel_a_percentage_point_diff_2023_2030.png")
                         )),
               width  = 11,
               height = 4,
               res    = 400,
               units  = 'in'
            )
            print(p_pp_diff)
            dev.off()
            gc() 
            
            
            p_pp_diff_pub <-
               ggplot() +
               geom_sf(
                  data = dt_pp_diff_plot,
                  aes(fill = coverage_diff_bin),
                  color = 'black',
                  linewidth = 0.1
               ) +
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,], fill = 'gray70', color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_pp_diff_plot[is.na(dt_pp_diff_plot$coverage_diff_bin) ,], fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               plot_disputed_outline(disputed_outline = disputed_outline) +
               theme_zd_maps_manual_fill(plot_colors = pp_plot_colors, scale_name = "Percentage Points") +
               labs(
                  title = NULL,
                  subtitle = NULL
               ) +
               annotate(
                  "text",
                  x = -Inf,  
                  y = Inf,   
                  label = "A",
                  hjust = -0.5,  
                  vjust = 1.5,   
                  size = 6,      
                  fontface = "bold"  
               )
            
         }
         
         
         {
            message("Panel B")
            
            dt_aroc_future    <- rbindlist(list_aroc_future)
            
            range(dt_aroc_future$aroc)
            dt_aroc_future$aroc_bins <- base::cut(dt_aroc_future$aroc, breaks = seq(0,.2,.01), right = TRUE)
            dt_panel_b <- merge(dt_aroc_future, loc_map, by = "location_id", all.x = TRUE)
            setorderv(dt_panel_b, c("location_id"))
            se$save_file(dt_panel_b, file.path(output_dir, glue::glue("dt_panel_b_{window_fname_rolling_benchmarks}.csv")))
            dt_panel_b_summary <- dt_aroc_future[, .N, by = aroc_bins]
            dt_panel_b_summary[, bin_level := as.integer(aroc_bins)]
            setorder(dt_panel_b_summary, aroc_bins)
            dt_panel_b_summary
            se$save_file(dt_panel_b_summary, file.path(output_dir, glue::glue("summary_panel_b_{window_fname_rolling_benchmarks}.csv")))
            
            
            
            aroc_future_breaks <- c(seq(0, 0.1, 0.02), max(dt_aroc_future$aroc))
            
            aroc_future_breaks_labels <- c(
               neg_label 
               , paste0(aroc_future_breaks[-length(aroc_future_breaks)]*100, "-", aroc_future_breaks[-1]*100, "%")
            )
            aroc_future_breaks_labels[length(aroc_future_breaks_labels)] <- "10%+"
            dt_aroc_future$aroc_bins <- base::cut(dt_aroc_future$aroc, breaks = aroc_future_breaks, labels = aroc_future_breaks_labels[2:length(aroc_future_breaks_labels)])
            dt_aroc_future[is.na(aroc_bins), aroc_bins := neg_label]
            dt_aroc_future$aroc_bins <- factor(dt_aroc_future$aroc_bins, levels = aroc_future_breaks_labels)
            
            aroc_future_plot_colors <- c(
               neg_color 
               
               , rcartocolor::carto_pal("PurpOr", n = length(aroc_future_breaks[aroc_future_breaks>0]))
            )
            
            names(aroc_future_plot_colors) <- aroc_future_breaks_labels
            
            c(Negative = "aquamarine", `0-2%` = "
            
            aroc_future_plot_colors <- 
               c(Negative = "aquamarine", `0-2%` = "
            
            dt_aroc_future_pre_plot <- copy(dt_aroc_future)
            dt_aroc_future_pre_plot[aroc==0, aroc := NA]
            dt_aroc_future_plot <- merge(shp_adm0_0, dt_aroc_future_pre_plot, by = "location_id", all.x = TRUE)
            
            
            dt_panel_b_summary_plot <- dt_aroc_future[, .N, by = aroc_bins]
            dt_panel_b_summary_plot[, n_total := sum(N)]
            dt_panel_b_summary_plot[, perc_countries := round(N / n_total * 100, 2)]
            setorderv(dt_panel_b_summary_plot, "aroc_bins")
            se$save_file(dt_panel_b_summary_plot, file.path(output_dir, glue::glue("summary_panel_b_plot_{window_fname_rolling_benchmarks}.csv")))
            
            p_aroc_future <- 
               ggplot() +
               geom_sf(
                  data = dt_aroc_future_plot,
                  
                  aes(fill = aroc_bins),
                  color = 'black',
                  linewidth = 0.1
               ) +
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_aroc_future_plot[is.na(dt_aroc_future_plot$aroc_bins),], fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               
               
               
               theme_zd_maps_manual_fill(plot_colors = aroc_future_plot_colors, scale_name = "AROC") +
               labs(
                  title = glue::glue("Annualized Rate of Change Required 2023 to 2030")
                  , subtitle = glue::glue("")
                  
               )
            
            CairoPNG(
               file.path(output_dir, 
                         paste0(
                            
                            glue::glue("panel_b_aroc_future_2023_2030.png")
                         )),
               width  = 11,
               height = 4,
               res    = 400,
               units  = 'in'
            )
            print(p_aroc_future)
            dev.off()
            gc()
            
            
            p_aroc_future_pub <- 
               ggplot() +
               geom_sf(
                  data = dt_aroc_future_plot,
                  
                  aes(fill = aroc_bins),
                  color = 'black',
                  linewidth = 0.1
               ) +
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_aroc_future_plot[is.na(dt_aroc_future_plot$aroc_bins),], fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               plot_disputed_outline(disputed_outline = disputed_outline) +
               
               
               
               theme_zd_maps_manual_fill(plot_colors = aroc_future_plot_colors, scale_name = "AROC") +
               labs(
                  title = NULL,
                  subtitle = NULL
               ) +
               annotate(
                  "text",
                  x = -Inf,  
                  y = Inf,   
                  label = "B",
                  hjust = -0.5,  
                  vjust = 1.5,   
                  size = 6,      
                  fontface = "bold"  
               )
            
         }
         
         
         
         {
            message("Panel C - see p_aroc_past_7_year_rolling_global")
            
            
            percentile_bands <- seq(0,100,10)
            percentile_band_names <- paste0(percentile_bands[1:length(percentile_bands)-1],
                                            "-",
                                            percentile_bands[2:length(percentile_bands)])
            
            percentile_band_names[length(percentile_band_names)+1] <- percentile_band_names[length(percentile_band_names)]
            dt_aroc_past_7_year_rolling[, percentile_global_band := findInterval(percentile_global, percentile_bands)]
            dt_aroc_past_7_year_rolling[, percentile_global_band := percentile_band_names[percentile_global_band]]
            
            setorderv(dt_aroc_past_7_year_rolling, c("location_id", "window"))
            se$save_file(dt_aroc_past_7_year_rolling, file.path(output_dir, glue::glue("dt_panel_c_{window_fname_rolling_benchmarks}.csv")))
            
            dt_aroc_percentile_summary <- dt_aroc_past_7_year_rolling[, .(ihme_loc_id, aroc_future, percentile_global_band)] 
            dt_aroc_percentile_summary_zeros <- dt_aroc_percentile_summary[aroc_future == 0]
            dt_aroc_percentile_summary <- dt_aroc_percentile_summary[aroc_future > 0]
            
            dt_aroc_percentile_summary_zeros <- dt_aroc_percentile_summary_zeros[, .(n_countries = uniqueN(ihme_loc_id)), by = sort(percentile_global_band)]
            dt_aroc_percentile_summary <- dt_aroc_percentile_summary[, .(n_countries = uniqueN(ihme_loc_id)), by = sort(percentile_global_band)]
            dt_aroc_percentile_summary[, n_total := sum(n_countries)]
            dt_aroc_percentile_summary[, perc_countries := round(n_countries / n_total * 100, 2)]
            
            sum(dt_aroc_percentile_summary$n_countries)
            
            setnames(dt_aroc_percentile_summary, "sort", "percentile_global_band")
            se$save_file(dt_aroc_percentile_summary, file.path(output_dir, glue::glue("summary_panel_c_{window_fname_rolling_benchmarks}.csv")))
            
            
            
         }
         
         
         
         {
            message("Panel D")
            
            dt_aroc_future    <- rbindlist(list_aroc_future)
            
            dt_aroc_past_best <- dt_aroc_past_7_year_rolling[, .(aroc_best = max(aroc)), by = location_id]
            dt_aroc_future <- merge(dt_aroc_future, dt_aroc_past_best, by = "location_id", all.x = TRUE)
            dt_aroc_future[, aroc_future_v_best := aroc / aroc_best]
            dt_aroc_future[aroc_future_v_best <= 0, aroc_future_v_best := NA]
            dt_aroc_future[, aroc_log_future_v_best := log(aroc_future_v_best)]
            dt_aroc_future[!is.finite(aroc_log_future_v_best), aroc_log_future_v_best := NA]
            
            
            range_future_v_best <- range(dt_aroc_future$aroc_future_v_best, na.rm = TRUE)
            
            plot_break_lims <- round(range_future_v_best)
            plot_breaks_future_v_best <- c(0, 1, 2, 4, 8, plot_break_lims[2])
            
            break_labels <- paste0(plot_breaks_future_v_best, "-", lead(plot_breaks_future_v_best))
            break_labels <- break_labels[-length(break_labels)]
            break_labels[length(break_labels)] <- paste0(plot_breaks_future_v_best[length(plot_breaks_future_v_best)-1], "+")
            
            dt_aroc_future$aroc_future_v_best_bin <- base::cut(dt_aroc_future$aroc_future_v_best, breaks = plot_breaks_future_v_best, labels = break_labels)
            dt_aroc_future[is.na(aroc_future_v_best_bin), aroc_future_v_best_bin := neg_label]
            break_labels <- c(neg_label, break_labels)
            dt_aroc_future$aroc_future_v_best_bin <- factor(dt_aroc_future$aroc_future_v_best_bin, levels = break_labels)
            
            dt_panel_d <- merge(dt_aroc_future, loc_map, by = "location_id", all.x = TRUE)
            setorderv(dt_panel_d, c("location_id"))
            se$save_file(dt_panel_d, file.path(output_dir, glue::glue("dt_panel_d_{window_fname_rolling_benchmarks}.csv")))
            
            dt_panel_d_summary <- dt_aroc_future[, .N, by = aroc_future_v_best_bin]
            dt_panel_d_summary[, n_total := sum(N)]
            dt_panel_d_summary[, perc_countries := round(N / n_total * 100, 2)]
            setorderv(dt_panel_d_summary, "aroc_future_v_best_bin")
            
            dt_panel_d_summary[, aroc_future_v_best_bin := sub("-", "_", aroc_future_v_best_bin)]
            se$save_file(dt_panel_d_summary, file.path(output_dir, glue::glue("summary_panel_d_{window_fname_rolling_benchmarks}.csv")))
            
            plot_colors_future_v_best <- c(
               neg_color
               , colorRampPalette(c( "
               , colorRampPalette(c("
            )
            names(plot_colors_future_v_best) <- break_labels
            
            c(`Negative` = "aquamarine", `0-1` = "
            
            dt_aroc_future_plot <- merge(shp_adm0_0, dt_aroc_future, by = c("location_id"), all.x = TRUE)
            
            
            
            
            p_aroc_future_v_best_bins <-
               ggplot() +
               geom_sf(
                  data = dt_aroc_future_plot,
                  aes(fill = aroc_future_v_best_bin),
                  
                  color = 'black',
                  linewidth = 0.1
               ) +
               
               
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_aroc_future_plot[is.na(dt_aroc_future_plot$aroc_future_v_best_bin),], fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               plot_disputed_outline(disputed_outline = disputed_outline) +
               
               
               theme_zd_maps_manual_fill(plot_colors = plot_colors_future_v_best, scale_name = "Multiples") +
               labs(
                  title = glue::glue("Future AROC as multiple of past best national AROC - {window_display}")
                  , subtitle = glue::glue("{past_negs_comment}")
               )
            
            CairoPNG(
               file.path(output_dir, 
                         paste0(
                            
                            glue::glue("panel_d_aroc_future_v_best_{window_fname_rolling_benchmarks}{past_negs_fname_stub}_bins.png")
                         )),
               width  = 11,
               height = 4,
               res    = 400,
               units  = 'in'
            )
            print(p_aroc_future_v_best_bins)
            dev.off()
            gc()
            
            
            
            
            
            
            p_aroc_future_v_best_bins <-
               ggplot() +
               geom_sf(
                  data = dt_aroc_future_plot,
                  aes(fill = aroc_future_v_best),
                  
                  color = 'black',
                  linewidth = 0.1
               ) +
               
               
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               labs(
                  title = glue::glue("Future AROC as multiple of past best national AROC - {window_display}")
                  , subtitle = glue::glue("By Country {past_negs_comment}")
                  
                  , pattern_fill = "Negative AROC"
               ) +
               
               
               
               
               
               
               
               
               
               
               
               theme_void() +
               scale_fill_gradient(
                  name = "Multiples",
                  transform = "log",
                  breaks = c(min(dt_aroc_future$aroc_future_v_best, na.rm = TRUE)*2, 1, 2, 4, 8),
                  labels = c("0-1", "1-2", "2-4", "4-8", "8+"), 
                  
                  low = "cyan",
                  high = "magenta",
                  na.value = "gray70"
               ) +
               guides(
                  fill = guide_legend( 
                     
                     title.position = 'left'
                     
                     
                     , reverse = TRUE 
                  )
               ) +
               theme(
                  panel.border             = element_rect(color = "black",fill = NA, linewidth = 1)
                  , legend.position        = "inside"
                  , legend.position.inside = c(0.075, 0.2)
                  , legend.title           = element_text(angle = 90, hjust = 0.5)
               )
            
            CairoPNG(
               file.path(output_dir, 
                         paste0(
                            
                            glue::glue("panel_d_aroc_future_v_best_{window_fname_rolling_benchmarks}{past_negs_fname_stub}_gradient2.png")
                            
                         )),
               width  = 11,
               height = 4,
               res    = 400,
               units  = 'in'
            )
            print(p_aroc_future_v_best_bins)
            dev.off()
            gc()
            
         }
         
         
         
         
         if(params$run_section_maps_histograms){
            message("-- Histograms")
            
            p <- dt_aroc_past_7_year_rolling %>% 
               ggplot() +
               geom_histogram(aes(x = aroc_future), bins = 100, fill = "blue", alpha = 0.5, position = "identity") +
               geom_histogram(aes(x = aroc), bins = 100, fill = "pink", alpha = 0.5, position = "identity") +
               theme_minimal() +
               labs(
                  title = "Distribution of AROCs"
                  , subtitle = glue::glue("Future vs. Past {past_negs_comment}")
                  , x = "AROC"
                  , y = "Count"
                  , fill = c("Future", "Past")
               )
            CairoPNG(file.path(output_dir, glue::glue("histogram_aroc_past_future_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.png"))
                     , width  = 10, height = 6, units = "in", res = 300)
            print(p)
            dev.off()
            
            
            
            
            p <-
               dt_percentiles[, -"percentile"] %>% 
               pivot_longer(cols = c(global_percentiles, global_percentiles_hmisc), names_to = "method", values_to = "percentile") %>% 
               ggplot() +
               geom_vline(xintercept = dt_percentiles[percentile %in% c("1%", "99%"), global_percentiles], linetype = "dashed", color = "blue") +
               geom_vline(xintercept = dt_percentiles[percentile %in% c("1%", "99%"), global_percentiles_hmisc], linetype = "dashed", color = "orange") +
               
               geom_text(
                  data = dt_percentiles[percentile %in% c("1%", "99%")],
                  aes(
                     x = global_percentiles,
                     y = 0,
                     label = paste0(gsub("%", "", percentile), c("st percentile", "th percentile"))
                  ),
                  vjust = c(-1, 2),
                  hjust = -1,
                  angle = 90,
                  color = "black"
               ) +
               geom_histogram(aes(x = percentile, fill = method), position = "identity", alpha = 0.5, bins = 100) +
               scale_fill_manual(values = c("blue", "orange")) +
               labs(
                  title = "Distribution of Global Percentiles"
                  , subtitle = glue::glue("Future AROC within past AROC distribution {past_negs_comment}")
               )
            theme_minimal()
            CairoPNG(file.path(output_dir, glue::glue("histogram_global_percentile_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.png"))
                     , width  = 10, height = 6, units = "in", res = 300)
            print(p)
            dev.off()
         }
         
         
         {
            
            dt_aroc_past_7_year_rolling[aroc_future == 0, `:=`(percentile_global = NA)]
            vars_keep <- c("location_id", "aroc_future", grep("percentile|prop", names(dt_aroc_past_7_year_rolling), value = TRUE))
            dt_aroc_past_7_year_rolling_pre_plot <- dt_aroc_past_7_year_rolling[, .SD[1], by = location_id][, ..vars_keep]
            
            
            
            
            percentile_bins <- seq(0,100,10)
            percentile_bin_names <- paste0(percentile_bins[1:length(percentile_bins)-1],
                                           "-",
                                           percentile_bins[2:length(percentile_bins)])
            
            plot_break_labels <- c(neg_label, percentile_bin_names)
            dt_aroc_past_7_year_rolling_pre_plot$percentile_bin <- base::cut(
               x      = dt_aroc_past_7_year_rolling_pre_plot$percentile_global,
               breaks = plot_breaks,
               labels = plot_break_labels[2:length(plot_break_labels)]
            )
            dt_aroc_past_7_year_rolling_pre_plot[is.na(percentile_bin), percentile_bin := neg_label]
            dt_aroc_past_7_year_rolling_pre_plot$percentile_bin <- factor(
               dt_aroc_past_7_year_rolling_pre_plot$percentile_bin,
               levels = plot_break_labels
            )
            
            plot_colors_pub <- c(neg_color, plot_colors_percentiles)
            names(plot_colors_pub) <- plot_break_labels
            
            c(Negative = "aquamarine", `0-10` = "
              `20-30` = "
              `50-60` = "
              `80-90` = "
            
            
            dt_aroc_past_7_year_rolling_plot <- merge(shp_adm0_0, dt_aroc_past_7_year_rolling_pre_plot, by = 'location_id', all.x = TRUE)
         }
         
         
         
         
         if(params$run_section_maps_scatters){
            
            
            
            
            message("-- Scatters")
            dt_aroc_pop_scatter <- copy(dt_aroc_past_7_year_rolling)
            
            
            n_to_label <- 15
            
            
            pop_to_label <- dt_aroc_pop_scatter[, .(pop = max(population_start)), by = ihme_loc_id][order(-pop)][n_to_label, pop]
            
            pal <- pal_dark_random(n = length(unique(dt_aroc_pop_scatter[population_start > pop_to_label, ihme_loc_id])))
            dput(pal)
            c("tomato", "firebrick3", "indianred", "orangered4", "mediumpurple", 
              "orchid4", "aquamarine4", "olivedrab3", "turquoise4", "darkslateblue", 
              "plum4", "orchid", "brown4", "darkviolet")
            
            
            p_aroc_pop_scatter <- ggplot(dt_aroc_pop_scatter, aes(x = population_start, y = aroc)) +
               geom_text_repel(
                  data = dt_aroc_pop_scatter[population_start > pop_to_label],
                  aes(label = ihme_loc_id, color = ihme_loc_id),
                  size = 3,
                  
                  max.overlaps = 50
               ) +
               geom_point(alpha = 0.1) +
               facet_wrap(~window, nrow = 4) +
               labs(
                  title = "Population vs. AROC"
                  , subtitle = glue::glue("Labelling top {n_to_label} highest population locations")
                  , x = "Population"
                  , y = "AROC"
               ) +
               scale_color_manual(values = pal) +
               theme_bw() +
               theme(
                  aspect.ratio = 1
               )
            
            
            pop_to_label <- dt_aroc_pop_scatter[, .(pop = max(population_start)), by = ihme_loc_id][order(-pop)][n_to_label, log(pop)]
            
            p_aroc_pop_scatter_log <- ggplot(dt_aroc_pop_scatter, aes(x = log(population_start), y = aroc)) +
               geom_text_repel(
                  data = dt_aroc_pop_scatter[log(population_start) > pop_to_label],
                  aes(label = ihme_loc_id, color = ihme_loc_id),
                  size = 3,
                  
                  max.overlaps = 50
               ) +
               geom_point(alpha = 0.1) +
               facet_wrap(~window, nrow = 4) +
               labs(
                  title = "Population (log) vs. AROC"
                  , subtitle = glue::glue("Labelling top {n_to_label} highest population locations {past_negs_comment}")
                  , x = "Log Population"
                  , y = "AROC"
               ) +
               scale_color_manual(values = pal) +
               theme_bw() +
               theme(
                  aspect.ratio = 1
               )
            
            
            cairo_pdf(
               file.path(output_dir, glue::glue("aroc_pop_scatters_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.pdf"))
               , width  = 20
               , height = 12
               , onefile = TRUE
            )
            print(p_aroc_pop_scatter)
            print(p_aroc_pop_scatter_log)
            dev.off()
         }
         
      }
      
      
      
      
      
      
      {
         p_aroc_past_7_year_rolling_global_pub <- ggplot() +
            geom_sf(data = dt_aroc_past_7_year_rolling_plot, aes(fill = percentile_bin), color = 'black', linewidth = 0.1) +
            
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$aroc_future), ], fill = "black", color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
            
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$percentile_bin),], fill = 'gray70', color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
            
            
            
            theme_zd_maps_manual_fill(plot_colors_pub, scale_name = "Percentiles") +
            labs(
               title = glue::glue("{title_stub} - {window_display}")
               , subtitle = glue::glue("{subtitle_stub} {past_negs_comment}")
            )
         
         CairoPNG(
            file.path(output_dir, 
                      paste0(
                         
                         glue::glue('panel_c_aroc_past_7_year_rolling_global_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.png')
                      )),
            height = 4,
            width  = 11,
            res    = 400,
            units  = 'in'
         )
         print(p_aroc_past_7_year_rolling_global_pub)
         dev.off()
         gc()
         
         
         p_aroc_past_7_year_rolling_global_pub_pub <- ggplot() +
            geom_sf(data = dt_aroc_past_7_year_rolling_plot, aes(fill = percentile_bin), color = 'black', linewidth = 0.1) +
            
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$aroc_future), ], fill = "black", color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
            
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$percentile_bin),], fill = 'gray70', color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
            plot_disputed_outline(disputed_outline = disputed_outline) +
            
            
            
            theme_zd_maps_manual_fill(plot_colors_pub, scale_name = "Percentiles") +
            labs(
               title = NULL,
               subtitle = NULL
            ) +
            annotate(
               "text",
               x = -Inf,  
               y = Inf,   
               label = "C",
               hjust = -0.5,  
               vjust = 1.5,   
               size = 6,      
               fontface = "bold"  
            )
      }
      
      
      
      if(TRUE){
         
         .paths <- file.path(
            output_dir,
            c(
               "panel_a_percentage_point_diff_2023_2030.png"
               , "panel_b_aroc_future_2023_2030.png"
               , "panel_c_aroc_past_7_year_rolling_global_2000_2019.png"
               , "sr_timeline_barstack_publication.png"
            )
         )
         if(all(file.exists(.paths))){
            .imgs <- purrr::reduce(lapply(.paths, magick::image_read), c)
            magick::image_write(.imgs, format = "pdf", path = file.path(output_dir, "panels_for_review_jm.pdf"))
         } else {
            .paths_existing <- .paths[file.exists(.paths)]
            .paths_missing <- setdiff(.paths, .paths_existing)
            message("images to combine do not all exist", toString(.paths_missing))
         }
      }
      
      message("Figure 4 for publication")
      
      p_combined <- p_pp_diff_pub + 
         plot_spacer() +  
         p_aroc_future_pub + 
         plot_spacer() +
         p_aroc_past_7_year_rolling_global_pub_pub +
         plot_layout(ncol = 1, heights = c(1, 0.01, 1, 0.01, 1)) +  
         plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))  
      
      
      
      Cairo::CairoPNG(
         file = file.path(output_dir, "Figure_4_panel_A_B_C.png"),
         width = 12,  
         height = 18,  
         units = "in",
         res = 1200
      )
      print(p_combined)
      dev.off()
      
      
      
      {  
         
         
         message("-- p_aroc_past_7_year_rolling_global")
         p_aroc_past_7_year_rolling_global <- ggplot() +
            geom_sf(data = dt_aroc_past_7_year_rolling_plot, aes(fill = percentile_global), color = 'black', linewidth = 0.1) +
            
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$aroc_future), ], fill = "black", color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$percentile_global_past),], fill = 'gray70', color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
            
            
            theme_zd_maps(plot_colors_percentiles, plot_breaks) +
            labs(
               title = glue::glue("{title_stub} - {window_display}")
               , subtitle = glue::glue("{subtitle_stub} {past_negs_comment}")
            )
         
         CairoPNG(
            file.path(output_dir, 
                      paste0(
                         
                         glue::glue('aroc_past_7_year_rolling_global_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.png')
                      )),
            height = 4,
            width  = 11,
            res    = 400,
            units  = 'in'
         )
         print(p_aroc_past_7_year_rolling_global)
         dev.off()
         gc()
         
         
         
         
         message("-- p_aroc_past_7_year_rolling_global_pop_wtd_hmisc")
         p_aroc_past_7_year_rolling_global_pop_wtd_hmisc <- ggplot() +
            geom_sf(data = dt_aroc_past_7_year_rolling_plot, aes(fill = percentile_global_hmisc), color = 'black', linewidth = 0.1) +
            
            geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$aroc_future), ], fill = "black", color = 'black', linewidth = 0.1) +
            geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
            geom_sf_pattern(
               data = dt_aroc_past_7_year_rolling_plot[dt_aroc_past_7_year_rolling_plot$aroc_future == 0, ],
               pattern = "stripe",
               pattern_spacing = 0.008,
               pattern_density = 0.008
            ) +
            geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
            labs(
               title = glue::glue("{title_stub} - {window_display} (population-weighted)")
               , subtitle = glue::glue("{subtitle_stub} {past_negs_comment}")
            ) +
            theme_zd_maps(plot_colors_percentiles, plot_breaks)
         
         
         
         if(params$map_remove_neg_past_aroc){
            
            message("-- p_aroc_past_7_year_rolling_gamma")
            p_aroc_past_7_year_rolling_gamma <- ggplot() +
               geom_sf(data = dt_aroc_past_7_year_rolling_plot, aes(fill = percentile_gamma), color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$aroc_future), ], fill = "black", color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf_pattern(
                  data = dt_aroc_past_7_year_rolling_plot[dt_aroc_past_7_year_rolling_plot$aroc_future == 0, ],
                  pattern = "stripe",
                  pattern_spacing = 0.008,
                  pattern_density = 0.008
               ) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               labs(
                  title = glue::glue("{title_stub} - {window_display} (gamma distribution)")
                  , subtitle = glue::glue("{subtitle_stub} {past_negs_comment}")
               ) +
               theme_zd_maps(plot_colors_percentiles, plot_breaks)
            CairoPNG(
               file.path(output_dir, 
                         paste0(
                            
                            glue::glue('aroc_past_7_year_rolling_gamma_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.png')
                         )),
               height = 4,
               width  = 11,
               res    = 400,
               units  = 'in'
            )
            print(p_aroc_past_7_year_rolling_gamma)
            dev.off()
            gc()
            
            
            message("-- p_aroc_past_7_year_rolling_exp")
            p_aroc_past_7_year_rolling_exp <- ggplot() +
               geom_sf(data = dt_aroc_past_7_year_rolling_plot, aes(fill = percentile_exp), color = 'black', linewidth = 0.1) +
               
               geom_sf(data = dt_aroc_past_7_year_rolling_plot[is.na(dt_aroc_past_7_year_rolling_plot$aroc_future), ], fill = "black", color = 'black', linewidth = 0.1) +
               geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_mask,],fill = 'gray70', color = 'black', linewidth = 0.1) +
               geom_sf_pattern(
                  data = dt_aroc_past_7_year_rolling_plot[dt_aroc_past_7_year_rolling_plot$aroc_future == 0, ],
                  pattern = "stripe",
                  pattern_spacing = 0.008,
                  pattern_density = 0.008
               ) +
               geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
               labs(
                  title = glue::glue("{title_stub} - {window_display} (exponential distribution)")
                  , subtitle = glue::glue("{subtitle_stub} {past_negs_comment}")
               ) +
               theme_zd_maps(plot_colors_percentiles, plot_breaks)
            CairoPNG(
               file.path(output_dir, 
                         paste0(
                            
                            glue::glue('aroc_past_7_year_rolling_exp_{window_fname_rolling_benchmarks}{past_negs_fname_stub}.png')
                         )),
               height = 4,
               width  = 11,
               res    = 400,
               units  = 'in'
            )
            print(p_aroc_past_7_year_rolling_exp)
            dev.off()
            gc()
            
         }
         
         
         
      }
      
   }
   
}

