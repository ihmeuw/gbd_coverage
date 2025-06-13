



params <- list(
   
   run_note = "Adding disputed territories to maps"
   , mark_best = TRUE 
   , mark_best_comment = "2024_11_13_cfadjust2 - disputed territory outlines added to maps."
   
   
   , run_section_flat_pop_counterfactual = TRUE
   , run_section_exploratory_analysis = FALSE
   , run_section_pub_timelines_barstacks = TRUE
   , run_section_CM_zd_sup_reg_timelines = FALSE
   , run_section_maps = TRUE
   , run_section_maps_pub_past_percentiles_benchmarks = TRUE
   , run_section_maps_distribution_fits = FALSE
   , run_section_maps_histograms = FALSE
   , run_section_maps_scatters = FALSE
   , run_section_simulation = FALSE
   
   
   , gbd_best = "2024_11_13_cfadjust2" 
   , dv_reconnect = NULL 
   
   
   , age_group_id_under_1 = 28L
   , sex_id_both = 3L
   , release_id_forecast = 16L
   , location_set_id_forecast = 39L
   , covidimputed_scenario = "_covidimputed"
   , zd_year_start = 2019L
   , zd_year_end = 2030L
   , zd_year_current = 2023L
   , year_posts_past = sort(seq(from = 2023, to = 1980, by = -7))
   , year_posts_future = seq(from = 2023, to = 2030, by = 7)
   
   
   , sim_using_all_countries = TRUE
   , sim_allow_coverage_to_drop_under_90 = FALSE
   
   
   , covid_scenarios = c("_covidimputed", "_covidfree")
   , covid_years = 2020:2023
   , diff_to_show = 0.0 
   , diff_to_label = 0.15
   
   
   , map_remove_neg_past_aroc = FALSE
   , map_percentile_method = NA 
   
   , shapefile_version = "current"
   , input_mean_coverage_version = "2024_08_27"
   
   
   , year_ids_milestones = c(
      "EPI Established"                                = 1974
      , "Smallpox Eradication"                         = 1980
      , "Universal Childhood\nImmunization Initiative" = 1984 
      , "GAVI Established"                             = 2000
      , "Global Vaccine Action Plan"                   = 2011
      , "COVID-19 Pandemic & IA 2030"                  = 2020
      , "Big Catchup"                                  = 2023
   )
)


paths <- list(
   
   r_lib_team_dir = "FILEPATH"
   
   
   , dpt1_best = file.path(modeled_root, params$gbd_best, paste0("vacc_dpt1", params$covidimputed_scenario, ".rds"))
   
   , dpt1_aggregates = file.path(draws_root_gbdxx, params$gbd_best, "aggregate_summaries_covidimputed/vacc_dpt1.csv")
   , code_root_vaccine_mbg = file.path("FILEPATH", Sys.info()[["user"]], "vaccine")
   
   
   
   
   , shapefile_current_root = "FILEPATH"
   
   , shapefile_vc_lbd_root = file.path("FILEPATH", params$shapefile_version)
   , input_vc_lbd_root = "FILEPATH"
   
)
