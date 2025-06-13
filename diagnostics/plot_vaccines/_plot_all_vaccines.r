













r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
  message("Starting arg-parser.")
  se$parse_all_named_cli_args(required_args = list(code_root     = "character"
                                                   , config_path = "character"))
} else {
  if(!is.na(Sys.getenv()[['CODE_ROOT']])){ 
    code_root <- Sys.getenv()[['CODE_ROOT']]
  } else {
    code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
  }
  config_path <- file.path(code_root, "pipeline_config.yaml")
}
message("code_root : ", code_root)





source(file.path(code_root, "init.r"))


pacman::p_load(data.table, haven, magrittr, ini, stringr, rhdf5, grid, RMySQL)

source("FILEPATH/read_excel.R")
source("FILEPATH/public.R")
source("FILEPATH/helpers.R")
source("FILEPATH/cluster_tools.r")
source("FILEPATH/ubcov_tools.r")
source("FILEPATH/get_ids.R")
source("FILEPATH/get_location_metadata.R")
locations <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)  
age_ids   <- get_ids("age_group")
sex_ids   <- get_ids("sex")

source(file.path(code_root, "FILEPATH/plot_gpr.R")) 
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"))

load_pipeline_config(step = "_plot_vaccines", config_path = config_path)
save_config(
  step = "_plot_vaccines"
  , config_path = config_path
  , save_root = file.path(modeled_root, date)
  , add_list = list(metadata = se$build_metadata_shell(code_root = code_root))
)


year_id_start <- as.integer(ifelse(is.null(year_id_start), year_start, year_id_start))
year_id_end   <- as.integer(ifelse(is.null(year_id_end), year_end, year_id_end))



root_path <- file.path("FILEPATH") 
path_intros_for_outro <- "FILEPATH/vaccine_intro.rds"





if (!current_gbd) cycle_year    <- 2019 else cycle_year <- "NULL"  
if (publication) add_wuenic     <- TRUE  
if (publication) national_only  <- TRUE  
if (publication) add_old_vers   <- "NULL"
if (publication) add_second_ver <- "NULL"


if (!is.null(custom_subset_mes)) {
  name <- paste("vaccine_subset", "eg", custom_subset_mes[[1]], data.date, sep = "_")
} else if (all_vaccines) {
  name <- paste("all_11_vaccines", data.date, sep = "_")
} else {
  name <- paste("vaccines", data.date, sep = "_")
}
if (!is.null(add_old_vers) & add_old_vers != "NULL")     name <- paste(name, "vs", old_vers_round, add_old_vers, sep="_")
if (!is.null(add_second_ver) & add_second_ver != "NULL") name <- paste(name, "vs", second_ver_rd, add_second_ver, sep="_")
if (national_only)                                       name <- paste(name, "NAT", sep="_") else name <- paste(name, "SUBNAT", sep="_")
if (!is.null(custom_subset_locs))                        name <- paste(name, "loc_subset", "eg", custom_subset_locs[[1]], sep = "_")
if (publication)                                         name <- paste("PUBLICATION", name, sep="_")

if (year_id_start != year_start)                         name <- paste(name, "start", year_id_start, sep="_")
if (year_id_end != year_end)                             name <- paste(name, "end", year_id_end, sep="_")

outpath <- file.path(diagnostics_root, "model_runs", paste0(name, ".pdf"))
outpath <- increment_file_version(outpath)


plot_gpr(
  code_root             = code_root,
  date                  = date,
  covid_status          = covid_status,
  current_gbd           = current_gbd,
  data_date             = data.date,
  output.path           = outpath,
  add.regions           = FALSE,
  add.outliers          = TRUE,
  y.axis                = TRUE,
  all_vaccines          = all_vaccines,
  compare               = add_old_vers,
  compare_round         = old_vers_round,
  compare_covid         = old_vers_covid_status,
  add_wuenic            = add_wuenic,
  compare_more          = add_second_ver,
  compare_more_round    = second_ver_rd,
  compare_more_covid    = second_ver_covid_status,
  data_compare          = data_compare,
  data_compare_date     = data_compare_date,
  data_compare_rd       = data_compare_rd,
  publication_plots     = publication,
  
  locations             = locations,
  year_id_start         = year_id_start,
  year_id_end           = year_id_end,
  dpt12                 = dpt12,
  path_intros_for_outro = path_intros_for_outro,
  to_model_root         = to_model_root,
  gbd_cycle             = gbd_cycle,
  custom_subset_mes     = custom_subset_mes,
  custom_subset_locs    = custom_subset_locs,
  custom_data_path      = custom_data_path
)


