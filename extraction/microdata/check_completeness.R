












library(data.table)
library(ggplot2)
library(stringr)


code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines")
R.utils::sourceDirectory(file.path(code_root, "vaccination_pipeline_functions/"))




if (!exists("config_path")) config_path <- file.path(code_root, "pipeline_config.yaml")


load_pipeline_config(step = "check_completeness", config_path = config_path)



stems <- c("bcg", "dpt", "polio", "pcv", "rota", "hib", "hepb", "mcv")



outdir               <- "FILEPATH"


resampled_comparison_paths <- c("FILEPATH/bcg1_cov.csv"   = "FILEPATH/bcg1_cov.csv",
                                "FILEPATH/dpt1_cov.csv"   = "FILEPATH/dpt1_cov.csv", 
                                "FILEPATH/dpt3_cov.csv"   = "FILEPATH/dpt3_cov.csv", 
                                "FILEPATH/mcv1_cov.csv"   = "FILEPATH/mcv1_cov.csv", 
                                "FILEPATH/polio3_cov.csv" = "FILEPATH/polio3_cov.csv")

non_resampled_comparison_paths <- c("FILEPATH/pcv3_cov.csv"  = "FILEPATH/pcv3_cov.csv", 
                                    "FILEPATH/rotac_cov.csv" = "FILEPATH/rotac_cov.csv", 
                                    "FILEPATH/hib3_cov.csv"  = "FILEPATH/hib3_cov.csv", 
                                    "FILEPATH/hepb3_cov.csv" = "FILEPATH/hepb3_cov.csv", 
                                    "FILEPATH/mcv2_cov.csv"  = "FILEPATH/mcv2_cov.csv")
  

produce_diagnostic_plots <- TRUE


resampled_comparison_path_table     <- data.table("new" = names(resampled_comparison_paths),
                                                  "old" = unname(resampled_comparison_paths),
                                                  "resampled" = TRUE)
non_resampled_comparison_path_table <- data.table("new" = names(non_resampled_comparison_paths),
                                                  "old" = unname(non_resampled_comparison_paths),
                                                  "resampled" = FALSE)
comparison_path_table <- rbind(resampled_comparison_path_table, non_resampled_comparison_path_table)



for (i in 1:nrow(comparison_path_table)) {

  
  file_path     <- comparison_path_table[i, new]
  is_resampled  <- comparison_path_table[i, resampled]
  data          <- fread(paste0(file_path))

  message(paste0("-- Checking: ", file_path))

  
  vaccine_columns <- names(data)[grepl(paste(stems, collapse = "|"), names(data))]

  
  if (data[is.na(N), .N] > 0)    stop(paste0(file, ": missing N values"))
  
  if (data[is.na(year), .N] > 0) stop(paste0(file, ": missing year values"))
  
  if (data[is.na(weight), .N] > 0) stop(paste0(file, ": missing weight values"))
  
  if(is_resampled){
    if (data[is.na(latitude),  .N] > 0) stop(paste0(file, ": missing lat. values"))
    if (data[is.na(longitude), .N] > 0) stop(paste0(file, ": missing long. values"))
  }
  
  for (vax in vaccine_columns) {
    if (data[is.na(get(vax)), .N] > 0)       stop(paste0(file, ": missing ", vax, " values"))
    if (data[(get(vax) - N) > .001, .N] > 0) stop(paste0(file, ": ", vax, " > N"))
    if (data[get(vax) < 0, .N] > 0)          stop(paste0(file, ": ", vax, " < 0"))
    if (data[get(vax) == Inf | get(vax) == -Inf, .N] > 0)    stop(paste0(file, ": ", vax, " < 0"))
  }

  if (data[outlier == 1, .N] > 0) stop(paste0(file, ": contains outliers"))

  message(paste0("---- Contains full N, year, lat/long, vax, and weight values, and vax is never greater than 100%"))
}



new_data           <- data.table()
newly_missing_data <- data.table()
shifted_data       <- data.table()
all_data           <- data.table()

for(i in 1:nrow(comparison_path_table)) {

  
  is_resampled    <- comparison_path_table[i, resampled]
  primary_path    <- comparison_path_table[i, new]
  comparison_path <- comparison_path_table[i, old]
  primary_data    <- fread(primary_path)
  comparison_data <- fread(comparison_path)

  
  message(primary_path)

  
  vaccine_columns <- names(primary_data)[grepl(paste(stems, collapse = "|"), names(primary_data))]
  keep_cols <- c("country", "svy_id", "year", "age_bin",
                 "point", "shapefile", "location_code",
                 "N", "N_obs", "weight", vaccine_columns)
  primary_data    <- primary_data[, keep_cols, with = FALSE]
  comparison_data <- comparison_data[, keep_cols, with = FALSE]

  
  primary_collapsed <- primary_data[, .("N" = sum(weight * N),  "vax" = sum(weight * get(vaccine_columns))),
                                    by = c("country", "year", "age_bin", "svy_id")]
  comparison_collapsed <- comparison_data[, .("N" = sum(weight * N),  "vax" = sum(weight * get(vaccine_columns))),
                                    by = c("country", "year", "age_bin", "svy_id")]

  
  primary_collapsed[, cov := vax / N]
  comparison_collapsed[, cov := vax / N]

  
  primary_collapsed[, ID := paste(svy_id, country, year, age_bin, sep = "_")]
  comparison_collapsed[, ID := paste(svy_id, country, year, age_bin, sep = "_")]

  new_data_IDs      <- primary_collapsed$ID[primary_collapsed$ID %!in% comparison_collapsed$ID]
  newly_missing_IDs <- comparison_collapsed$ID[comparison_collapsed$ID %!in% primary_collapsed$ID]

  new           <- primary_collapsed[ID %in% new_data_IDs, ]
  newly_missing <- comparison_collapsed[ID %in% newly_missing_IDs, ]

  new$me_name           <- vaccine_columns
  newly_missing$me_name <- vaccine_columns

  new$resampled           <- is_resampled
  newly_missing$resampled <- is_resampled

  new_data           <- rbind(new_data, new)
  newly_missing_data <- rbind(newly_missing_data, newly_missing)

  
  setnames(primary_collapsed, c("N", "vax", "cov"), c("N_new", "vax_new", "cov_new"))
  setnames(comparison_collapsed, c("N", "vax", "cov"), c("N_old", "vax_old", "cov_old"))
  data <- merge(primary_collapsed, comparison_collapsed,
                by = c("country", "year", "age_bin", "svy_id", "ID"))
  data <- data[, .(country, year, age_bin, svy_id, ID, resampled = is_resampled, N_new, N_old, vax_new, vax_old, cov_new, cov_old)]

  
  data[, diff := round(abs(cov_new - cov_old), digits = 2)]
  data$me_name <- vaccine_columns

  
  shifted_data_vax <- data[diff > 0, ]
  shifted_data     <- rbind(shifted_data, shifted_data_vax)

  all_data <- rbind(data, all_data)
}



if(!dir.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}
fwrite(all_data, file = file.path(outdir, "all_data.csv"))
fwrite(new_data, file = file.path(outdir, "new_data.csv"))
fwrite(newly_missing_data, file = file.path(outdir, "newly_missing_data.csv"))
fwrite(shifted_data, file = file.path(outdir, "shifted_data.csv"))



if (produce_diagnostic_plots){
  
  gg_plot <-
    ggplot(data = all_data[year > 1999, ], mapping = aes(x = cov_old, y = cov_new)) +
    geom_point() +
    labs(x = "Old",
         y = paste0("New")) +
    geom_abline(slope = 1,
                intercept = 0,
                color = "red",
                linetype = "dashed"
    ) +
    theme_bw() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))  +
    geom_text(aes(label=ifelse(diff > 0.01, ID, "")),
              hjust=0, vjust=0, size = 3) +
    facet_wrap(~ me_name)

  ggsave(gg_plot, file = paste0(outdir, "A_B_comparison.png"),
         width = 15, height = 9)
}







