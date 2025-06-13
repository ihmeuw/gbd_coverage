













library(data.table)
library(ggplot2)
library(magrittr)



raw_file_path                 <- "FILEPATH"
ref_data_repo                 <- "FILEPATH"
unconventional_penta_filepath <- "FILEPATH/penta_unconventional_component.csv"


date <- Sys.Date()



'%!in%' <- function(x,y)!('%in%'(x,y))


min_na_rm <- function(x,...) {
  if (all(is.na(x))) {
    return(as.numeric(NA))
  } else {
    return(min(x, na.rm = T))
  }
}

max_na_rm <- function(x,...) {
  if (all(is.na(x))) {
    return(as.numeric(NA))
  } else {
    return(max(x, na.rm = T))
  }
}


get_mics_nids <- function(raw_file_path) {

  
  get_nid_from_filename <- function(raw_file) {
    string_split_file <- unlist(strsplit(raw_file, "_"))
    nid_csv           <- string_split_file[length(string_split_file)]
    nid               <- as.integer(gsub(".csv", "", nid_csv))
    return(nid)
  }

  
  all_raw_files  <- list.files(raw_file_path)
  mics_raw_files <- all_raw_files[grepl("UNICEF_MICS", all_raw_files)]

  
  mics_nids <- unlist(lapply(mics_raw_files, get_nid_from_filename))
  return(mics_nids)
}











lit_extraction  <- fread(file.path(ref_data_repo, "literature_extraction_coverage.csv"))
mics_nids       <- get_mics_nids(raw_file_path)
lit_extraction  <- lit_extraction[nid %in% mics_nids, ]


lit_extraction[, ihme_loc_id := tstrsplit(location_name_short_ihme_loc_id, "[|]")[[2]]]




alt_penta   <- fread(unconventional_penta_filepath)
alt_penta_1 <- alt_penta[DTAP==1 & HepB==0 & HiB==1 & IPV==1]
alt_penta_2 <- alt_penta[DTAP==1 & HepB==1 & HiB==0 & IPV==1]


duples <- list(c("vacc_dpt1",  "vacc_tetra1"),
               c("vacc_dpt1",  "vacc_pent1"),
               c("vacc_dpt2",  "vacc_pent2"),
               c("vacc_dpt2",  "vacc_tetra2"),
               c("vacc_dpt3",  "vacc_tetra3"),
               c("vacc_dpt3",  "vacc_pent3"),
               c("vacc_hib3",  "vacc_pent3"),
               c("vacc_hib3",  "vacc_tetra3"),
               c("vacc_hepb3", "vacc_pent3"),
               c("vacc_mcv1",  "vacc_mmr1"),
               c("vacc_mcv2",  "vacc_mmr2"),
               c("vacc_rcv1",  "vacc_mmr1"),
               c("vacc_rcv2",  "vacc_mmr2"))
duples_alt_1 <- list(c("vacc_dpt1",  "vacc_tetra1"),
                     c("vacc_dpt1",  "vacc_pent1"),
                     c("vacc_dpt2",  "vacc_pent2"),
                     c("vacc_dpt2",  "vacc_tetra2"),
                     c("vacc_dpt3",  "vacc_tetra3"),
                     c("vacc_dpt3",  "vacc_pent3"),
                     c("vacc_hib3",  "vacc_pent3"),
                     c("vacc_hib3",  "vacc_tetra3"),
                     c("vacc_polio3", "vacc_pent3"),  
                     c("vacc_mcv1",  "vacc_mmr1"),
                     c("vacc_mcv2",  "vacc_mmr2"),
                     c("vacc_rcv1",  "vacc_mmr1"),
                     c("vacc_rcv2",  "vacc_mmr2"))
duples_alt_2 <- list(c("vacc_dpt1",  "vacc_tetra1"),
                     c("vacc_dpt1",  "vacc_pent1"),
                     c("vacc_dpt2",  "vacc_pent2"),
                     c("vacc_dpt2",  "vacc_tetra2"),
                     c("vacc_dpt3",  "vacc_tetra3"),
                     c("vacc_dpt3",  "vacc_pent3"),
                     c("vacc_polio3", "vacc_pent3"),  
                     c("vacc_hib3",  "vacc_tetra3"),
                     c("vacc_hepb3", "vacc_pent3"),
                     c("vacc_mcv1",  "vacc_mmr1"),
                     c("vacc_mcv2",  "vacc_mmr2"),
                     c("vacc_rcv1",  "vacc_mmr1"),
                     c("vacc_rcv2",  "vacc_mmr2"))



for (i in duples) {
  lit_extraction <- lit_extraction[!nid %in% unique(alt_penta$survey_id), (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))),
                                                                                           get(i[2]), get(i[1]))]
}
for (i in duples_alt_1) {
  lit_extraction <- lit_extraction[nid %in% unique(alt_penta_1$survey_id), (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))),
                                                                                            get(i[2]), get(i[1]))]
}
for (i in duples_alt_2) {
  lit_extraction <- lit_extraction[nid %in% unique(alt_penta_2$survey_id), (i[1]) := ifelse((!is.na(get(i[2])) & get(i[2]) > get(i[1])) | (!is.na(get(i[2])) & is.na(get(i[1]))),
                                                                                            get(i[2]), get(i[1]))]
}




report <- suppressWarnings(melt(lit_extraction,
                                id.vars = c("age_start", "age_end",  "ihme_loc_id",  "location_code", "nid", "parent_location", "sample_size","year_end", "year_start"),
                                measure.vars = c("vacc_bcg",
                                                 "vacc_yfv",
                                                 "vacc_meng",
                                                 "vacc_mcv1",   "vacc_mcv2",
                                                 "vacc_mmr1",   "vacc_mmr2",
                                                 "vacc_rcv1",   "vacc_rcv2",
                                                 "vacc_dpt1",   "vacc_dpt2",   "vacc_dpt3",
                                                 "vacc_hepb1",  "vacc_hepb2",  "vacc_hepb3",
                                                 "vacc_hib1",   "vacc_hib2",   "vacc_hib3",
                                                 "vacc_pcv1",   "vacc_pcv2",   "vacc_pcv3",
                                                 "vacc_pent1",  "vacc_pent2",  "vacc_pent3",
                                                 "vacc_polio1", "vacc_polio2", "vacc_polio3",
                                                 "vacc_rota1",  "vacc_rota2",  "vacc_rota3",
                                                 "vacc_tetra1", "vacc_tetra2", "vacc_tetra3")))



report <- report[!is.na(value) & value != "", ]

report <- report[!is.na(ihme_loc_id), ]

setnames(report, "year_start", "survey_yr")




rotac_schedule <- readRDS(file.path(ref_data_repo, "vaccine_schedule.rds"))
setnames(rotac_schedule, "me_name", "rota_complete")
rotac_schedule[, variable := paste0("vacc_rota", doses)]
rotac <- merge(report, rotac_schedule, by=c("ihme_loc_id", "variable"), all.x=T)[!is.na(rota_complete) & !is.na(value)]
rotac[, `:=` (variable=rota_complete, location_id=NULL, doses=NULL, rota_complete=NULL)]
report <- rbind(report, rotac)


for_now <- copy(report)










special_age_lit_extractions <- lit_extraction[age_start > 12, ]
special_age_lit_extractions <- unique(special_age_lit_extractions[, .(nid, age_start, age_end)])




username        <- Sys.info()[["user"]]
core_repo       <- paste0("FILEPATH", username, "FILEPATH")
vaccines_repo   <- paste0("FILEPATH", username, "FILEPATH")
extraction_root <- "FILEPATH"


source('FILEPATH/setup.R')
lapply(c("proto", "findpython", "argparse", "data.table", "magrittr", "survey", "parallel", "plyr", "dplyr", "haven",
         "rgeos", "raster", "rgdal", "dismo", "gbm", "foreign", "doParallel", "grid", "gridExtra", "gtools", "ggplot2",
         "sf", "assertthat", "INLA", "seegSDM", "seegMBG", "pacman", "glmnet", "RMySQL"), library, character.only = TRUE)


library("fasterize", lib.loc="FILEPATH", character.only=TRUE)
library("binom", lib.loc="FILEPATH", character.only=TRUE)


source(file.path("FILEPATH", username, "FILEPATH/processing_functions.R"))




topic       <- "vaccines"
config.path <- "FILEPATH/collapse_config.csv"
parallel    <- FALSE



special_ages <- data.table()
for (i in 1:nrow(special_age_lit_extractions)){

  
  message("\nNow working on ", i)
  nid       <- special_age_lit_extractions[i, nid]
  age_start <- special_age_lit_extractions[i, age_start]
  age_end   <- special_age_lit_extractions[i, age_end]
  test      <- process_mics(nid)

  
  if (length(vaccines[vaccines %in% names(test)]) > 0) {
    dataset     <- prep_for_tabulation_mics_special_age(nid, team="gbd", vaccines.=c(vaccines, "rotac"), age_start=age_start, age_end=age_end)
    
    dataset  <- dataset[[2]] %>% as.data.table
    config   <- load.settings(config.path, topic)
    tab_data <- collapse.run.mics.special.age(dataset, config=config)
  } else {
    tab_data <- data.table()
  }

  
  special_ages <- rbind(special_ages, tab_data, fill=T)
}


if (length(unique(special_ages$nid)) != length(unique(special_age_lit_extractions$nid))){
  stop("Not all NIDs got processed. Look at your error message. Probably a permissions issue?")
}




special_ages <- merge(special_ages, special_age_lit_extractions, by="nid")
setnames(special_ages, c("mean"), c("data"))


dir.create(paste0("FILEPATH", date))
fwrite(special_ages, paste0("FILEPATH", date, "/special_tabulations.csv"))







username    <- Sys.info()[["user"]]
decomp_step <- "iterative"
source("FILEPATH/get_location_metadata.R")
source(paste0("FILEPATH", username, "FILEPATH/init.r"))


locs <- get_location_metadata(location_set_id=location_set_id, release_id = release_id)[level >= 3, ]



regular_ages <- data.table()
for (mics_nid in mics_nids) {
  tabulated_survey_path <- paste0("FILEPATH", mics_nid, ".csv")
  if (file.exists(tabulated_survey_path)) {
    temp         <- fread(tabulated_survey_path)
    regular_ages <- rbind(regular_ages, temp, fill = TRUE)
  }
}
setnames(regular_ages, "mean", "data")



regular_ages[, variance := standard_error ^ 2]  
regular_ages[, age_group_id := 22]
regular_ages[, cv_survey := 1]


drop.locs <- regular_ages[!(ihme_loc_id %in% locs$ihme_loc_id), ihme_loc_id] %>% unique
if (length(drop.locs) > 0) {
  print(paste0("UNMAPPED LOCATIONS (DROPPING): ", toString(drop.locs)))
  regular_ages <- regular_ages[!(ihme_loc_id %in% drop.locs)]
}


if (nrow(regular_ages[data %in% c(0, 1)]) > 0) {
  regular_ages.w <- regular_ages[data %in% c(0, 1)]
  sample_size <- regular_ages.w$sample_size
  n  <- ifelse(regular_ages.w$data==0, 0, sample_size)
  ci <- binom.confint(n, sample_size, conf.level=0.95, methods="wilson")
  se <- (ci$upper - ci$lower) / 3.92
  variance <- se ^ 2 * 2.25 
  regular_ages[data %in% c(0, 1)]$variance <- variance
}


regular_ages <- regular_ages[design_effect < 1, variance := variance * 2.25 / design_effect]
regular_ages <- regular_ages[design_effect < 1, design_effect := 2.25]

regular_ages <- regular_ages[age_year == 1, ]
regular_ages[, `:=` (age_bin=NULL, age_bin_agg=NULL, age_bin_agg_id=NULL)]
regular_ages$age_start <- 12
regular_ages$age_end   <- 23
regular_ages <- regular_ages[survey_name == "UNICEF_MICS", ]


both                <- unique(for_now[, .(nid, age_start)])
two_ages            <- both$nid[which(duplicated(both$nid))]
single_ages         <- both[nid %!in% two_ages, ]
single_ages_to_drop <- single_ages[age_start != 12, nid]


regular_ages <- regular_ages[nid %!in% single_ages_to_drop, ]




new <- rbind(regular_ages, special_ages, fill=T)
new <- new[me_name %in% unique(for_now$variable), ]
new[, parent_location := gsub("_*[0-9]", "", ihme_loc_id)]
new <- unique(new)

for_now <- unique(for_now)

test <- merge(for_now, new,  
              by.x=c("parent_location","ihme_loc_id", "survey_yr", "variable", "nid", "age_start", "age_end"),
              by.y=c("parent_location","ihme_loc_id", "year_start", "me_name", "nid", "age_start", "age_end"), all.x=T, all.y=T)

keep <- test[, .(parent_location, ihme_loc_id, year_start, nid, variable, age_start, age_end, sample_size.x, value, data)]
colnames(keep) <- c("parent_location", "ihme_loc_id", "year_start", "nid", "me_name","age_start", "age_end", "sample_size", "report", "tabulation")


keep[, report := (as.numeric(report) / 100)]
keep[, difference := (report - tabulation)]

hist(keep$difference, breaks=100)

keep <- keep[!is.na(tabulation) | !is.na(report), ]

keep[ihme_loc_id=="" & nid==264590, `:=` (ihme_loc_id="MEX", parent_location="MEX")]  


fwrite(keep, paste0("FILEPATH", date, "/mics_for_review_with_subnationals.csv"))
fwrite(keep, file.path(ref_data_repo, "mics_for_review_with_subnationals.csv"))










threshold        <- 0.1 
subnat_threshold <- 0.9 


mics_compare_df <- fread(paste0("FILEPATH", date, "/mics_for_review_with_subnationals.csv"))


diagnostic_output_dir <- paste0("FILEPATH", date, "/")
ifelse(!dir.exists(diagnostic_output_dir), dir.create(diagnostic_output_dir), FALSE)


final_output_dir <- paste0("FILEPATH", date, "/")
ifelse(!dir.exists(final_output_dir), dir.create(final_output_dir), FALSE)







fwrite(mics_compare_df, paste0(diagnostic_output_dir, "mics_compare_df.csv"))


mics_nat_df    <- subset(mics_compare_df, !grepl("_", ihme_loc_id))
mics_subnat_df <- subset(mics_compare_df, grepl("_", ihme_loc_id))


mics_nat_df_nids <- unique(mics_nat_df$nid)


add_to_nat_df <- mics_subnat_df[!nid %in% mics_nat_df_nids]
actually_still_subnat <- data.table()
for (n in unique(add_to_nat_df$nid)) {
  test <- add_to_nat_df[nid==n]
  test <- test[, count := length(unique(test$ihme_loc_id))]
  if(unique(test$count) > 1){
    actually_still_subnat <- rbind(actually_still_subnat, test)
  }
}
actually_still_subnat[, count := NULL]


mics_nat_df_nids <- c(mics_nat_df_nids, unique(actually_still_subnat$nid))


add_to_nat_df  <- add_to_nat_df[!nid %in% mics_nat_df_nids]
mics_subnat_df <- mics_subnat_df[nid %in% mics_nat_df_nids]


mics_nat_df <- rbind(mics_nat_df, add_to_nat_df)

if (nrow(mics_nat_df) + nrow(mics_subnat_df) != nrow(mics_compare_df)) {
  stop("Something is going wrong in subnational assignments!")
}





mics_nat_df[is.na(report) & !is.na(tabulation), tab_only := TRUE]
mics_nat_df[, rows_per_me_nid := .N, by = c("nid", "me_name")]
mics_nat_df[, any_comparisons := sum(!is.na(difference)), by = c("nid", "me_name")]
mics_nat_df[tab_only == TRUE & rows_per_me_nid > 1 & any_comparisons > 0, remove_extraneous := TRUE]

mics_nat_df <- subset(mics_nat_df, is.na(remove_extraneous))
mics_nat_df[, tab_only := NULL]
mics_nat_df[, rows_per_me_nid := NULL]
mics_nat_df[, remove_extraneous := NULL]









subnat_nids <- unique(mics_subnat_df$nid)


mics_nat_df[nid %in% subnat_nids, subnational := TRUE]
mics_nat_df[subnational != TRUE, subnational := FALSE]



subnat_loc_ids <- unique(mics_subnat_df$ihme_loc_id)
if (nrow(mics_nat_df[ihme_loc_id %in% subnat_loc_ids & subnational == FALSE])) {
  warning("There are some surveys from GBD subnational countries with only national report extractions - check CSV")
  fwrite(mics_nat_df[ihme_loc_id %in% subnat_loc_ids & subnational == FALSE],
         file = paste0(diagnostic_output_dir, "gbd_subnational_locs_only_national_estimates.csv"))
}


dir.create(paste0(diagnostic_output_dir, "subnationals/"))

df_cors <- lapply(unique(mics_subnat_df$nid), function(a_nid) {

  df_subset <- subset(mics_subnat_df, nid == a_nid)
  parent_loc <- unique(df_subset$parent_location)
  df_subset[, age_cohort := paste0(age_start, "_", age_end)]

  age_cors <- lapply(unique(df_subset$age_cohort), function(ac) {

    df_age <- df_subset[age_cohort == ac]

    
    
    cor_na_rm <- function(x, y, ...) {
      if (all(is.na(x) + is.na(y))) {
        return(as.numeric(NA))
      } else {
        return(cor(x, y, use="complete.obs", ...))
      }
    }

    df_age[, subnat_sp_cor := cor_na_rm(x=report, y=tabulation,
                                        method = 'spearman'),
           by = "me_name"]

    
    
    cors <- unique(subset(df_age, select = c("me_name", "subnat_sp_cor",
                                             "age_start", "age_end",
                                             "nid")))
    cors[, a_label := paste0("sp_cor: ", round(subnat_sp_cor, 2))]

    if (nrow(df_age[!is.na(subnat_sp_cor)]) > 0) {

      gg_age <- ggplot(df_age,
                       aes(x = report, y = tabulation)) +
        geom_point(alpha = 0.5) +
        geom_abline() +
        geom_text(data = cors, x = 1, y = 0, aes(label = a_label), hjust=1) +
        theme_bw() +
        geom_smooth(method = "lm") +
        theme_bw() +
        xlim(0,1) +
        ylim(0,1) +
        facet_wrap(~me_name) +
        coord_equal() +
        labs(title = "Report vs tabulation",
             subtitle = paste(parent_loc , " | ", a_nid, " | ages: ", ac),
             x = "Report",
             y = "Tabulation")

      png(file = paste0(diagnostic_output_dir, "FILEPATH", parent_loc, "_", a_nid, "_", ac, ".png"),
          height = 10,
          width = 18,
          units = "in",
          res = 300)
      print(gg_age)
      dev.off()

    }

    
    cors[, a_label := NULL]

    
    return(cors)

  })

  age_cors <- rbindlist(age_cors)

  return(age_cors)

})


df_cors <- rbindlist(df_cors)


mics_nat_df <- merge(mics_nat_df, df_cors,
                     by = c("me_name", "age_start", "age_end", "nid"),
                     all.x = TRUE)




mics_nat_df[abs(difference) > threshold & !is.na(difference), row_national_ok := FALSE]
mics_nat_df[abs(difference) <= threshold & !is.na(difference), row_national_ok := TRUE]


mics_nat_df[subnational == TRUE & subnat_sp_cor >= subnat_threshold & !is.na(subnat_sp_cor),
            row_subnational_ok := TRUE]

mics_nat_df[subnational == TRUE & subnat_sp_cor < subnat_threshold & !is.na(subnat_sp_cor),
            row_subnational_ok := FALSE]





mics_nat_df[, n_rows_by_me_nid := .N, by = c("nid", "me_name")]


mics_nat_df[, max_abs_diff_by_me_nid := max_na_rm(abs(difference), na.rm = T), by = c("nid", "me_name")]


mics_nat_df[abs(max_abs_diff_by_me_nid) > threshold & !is.na(max_abs_diff_by_me_nid), me_nid_national_ok := FALSE]
mics_nat_df[abs(max_abs_diff_by_me_nid) <= threshold & !is.na(max_abs_diff_by_me_nid), me_nid_national_ok := TRUE]




mics_nat_df[, min_subnat_sp_cor_by_me_nid := min_na_rm(subnat_sp_cor), by = c("nid", "me_name")]


mics_nat_df[subnational == TRUE & min_subnat_sp_cor_by_me_nid >= subnat_threshold & !is.na(min_subnat_sp_cor_by_me_nid),
            me_nid_subnational_ok := TRUE]
mics_nat_df[subnational == TRUE & min_subnat_sp_cor_by_me_nid < subnat_threshold & !is.na(min_subnat_sp_cor_by_me_nid),
            me_nid_subnational_ok := FALSE]



national_by_me_nid <- unique(subset(mics_nat_df, select = c("nid", "me_name", "me_nid_national_ok")))
national_by_me_nid[, n_mes_in_nid := .N, by = nid]
national_by_me_nid[, n_mes_ok_in_nid := sum(me_nid_national_ok == TRUE, na.rm = T), by = nid]
national_by_me_nid[, n_mes_not_ok_in_nid := sum(me_nid_national_ok == FALSE, na.rm = T), by = nid]


mics_nat_df <- merge(mics_nat_df, national_by_me_nid, all.x = T, by = c("nid", "me_name", "me_nid_national_ok"))


mics_nat_df[, pct_ok_mes := n_mes_ok_in_nid / (n_mes_ok_in_nid + n_mes_not_ok_in_nid)]





mics_nat_df[me_nid_national_ok == TRUE & (subnational %in% c(FALSE, NA)), decision := "use_microdata"]


mics_nat_df[me_nid_national_ok == FALSE, decision := "use_report_extractions"]


mics_nat_df[me_nid_national_ok == TRUE & me_nid_subnational_ok == TRUE & subnational == TRUE, decision := "use_microdata"]


mics_nat_df[me_nid_national_ok == TRUE & me_nid_subnational_ok == FALSE & subnational == TRUE, decision := "use_report_extractions"]


mics_nat_df[me_nid_national_ok == TRUE & is.na(me_nid_subnational_ok) & subnational == TRUE, decision := "use_microdata"]



mics_nat_df[is.na(me_nid_national_ok) & is.na(report) & !is.na(tabulation) & pct_ok_mes >= 0.9 & !is.nan(pct_ok_mes),
            decision := "use_microdata"]


mics_nat_df[is.na(me_nid_national_ok) & is.na(report) & !is.na(tabulation) & pct_ok_mes < 0.9 & !is.nan(pct_ok_mes),
            decision := "drop"]


mics_nat_df[is.na(me_nid_national_ok) & is.na(report) & !is.na(tabulation) & is.nan(pct_ok_mes),
            decision := "use_microdata"]








if (nrow(mics_nat_df[is.na(tabulation) & !is.na(report)]) > 0) {

  fwrite(mics_nat_df[is.na(tabulation) & !is.na(report)],
         file = paste0(diagnostic_output_dir, "surveys_with_reports_but_no_tabs.csv"))

  
  mics_nat_df[is.na(tabulation) & !is.na(report), report_no_tab := TRUE]

  
  mics_nat_df[n_rows_by_me_nid > 1,
              any_valid_microdata := any(decision == "use_microdata", na.rm = T),
              by = c("me_name", "nid")]

  
  mics_nat_df[n_rows_by_me_nid > 1 & report_no_tab == TRUE &  is.na(decision) & any_valid_microdata == TRUE,
              decision := "drop"]

  
  
  
  mics_nat_df[report_no_tab == TRUE & (any_valid_microdata == FALSE | n_rows_by_me_nid == 1),
              decision := "use_report_extractions"]

  
  mics_nat_df[, report_no_tab := NULL]

}


if (nrow(mics_nat_df[is.na(decision)]) > 0) {
  warning("There are unclassified surveys! See csv files.")
  fwrite(mics_nat_df[is.na(decision)],
         file = paste0(diagnostic_output_dir, "unclassified_surveys.csv"))
}





fwrite(mics_nat_df, file = paste0(diagnostic_output_dir, "mics_compare_national_processed.csv"))
fwrite(mics_subnat_df, file = paste0(diagnostic_output_dir, "mics_compare_subnational_processed.csv"))


decision_df <-  subset(mics_nat_df,
                       select = c("me_name", "ihme_loc_id",
                                  "nid", "decision")) %>% unique()




final_nids <- unique(subset(mics_nat_df, select = c("nid", "me_name")))
final_nids[, me_nid := paste0(me_name, "_", nid)]
final_me_nids <- unique(final_nids$me_nid)

original_nids <- unique(subset(mics_compare_df, select = c("nid", "me_name")))
original_nids[, me_nid := paste0(me_name, "_", nid)]
orig_me_nids <- unique(original_nids$me_nid)

subnational_nids <- unique(subset(mics_subnat_df, select = c("nid", "me_name")))
subnational_nids[, me_nid := paste0(me_name, "_", nid)]
subnat_me_nids <- unique(subnational_nids$me_nid)


missing_subnat_me_nids <- subset(subnational_nids, me_nid %in% subnat_me_nids[!(subnat_me_nids %in% final_me_nids)])









subnat_decisions <- lapply(1:nrow(missing_subnat_me_nids), function(i) {

  a_me <- missing_subnat_me_nids[i, me_name]
  a_nid <- missing_subnat_me_nids[i, nid]

  df_subset <- mics_subnat_df[me_name == a_me & nid == a_nid]
  cor_subset <- df_cors[me_name == a_me & nid == a_nid]

  
  if (any(!is.na(cor_subset$spearman))) {
    a_min_cor <- min(cor_subset$spearman, na.rm = T)
  } else {
    a_min_cor <- as.numeric(NA)
  }

  
  tab_rows <- nrow(df_subset[!is.na(tabulation)])

  
  
  

  if ((a_min_cor >= subnat_threshold | is.na(a_min_cor)) & tab_rows >= 1) {
    decision <- "use_microdata"
  } else if ((!is.na(a_min_cor) & a_min_cor < subnat_threshold) | (is.na(a_min_cor) & tab_rows == 0)) {
    decision <- "use_report_extractions"
  }
  return(data.table(me_name = a_me,
                    nid = a_nid,
                    decision = decision,
                    ihme_loc_id = unique(df_subset$parent_location)))
})

subnat_decisions <- rbindlist(subnat_decisions)

decision_df <- rbind(decision_df, subnat_decisions, fill = T, use.names = T)
decision_df <- subset(decision_df, nid != 156268) 
decision_df <- decision_df[which(decision_df$ihme_loc_id != ""),]

decision_df[nid==527453 & decision=='use_report_extractions', decision:='use_microdata']

decision_df[nid==264590 & decision=='use_report_extractions', decision:='use_microdata']

decision_df <- subset(decision_df, nid %!in% c(27022, 141914)) 
ratio_decisions <- function(n) {
  message(n)
  
  df_subset <- decision_df[nid == n]

  ratio_table <- data.table(rbind(c("hepb3", "dpt3"),
                                  c("dpt1", "dpt3"),
                                  c("hib3", "dpt3"),
                                  c("pcv3", "dpt3"),
                                  c("rotac", "dpt3"),
                                  c("mcv2", "mcv1"),
                                  c("hepb3", "dpt3"),
                                  c("rcv1", "mcv1")))
  names(ratio_table) <- c("numerator", "denominator")

  ratio_decisions <- lapply(1:nrow(ratio_table), function(i) {
    num <- ratio_table[i, numerator]
    den <- ratio_table[i, denominator]

    num_decision <- df_subset[me_name == paste0("vacc_", num), decision]
    den_decision <- df_subset[me_name == paste0("vacc_", den), decision]

    
    if ((length(num_decision) == 0) | (length(den_decision) == 0)) {
      num_decision <- NA
      den_decision <- NA
    }

    return(data.table(num = num,
                      den = den,
                      num_decision = num_decision,
                      den_decision = den_decision))
  }) %>% rbindlist()

  ratio_decisions[, me_name := paste0("ratio_", num, "_", den)]

  

  
  ratio_decisions[num_decision == "use_report_extractions" & den_decision == "use_report_extractions", decision := "use_report_extractions"]

  
  ratio_decisions[num_decision == "use_microdata" & den_decision == "use_microdata", decision := "use_microdata"]

  
  ratio_decisions[num_decision == "use_report_extractions" & den_decision == "use_microdata", decision := "use_report_extractions"]

  
  ratio_decisions[num_decision == "use_microdata" & den_decision == "use_report_extractions", decision := "use_report_extractions"]

  
  ratio_decisions[, nid := n]
  ratio_decisions[, ihme_loc_id := unique(df_subset$ihme_loc_id)]

  ratio_decisions <- subset(ratio_decisions,
                            !is.na(decision),
                            select = c("me_name", "nid", "ihme_loc_id", "decision"))

  return(ratio_decisions)

}

ratio_decision_df <- lapply(unique(decision_df$nid), ratio_decisions) %>% rbindlist




decision_df <- rbind(decision_df, ratio_decision_df, fill = T, use.names = T)
decisions_pre_multi_dose_standardizing <- copy(decision_df)








dpt_report_nids    <- decision_df[me_name %in% c("vacc_dpt1", "vacc_dpt3") & decision == "use_report_extractions", unique(nid)]
dpt_microdata_nids <- decision_df[me_name %in% c("vacc_dpt1", "vacc_dpt3") & !(nid %in% dpt_report_nids), unique(nid)]

decision_df[nid %in% dpt_report_nids & me_name %in% c("vacc_dpt1", "vacc_dpt2", "vacc_dpt3"), decision := "use_report_extractions"]
decision_df[nid %in% dpt_microdata_nids & me_name %in% c("vacc_dpt1", "vacc_dpt2", "vacc_dpt3"), decision := "use_microdata"]


stems <- c("hib", "hepb", "pcv", "polio")
for (stem in stems) {

  
  final_dose                    <- paste0("vacc_", stem, "3")
  final_dose_decision_microdata <- decision_df[me_name == final_dose & decision == "use_microdata", unique(nid)]
  final_dose_decision_report    <- decision_df[me_name == final_dose & decision == "use_report_extractions", unique(nid)]

  
  decision_df[grepl(stem, me_name) & !grepl("ratio", me_name) & nid %in% final_dose_decision_microdata, decision := "use_microdata"]
  decision_df[grepl(stem, me_name) & !grepl("ratio", me_name) & nid %in% final_dose_decision_report, decision := "use_report_extractions"]

  
  first_dose               <- paste0("vacc_", stem, "1")
  nids                     <- decision_df[grepl(stem, me_name) & !grepl("ratio", me_name), unique(nid)]
  nids_with_final_dose     <- decision_df[me_name == final_dose, unique(nid)]
  nids_missing_final_dose  <- nids[!nids %in% nids_with_final_dose]

  first_dose_decision_microdata <- decision_df[grepl(stem, me_name) & !grepl("ratio", me_name) & nid %in% nids_missing_final_dose & decision == "use_microdata", unique(nid)]
  first_dose_decision_report    <- decision_df[grepl(stem, me_name) & !grepl("ratio", me_name) & nid %in% nids_missing_final_dose & decision == "use_report_extractions", unique(nid)]

  decision_df[grepl(stem, me_name) & !grepl("ratio", me_name) & nid %in% first_dose_decision_microdata, decision := "use_microdata"]
  decision_df[grepl(stem, me_name) & !grepl("ratio", me_name) & nid %in% first_dose_decision_report, decision := "use_report_extractions"]
}



rota_last_dose_c_nids <- decision_df[me_name == "vacc_rotac", unique(nid)]
rota_last_dose_3_nids <- decision_df[me_name == "vacc_rota3" & !(nid %in% rota_last_dose_c_nids), unique(nid)]
rota_last_dose_2_nids <- decision_df[me_name == "vacc_rota2" & !(nid %in% c(rota_last_dose_c_nids, rota_last_dose_3_nids)), unique(nid)]
rota_last_dose_1_nids <- decision_df[me_name == "vacc_rota1" & !(nid %in% c(rota_last_dose_c_nids, rota_last_dose_3_nids, rota_last_dose_2_nids)), unique(nid)]


rota_last_dose_c_decision_microdata <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_c_nids & decision == "use_microdata", unique(nid)]
rota_last_dose_c_decision_report <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_c_nids & decision == "use_report_extractions", unique(nid)]

rota_last_dose_3_decision_microdata <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_3_nids & decision == "use_microdata", unique(nid)]
rota_last_dose_3_decision_report <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_3_nids & decision == "use_report_extractions", unique(nid)]

rota_last_dose_2_decision_microdata <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_2_nids & decision == "use_microdata", unique(nid)]
rota_last_dose_2_decision_report <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_2_nids & decision == "use_report_extractions", unique(nid)]

rota_last_dose_1_decision_microdata <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_1_nids & decision == "use_microdata", unique(nid)]
rota_last_dose_1_decision_report <-
  decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_1_nids & decision == "use_report_extractions", unique(nid)]


decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_c_decision_microdata, decision := "use_microdata"]
decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_c_decision_report, decision := "use_report_extractions"]

decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_3_decision_microdata, decision := "use_microdata"]
decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_3_decision_report, decision := "use_report_extractions"]

decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_2_decision_microdata, decision := "use_microdata"]
decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_2_decision_report, decision := "use_report_extractions"]

decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_1_decision_microdata, decision := "use_microdata"]
decision_df[grepl("rota", me_name) & !grepl("ratio", me_name) & nid %in% rota_last_dose_1_decision_report, decision := "use_report_extractions"]






rotac_schedule <- readRDS(file.path(ref_data_repo, "vaccine_schedule.rds"))
setnames(rotac_schedule, "me_name", "rota_complete")
rotac_schedule[, me_name := paste0("vacc_rota", doses)]
rotac <- merge(decision_df, rotac_schedule, by=c("ihme_loc_id", "me_name"), all.x=T)[!is.na(rota_complete) & !is.na(decision)]
rotac[, `:=` (me_name=rota_complete, location_id=NULL, doses=NULL, rota_complete=NULL)]
decision_df <- rbind(decision_df, rotac)



previous_decisions  <- fread("FILEPATH/mics_comparison_decisions_by_me_nid.csv")
decision_comparison <- merge(decision_df, previous_decisions, by = c("nid", "ihme_loc_id", "me_name"), all.x = TRUE)
setnames(decision_comparison, c("decision.x", "decision.y"), c("new", "old"))
changed_decisions   <- decision_comparison[!is.na(old) & old != "NA" & !is.na(new) & new != old, ]

fwrite(changed_decisions, paste0(final_output_dir, "changed_decisions.csv"))

fwrite(decision_df, paste0(final_output_dir, "mics_comparison_decisions_by_me_nid.csv"))

fwrite(decision_df, file.path(ref_data_repo, "mics_comparison_decisions_by_me_nid.csv"))  



summarize_me_match <- function(me) {
  df_subset <- subset(mics_nat_df, me_name == me)

  
  df_summary <- data.table(me_name = me,
                           total_rows = nrow(df_subset),
                           both_complete = nrow(subset(df_subset, !is.na(tabulation) & !is.na(report))),
                           both_missing = nrow(subset(df_subset, is.na(tabulation) & is.na(report))),
                           missing_tabulation_only = nrow(subset(df_subset, is.na(tabulation) & !is.na(report))),
                           missing_report_only = nrow(subset(df_subset, !is.na(tabulation) & is.na(report))))
  return(df_summary)
}

me_match_summaries <- rbindlist(lapply(unique(mics_nat_df$me_name), summarize_me_match))

fwrite(me_match_summaries, paste0(diagnostic_output_dir, "summaries_of_comparisons_by_me.csv"))



summarize_me_threshold <- function(me) {
  df_subset <- subset(mics_nat_df, me_name == me)

  
  df_summary <- data.table(me_name = me,
                           threshold = threshold,
                           total_rows = nrow(df_subset),
                           rows_with_difference = nrow(subset(df_subset, !is.na(difference))),
                           rows_dropped_by_threshold = nrow(subset(df_subset, !is.na(difference) & abs(difference) > threshold)),
                           rows_with_difference_post_2000 = nrow(subset(df_subset, !is.na(difference) & year_start >= 2000)),
                           rows_dropped_by_threshold_post_2000 = nrow(subset(df_subset, !is.na(difference) & abs(difference) > threshold & year_start >= 2000)))
  df_summary[, pct_dropped_by_threshold := round(rows_dropped_by_threshold / rows_with_difference, 2)]
  df_summary[, pct_dropped_by_threshold_post_2000 := round(rows_dropped_by_threshold_post_2000 / rows_with_difference_post_2000, 2)]

  
  return(df_summary)
}

me_threshold_summaries <- rbindlist(lapply(unique(mics_nat_df$me_name), summarize_me_threshold))

fwrite(me_threshold_summaries, paste0(diagnostic_output_dir, "summaries_of_threshold_effects_by_me.csv"))

mics_nat_df[abs(difference) > threshold, above_threshold := TRUE]
mics_nat_df[abs(difference) <= threshold, above_threshold := FALSE]


gg_threshold <- ggplot(mics_nat_df[!is.na(difference)],
                       aes(x = report, y = tabulation, group = me_name)) +
  geom_point(alpha = 0.5, aes(color = above_threshold)) +
  geom_abline() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~me_name, nrow = 4) +
  coord_equal() +
  labs(title = "Report vs tabulation",
       subtitle = paste0("Threshold: ", threshold),
       x = "Report",
       y = "Tabulation",
       color = "Above threshold?") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"))

png(file = paste0(diagnostic_output_dir, "plot_report_vs_tabulation.png"),
    height = 10,
    width = 16,
    units = "in",
    res = 300)
print(gg_threshold)
dev.off()


for (me in unique(mics_nat_df$me_name)) {
  gg_histogram <- ggplot(mics_nat_df[!is.na(difference) & me_name == me]) +
    geom_histogram(aes(x = difference), binwidth = 0.01) +
    geom_vline(xintercept = threshold, color = "red") +
    geom_vline(xintercept = -threshold, color = "red") +
    theme_bw() +
    xlim(-1,1) +
    labs(title = paste0("Histogram of differences: ", me),
         subtitle = paste0("Threshold: ", threshold),
         x = "Difference",
         y = "Count (Rows)")

  png(file = paste0(diagnostic_output_dir, "histogram_threshold_", me, ".png"),
      height = 7,
      width = 12,
      units = "in",
      res = 300)
  print(gg_histogram)
  dev.off()
}




