





library(data.table)


vaccines_root <- "FILEPATH"
merged_matches_path <- paste0(vaccines_root, "FILEPATH/source_comparison_match.csv")
manual_matches_path <- paste0(vaccines_root, "FILEPATH/source_comparison_mismatch_manually_matched.csv")

merged_matches <- fread(merged_matches_path)
manual_matches <- fread(manual_matches_path)


 

source_comparison <- rbind(merged_matches, 
                           manual_matches)

source_comparison <- unique(source_comparison)

source_comparison <- source_comparison[order(iso3, year_start, year_end), ]

write.csv(source_comparison, file = paste0(vaccines_root, "FILEPATH/merged_and_manual_matches_01.csv"), row.names = FALSE)
