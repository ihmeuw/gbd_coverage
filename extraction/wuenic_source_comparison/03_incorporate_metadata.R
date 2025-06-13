




library(data.table)
library(XML)

vaccines_root <- "FILEPATH"
source_comparison_path <- paste0(vaccines_root, "FILEPATH/merged_and_manual_matches_02.csv")


source("FILEPATH/load_packages.R")
 


core_repo     = 'FILEPATH'
resolve_urls  = TRUE



supp_source <- function(fp) suppressMessages(source(fp))
suppressMessages( load_packages(c('data.table')) )
supp_source( paste0(core_repo,'FILEPATH/ghdx_query_functions.R') )


source_comparison <- fread(source_comparison_path)
setnames(source_comparison, "ihme_id", "nid")

if(!('nid' %in% names(source_comparison)))       setnames(source_comparison, 'svy_id',  'nid')
if(!('location_code' %in% names(source_comparison))){
  setnames(source_comparison, 'GAUL_CODE', 'location_code')
}

all_nids <- unique(source_comparison[!is.na(nid), nid])

message("Pulling GHDx metadata for all NIDs...")

ghdx_meta <- ghdx_construct_pub_table(
  nids         = all_nids,
  core_repo    = core_repo,
  resolve_urls = resolve_urls
)

source_comparison_citations <- merge(source_comparison, ghdx_meta, all.x = TRUE, by = "nid")
source_comparison_citations <- source_comparison_citations[, .(iso3, nid, who_id, survey_series, ihme_survey, who_survey_english, year_start, year_end, ghdx_url, citation)]
setnames(source_comparison_citations, c("nid", "who_survey_english"), c("ihme_nid", "who_survey"))
source_comparison_citations$citation <- as.character(source_comparison_citations$citation)
source_comparison_citations <- source_comparison_citations[order(iso3, year_start, year_end), ]

who_missing_surveys  <- source_comparison_citations[!is.na(ihme_nid) & is.na(who_id) & (is.na(who_survey) | who_survey == ""), ]
ihme_missing_surveys <- source_comparison_citations[is.na(ihme_nid) & !is.na(who_id) & (is.na(ihme_survey) | ihme_survey == ""), ]


fwrite(source_comparison_citations, file = paste0(vaccines_root, "FILEPATH/source_comparison_with_metadata.csv"), row.names = FALSE)
fwrite(who_missing_surveys, file = paste0(vaccines_root, "FILEPATH/who_missing_surveys.csv"), row.names = FALSE)
fwrite(ihme_missing_surveys, file = paste0(vaccines_root, "FILEPATH/ihme_missing_surveys.csv"), row.names = FALSE)




