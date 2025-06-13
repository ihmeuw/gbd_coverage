#' @title get_unprocessed_nids

#' @description Find all nids in the raw extract folder that do not have a





#' @param extraction_root default "FILEPATH"

#' @return list of nids with 3 vectors, corresponding with the 3 cases in the


#' @importFrom stringr str_extract_all
#' @concept extraction_functions
get_unprocessed_nids <- function(extraction_root = "FILEPATH") {
  
  extraction_log <- fread(file.path(extraction_root, "FILEPATH/log.csv"))
  log_nids <- unique(extraction_log$nid)

  raw_filenames <- list.files(path = file.path(extraction_root, "/00_raw"), pattern = "\\.csv$")

  
  
  file_numbers <- stringr::str_extract_all(raw_filenames, "\\d+")
  raw_nids <- sapply(file_numbers, function(x) as.numeric(x[length(x)]))

  missing_from_log <- raw_nids[raw_nids %!in% log_nids]
  message("The following nids are present in the 00_raw folder but not in the nid log: ", paste(missing_from_log, collapse = ", "))

  
  processed_filenames <- list.files(path = file.path(extraction_root, "/01_processed"), pattern = "\\.csv$")
  processed_nids <- unlist(stringr::str_extract_all(processed_filenames, "\\d+"))

  missing_from_processed <- raw_nids[raw_nids %!in% processed_nids]
  message("\nThe following nids are present in the 00_raw folder but not in the 01_processed folder: ", paste(missing_from_processed, collapse = ", "))

  unaccounted_nids <- missing_from_log[missing_from_log %!in% missing_from_processed]
  message("\nThe following nids are present in the 00_raw folder and 01_processed folder but have no log entry: ", paste(unaccounted_nids, collapse = ", "))

  return(list("missing_from_log" = missing_from_log,
              "missing_from_processed" = missing_from_processed,
              "unaccounted_nids" = unaccounted_nids))
}
