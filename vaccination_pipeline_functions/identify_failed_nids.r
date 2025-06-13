#' @title Identify Failed NIDs

#' @description Identifies failed NIDs if their most recent date suggests they failed.


#' @param compare_nid_list A vector of nids set to be processed to compare against. If NULL or "all", ignore.

#' @return NULL

#' @import dplyr
#' @export

library(dplyr)

identify_failed_nids <- function(compare_nid_list = NULL) {
  if (is.null(compare_nid_list)) return("compare_nid_list is NULL")
  if (compare_nid_list == "all") invisible()
  log_data <- read.csv("FILEPATH/log.csv")
  
  log_data$checked <- as.Date(log_data$checked)
  
  
  filtered_data <- log_data %>%
    group_by(nid) %>%
    filter(checked == max(checked)) %>%
    ungroup()
  
  filtered_data <- filtered_data[filtered_data$check_process == "Failure",]
  final_nids <- unique(filtered_data$nid)
  if (is.null(compare_nid_list) || length(compare_nid_list) == 0) {
    print("Failed NIDs that need processing: ")
    print(paste0(final_nids, collapse = ", "))
  } else {
    print("Failed NIDs that will not yet be processed, per the vaccination pipeline config: ")
    print(paste0(final_nids[!final_nids %in% compare_nid_list], collapse = ", "))
  }
}
