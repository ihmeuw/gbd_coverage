

username <- Sys.info()[["user"]]

library(openxlsx)
df <- read.xlsx("FILEPATH/literature_extraction_tracking.xlsx", "cooper_coverage")
setDT(df) 


file_name <- "FILENAME"  
refresh <- fread(paste0("FILEPATH", file_name, ".csv"))  
refresh <- refresh[, names(refresh)[names(refresh) %in% names(df)], with=FALSE]


refresh_new <- refresh[!nid %in% unique(df$nid)]
df <- rbind(df, refresh_new, fill = TRUE)


invisible(lapply(names(refresh), function(x) refresh[, (x) := as.character(get(x))]))
invisible(lapply(names(df), function(x) df[, (x) := as.character(get(x))]))
invisible(lapply(names(refresh), function(x) refresh[is.na(get(x)), (x) := ""]))
invisible(lapply(names(df), function(x) df[is.na(get(x)), (x) := ""]))

cols <- c("microdata_status", "tabulations_status")
changes <- fsetdiff(refresh[, c("nid", cols), with=FALSE], df[, c("nid", cols), with=FALSE])[, nid]

for (change in changes) {
  new_1 <- refresh[nid %in% change, cols[1], with=FALSE]
  new_2 <- refresh[nid %in% change, cols[2], with=FALSE]
  new <- ""
  
  if (new_1 != df[nid %in% change, cols[1], with=FALSE]) new <- paste(new, paste0("Change: ", new_1), sep=ifelse(nchar(new) >= 1, "; ", ""))
  if (new_2 != df[nid %in% change, cols[2], with=FALSE]) new <- paste(new, paste0("Change: ", new_2), sep=ifelse(nchar(new) >= 1, "; ", ""))
  
  if (new_1 != df[nid %in% change, cols[1], with=FALSE]) df[nid==change, (cols[1]) := new_1]
  if (new_2 != df[nid %in% change, cols[2], with=FALSE]) df[nid==change, (cols[2]) := new_2]
  
  df[nid %in% change, check_updates := paste0(check_updates, paste0("NEW ", format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d"), " - ", new), sep=ifelse(nchar(check_updates) >= 1, "; ", ""))]  
}


if (length(nrow(df[acceptance_status=="Excluded" & exclusion_status==""])) > 1) stop("Hang on! You have Excluded rows missing an exclusion_status. Fix in Google df and re-prep before uploading.")





fwrite(df, paste0("FILEPATH", format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d_%H:%M"), ".csv"), row.names=FALSE)


invisible(lapply(c("processing_status", "keywords", "secondary_data_type", "ownership"), function(x) df[, (x) := ""]))
df[acceptance_status %in% c("Accepted but want to replace", "Accepted", "Need more information to assess"), processing_status := "Assigned"]
df[acceptance_status %in% c("Accepted but want to replace", "Accepted", "Need more information to assess", "To be reviewed"), exclusion_status := ""]
df[acceptance_status %in% c("To be reviewed", "Excluded"), assigned := ""]
df[acceptance_status=="Excluded", add_comment := exclusion_status]
df[processing_status=="Assigned", assigned := username]



fwrite(df[, .(nid, title, assigned, additional_data_needs, add_comment, acceptance_status, exclusion_status, microdata_status, tabulations_status,
              processing_status, keywords, secondary_data_type, ownership)],
       paste0("FILEPATH", format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y-%m-%d_%H:%M"), ".csv"), row.names=FALSE)




