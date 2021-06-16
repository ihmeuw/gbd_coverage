# HEADER ------------------------------------------------------------------
# Author:  FILEPATH
# Date:    2019-02-27
# Project: GBD Vaccines: Data Prep for DPT ordinal regression model
# Purpose: Take GBD survey + admin + lit data (cleaned and outliered) and calculate vacc_dpt12_cond
#          for ordinal regression
# Details: Based on MBG code
#**************************************************************************


### make a copy of the working dataframe
backup <- copy(df)

### subset so dt only has rows related to dpt
dpt <- c("vacc_dpt1", "vacc_dpt3")
dt <- backup[me_name %in% dpt]

# add in print() messages to self
print("Preparing vacc_dpt12_cond per unique survey/location/year/cohort")

# create unique identifier for all locations/years/cohorts/surveys in data
dt[, unique_pair_id := paste(ihme_loc_id, year_id, age_year, nid, survey_name, age_start, sep = "_")]  #

# drop incomplete pairs (i.e. if survey/year/cohort only reports dpt1 OR dpt3, since we can only include surveys with both for conditional computation)  
keep <- dt[, .N, by = "unique_pair_id"][N == 2, unique_pair_id]
test_pairs <- dt[unique_pair_id %in% keep]

test_pairs <- test_pairs[nid != 120905] 

# also subset so only working on rows with data, removing both DPT1 and 3 if one is missing
test_pairs <- test_pairs[!(is.na(data))]
keep2      <- test_pairs[, .N, by = "unique_pair_id"][N == 2, unique_pair_id]
test_pairs <- dt[unique_pair_id %in% keep2]

# remove rows in test_pairs where DPT3 > DPT1
wide       <- dcast.data.table(test_pairs, unique_pair_id ~ me_name, value.var = "data") 
wide       <- wide[, dpt_diff := vacc_dpt1 - vacc_dpt3]
positive   <- wide[dpt_diff >= 0]


keep3      <- unique(positive$unique_pair_id)
test_pairs <- test_pairs[unique_pair_id %in% keep3]

# make a copy of pairs, keeping only vacc_dpt1 or dpt3 with all columns to merge dpt12_cond_data on to
to_merge <- copy(test_pairs)
to_merge <- to_merge[me_name == "vacc_dpt1"]
to_merge[, me_name := "vacc_dpt12_cond"]
to_merge[, data := NULL]

to_merge[, variance := ""]

# make a separate copy of pairs only keeping key columns
new  <- copy(test_pairs)
new  <- new[, .(survey_name, ihme_loc_id, year_id, age_year, me_name, data)]

# make separate data tables of each antigen
dpt1 <- new[me_name == "vacc_dpt1"]
dpt3 <- new[me_name == "vacc_dpt3"]

# make "data" columns unique and get rid of "me_name" in prep for later merge
setnames(dpt1, "data", "dpt1_data")
setnames(dpt3, "data", "dpt3_data")
dpt1[, me_name := NULL]
dpt3[, me_name := NULL]

# make a new data table with dpt1_data and dpt3_data as separate columns
finally <- merge(dpt1, dpt3, by = c("survey_name", "ihme_loc_id", "year_id", "age_year"))


# now with dpt1_cov and dpt3_cov columns in same data table, do the math for vacc_dpt12_cond!
print("Doing the math")
finally[, dpt12_cond_data := ((dpt1_data - dpt3_data)/(1-dpt3_data))]

# only need to keep the dpt12_cond_data column
finally[, dpt1_data := NULL]
finally[, dpt3_data := NULL]

# merge back to "to_merge" sheet, rename "dpt12_cond_data" as "data", remove unique id column
print("Preparing new rows to rbind")
condition <- merge(finally, to_merge, by = c("survey_name", "ihme_loc_id", "year_id", "age_year"))
setnames(condition, "dpt12_cond_data", "data")
condition[, unique_pair_id := NULL]

# rbind these new rows back to full dataset with all other me_names
print("Merging with working df")
df <- rbind(condition, df, fill=TRUE, use.names=TRUE)

# all done
print("Back to standard GBD prep")
