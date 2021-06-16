#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    September 2018
# Purpose: Prep vaccination code, set repo and paths
# Run:     source("FILEPATH/init.r")
#***********************************************************************************************************************


#----PREP---------------------------------------------------------------------------------------------------------------
### set OS flexibility
os <- .Platform$OS.type
username <- Sys.info()[["user"]]
system <- Sys.info()["sysname"]
if (system=="Linux") {
  j <- "FILEPATH/"
  j_root <- "FILEPATH/"
  h <- paste0("FILEPATH/")
  h_root <- paste0("FILEPATH/")
} else if (system=="Darwin") { 
  j <- "FILEPATH/"
  j_root <- "FILEPATH/"
  h <- "FILEPATH/"
  h_root <- "FILEPATH/"
} else { 
  j <- "FILEPATH/"
  j_root <- "FILEPATH/"
  h <- "FILEPATH/"
  h_root <- "FILEPATH/"
}

### load packages
library(data.table)
library(dplyr)
library(magrittr)

### path locals
ref_data_repo <- "FILEPATH/reference"
setwd(ref_data_repo)
paths <- fread("paths.csv", header=TRUE)
paths <- paths[obj=="user_root", path5 := paste0("repo_root, /", username)]  
fwrite(paths, file=file.path(ref_data_repo, "paths.csv"))  
source(paths[obj=="ubcov_tools", 2, with=FALSE] %>% gsub("J:/", j, .) %>% unlist)
path_loader("paths.csv")
source(ubcov_tools)
if (username != "USERNAME") source(db_tools)  
if (os != "windows") source(file.path(ubcov_central, "FILEPATH/cluster_tools.r"))

### function to clean paths referenced in other scripts
path.clean <- function(path) {
  if (os=="windows") {
    path <- gsub("FILEPATH/", j, path)
    path <- gsub(paste0("FILEPATH/", username), h, path)
  } else {
    path <- gsub("FILEPATH/", j, path)
    path <- gsub("FILEPATH/", h, path)
  }
  
  return(path)
}

# Set working directory to code_root as original 'init.r' intended
setwd(code_root)

#***********************************************************************************************************************