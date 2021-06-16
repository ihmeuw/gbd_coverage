#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    11/14/2017
# Purpose: Function for reading in excel files
# Inputs:  dataset
# Run:     source("FILEPATH/read_excel.R", echo=TRUE)
#***********************************************************************************************************************


#----FUNCTION-----------------------------------------------------------------------------------------------------------
source("FILEPATH/load_packages.R")
load_packages("readxl")

### supress warnings in readxl package
read_excel <- function(...) {
  
  quiet_read <- purrr::quietly(readxl::read_excel)
  out <- quiet_read(...)
  
  if(length(c(out[["warnings"]], out[["messages"]]))==0)
    return(out[["result"]])
  
  else readxl::read_excel(...)
  
}
#***********************************************************************************************************************