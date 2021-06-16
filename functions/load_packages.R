#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Date:    11/06/2017
# Purpose: Function for loading R packages 
# Inputs:  pkg - list of packages to load (as character string)
#***********************************************************************************************************************


#----FUNCTION-----------------------------------------------------------------------------------------------------------
load_packages <- function(pkg, library_path="FILEPATH") {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  old.pkg <- pkg[(pkg %in% installed.packages()[, "Package"])]
  
  ### if some packages are not already installed, install them in library_path and then load them from library_path
  if (length(new.pkg)) {
    for (x in new.pkg) {
      if (!x %in% list.files(library_path)) {
        install.packages(x, dependencies=TRUE, lib=library_path)
        library(x, lib.loc=library_path, character.only=TRUE)
      } else { library(x, lib.loc=library_path, character.only=TRUE) }
    }
  } 
  
  ### load the packages that are already installed
  sapply(old.pkg, library, character.only=TRUE)
  
}
#***********************************************************************************************************************