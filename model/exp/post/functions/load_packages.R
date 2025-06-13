


load_packages <- function(pkg, library_path="FILEPATH") {
  
  
  
  
  
  
  
  
  
  
  
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  old.pkg <- pkg[(pkg %in% installed.packages()[, "Package"])]
  
  
  if (length(new.pkg)) {
    for (x in new.pkg) {
      
      if (!x %in% list.files(library_path)) {
        
        install.packages(x, dependencies=TRUE, lib=library_path)
        library(x, lib.loc=library_path, character.only=TRUE)
        
      } else { 
        
        library(x, lib.loc=library_path, character.only=TRUE) 
      }
    }
  } 
  
  
  sapply(old.pkg, library, character.only=TRUE)
  
}

