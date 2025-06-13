#' @title Palette for GBD super regions

#' @param add_global [lgl] Add a color for Global? (black)
#' @param names_short [lgl] Use short names for super regions? e.g. "HI" for "High-income"

#' @return [chr] Named character vector with colors for GBD super regions
palette_gbd_super_regions <- function(add_global = FALSE, names_short = FALSE){
   
   stopifnot(is.logical(add_global))
   stopifnot(is.logical(names_short))
   
   sr_palette <- c(
      `Central Europe, Eastern Europe, and Central Asia` = "
      `High-income`                                      = "
      `Latin America and Caribbean`                      = "
      `North Africa and Middle East`                     = "
      `South Asia`                                       = "
      `Southeast Asia, East Asia, and Oceania`           = "
      `Sub-Saharan Africa`                               = "
   )
   
   if(names_short){
      sr_palette <- setNames(
         sr_palette,
         c("CEECA", "HI", "LAC", "NAMENA", "SA", "SEAEO", "SSA")
      )
   }
   
   if(add_global){
      sr_palette <- c(`Global` = "black", sr_palette)
   }
   
   return(sr_palette)
}