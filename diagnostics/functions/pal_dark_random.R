





#' @param n [int] how many colors to pick
#' @param seed [int] user-defined seed for reproducibility/variation

#' @return [chr] e.g. c("seagreen", "deeppink")
#' @export

#' @examples
pal_dark_random <- function(n, seed = 12345){
   set.seed(seed)
   
   lights <- 'gr(a|e)y|almond|[1-2]|aquamarine
   |gold|gainsboro|burlywood|alice|green$|misty|powder
   |white|seagreen|cream|wheat|lemon|yellow|khaki|ivory
   |bisque|moccasin|seashell|azure|lavender|blush|snow
   |beige|honeydew|linen|whip|light|puff|corn|pale|lace
   |^aquamarine$|^chartreuse$|^thistle'
   all_colors <- grDevices::colors()[grep(lights, grDevices::colors(), invert = TRUE)]
   if(n>length(all_colors)) stop("Not enough colors available - n must be <= ", length(all_colors))
   pal <- sample(all_colors, size = n)
   return(pal)
}