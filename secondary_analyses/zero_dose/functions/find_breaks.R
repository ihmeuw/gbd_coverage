

#' @param range [num] The range of the breaks
#' @param by_x 

#' @return
#' @export

#' @examples
find_breaks <- function(range, by_x){
   stopifnot(length(range) == 2)
   stopifnot(length(by_x) == 1)
   return(
      seq(
         floor(range[1] / by_x) * by_x
         , ceiling(range[2] / by_x) * by_x
         , by_x
      )
   )
}