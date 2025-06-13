plot_disputed_outline <- function(disputed_outline){
   return(
      list(
         geom_path(data = disputed_outline, aes(x = long, y = lat, group = group), color = 'white', linewidth = 0.1)
         , geom_path(data = disputed_outline, aes(x = long, y = lat, group = group), color = 'black',  linewidth= 0.1, linetype = 2)
      )
   )
}