#'  @title Theme for ZD maps - manual fill

#' @param plot_colors [chr] vector of color codes
#' @param scale_name [chr] name of the scale for the legend colorbar

#' @return [ggplot list]
theme_zd_maps_manual_fill <- function(plot_colors, scale_name){
   
   list(
      theme_void()
      , guides(fill = guide_legend(
         title.position = 'left'
         , barwidth = 1
         , barheight = 1
         , reverse = TRUE
      ))
      , scale_fill_manual(
         name = scale_name
         , values = plot_colors
         , na.value = "grey70"
         , na.translate = FALSE
      )
      , theme(
         panel.border             = element_rect(color = "black",fill = NA, linewidth = 1)
         , legend.position        = "inside"
         , legend.position.inside = c(0.075, 0.25)
         , legend.title           = element_text(angle = 90, hjust = 0.5)
      )
   )
}