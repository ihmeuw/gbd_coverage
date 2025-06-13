#' @title Theme for ZD maps

#' @param plot_colors [chr] vector of color codes
#' @param plot_breaks [dbl] vector of breaks for `plot_color`
#' @param scale_name [chr] name of the scale for the legend colorbar

#' @return [ggplot list]
theme_zd_maps <- function(plot_colors, plot_breaks, scale_name = "Percentile"){
   list(
      theme_void()
      , guides(
         fill = guide_colorsteps(
            title.position = 'left'
            
            
            , barwidth = 1
            , barheight = 1 * length(plot_colors)
         )
      )
      , scale_fill_stepsn(
         colors = plot_colors,
         breaks = plot_breaks,
         limits = c(min(plot_breaks), max(plot_breaks)),        
         name = scale_name,
         na.value = "grey70"
      )
      , theme(
         panel.border             = element_rect(color = "black",fill = NA, linewidth = 1)
         , legend.position        = "inside"
         
         , legend.position.inside = c(0.15, 0.25)
         , legend.title           = element_text(angle = 90, hjust = 0.5)
         
      )
   )
}