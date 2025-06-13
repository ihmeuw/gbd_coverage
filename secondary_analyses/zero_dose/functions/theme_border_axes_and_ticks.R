theme_border_axes_and_ticks <- function(){
   theme(
      
      panel.grid = element_blank()
      , panel.border = element_blank()
      , axis.line = element_line()
      
      , axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1)
      , axis.ticks.x.bottom = element_line()
      
      , axis.text.y = element_blank()
      , axis.ticks.y = element_blank()
      , axis.text.y.left = element_text()
      , axis.ticks.y.left = element_line()
      
      , axis.title.x = element_text(margin = margin(t = 10))
   )
}