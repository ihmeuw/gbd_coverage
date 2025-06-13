plot_sr_barstack_base <- function(
      DT
      , barchart_year_start
      , barchart_year_end
      , aes_map_super_reg_long
      , y_lab
      , title
      , top_million
){
   
   y_breaks <- seq(0, max(DT$n_zd, top_million * 10 ^ 7), 1e7)
   return(
      list(
         geom_bar(aes(x = year_id, y = n_zd, fill = super_region_name), position = "stack", stat = "identity", show.legend = FALSE, width = 0.75),
         scale_fill_manual(values = aes_map_super_reg_long),
         scale_y_continuous(
            breaks = y_breaks
            , limits = c(0, max(y_breaks))
            , labels = unit_format(unit = "M", scale = 1e-6)
            , expand = c(0, 0)
         ), 
         scale_x_continuous(
            limits = c(barchart_year_start, barchart_year_end)
            , breaks = unique(c(seq(barchart_year_start + 1, barchart_year_end - 1, 5)))
            , minor_breaks = NULL
            , expand = c(0, 0)
         ),
         labs(
            x = "Year",
            , y = y_lab
            , title = title
            
         ),
         theme_minimal(),
         theme(
            
            axis.text.x = element_text(angle = 45, hjust = 1)
            , axis.ticks = element_blank()
            , panel.border = element_blank()
            , axis.line = element_line()
         )
         
      )
   )
}
