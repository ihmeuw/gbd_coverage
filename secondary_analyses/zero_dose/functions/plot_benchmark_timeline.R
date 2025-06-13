plot_benchmark_timeline <- function(
        DT,
        year_id_plot_left,
        year_id_plot_right,
        params,
        milestone_y_adjust = 0,
        sr_label_varname,
        title,
        y_lab,
        add_benchmark_annotation = TRUE,
        label_year = 1980,
        label_justify = 1,
        label_nchar = 40
) {
    
    template <- list(
        geom_vline(xintercept = params$year_ids_milestones, color = "grey20", alpha = 0.4),
        geom_line(data = DT[super_region_name == "Global" & year_id <= 2023], linewidth = 1, linetype = "solid", alpha = 0.8, show.legend = TRUE, key_glyph = "path"),
        geom_line(data = DT[super_region_name != "Global" & year_id <= 2023], linewidth = 1, linetype = "solid", alpha = 0.8, show.legend = TRUE, key_glyph = "path"),
        
        
        geom_line(data = DT[super_region_name == "Global" & year_id >= 2023], linewidth = 2, linetype = "dotted", alpha = 0.5, show.legend = FALSE),
        geom_point(data = DT[super_region_name == "Global" & year_id == 2030], shape = 18, size = 5, alpha = 0.8, show.legend = FALSE),
        geom_line(data = DT[super_region_name != "Global" & year_id >= 2023], linewidth = 1, linetype = "dotted", alpha = 0.5, show.legend = FALSE),
        geom_point(data = DT[super_region_name != "Global" & year_id == 2030], shape = 18, size = 3, alpha = 0.8, show.legend = FALSE),
        scale_linetype_manual(values = c("solid", "dashed"), guide = "none"),
        theme_minimal(),
        scale_color_manual(
            values = aes_map_super_reg_long
            , labels = function(x) stringr::str_wrap(x, width = 30)
        ),
        scale_fill_manual(
          values = aes_map_super_reg_long
          , labels = function(x) stringr::str_wrap(x, width = 30)
        ),
        labs(
          x = "Year",
          y = y_lab,
          title = title,
          color = "Super Region",
          fill = "Super Region"
        ),
        
        scale_x_continuous(
            limits = c(year_id_plot_left, year_id_plot_right)
            , breaks = unique(c(
                seq(1980, year_id_plot_right, 5)
                
                
            ))
            , minor_breaks = NULL
        ), 
        theme(
            legend.position = "right"
            , axis.text.x = element_text(angle = 45, hjust = 1)
            , axis.ticks = element_blank()
        ) 
        
    )
    
    
    
    if(add_benchmark_annotation)
        template <- c(
            template,
            annotate(
                "text"
                , x = params$year_ids_milestones + 0.5
                , y = milestone_y_adjust
                , label = names(params$year_ids_milestones) 
                
                , hjust = -0.02
                , vjust = 1
                , angle = 90
                , color = "grey20"
                , alpha = 0.5
                , size = 4
            )
            
        )
    
    
    if(grepl("counts", y_lab, ignore.case = TRUE)){
        top_million <- as.integer(substr(max(DT$n_zd), 1,1)) + 1
        template <- c(
            template, 
            templ_y_scale <- scale_y_continuous(breaks = seq(0, max(DT$n_zd, top_million * 10^7), 1e7), labels = unit_format(unit = "M", scale = 1e-6))
        )
    }
    
    return(template)
}
