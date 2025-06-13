library(ggplot2)
library(sf)
library(Cairo)
library(data.table)


dt_data <- fread("FILEPATH/me_name_nid_counts.csv")
setnames(dt_data, "Country-Years Count", "country_years")


shp_adm0_0 <- sf::read_sf(dsn = file.path("FILEPATH", 'lbd_standard_admin_0.shp'))
setnames(shp_adm0_0, "loc_id", "location_id")


countries_to_remove <- c("Antarctica")
shp_adm0_0 <- shp_adm0_0[!shp_adm0_0$ADM0_NAME %in% countries_to_remove, ]


dt_sf_plot <- merge(shp_adm0_0, dt_data, by = "location_id", all.x = TRUE)

dt_sf_plot$country_years <- as.numeric(dt_sf_plot$country_years)


summary(dt_sf_plot$country_years)


dt_sf_plot$color_group <- cut(
  dt_sf_plot$country_years,
  breaks = c(0, 1, 2, 5, 9, Inf),  
  labels = c("1", "2", "3-5", "6-9", "10+"),  
  include.lowest = TRUE
)


p <- ggplot() +
  geom_sf(
    data = dt_sf_plot,
    aes(fill = color_group),  
    color = 'black',  
    linewidth = 0.1  
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#FFFF00",       # Yellow
      "2" = "#0BDA51",       # Green
      "3-5" = "#40B5AD",     # Dark Cyan
      "6-9" = "#6F8FAF",     # Dark Blue
      "10+" = "#301934"      # Dark Purple
    ),
    na.value = "grey70",     # Color for missing values
    name = "Country Years"   # Legend title
  ) +
  
  geom_sf(data = shp_adm0_0[shp_adm0_0$ADM0_NAME %in% countries_to_remove,], 
          fill = 'gray70', color = 'black', linewidth = 0.1) +
  
  geom_sf(data = dt_sf_plot[is.na(dt_sf_plot$country_years),], 
          fill = 'grey70', color = 'black', linewidth = 0.1) +
  
  geom_sf(data = shp_adm0_0, fill = NA, color = 'black', linewidth = 0.1) +
  theme_void() +
  theme(
    panel.border = element_blank(),  
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(hjust = 0.5),  
    legend.text = element_text(size = 10)  
  ) +
  labs(
    title = "IPV Administrative Data Availability",
  )

CairoPNG(
  file.path("FILEPATH", "country_years_map_custom_colors.png"),
  width = 11, height = 4, res = 400, units = 'in'
)
print(p)
dev.off()


gc()
