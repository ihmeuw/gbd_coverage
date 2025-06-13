





get_location_hierarchy <- function(location_set_id = 22, release_id, standard_location_set_id = 101) {
   
   
   df <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)
   
   std_locs <- get_location_metadata(location_set_id = standard_location_set_id, release_id = release_id)[, location_id]
   
   
   hierarchy <- str_split_fixed(df$path_to_top_parent, ",", max(df$level) + 1) %>% data.table
   hierarchy <- hierarchy[, lapply(.SD, as.numeric)]
   setnames(hierarchy, names(hierarchy), paste0("level_", seq(0, max(df$level))))
   df <- cbind(df, hierarchy)
   
   
   df[, standard_location := as.integer(location_id %in% std_locs)]
   
   return(df[])
}