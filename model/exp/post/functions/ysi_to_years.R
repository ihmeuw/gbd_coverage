

ysi_to_years <- function(vaccine,
                         me_outdir,
                         ndraws,
                         year_start,
                         year_end) {
  
  intros <- readRDS(vacc_intro_path)
  intros <- intros[me_name==vaccine]
  intros[, country := unlist(substr(ihme_loc_id, 1, 3))]
  intros[, n_intros := length(unique(cv_intro)), by = c("country", "me_name")]

  locs <- as.numeric(gsub('.csv', '', list.files(me_outdir)))
  locs <- locs[locs %in% unique(intros[n_intros == 1 | ihme_loc_id == country]$location_id)]   

  for(loc in locs){
    intro_year <- pmax(unique(intros[location_id==loc]$cv_intro), 1980)


    print(paste0('location id is equal to ',loc))

    draw_names <- paste0('draw_', 0:(ndraws - 1))

    df <- fread(paste0(me_outdir, '/', loc, '.csv'))

    
    df_zeros <- copy(df)
    df_zeros[,(draw_names) := 0]
    df_zeros <- df_zeros[year_id < intro_year]

    
    df[, ysi := year_id - year_start]
    df[, year_id := intro_year + ysi]
    df <- df[year_id <= year_end]
    df[, ysi := NULL]

    all <- rbind(df_zeros, df)

    fwrite(all, paste0(me_outdir, loc, '.csv'))

  }
}
