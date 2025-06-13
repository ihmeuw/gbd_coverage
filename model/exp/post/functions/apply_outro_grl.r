
#' @title Apply outro year for GRL

#' @description Apply custom BCG outro and re-intro year for GRL

#' @param df A data.table with columns 'location_id', 'year_id', 'draw_*'
#' @param me The vaccine name

#' @return A data.table with BCG vaccine outro year set to 0 at specific locations and years

#' @concept prep_draws_individual_functions
apply_outro_grl <- function(df, me) {

  vacc.intro <- data.table(readRDS(vacc_intro_for_outro_path))[me_name == me]
  vacc.outro <- vacc.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.outro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df[location_id==349 & year_id %in% c(1991:1995), (cols) := 0]  
  df$cv_intro_years <- NULL
  return(df)

}
