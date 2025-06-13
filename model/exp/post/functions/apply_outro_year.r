#' @title Apply outro year

#' @description Apply BCG outro_year force to 0 at the draw level (currently not transferrable to ratio vax until update how cv_outro + cv_intro_years cols are made together)

#' @param df A data.table with columns 'location_id', 'year_id', 'draw_*'
#' @param me The vaccine name

#' @return A data.table with BCG vaccine outro year set to 0 at the draw level

#' @concept prep_draws_individual_functions
apply_outro_year <- function(df, me) {

  vacc.intro <- data.table(readRDS(vacc_intro_for_outro_path))[me_name==me]
  vacc.outro <- vacc.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.outro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[cv_intro_years == 0, (cols) := 0]
  df$cv_intro_years <- NULL
  return(df)

}
