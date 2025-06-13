#' @title Set intro

#' @description Zero out draws for locations before intro year

#' @param df A data.table containing vaccine coverage estimates for a specific vaccine.
#' @param me A character string specifying the vaccine of interest.

#' @return A data.table with introduction dates set.

#' @concept prep_draws_individual_functions
set_intro <- function(df, me) {

  vacc.intro <- data.table(readRDS(vacc_intro_for_outro_path))[me_name==me]
  vacc.intro <- vacc.intro[, .(location_id, year_id, cv_intro_years)]
  df <- merge(df, vacc.intro, by=c('location_id', 'year_id'), all.x=TRUE)
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[cv_intro_years < 1, (cols) := 0]
  df$cv_intro_years <- NULL
  return(df)

}
