#' @title Set intro epi

#' @description This function applies delayed introduction dates for EPI vaccines to a data frame containing vaccine coverage estimates.

#' @param df A data.table containing vaccine coverage estimates for a specific vaccine.
#' @param me A character string specifying the vaccine of interest.

#' @return A data.table with introduction dates set.

#' @concept prep_draws_individual_functions
set_intro_epi <- function(df, me) {

  varnames_df_orig <- names(df)
  
  vacc.epi.intro <- data.table(readRDS(vacc_intro_path))[me_name==me]
  
  
  vacc.epi.intro.parents <-unique(vacc.epi.intro[location_id %in% unique(locs$parent_id) & cv_intro > 1979]$location_id)
  
  if(length(vacc.epi.intro.parents) > 0){
    
    for(p in vacc.epi.intro.parents){
      
      vacc.epi.intro.children <- vacc.epi.intro[(location_id %in% locs[parent_id==p]$location_id |
                                                   location_id %in% locs[parent_id %in% locs[parent_id==p]$location_id]$location_id) &
                                                  cv_intro < unique(vacc.epi.intro[location_id ==p]$cv_intro)]
      
      vacc.epi.intro          <- vacc.epi.intro[!(location_id %in% unique(vacc.epi.intro.children$location_id))]
      vacc.epi.intro.children[, cv_intro := unique(vacc.epi.intro[location_id == p]$cv_intro)]
      vacc.epi.intro.children[, cv_intro_years := pmax(0, year_id - (cv_intro - 1))]
      vacc.epi.intro          <- rbind(vacc.epi.intro, vacc.epi.intro.children)
    }
  }

  vacc.epi.intro <- vacc.epi.intro[, .(location_id, year_id, cv_intro_years)]
  
  
  varnames_merge    <- c("location_id", "year_id")
  varnames_draws    <- grep("draw_", names(df), value = TRUE)
  df                <- merge(df, vacc.epi.intro, by = varnames_merge, all.x=TRUE)
  df                <- df[cv_intro_years < 1, (varnames_draws) := 0]
  
  varnames_drop <- setdiff(names(df), varnames_df_orig)
  df[, c(varnames_drop) := NULL]
  
  
  assert_data_schema(df, varnames_strict = varnames_df_orig)
  
  return(df)
}
