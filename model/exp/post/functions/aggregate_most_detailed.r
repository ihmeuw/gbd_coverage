#' @title Aggregate most detailed

#' @description discard all but most detailed location, aggregate up the location hierarchy


#' @param df A data frame containing the estimates to be aggregated.
#' @param varname A character string specifying the name of the column containing the estimates.

#' @return A data frame with the aggregated estimates.

#' @concept prep_draws_custom_functions
aggregate_most_detailed <- function(df, varname="est") {

  
  df_all <- merge(df[, c("location_id", "year_id", "age_group_id", "sex_id", "draw", varname), with=FALSE],
                  locations[, c("location_id", "most_detailed"), with=FALSE],
                  by="location_id", all.x=TRUE)

  
  population <- get_population(
    age_group_id    = unique(df_all$age_group_id),
    sex_id          = unique(df_all$sex_id),
    location_id     = unique(df_all$location_id),
    year_id         = unique(df_all$year_id),
    release_id      = release_id,
    location_set_id = location_set_id,
    forecasted_pop  = fhs_run_TF
  )

  
  by_vars  <- c("year_id", "age_group_id", "sex_id", "draw", "parent_id")
  loc_cols <- c("parent_id", "level")

  
  df_most_detailed <- df_all[most_detailed==1, c("location_id", by_vars[!by_vars %in% "parent_id"], varname), with=FALSE]
  for (location_level in c(6, 5, 4)) {

    
    df_agg <- merge(df_most_detailed, locations[, c("location_id", loc_cols[!loc_cols %in% colnames(df_most_detailed)]), with=FALSE], by="location_id", all.x=TRUE)
    df_agg <- df_agg[level==location_level, c("location_id", by_vars, varname), with=FALSE]
    df_agg <- merge(df_agg, population[, .(location_id, year_id, age_group_id, sex_id, population)], by=c("location_id", "year_id", "age_group_id", "sex_id"), all.x=TRUE)

    
    df_agg[, sum_pop := sum(population), by=by_vars]
    df_agg[, mu      := get(varname) * (population / sum_pop)]
    df_agg[, agg     := sum(mu), by=by_vars]
    df_aggregated <- unique(df_agg[, c(by_vars, "agg"), with=FALSE])

    
    setnames(df_aggregated, c("parent_id", "agg"), c("location_id", varname))
    df_most_detailed <- rbind(df_most_detailed, df_aggregated, fill=TRUE)

  }

  return (df_most_detailed)

}
