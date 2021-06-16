

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                   Custom subnational GBD logit raking function                      #
#                                   8 June 2019                                       #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#' Modified from MBG calculate_raking_factors and apply_raking_factors functions
#' @param rake_targets List of raking targets with location and year
#' @param gbd_loc_id gbd location id that matches name in rake_targets
#' @param cell_pred draws from subnational location
#' @param nyears number of years in data
#' @param year_list list of years in data
#' @param population vector of population previously pulled using get_population function
#' @param year_list integer vector of years
#' @param save_vec default `NULL`. If null, uses names from subnational_rake_output. Otherwise takes a character vector with the names of objects in subnational_rake_output to save.
#' @param save_draws_as character string default `RDS`. additional supported arguments `rds` and `Rdata`. The file type to save raked draws. If using `RDS` or `rds`, the file extension will be .RDS or .rds accordingly.
#'
#' @return raked "cell_pred"

gbd_raking <- function(rake_targets, gbd_loc_id, cell_pred, nyears, year_list, population){

  lyv = c('name','year', 'value')
  rake_targets = copy(rake_targets[, lyv, with = F])
  setnames(rake_targets, lyv, c('loc', 'year', 'target'))


  sr_locs <- c(gbd_loc_id)
  cys = setDT(expand.grid(loc = sr_locs, year = year_list))
  start_row = nrow(cys)
  cys = merge(cys, rake_targets, by= c('loc','year'), all.x= T)



  #first format the weight_brick
  raker = data.table(loc_means = rowMeans(cell_pred),
                     loc = rep.int(gbd_loc_id, nyears),
                     year = as.vector(outer(Y = year_list, X = rep.int(1, 1))),
                     weight = population)

  raker[,cell_pred_id := .I]

  #specify by_vars
  byvars = c('loc', 'year')

  #remove NA weight values from raker
  pre = dim(raker)[1]
  post = dim(raker)[1]

  if(pre != post){
    warning(paste0(pre - post, ' (', scales::percent((pre - post)/pre), ') pixels (over the whole cube) were removed because of NA weight_brick or pixel values'))
  }

  #for each country year, find the logit raking factor
  #redefine cys
  cys = unique(raker[,.(loc, year)])
  rf = lapply(seq(nrow(cys)), function(x) {

    #year and location
    theloc = cys[x,loc]
    theyear = cys[x,year]

    message("############## ", theloc, " in ", theyear)

    if (nrow(rake_targets[loc == theloc & year == theyear]) == 0) {
      if(if_no_gbd == "return_na"){
        return(7)
      } else {
        return(0)
      }
    } else if (rake_targets[loc == theloc & year == theyear, .(target)] == 0 & zero_heuristic == T) {
      # catch true zeroes (i.e. pre-introduction of an intervention) and return -9999. This will speed things up & will replace later with 0
      return(-9999)
    } else {
      ret <- try(LogitFindK(gbdval     = rake_targets[loc == theloc & year == theyear,.(target)],
                            pixelval   = cell_pred[raker[loc == theloc & year == theyear, cell_pred_id],], #pass the cell pred rows that corrospond to this country year
                            weightval  = raker[loc == theloc & year == theyear, weight],
                            MaxJump    = MaxJump,
                            MaxIter    = MaxIter,
                            FunTol     = FunTol,
                            approx_0_1 = approx_0_1))
      return(ret)
    }
  })

  cys[,raking_factor := unlist(rf)]
  cys = merge(cys, rake_targets, all.x = T, by =c('loc', 'year'))

  #calculate the starting point post hoc for standardized reporting
  rak = raker[, list(px_mean = sum(loc_means * weight, na.rm = T), sumweight = sum(weight, na.rm = T)), by = byvars]
  rak[, start_point := px_mean/sumweight]

  rak = merge(rak, cys, all.x = T, by = c('loc','year'))

  #raking factors at the cys level
  rak = rak[, .(loc, year, start_point, target, raking_factor)]
  rake_dt <- rak


  ############## now apply raking factors
  cpdim = dim(cell_pred)
  thelocs = unique(level5_extra$location_id)
  thelocs = thelocs[!is.na(thelocs)]

  #create comparison to cell pred to fill in the raking factors
  dt = rbindlist(lapply(unique(rake_dt[,year]), function(x) data.table(loc = thelocs,
                                                                       year = x)))
  dt[, id:= .I]

  #merge on the raking factors
  rake_dt = merge(dt, rake_dt, all.x = T, by = c('year'))
  setorder(rake_dt, id)
  rake_dt <- rake_dt[order(loc.x),]

  ## rake!
  raked_cell_pred = invlogit(logit(cell_pred) + rake_dt[,raking_factor])

  return(raked_cell_pred)

}


gbd_raking_level4 <- function(rake_targets, gbd_loc_id, cell_pred, nyears, year_list, population){

  lyv = c('name','year', 'value')
  rake_targets = copy(rake_targets[, lyv, with = F])
  setnames(rake_targets, lyv, c('loc', 'year', 'target'))


  sr_locs <- c(gbd_loc_id)
  cys = setDT(expand.grid(loc = sr_locs, year = year_list))
  start_row = nrow(cys)
  cys = merge(cys, rake_targets, by= c('loc','year'), all.x= T)



  #first format the weight_brick
  raker = data.table(loc_means = rowMeans(cell_pred),
                     loc = rep.int(gbd_loc_id, nyears),
                     year = as.vector(outer(Y = year_list, X = rep.int(1, 1))),
                     weight = population)

  raker[,cell_pred_id := .I]

  #specify by_vars
  byvars = c('loc', 'year')

  #remove NA weight values from raker
  pre = dim(raker)[1]
  post = dim(raker)[1]

  if(pre != post){
    warning(paste0(pre - post, ' (', scales::percent((pre - post)/pre), ') pixels (over the whole cube) were removed because of NA weight_brick or pixel values'))
  }

  #for each country year, find the logit raking factor
  #redefine cys
  cys = unique(raker[,.(loc, year)])
  rf = lapply(seq(nrow(cys)), function(x) {

    #year and location
    theloc = cys[x,loc]
    theyear = cys[x,year]

    message("############## ", theloc, " in ", theyear)

    if (nrow(rake_targets[loc == theloc & year == theyear]) == 0) {
      if(if_no_gbd == "return_na"){
        return(NA)
      } else {
        return(0)
      }
    } else if (rake_targets[loc == theloc & year == theyear, .(target)] == 0 & zero_heuristic == T) {
      # catch true zeroes (i.e. pre-introduction of an intervention) and return -9999. This will speed things up & will replace later with 0
      return(-9999)
    } else {
      ret <- try(LogitFindK(gbdval     = rake_targets[loc == theloc & year == theyear,.(target)],
                            pixelval   = cell_pred[raker[loc == theloc & year == theyear, cell_pred_id],], #pass the cell pred rows that corrospond to this country year
                            weightval  = raker[loc == theloc & year == theyear, weight],
                            MaxJump    = MaxJump,
                            MaxIter    = MaxIter,
                            FunTol     = FunTol,
                            approx_0_1 = approx_0_1))
      return(ret)
    }
  })

  cys[,raking_factor := unlist(rf)]
  cys = merge(cys, rake_targets, all.x = T, by =c('loc', 'year'))

  #calculate the starting point post hoc for standardized reporting
  rak = raker[, list(px_mean = sum(loc_means * weight, na.rm = T), sumweight = sum(weight, na.rm = T)), by = byvars]
  rak[, start_point := px_mean/sumweight]

  rak = merge(rak, cys, all.x = T, by = c('loc','year'))

  #raking factors at the cys level
  rak = rak[, .(loc, year, start_point, target, raking_factor)]
  rake_dt <- rak


  ############## now apply raking factors
  cpdim = dim(cell_pred)
  thelocs = unique(level4_extra$location_id)
  thelocs = thelocs[!is.na(thelocs)]

  #create comparison to cell pred to fill in the raking factors
  dt = rbindlist(lapply(unique(rake_dt[,year]), function(x) data.table(loc = thelocs,
                                                                       year = x)))
  dt[, id:= .I]

  #merge on the raking factors
  rake_dt = merge(dt, rake_dt, all.x = T, by = c('year'))
  setorder(rake_dt, id)
  rake_dt <- rake_dt[order(loc.x),]

  ## rake!
  raked_cell_pred = invlogit(logit(cell_pred) + rake_dt[,raking_factor])

  return(raked_cell_pred)

}

