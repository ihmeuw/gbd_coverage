#----HEADER-------------------------------------------------------------------------------------------------------------
# Author:  USERNAME
# Purpose: Tabulate unit record data extracted with UbCov; saves dataset by NID
#               GBD - Collapse by GBD location ids, save in 'FILEPATH/gbd'
# Run:     source("FILEPATH/tabulate_gbd.r")
#***********************************************************************************************************************


#----CORE FUNCTIONS-----------------------------------------------------------------------------------------------------

### cleanup path
path.clean <- function(path) {
  
  os <- .Platform$OS.type
  if (os == "windows") {
    j <- "J:/"
    h <- "H:/"
    path <- gsub("FILEPATH/", j, path)
    path <- gsub(paste0("FILEPATH/"), h, path)
  } else {
    j <- "FILEPATH/"
    h <- paste0("FILEPATH/")
    path <- gsub("J:/", j, path)
    path <- gsub("H:/", h, path)
  }
  return(path)
  
}

### load in file
load.file <- function(path) {
  
  path <- path.clean(path)
  ext <- strsplit(basename(path), "[.]")[[1]][2] %>% tolower
  if (ext=="csv") {
    df <- fread(path)
  } else if (ext %in% c("dta", "tmp")) {
    df <- try(read_dta(path))
    if ('try-error' %in% class(df)) df <- readstata13(path)
  } else stop("Path not accepted")
  return(data.table(df))
  
}

### load config settings
load.settings <- function(path, topic=NA) {
  
  df <- load.file(path)
  # if topic provided, subset
  if (!is.na(topic)) {
    tsub <- topic
    df <- df[topic==tsub]
    if (nrow(df) == 0 | nrow(df) > 1) stop("BREAK || ", nrow(df), " rows in config file with topic (", topic, ")")
  }
  settings <- lapply(names(df), function(n) {
    i <- df[[n]]
    if (n=="custom_age_cuts") {
      if (!is.na(i) & !is.null(i) & i != "") i <- unlist(strsplit(gsub(" ", "", i), ",")) %>% as.numeric
    } else if (grepl("root", n)) {
      if (grepl("J:/", i)) i <- gsub("J:/", j, i)
    } else if (n=="cond_age_cuts") {
      if (!is.na(i) & !is.null(i) & i != "") i <- eval(parse(text=gsub("\"\"", "\"", i)))
    } else if (grepl(",", i)) {
      if (!is.na(i) & !is.null(i) & i != "") i <- unlist(strsplit(gsub(" ", "", i), ","))
    } else if (length(i) == 1L) {
      if (!is.na(i) & !is.null(i) & i != ""){
        if (i=="NULL") i <- NULL
      }
    }
    return(i)
  })
  names(settings) <- names(df)
  return(settings)
  
}

### remove extension
no.ext <- function(path) {
  strsplit(basename(path), "[.]")[[1]][1]
}

### create survey design object
setup.design <- function(df, var) {
  
  ### conservative adjustment recommended for single-PSU strata.  Centers the data for the single-PSU stratum 
  ### around the sample grand mean rather than the stratum mean
  options(survey.lonely.psu='adjust')
  
  ### conservative adjustment recommended for single-PSU within subpopulations
  options(survey.adjust.domain.lonely=TRUE)
  
  ### check for survey design vars
  check_list <- c("strata", "psu", "pweight")
  for (i in check_list) {
    # assign to *_formula the variable if it exists and nonmissing, else NULL
    assign(paste0(i, "_formula"),
           ifelse(i %in% names(df) & nrow(df[!is.na(i)]) > 0, paste("~", i), NULL) %>% as.formula
    )
  }
  
  ### set svydesign
  return(svydesign(id=psu_formula, weight=pweight_formula, strat=strata_formula, data=df[!is.na(var)], nest=TRUE))
  
}

### check for cases where subnational weights present
### if present, set pweight equal to those weights for collapse
reweight_for_subnat_collapse <- function(df, by_vars) {
  
  weights_below_admin0 <- grep("pweight_admin_.", names(df), value=TRUE)
  cols_na <- sapply(df, function(x)all(is.na(x)))
  na_cols <- names(df)[cols_na]
  non_na_admin_wts <- weights_below_admin0[! weights_below_admin0 %in% na_cols]
  
  if (length(non_na_admin_wts) > 0){
    # regex the column whose weights represent that level
    admin_ids <- grep(by_vars, pattern='admin_.*id', value=TRUE)
    admins_w_subnat_info <- substring(admin_ids, 1, 7)
    correct_pweight_for_collapse <- paste0('pweight_', admins_w_subnat_info)
    # set pweight to that weight
    # produce subnationally weighted estimates when data allows
    if (correct_pweight_for_collapse != 'pweight_') {
      df[, pweight := get(correct_pweight_for_collapse)]
    }
  }
  
  return(df)
  
}

### core function to collapse dataframe
collapse.by <- function(df, var, by_vars, calc.sd = FALSE) {
  
  ### subset dataframe to where not missing variable or survey design variables
  df.c <- df[!is.na(get(var)) & !is.na(strata) & !is.na(psu) & !is.na(pweight)] %>% copy
  df <- reweight_for_subnat_collapse(df, by_vars)
  
  ### setup design for national aggregates and cases where subnational estimates use national weights
  design <- setup.design(df, var)
  
  ### setup by the by call as a formula
  by_formula <- as.formula(paste0("~", paste(by_vars, collapse="+")))
  
  ### calculate number of observations, number of clusters, strata
  meta <- df[, list(sample_size = length(which(!is.na(get(var)))),
                    nclust      = length(unique(psu)),
                    nstrata     = length(unique(strata)),
                    var         = var
                 ), by          = by_vars]
  
  ### calculate mean and standard error by by_vars.  Design effect is dependent on the scaling of the sampling weights
  est <- svyby(~get(var), by_formula, svymean, design=design, deff="replace", na.rm=TRUE, drop.empty.groups=TRUE, keep.names=FALSE, multicore=TRUE) %>% data.table
  old <- c("get(var)", "DEff.get(var)", "se")
  new <- c("mean", "design_effect", "standard_error")
  setnames(est, old, new)
  
  ### merge
  out <- merge(meta, est, by=by_vars)
  
  ### calculate standard deviation by by_vars(s), Design effect is dependent on the scaling of the sampling weights
  if (calc.sd) {
    stdev <- svyby(~get(var), by_formula, svyvar, design=design, deff="replace", na.rm=TRUE, drop.empty.groups=TRUE, keep.names=FALSE, multicore=TRUE) %>% data.table
    setnames(stdev, "get(var)", "variance")
    stdev <- stdev[, standard_deviation := sqrt(variance)]
    stdev <- stdev[, standard_deviation_se := 1 / (2 * standard_deviation) * se]
    stdev <- stdev[, c("se", "variance") := NULL]
    # merge
    out <- merge(out, stdev, by=by_vars)
  }
  
  # return
  return(out)
  
}

### detect columns that are present and not entirely na
nonmiss <- function(df, vars, reverse=FALSE) {
  
  vars.sub <- intersect(vars, names(df))
  i <- lapply(vars.sub, function(x) !all(is.na(df[[x]]))) %>% unlist
  if (reverse) return(setdiff(vars, vars.sub[i]))
  else return(vars.sub[i])
  
}

### expand categorical variables to binary
categ_to_bin <- function(df, var) {
  
  vals <- df[!is.na(get(var))][[var]] %>% unique
  for (x in vals) {
    col <- paste0(var, "_", x)
    df <- df[!is.na(get(var)), (col) := ifelse(get(var) == x, 1, 0)]
  }
  newcols <- paste0(var, "_", vals)
  
  # return
  return(list(df=df, cols=newcols))
  
}

### cut ages and create age_start age_end with a text conditional
cut.ages <- function(df, var, cut, subset=NA) {
  
  if (is.na(subset)) subset.cmd <- "1==1"
  index <- findInterval(df[eval(parse(text=subset))][[var]], cut, right=FALSE)
  df <- df[eval(parse(text=subset)), `:=` (age_start =  cut[index], age_end = cut[index+1])]
  return(df)
  
}

update_granular_pweights <- function(pweight_colname, df) {
  
  ntl_pweight_not_na <- any(!is.na(df$pweight))
  sub_pweight_na <- any(is.na(df[, get(pweight_colname)]))
  if (ntl_pweight_not_na & sub_pweight_na){
    df <- df[, (pweight_colname) := pweight]
  }
  return(df)
  
}

### subset collapse frame to rows that aren't var, stratification vars (by_vars), design vars
### additionally, remove groupings with 1 person
collapse.subset <- function(df, var, by_vars, design_vars) {
  
  # subset to rows not missing variable, stratify, design
  cols <- c(var, by_vars, design_vars)
  n.before <- nrow(df)
  for (i in cols){
    if (grepl(x=i, pattern='pweight_')){
      # set NA subnational pweights to national values
      df <- update_granular_pweights(pweight_colname=i, df)
    }
    df <- df[!is.na(get(i))]
  }
  n.dropped <- n.before - nrow(df)
  
  # drop rows in groupings with only 1 row 
  df <- df[, lonely := lapply(.SD, length), .SDcols=var, by=by_vars]
  df <- df[lonely != 1]
  df <- df[, lonely := NULL]
  
  # return
  return(list(data=df, dropped=n.dropped))
  
}

### replace ihme_loc_id with admin_*_id if collapsing on the location
clean.subnat <- function(df, vars.subnat) {
  
  # if subnational, rename geo vars
  rename <- intersect(vars.subnat, names(df))
  if (length(rename) > 0) {
    setnames(df, rename, rep("ihme_loc_id", length(rename)))
    if ("ihme_loc_id" %in% names(df)) df <- df[, ihme_loc_id := NULL]
  }
  return(df)
  
}
#***********************************************************************************************************************


#----MAIN FUNCTION------------------------------------------------------------------------------------------------------
collapse.run <- function(df, config, quiet=TRUE, cores=1) {
  
  ### list out default settings
  config.default <- list(
    
    # collapse vars
    vars             = c("var"),     		## Variables to collapse options
    vars.categ       = NULL,            ## Categorical vars to split out into binary
    calc.sd          = FALSE,           ## Whether to calculate standard deviation
    # collapse over
    cv.manual        = NULL,            ## List of other variables to collapse by
    cv.detect        = TRUE,            ## if TRUE, detects columns with cv_* to collapse by
    # demographics
    by_sex           = TRUE,            ## if TRUE, collapses by vars.sex
    by_age           = TRUE,            ## if TRUE, collapses by vars.age
    gbd_age_cuts     = TRUE,            ## if TRUE, uses default GBD age cuts
    aggregate_under1 = TRUE,            ## if TRUE, aggregates < 1
    custom_age_cuts  = NULL,            ## List of custom age cuts
    cond_age_cuts    = NULL,            ## List of conditional custom age cuts 
    # settings
    sample_threshold = NA,              ## Minimum sample size threshold, drops result if sample_size < sample_threshold
    # meta vars
    vars.meta        = c(               ## Default meta variables
      "nid", "survey_name", 			
      "ihme_loc_id", "year_start",
      "year_end", "survey_module",
      "file_path"
    ),
    # subnational vars
    vars.subnat      = c(               ## Default subnational vars
      "admin_1_id", 
      "admin_1_urban_id", 
      "admin_2_id",
      "admin_3_id",
      "admin_4_id",
      "admin_5_id"),  
    # sex variable
    vars.sex         = c("sex_id"),			## Default sex variable
    # age variable
    vars.age         = c("age_year"),	  ## Default age variable
    # design vars
    # if TRUE, use weighted mean collapse; otherwise, use typical survey design collapse
    census_data      = FALSE, 
    # survey 
    vars.design      = c(               ## Default survey design variables
      "strata", "psu", "pweight", "pweight_admin_1", "pweight_admin_2", "pweight_admin_3"
    )
  )
  
  ### set settings that were passed through
  if (!is.null(config)) lapply(names(config), function(x) assign(x, config[[x]], pos=1))
  
  ### set default settings that haven't been passed through call
  lapply(names(config.default), function(x) if (!exists(x)) assign(x, config.default[[x]], pos=1))
  
  ### prep dataset
  ### set collapse vars
  ### split up categorical vars into binary
  if (!is.null(vars.categ)) if (!is.na(vars.categ) & vars.categ != "") {
    for (var in vars.categ) {
      ctb <- categ_to_bin(df, var)
      df <- ctb[['df']]
      new.vars <- ctb[['cols']]
      vars <- c(vars, new.vars)
    }
  }
  ### set vars to collapse on (nonmissing vars)
  vars.check <- nonmiss(df, vars)
  if (length(vars.check) < 1L) stop(paste0("BREAK || All vars (", toString(vars), ") are either not in dataset or completely missing"))
  vars <- vars.check
  
  ### default start with vars.meta
  vars.stratify <- vars.meta
  if (by_sex) vars.stratify <- c(vars.stratify, vars.sex)
  if (by_age) vars.stratify <- c(vars.stratify, "age_start", "age_end")
  # detect nonmissing cv_* columns if requested
  if (cv.detect) {
    vars.cv <- grep("^cv_", names(df), value=TRUE)
    if (length(vars.cv) > 0) {
      vars.cv <- nonmiss(df, vars.cv)
      vars.stratify <- c(vars.stratify, vars.cv)
    }
  }
  
  ### insert manually entered covariates to collapse over
  if (!is.null(cv.manual)) {
    vars.cv <- nonmiss(df, cv.manual)
    vars.stratify <- c(vars.stratify, vars.cv)
  }
  
  ### detect nonmissing admin_*_ids, for each location set create new list of vars to collapse by
  vars.subnat <- intersect(vars.subnat, names(df))
  vars.subnat <- nonmiss(df, vars.subnat)
  if (length(vars.subnat) != 0L) {
    vars.stratify <- lapply(c("ihme_loc_id", vars.subnat), function(x) c(vars.stratify, x) %>% unique)
  } else {
    vars.stratify <- list(vars.stratify)
  }
  # detect garbage code admin_*_ids to drop duplicate data
  for (var.subnat in vars.subnat) {
    df[get(var.subnat)==get("ihme_loc_id"), (var.subnat) := NA_character_]
  }
  
  ### error checks
  ### insert missing design variables
  ### subnational pweights are set to NA later because they trigger additional
  ### collapses if present
  vars.design.missing <- nonmiss(df, vars.design, reverse=TRUE)
  subnat_pweights <- c("pweight_admin_1", "pweight_admin_2", "pweight_admin_3")
  if (length(vars.design.missing) != 0L) {
    df <- df[, (vars.design.missing) := 1]
    absent_subnat_pweights <- subnat_pweights[subnat_pweights %in% vars.design.missing]
    df <- df[, (absent_subnat_pweights) := NA]
  }
  
  ### ensure design vars are numeric
  for (var in vars.design) {
    if (class(df[[var]]) != "numeric") {
      df[[var]] <- as.numeric(df[[var]])
      if (!quiet) print(paste0("ubCov Collapse || Setup || Coercing ", var, " to numeric"))
    }
  }
  
  ### drop if pweight == 0
  n <- nrow(df[pweight==0])
  if (n > 0) {
    df <- df[pweight != 0]
    if (!quiet) print(paste0("ubCov Collapse || Setup || Dropping ", n, " rows with pweight == 0"))
  }
  
  ### set errors
  errors <- list(missing_design=vars.design.missing)
  
  ### collapse
  ### make frame of collapse
  cf.list <- expand.grid(vars=vars, vars.stratify=vars.stratify)
  
  ### collapse
  cf <- mclapply(1:nrow(cf.list), function(i) {
    vars <- cf.list$vars[i] %>% as.character
    vars.stratify <- cf.list$vars.stratify[i] %>% unlist
    # message
    if (!quiet) print(paste0("ubCov Collapse || Collapsing (", vars, ") by (", toString(vars.stratify), ")"))
    # subset dataset to frame with var, by_vars, and design_vars
    sf <- collapse.subset(df, var=vars, by_vars=vars.stratify, design_vars=vars.design)
    missing <- sf[['missing']]
    sf <- sf[['data']]
    # only proceed if any data post subset
    if (nrow(sf) > 0) {
      # collapse dataset
      if (census_data) sf.out <- census.collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      if (!census_data) sf.out <- collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      # clean subnationals if necessary to map to ihme_loc_id
      sf.out <- clean.subnat(sf.out, vars.subnat)
      sf.out <- rbind(sf.out, missing=missing)
      return(sf.out)
    } else {
      if (!quiet) print(paste0("ubCov Collapse || SKIPPING || Variable (", vars, ") has no observations post subset with (", toString(vars.stratify), ")"))
      return(NULL)
    }
  }, mc.cores=cores) %>% rbindlist(., use.names=TRUE, fill=TRUE)
  
  ### adjustments
  ### if sample_threshold, drop groups below threshold
  if (!is.na(sample_threshold)) cf <- cf[sample_size >= sample_threshold]
  
  ### if mean==0/1 use Wilson Interval Method 
  if (nrow(cf[mean %in% c(0,1)]) > 0) {
    cf.w <- cf[mean %in% c(0,1)]
    sample_size <- cf.w$sample_size
    n <- ifelse(cf.w$mean==0, 0, sample_size)
    ci <- binom.confint(n, sample_size, conf.level = 0.95, methods = "wilson")
    se <- (ci$upper - ci$lower)/3.92
    se <- se * sqrt(2.25) 
    cf[mean %in% c(0,1)]$standard_error <- se
  }
  
  ### census data does not use traditional survey design and does not have a design effect
  if ("strata" %in% vars.design.missing & !census_data) {
    cf <- cf[design_effect < 2.25, inflate := 2.25/design_effect]
    cf <- cf[design_effect < 2.25, standard_error := standard_error * sqrt(inflate)]
    cf <- cf[, inflate := NULL]
    cf <- cf[design_effect < 2.25, design_effect := 2.25]
  }
  
  # return
  return(cf)
  
}

#***********************************************************************************************************************


#----START TABULATION FUNCTION------------------------------------------------------------------------------------------

### open function
tabulate_gbd <- function(nid) {
  
#***********************************************************************************************************************

  
#----COLLAPSE-----------------------------------------------------------------------------------------------------------
  
  ### settings
  topic       <- "vaccines"
  config.path <- "FILEPATH/collapse_config.csv"
  parallel    <- FALSE
  
  ### prep for tabulation
  # check for completeness, make sure all needed columns exist, and at least one vaccine column; reshape long
  dataset     <- prep_for_tabulation(nid, team="gbd", vaccines.=c(vaccines, 
                                                                  "rotac",
                                                                  "dpt3_timeliness_ratio",
                                                                  paste(indicators, collapse="_"),
                                                                  paste(indicators_no_mcv2, collapse="_"),
                                                                  paste(three_indicators, collapse="_"),
                                                                  paste(four_indicators_1, collapse="_"),
                                                                  paste(four_indicators_2, collapse="_"),
                                                                  paste(four_indicators_3, collapse="_"),
                                                                  paste(four_indicators_4, collapse="_"),
                                                                  paste0("correlation_", probabilities_to_model),
                                                                  paste0(c(vaccines, "rotac"), "_CARD"),
                                                                  paste0(c(vaccines, "rotac"), "_RECALL")
                                                                  ) )
  
  if (dataset[[1]] != FALSE) {
    
    ### launch collapse
    dataset  <- dataset[[2]] %>% as.data.table
    config   <- load.settings(config.path, topic)
    tab_data <- collapse.run(dataset, config=config)
    
    ### save
    tab_data[, var := NULL]
    write.csv(tab_data, file.path(extraction_root, "tabulated/gbd", paste0(nid, ".csv")), row.names=FALSE)
    message(paste0("Tabulated data for NID ", nid, " saved"))
    
  } else message("Tabulation prep failed (missing required columns)")
  
#***********************************************************************************************************************


#----END FUNCTION-------------------------------------------------------------------------------------------------------

### end of function
}
#***********************************************************************************************************************