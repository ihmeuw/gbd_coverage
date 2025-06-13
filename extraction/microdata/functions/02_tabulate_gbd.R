



















path.clean <- function(path) {

  os <- .Platform$OS.type
  if (os == "windows") {
    j <- "J:/"
    h <- "H:/"
    path <- gsub("FILEPATH", j, path)
    path <- gsub(paste0("FILEPATH", Sys.info()[["user"]], "/"), h, path)
  } else {
    j <- "FILEPATH"
    h <- paste0("FILEPATH", Sys.info()[["user"]], "/")
    path <- gsub("J:/", j, path)
    path <- gsub("H:/", h, path)
  }
  return(path)
}


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


load.settings <- function(path, topic=NA, age_cohort=TRUE) {

  
  df <- load.file(path)
  if (age_cohort == TRUE) {
    df <- df[age_cohort == TRUE, ]
  } else {
    df <- df[age_cohort == FALSE, ]
  }

  
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


no.ext <- function(path) {
  strsplit(basename(path), "[.]")[[1]][1]
}


setup.design <- function(df, var) {

  
  
  options(survey.lonely.psu='adjust')

  
  options(survey.adjust.domain.lonely=TRUE)

  
  check_list <- c("strata", "psu", "pweight")
  for (i in check_list) {
    
    assign(paste0(i, "_formula"),
           ifelse(i %in% names(df) & nrow(df[!is.na(i)]) > 0, paste("~", i), NULL) %>% as.formula
    )
  }

  
  return(svydesign(id=psu_formula, weight=pweight_formula, strat=strata_formula, data=df[!is.na(var)], nest=TRUE))
}



reweight_for_subnat_collapse <- function(df, by_vars) {

  weights_below_admin0 <- grep("pweight_admin_.", names(df), value=TRUE)
  cols_na <- sapply(df, function(x)all(is.na(x)))
  na_cols <- names(df)[cols_na]
  non_na_admin_wts <- weights_below_admin0[! weights_below_admin0 %in% na_cols]

  if (length(non_na_admin_wts) > 0){
    
    admin_ids <- grep(by_vars, pattern='admin_.*id', value=TRUE)
    admins_w_subnat_info <- substring(admin_ids, 1, 7)
    correct_pweight_for_collapse <- paste0('pweight_', admins_w_subnat_info)
    
    if (correct_pweight_for_collapse != 'pweight_') {
      df[, pweight := get(correct_pweight_for_collapse)]
    }
  }
  return(df)

}


collapse.by <- function(df, var, by_vars, calc.sd = FALSE) {

  
  
  df[is.na(age_bin), age_bin := 0]
  df[is.na(age_bin_agg), age_bin_agg := ""]

  
  df.c <- df[!is.na(get(var)) & !is.na(strata) & !is.na(psu) & !is.na(pweight)] %>% copy
  df   <- reweight_for_subnat_collapse(df, by_vars)

  
  by_formula <- as.formula(paste0("~", paste(by_vars, collapse="+")))

  
  meta <- df[, list(sample_size = length(which(!is.na(get(var)))),
                    nclust      = length(unique(psu)),
                    nstrata     = length(unique(strata)),
                    var         = var
  ), by          = by_vars]

  
  
  
  
  
  
  

  
  
  by_expression_data_table <- paste0("paste0(", paste(by_vars, collapse=", '_', "), ")")
  df[, id := eval(parse(text = by_expression_data_table))]
  unique_group_by_ids <- unique(df$id)

  est <- data.table()
  cat("---- Beginning survey mean calculation for each grouped-by subset of data\n")

  for(i in 1:length(unique_group_by_ids)) {
    unique_group_by_id <- unique_group_by_ids[i]
    cat(paste0("------ (", i, "/", length(unique_group_by_ids), "): ", unique_group_by_id, "\n"))

    
    design <- setup.design(df[id == unique_group_by_id, ], var)
    est_id <- svyby(~get(var), by_formula, svymean, design=design, deff="replace", na.rm=TRUE, drop.empty.groups=TRUE, keep.names=FALSE, multicore=FALSE) %>% data.table
    est    <- rbind(est, est_id, fill = TRUE)
  }

  
  old <- c("get(var)", "DEff.get(var)", "se")
  new <- c("mean", "design_effect", "standard_error")
  setnames(est, old, new)

  
  out <- merge(meta, est, by=by_vars)

  
  if (calc.sd) {
    stdev <- svyby(~get(var), by_formula, svyvar, design=design, deff="replace", na.rm=TRUE, drop.empty.groups=TRUE, keep.names=FALSE, multicore=TRUE) %>% data.table
    setnames(stdev, "get(var)", "variance")
    stdev <- stdev[, standard_deviation := sqrt(variance)]
    
    stdev <- stdev[, standard_deviation_se := 1 / (2 * standard_deviation) * se]
    stdev <- stdev[, c("se", "variance") := NULL]
    
    out <- merge(out, stdev, by=by_vars)
  }

  
  out[age_bin == 0, age_bin := NA]
  out[age_bin_agg == "", age_bin_agg := NA]

  
  return(out)

}


nonmiss <- function(df, vars, reverse=FALSE) {

  vars.sub <- intersect(vars, names(df))
  i <- lapply(vars.sub, function(x) !all(is.na(df[[x]]))) %>% unlist
  if (reverse) return(setdiff(vars, vars.sub[i]))
  else return(vars.sub[i])

}


categ_to_bin <- function(df, var) {

  vals <- df[!is.na(get(var))][[var]] %>% unique
  for (x in vals) {
    col <- paste0(var, "_", x)
    df <- df[!is.na(get(var)), (col) := ifelse(get(var) == x, 1, 0)]
  }
  newcols <- paste0(var, "_", vals)

  
  return(list(df=df, cols=newcols))

}


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



collapse.subset <- function(df, var, by_vars, design_vars) {

  
  cols <- c(var, by_vars, design_vars)
  n.before <- nrow(df)
  for (i in cols){
    if (grepl(x=i, pattern='pweight_')){
      
      df <- update_granular_pweights(pweight_colname=i, df)
    }
    
    if (!grepl(x=i, pattern='age_bin')) {
      df <- df[!is.na(get(i))]
    }
  }
  n.dropped <- n.before - nrow(df)

  
  df <- df[, lonely := lapply(.SD, length), .SDcols=var, by=by_vars]
  df <- df[lonely != 1]
  df <- df[, lonely := NULL]

  
  return(list(data=df, dropped=n.dropped))

}


clean.subnat <- function(df, vars.subnat) {

  
  rename <- intersect(vars.subnat, names(df))
  if (length(rename) > 0) {
    setnames(df, rename, rep("ihme_loc_id", length(rename)))
    if ("ihme_loc_id" %in% names(df)) df <- df[, ihme_loc_id := NULL]
  }
  return(df)

}




collapse.run <- function(df, config, quiet=TRUE, cores=1) {

  
  config.default <- list(

    
    vars             = c("var"),     		
    vars.categ       = NULL,            
    calc.sd          = FALSE,           
    
    cv.manual        = NULL,            
    cv.detect        = TRUE,            
    
    by_sex           = TRUE,            
    by_age           = TRUE,            
    gbd_age_cuts     = TRUE,            
    aggregate_under1 = TRUE,            
    custom_age_cuts  = NULL,            
    cond_age_cuts    = NULL,            
    
    sample_threshold = NA,              
    
    vars.meta        = c(               
      "nid", "survey_name",
      "ihme_loc_id", "year_start",
      "year_end", "survey_module",
      "file_path"
    ),
    
    vars.subnat      = c(               
      "admin_1_id",
      "admin_1_urban_id",
      "admin_2_id",
      "admin_3_id",
      "admin_4_id",
      "admin_5_id"),
    
    vars.sex         = c("sex_id"),			
    
    vars.age         = c("age_year"),	  
    
    
    census_data      = FALSE,
    
    vars.design      = c(               
      "strata", "psu", "pweight", "pweight_admin_1", "pweight_admin_2", "pweight_admin_3"
    )
  )

  
  if (!is.null(config)) lapply(names(config), function(x) assign(x, config[[x]], pos=1))

  
  invisible(lapply(names(config.default), function(x) if (!exists(x)) assign(x, config.default[[x]], pos=1)))

  
  if (!is.null(vars.categ)) {
    if (!is.na(vars.categ) & vars.categ != "") {
      for (var in vars.categ) {
        ctb      <- categ_to_bin(df, var)
        df       <- ctb[['df']]
        new.vars <- ctb[['cols']]
        vars     <- c(vars, new.vars)
      }
    }
  }

  
  vars.check <- nonmiss(df, vars)
  if (length(vars.check) < 1L) stop(paste0("BREAK || All vars (", toString(vars), ") are either not in dataset or completely missing"))
  vars <- vars.check

  
  vars.stratify <- vars.meta
  if (by_sex) vars.stratify <- c(vars.stratify, vars.sex)
  if (by_age) vars.stratify <- c(vars.stratify, "age_start", "age_end")

  
  if (cv.detect) {
    vars.cv <- grep("^cv_", names(df), value=TRUE)
    if (length(vars.cv) > 0) {
      vars.cv <- nonmiss(df, vars.cv)
      vars.stratify <- c(vars.stratify, vars.cv)
    }
  }

  
  if (!is.null(cv.manual)) {
    vars.cv <- nonmiss(df, cv.manual)
    vars.stratify <- c(vars.stratify, vars.cv)
  }

  
  vars.subnat <- intersect(vars.subnat, names(df))
  vars.subnat <- nonmiss(df, vars.subnat)
  if (length(vars.subnat) != 0L) {
    vars.stratify <- lapply(c("ihme_loc_id", vars.subnat), function(x) c(vars.stratify, x) %>% unique)
  } else {
    vars.stratify <- list(vars.stratify)
  }
  
  for (var.subnat in vars.subnat) {
    
    df[get(var.subnat) == ihme_loc_id, (var.subnat) := NA_character_]
  }

  
  
  vars.design.missing <- nonmiss(df, vars.design, reverse=TRUE)
  subnat_pweights <- c("pweight_admin_1", "pweight_admin_2", "pweight_admin_3")
  if (length(vars.design.missing) != 0L) {
    df <- df[, (vars.design.missing) := 1]
    absent_subnat_pweights <- subnat_pweights[subnat_pweights %in% vars.design.missing]
    df <- df[, (absent_subnat_pweights) := NA]
  }

  
  for (var in vars.design) {
    if (class(df[[var]]) != "numeric") {
      df[[var]] <- as.numeric(df[[var]])
      if (!quiet) print(paste0("ubCov Collapse || Setup || Coercing ", var, " to numeric"))
    }
  }

  
  n <- nrow(df[pweight==0])
  if (n > 0) {
    df <- df[pweight != 0]
    if (!quiet) print(paste0("ubCov Collapse || Setup || Dropping ", n, " rows with pweight == 0"))
  }

  
  errors <- list(missing_design=vars.design.missing)

  
  cf.list <- expand.grid(vars=vars, vars.stratify=vars.stratify)

  
  cf <- lapply(1:nrow(cf.list), function(i) {

    
    vars <- cf.list$vars[i] %>% as.character
    vars.stratify <- cf.list$vars.stratify[i] %>% unlist

    
    if (!quiet) print(paste0("ubCov Collapse || Collapsing (", vars, ") by (", toString(vars.stratify), ")"))

    
    sf <- collapse.subset(df, var=vars, by_vars=vars.stratify, design_vars=vars.design)
    missing <- sf[['dropped']]
    sf <- sf[['data']]

    
    if (nrow(sf) > 0) {

      
      if (census_data) sf.out <- census.collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)
      if (!census_data) sf.out <- collapse.by(sf, var=vars, by_vars=vars.stratify, calc.sd=calc.sd)

      
      sf.out <- clean.subnat(sf.out, vars.subnat)
      return(sf.out)
    } else {
      if (!quiet) print(paste0("ubCov Collapse || SKIPPING || Variable (", vars, ") has no observations post subset with (", toString(vars.stratify), ")"))
      return(NULL)
    }
  }) %>% rbindlist(., use.names=TRUE, fill=TRUE)

  
  if (!is.na(sample_threshold)) cf <- cf[sample_size >= sample_threshold]

  
  if (nrow(cf[mean %in% c(0,1)]) > 0) {
    cf.w <- cf[mean %in% c(0,1)]
    sample_size <- cf.w$sample_size
    n <- ifelse(cf.w$mean==0, 0, sample_size)
    ci <- binom.confint(n, sample_size, conf.level = 0.95, methods = "wilson")
    se <- (ci$upper - ci$lower)/3.92
    se <- se * sqrt(2.25) 
    cf[mean %in% c(0,1)]$standard_error <- se
  }

  
  
  if ("strata" %in% vars.design.missing & !census_data) {
    cf <- cf[design_effect < 2.25, inflate := 2.25/design_effect]
    cf <- cf[design_effect < 2.25, standard_error := standard_error * sqrt(inflate)]
    cf <- cf[, inflate := NULL]
    cf <- cf[design_effect < 2.25, design_effect := 2.25]
  }

  
  return(cf)

}






tabulate_gbd <- function(nid) {

  
  
  cat(paste0("======================================================\n||   TABULATED (GBD): ", nid, "\n"))

  
  username        <- Sys.info()[["user"]]
  vaccines_repo   <- paste0("FILEPATH", username, "FILEPATH")
  topic           <- "vaccines"
  config_path     <- paste0(vaccines_repo, "FILEPATH/collapse_config.csv")
  parallel        <- FALSE

  
  cat("||-- Load and Prep Data\n")
  dataset     <- prep_for_tabulation(nid, team = "gbd", vaccines. = c(vaccines, "rotac", "dpt3_timeliness_ratio"))


  if (dataset[[1]] != FALSE) {

    
    dataset  <- dataset[[2]] %>% as.data.table
    config   <- load.settings(config_path, topic)

    if (nrow(dataset) == 0) {
      log_empty_dataset(nid, date, script = "02_gbd", step = 1, object_name = "dataset")
    }

    cat("||-- Collapse Data\n")
    tab_data <- collapse.run(dataset, config=config)

    
    if (nrow(tab_data) == 0) {
      log_empty_dataset(nid, date, script = "02_gbd", step = 2, object_name = "tab_data")
    }

    
    
    
    vax_targets <- set_target_ages(tab_data, vaccines.=c(vaccines,"rotac"))
    setnames(vax_targets, "age_cohort", "me_cohort_schedule")
    tab_data  <- merge(tab_data, vax_targets, by="me_name", all.x=TRUE)

    
    archive_folder <- format(Sys.time(), "%Y_%m_%d")
    dir.create(file.path(extraction_root, "FILEPATH", "Archive", archive_folder), recursive = T)
    write.csv(tab_data, file.path(extraction_root, "FILEPATH", "Archive", archive_folder, paste0(nid, ".csv")), row.names=FALSE)

    
    cat("||-- Save Data\n")
    tab_data[, var := NULL]
    write.csv(tab_data, file.path(extraction_root, "FILEPATH", paste0(nid, ".csv")), row.names=FALSE)
    cat(paste0("||-- Tabulate GBD Complete: ", nid, "\n"))
    cat("******************************************************\n")

  } else {
    cat("Tabulation prep failed (missing required columns)")
  }
}


