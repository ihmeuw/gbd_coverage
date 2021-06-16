###########################################################
### Author: USERNAME
### Date: 1/26/2015
### Project: ST-GPR
### Purpose: Utility functions for modeling
###########################################################


####################################################################################################################################################
# 															 ST-GPR Output-Pulling Functions
####################################################################################################################################################
source('FILEPATH/db_tools.r')

library(rhdf5)
library(data.table)


model_load <- function(run_id, obj, holdout=0 , param_set = 0, last_old_run_id = 46904) {
  run_id <- as.integer(run_id)
  if(run_id < last_old_run_id){
    if(obj == 'stage1') obj = 'prior'
    if(obj == 'amp_nsv') obj = 'st_amp'
    return(model_load_old(run_id, obj))
  }else{
    path <- model_path(run_id, obj, holdout, param_set)
    if (obj == 'stage1') {
      return(h5read(path, obj) %>% data.table)
    } else if ((obj == 'parameters')){
      return(fread(sprintf('%s/%i/parameters.csv', stgpr_path('cluster_model_output'), run_id)))
    } else{
      return(h5read.py(path, obj) %>% data.table)
    }
  }
  print(paste("Loaded", obj, sep=" "))
}


h5read.py = function(h5File, name) {

  listing = h5ls(h5File)

  if (!(name %in% listing$name)) stop(paste0(name, " not in HDF5 file"))

  # only take requested group (df) name
  listing = listing[listing$group==paste0('/',name),]

  data_nodes = grep("_values", listing$name)
  name_nodes = grep("_items", listing$name)

  data_paths = paste(listing$group[data_nodes], listing$name[data_nodes], sep = "/")
  name_paths = paste(listing$group[name_nodes], listing$name[name_nodes], sep = "/")

  columns = list()
  for (idx in seq(data_paths)) {
    data <- data.frame(t(h5read(h5File, data_paths[idx])))
    names <- t(h5read(h5File, name_paths[idx]))

    if (length(names) != length(data)){
    }else{
      entry <- data.frame(data)
      colnames(entry) <- names
      columns <- append(columns, entry)
    }
  }

  data <- data.frame(columns)

  return(data)
}


####################################################################################################################################################
# 															 Math Functions
####################################################################################################################################################

logit <- function(x) {
	return(log(x/(1-x)))
}

logit_offset <- function(x, offset) {

	x_len = length(x)

	value <- vector(mode="numeric", length=x_len)

	for (i in 1:x_len) {

		if (x[i]==1) {
			value[i] <- x[i] - offset
		} else if (x[i]==0)  {
			value[i] <- x[i] + offset
		} else value[i] <- x[i]

	}

	return(log(value/(1-value)))
}

inv.logit <- function(x) {
	return(exp(x)/(exp(x)+1))
}

transform_data <- function(var, space, reverse=F) {

  if (space %in% c('NA', NA, '', 'none', 'None')){
    # no need to do a thing, untransformed
  }else{
  	if (space == "logit" & reverse==F) {
  		var <- logit(var)
  	} else if (space == "logit" & reverse==T) {
  		var <- inv.logit(var)
  	} else if (space == "log" & reverse==F) {
  		var <- log(var)
  	} else if (space == "log" & reverse==T) {
  		var <- exp(var)
  	}
  }
	return(var)

}

delta_transform <- function(data, variance, space, reverse=F) {

  if (space %in% c('NA', NA, '', 'none', 'None')){
  }else{

  	if (space == "logit" & reverse==F) {
  		variance <- variance * (1/(data*(1-data)))^2
  	} else if (space == "logit" & reverse==T) {
  		variance <- variance / (1/(data*(1-data)))^2
  	} else if (space == "log" & reverse==F) {
  		variance <- variance * (1/data)^2
  	} else if (space == "log" & reverse==T) {
  		 variance <- variance / (1/data)^2
  	}
  }
  return(variance)
}

####################################################################################################################################################
# 															 Random Functions
####################################################################################################################################################

#' Check the run status (model_status) of the ST-GPR run id.
#' 
#' @param run_id ST-GPR run id (ST-GPR version id)
#' @param verbose TRUE if model status (success, failure, stage1, etc) should be printed
#' 
#' @return int representing the status of the run. 0: failure, 1: success, 2: running
check_run <- function(run_id, verbose = TRUE) {
  if (length(run_id) != 1) {
    stop(paste0("Must pass in exactly 1 value for run_id; given ", length(run_id)))
  }
  
  res <- query(paste("SELECT model_status_name FROM stgpr.stgpr_version",
                     "LEFT JOIN stgpr.model_status USING (model_status_id)",
                     "WHERE stgpr_version_id =", run_id),
               conn_def = "stgpr-uploader"
  )
  
  # Guard against us not finding any model registered in the database
  if (nrow(res) == 0) {
    stop(paste0("No data found for ST-GPR version ", run_id,
                ". Are you sure the version id exists?"))
  }
  
  if (verbose) {
    cat(paste0("ST-GPR version ", run_id, "'s status is '", res, "'.\n"))
  }
  
  if (res$model_status_name == "success") {
    return(1)
  } else if (res$model_status_name == "failure") {
    return(0)
  } else {
    return(2)
  }
}

