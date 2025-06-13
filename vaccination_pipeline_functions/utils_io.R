clean_path <- function(..., mustWork = NA){
   
   normalizePath(file.path(...), mustWork = mustWork)
}

make_directory <- function(path, .recursive = TRUE, .showWarnings = FALSE) {
   
   
   
   #' @param path [path] directory to create
   #' @param .recursive [lgl] dir.create arg (default = TRUE)
   #' @param .showWarnings [lgl] dir.create arg (default = FALSE)
   
   #' @return [lgl] did directory creation succeed or fail?
   
   dir.create(path, recursive = .recursive, showWarnings = .showWarnings)
}



#' @param path_to_file [chr] full path with extenstion
#' @param verbose [lgl] noisy or quiet function?
#' @param csv_opt [chr] namespaced function call for csv reads (default `"data.table::fread"`)

#' @return [file] an object of appropriate file type
#' @export

#' @examples
read_file <- function(path_to_file, verbose = FALSE, csv_opt = "data.table::fread"){
   
   if(verbose) message("Reading file: ", path_to_file)
   
   
   ext <- tools::file_ext(path_to_file)
   suppressWarnings(numeric_chk <- as.numeric(ext))
   if(is.numeric(numeric_chk) & !is.na(numeric_chk)) stop("File extension is numeric, must be character: ", ext)
   ext <- tolower(ext)
   if(!grepl("::", csv_opt)) stop("csv_opt must be a namespaced function call e.g. data.table::fread - instead got ", csv_opt)
   valid_file_extensions <- toString(c("csv", "yaml", "rds", "json"))
   
   read_fun <- switch(
      ext,
      "csv"  = getFromNamespace(
         x  = strsplit(csv_opt, "::")[[1]][2],
         ns = strsplit(csv_opt, "::")[[1]][1]
      ),
      "yaml" = yaml::read_yaml,
      "rds"  = readRDS,
      "json" = jsonlite::fromJSON,
      {
         stop(paste0("This function only supports ", valid_file_extensions, " file extensions (case-insensitive)."), 
              " Update if more options are needed: ", ext)
      }
   )
   
   return(read_fun(path_to_file))
}




#' @param fname_nc [chr] path to the nc file

#' @return [chr] name of the value variable(s)
#' @export

#' @examples
get_nc_value_name <- function(fname_nc) {
   
   library(ncdf4)
   library(ncdf4.helpers)
   
   
   nc_obj       <- nc_open(fname_nc)
   names_values <- names(nc_obj$var)
   
   nc_close(nc_obj)
   return(names_values)
}



#' @param fname_nc [chr] path to the nc file
#' @param start [?] index to start at?
#' @param count [?] number of values to read?
#' @param df_return [lgl] return as a data.frame? If false, return as Rarray.

#' @return [data.frame / Rarray] Depends on `df_return` argument
#' @export

#' @examples
read_nc <- function(fname_nc, start = NA, count = NA, df_return = TRUE) {
   
   library(ncdf4)
   library(ncdf4.helpers)
   library(data.table)
   
   
   ncin <- nc_open(fname_nc)
   print(ncin)
   
   dimname <- get_nc_value_name(fname_nc = fname_nc)
   
   Rarray <- ncvar_get(ncin, dimname, start = start, count = count, collapse_degen = FALSE)
   
   fillvalue <- ncatt_get(ncin, dimname, "_FillValue")
   
   array_dim <- ncdf4.helpers::nc.get.dim.names(ncin, dimname)
   
   nc_close(ncin)
   
   array_dim_list <- list()
   for(i in array_dim) {
      array_dim_list[[i]] <- ncin$dim[[i]]$vals
   }
   
   Rarray[Rarray == fillvalue$value] <- NA
   
   for(i in 1:length(array_dim_list)) {
      dimnames(Rarray)[[i]] <- array_dim_list[[i]]
   }
   
   names(attributes(Rarray)$dimnames) <- array_dim
   if(df_return) {
      return(data.frame(reshape2::melt(Rarray)))
   } else {
      return(Rarray)
   }
}






#' @param outpath [chr] full path to file

#' @return [chr] new path with version number appended (if necessary)
#' @export

#' @examples








increment_file_version <- function(outpath){
   if(file.exists(outpath)){
      fname_og <- basename(outpath)
      fname_split <- strsplit(fname_og, "\\.")[[1]]
      dirname_og <- dirname(outpath)
      outpath_new <- file.path(dirname_og, paste0(fname_split[1], "_v1", ".", fname_split[2]))
      idx <- 2
      
      while(file.exists(outpath_new)){
         fname_new <- paste0(fname_split[1], "_v", idx, ".", fname_split[2])
         outpath_new <- file.path(dirname_og, paste0(fname_new))
         idx <- idx + 1
      }
   } else {
      outpath_new <- outpath
   }
   
   return(outpath_new)
}






#' @param dir path to directory with versioned dirs
#' @param date character in be YYYY_MM_DD format

#' @return largest version in directory tree or 0 if there are no version OR

get_latest_output_date_index <- function(dir, date) {
   currentfolders <- list.files(dir)
   
   
   pat <- sprintf("^%s[.]\\d{2}$", date)
   date_dirs <- grep(pat, currentfolders, value = T)
   
   if (length(date_dirs) == 0) {
      return(0)
   }
   
   
   date_list <- strsplit(date_dirs, "[.]")
   
   inds <- unlist(lapply(date_list, function(x) x[2]))
   if (is.na(max(inds, na.rm = T))) inds <- 0
   
   return(max(as.numeric(inds)))
}





#' @param root path to root of output results
#' @param date character date in form of "YYYY_MM_DD" or "today". "today" will be interpreted as today's date.
make_new_output_dir <- function(root, date) {
   if (date == "today") {
      date <- format(Sys.Date(), "%Y_%m_%d")
   }
   cur.version <- get_latest_output_date_index(root, date = date)
   
   dir.name <- sprintf("%s.%02i", date, cur.version + 1)
   dir.path <- file.path(root, dir.name)
   if (!dir.exists(dir.path)) {
      
      old.umask <- Sys.umask()
      Sys.umask("002")
      dir.create(dir.path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
      Sys.umask(old.umask)
   }
   return(dir.path)
}
