

#' @param x [any] some element

#' @return [std_err] stop if not length 1
#' @export

#' @examples
assert_scalar = function(x){
   if(!(is.atomic(x) && length(x) == 1L)){
      stop("x must be atomic and length 1L")
   }
}



#' @param x [obj] some R object

#' @return [lgl] TRUE / FALSE
is_an_error <- function(x) {
   
   (inherits(x, "simpleError") | inherits(x, "try-error"))
} 
















#' @param nonzero_only [lgl] assert all files of interest have non-zero size, and are not directories
#' @param exclude_dirs [lgl] if FALSE, allows asserting directories as well as files.
#' @param verbose [lgl] if FALSE, no user messages
#' @param v_verbose [lgl] if TRUE, message all files being checked
#' @param report_path [path] full .yaml path to write report, if `NULL`, no report written

#' @importFrom DescTools SplitPath
#' @importFrom yaml write_yaml


#' @return [chr] invisible: vector of file names found on disk
assert_files_exist <- function(
      fpaths_expected
      , nonzero_only = TRUE
      , exclude_dirs = TRUE
      , verbose      = TRUE
      , v_verbose    = FALSE
      , report_path  = NULL
){
   
   
   wl_default <- getOption("warning.length")
   options(warning.length = 8100)
   on.exit(options(warning.length = wl_default), add = TRUE, after = FALSE)
   
   
   stopifnot(is.logical(nonzero_only))
   stopifnot(is.logical(exclude_dirs))
   stopifnot(is.logical(verbose))
   stopifnot(is.logical(v_verbose))
   
   if(verbose)   message("Asserting file presence:")
   if(v_verbose) message(" ", paste(fpaths_expected, collapse = "\n "))
   
   finfo <- file.info(fpaths_expected)
   
   not_a_file        <- finfo[finfo$isdir %in% TRUE, ]
   fnames_not_a_file <- rownames(not_a_file)
   fnames_on_disk    <- fpaths_expected[file.exists(fpaths_expected)]
   fnames_missing    <- setdiff(fpaths_expected, fnames_on_disk)
   fsize_zero        <- finfo[finfo$size %in% 0 & finfo$isdir %in% FALSE,]
   fnames_size_zero  <- rownames(fsize_zero)
   fnames_size_NA    <- rownames(finfo[is.na(finfo$size), ])
   
   if(!is.null(report_path)){
      
      stopifnot(is.character(report_path))
      report_path_ext <- DescTools::SplitPath(report_path)$ext
      stopifnot(report_path_ext %in% c("yaml", "yml"))
      
      if(verbose) on.exit(message("Report written to: ", report_path), add = TRUE, after = FALSE)
      report <- list(
         fnames_on_disk      = fnames_on_disk
         , fnames_size_NA    = fnames_size_NA
         , fnames_size_zero  = fnames_size_zero
         , fnames_missing    = fnames_missing
         , fnames_not_a_file = fnames_not_a_file
      )
      yaml::write_yaml(report, report_path)
   }
   
   if(length(fnames_missing)) stop("Missing files on disk:\n    ", paste(fnames_missing, collapse = "\n    "))
   if(verbose) message("  Passing.")
   
   if(exclude_dirs){
      if(nrow(not_a_file)) {
         fnames_not_a_file_msg <- paste(fnames_not_a_file, collapse = "\n    ")
         stop("Some queried files are actually directories:\n",
              "    ", fnames_not_a_file_msg)
      }
   }
   
   if(nonzero_only){
      if(verbose) message("Asserting non-zero file size:")
      if(nrow(fsize_zero)) {
         fnames_size_zero_msg <- paste(fnames_size_zero, collapse = "\n    ")
         stop("Some files have zero size:\n",
              "    ", fnames_size_zero_msg)
      }
      if(verbose) message("  Passing.")
   }
   
   return(invisible(fnames_on_disk))
   
}



#' @param x [list] a named list

#' @return [std_err] error message if failed
#' @export

#' @examples
assert_named_list <- function(x){
   if(!is.null(x)){
      .err_msg <- "x must be a named list (not whitespace)"
      if(!is.list(x))                   stop(.err_msg)
      if(is.null(names(x)))             stop(.err_msg)
      if(any(is.na(names(x))))          stop(.err_msg)
      names(x) <- trimws(names(x))
      if(any(nchar(names(x)) == 0))     stop(.err_msg)
   }
}











#' @param my_dt [data.table] your data
#' @param varnames_strict [chr] strictly required vector of column names - all required, no others allowed
#' @param strict_order_req [lgl] if varnames_strict is not NULL, should the order of columns be asserted?
#' @param varnames_req [chr] vector of column names required in my_dt (include)
#' @param varnames_forbid [chr] vector of column names forbidden in my_dt (exclude)
#' @param regex_req [chr] regex pattern for required column names
#' @param regex_forbid [chr] regex pattern for forbidden coumn names (default `\.x` and `\.y` to guard against invalid merges)
#' @param data_types [named list] `key=value` pair list of column names and required data type
#' @param verbose [lgl] confirmation message if passing all assertions

#' @return [std_err] message or error
assert_data_schema <- function(
      my_dt
      , varnames_strict  = NULL
      , strict_order_req = FALSE
      , varnames_req     = NULL
      , regex_req        = NULL
      , varnames_forbid  = NULL
      , regex_forbid     = c("\\.x", "\\.y")
      , data_types       = NULL
      , verbose          = FALSE
){
   
   
   x <- data.table::copy(my_dt)
   
   
   
   callstack_name <- get_object_name_from_call()
   obj_name <- deparse(substitute(my_dt))
   my_dt_name <- ifelse(identical(callstack_name, obj_name), obj_name, paste0(callstack_name, ":", obj_name))
   
   if(!is.null(varnames_strict)){
      if(strict_order_req){
         varnames_have <- names(x)
         varnames_need <- varnames_strict
         strict_varname_success <- identical(varnames_have, varnames_need)
      } else {
         varnames_have <- sort(names(x))
         varnames_need <- sort(varnames_strict)
         strict_varname_success <- identical(varnames_have, varnames_need)
      }
      
      if(!strict_varname_success) {
         stop(my_dt_name, ": ", 
              "Strict varname check failure:\n",
              "  strict_order_req = ", as.character(strict_order_req), "\n",
              "  varnames_have = ", paste(varnames_have, collapse = ", "), "\n",
              "  varnames_need = ", paste(varnames_need, collapse = ", "), "\n"
         )
      }
   }
   
   
   if(!is.null(varnames_req)) {
      missing_colnames <- paste(setdiff(varnames_req, names(x)), collapse = ", ")
      if(nchar(missing_colnames)) stop(my_dt_name, ": ", "Missing these required column names:\n", "    ", missing_colnames)
   } 
   
   if(!is.null(regex_req)) {
      rgx_list <- lapply(regex_req, x = x, function(rgx, x){
         if(!any(grepl(rgx, names(x)))) return(rgx)
      })
      rgx_failures <- paste(unlist(rgx_list), collapse = ", ")
      if(nchar(rgx_failures)) stop(my_dt_name, ": ", "No column name contains the required pattern(s):\n", "    ", rgx_failures)
   }
   
   if(!is.null(varnames_forbid)){
      forbidden_colnames <- paste(names(x)[which(names(x) %in% varnames_forbid)], collapse = ", ")
      if(nchar(forbidden_colnames)) stop(my_dt_name, ": ", "Forbidden column names found:\n", "    ", forbidden_colnames)
   }
   
   if(!is.null(regex_forbid)){
      rgx_list <- lapply(regex_forbid, x = x, function(rgx, x){
         if(any(grepl(rgx, names(x)))) return(names(x)[grepl(rgx, names(x))])
      })
      rgx_failures <- paste(unlist(rgx_list), collapse = ", ")
      if(nchar(rgx_failures)) stop(my_dt_name, ": ", "Some column name(s) contain the forbidden pattern(s): ", paste(regex_forbid, collapse = ", "), "\n", "    ", rgx_failures)
   }
   
   if(!is.null(data_types)){
      
      data_types <- lapply(data_types, tolower)
      
      valid_data_types <- 
         tolower(
            c(
               "Numeric",
               "Integer",
               "Logical",
               "Character",
               "Complex",
               "Raw",
               "Factor",
               "Date",
               "POSIXct",
               "POSIXlt"
            )
         )
      
      invalid_types <- unlist(data_types)[!unlist(data_types) %in% valid_data_types]
      if(length(invalid_types)) {
         invalid_types <- paste(invalid_types, collapse = ", ")
         if(!all(unlist(data_types)) %in% valid_data_types) {
            stop("Some data_types are not supported:\n", 
                 "    valid types: ", paste(valid_data_types, collapse = ", "), "\n",
                 "    invalid types: ", invalid_types)
         }
      }
      
      if(any(unlist(lapply(data_types, length)) > 1)) stop(my_dt_name, ": ", "data_types may only contain named `key = value` pairs")
      if(any(unlist(lapply(names(data_types), nchar)) == 0)) stop(my_dt_name, ": ", "data_types may only contain named `key = value` pairs")
      if(!all(names(data_types) %in% names(x))) stop(my_dt_name, ": ", "`names(data_types)` must all be columns in `x`")
      
      coltypes_actual        <- unlist(lapply(names(data_types), function(dtype) typeof(x[[dtype]])))
      names(coltypes_actual) <- names(data_types)
      coltype_success_mask   <- unlist(purrr::map2(names(data_types), data_types, function(.key, .val){typeof(x[[.key]]) == .val}))
      
      if(any(coltype_success_mask == FALSE)){
         coltype_fail_mask <- !coltype_success_mask
         coltype_fails     <- coltypes_actual[coltype_fail_mask]
         stop(my_dt_name, ": ", "Columns failing type-matching.  Actual column data-types are:", "\n    ", paste(capture.output(coltype_fails), collapse = "\n    "))
      }
   }
   
   if(verbose) message(my_dt_name, ": ", "Passing data schema validation.")
   
}







#' @param dt [data.table] table of data
#' @param id_varnames [chr] vector of id variable names that, in combination (e.g. expand.grid) will uniquely ID all rows of data
#' @param verbose [lgl] print success message?
#' @param hard_stop [lgl] default TRUE - stop if non-square, warn if FALSE
#' @param no_na_varnames [chr] optional (defualt NULL) vector of variable names that must not be NA to be considered 'square'

#' @return [list] 2 data.tables - duplicated rows and missing rows
#' @export

#' @examples
assert_square <- function(dt, id_varnames, no_na_varnames = NULL, verbose = TRUE, hard_stop = TRUE){
   
   
   if(!is.data.table(dt)) stop("dt must be a data.table")
   if(!is.character(id_varnames)) stop("id_varnames must be a character vector")
   if(!is.logical(verbose)) stop("verbose must be a logical")
   varnames_missing <- setdiff(id_varnames, names(dt))
   if(length(varnames_missing)) stop("Not all id_varnames are present in the data.table: ", toString(varnames_missing))
   
   
   id_vars               <- lapply(id_varnames, function(x) unique(dt[[x]]))
   id_vars_square        <- do.call(data.table::CJ, id_vars)
   names(id_vars_square) <- id_varnames
   
   
   missing_rows <- data.table::fsetdiff(id_vars_square, dt[, ..id_varnames], all = FALSE)
   if(nrow(missing_rows) > 0) {
      message(paste0("Missing rows in the data.table, example: ", toString(paste(names(missing_rows), missing_rows[1, ], sep = ":") )))
   }
   
   
   duplicated_rows <- dt[duplicated(dt, by = id_varnames), ]
   if(nrow(duplicated_rows) > 0) {
      message(paste0("Duplicated rows in the data.table, example: ", toString(paste(names(duplicated_rows), duplicated_rows[1, ], sep = ":") )))
   }
   
   non_square_list <- list(
      duplicated_rows = duplicated_rows, 
      missing_rows    = missing_rows
   )
   
   
   if(!is.null(no_na_varnames)){
      assert_no_na(dt, no_na_varnames, verbose = verbose)
   }
   
   if(any(unlist(lapply(non_square_list, nrow))) > 0 ){
     
     assign("NON_SQUARE_LIST", non_square_list, envir = .GlobalEnv)
     
     if(hard_stop){
       stop("Your data.table is not square - see NON_SQUARE_LIST for duplicated and / or missing rows.")
     } else {
       warning("Your data.table is not square - see NON_SQUARE_LIST for duplicated and / or missing rows.")
     }
     
   }
   
   dt_name <- deparse(substitute(dt))
   if (verbose) message(dt_name, " is square by: ", toString(id_varnames))
   
   return(invisible())
   
}



#' @param dt [data.table]
#' @param varnames [chr] vector of variable names to check for NA values

#' @return [none] stop if NA values found in `varnames`
#' @export

#' @examples
assert_no_na <- function(dt, varnames, verbose = TRUE){
   if(!is.data.table(dt)) stop("dt must be a data.table")
   if(!is.character(varnames)) stop("varnames must be a character vector")
   if(!all(varnames %in% names(dt))) stop("Not all varnames are present in the data.table")
   
   na_rows <- dt[dt[, .I[apply(.SD, 1, anyNA)], .SDcols = varnames]]
   
   if(nrow(na_rows) > 0) {
      stop("NA values found in the following rows: ", toString(paste(names(na_rows), na_rows[1, ], sep = ":") ))
   }
   
   if(verbose) message("No NA values found in: ", toString(varnames))
}




#' @param df [data.frame]
#' @param varname [chr] name of a column
#' @param vals_required [vector] values required in the column (only unique values searched)
#' @param verbose [lgl] if TRUE message user if passing (otherwise, pass silently)
#' @param v_verbose [lgl] if TRUE message the required values

#' @return [vector] missing values, if any found
#' @export

#' @examples
assert_required_values <- function(df, varname, vals_required, verbose = TRUE, v_verbose = FALSE){
   
   assert_scalar(varname)
   if(!is.data.frame(df))        stop("df must be a data.frame")
   if(!is.character(varname))    stop("varname must be a character vector")
   if(!is.logical(verbose))      stop("verbose must be a logical")
   if(is.list(vals_required))    stop("vals_required must be a simple vector")
   if(!is.vector(vals_required)) stop("vals_required must be a simple vector")
   
   
   callstack_name <- get_object_name_from_call()
   obj_name <- deparse(substitute(df))
   display_name <- ifelse(identical(callstack_name, obj_name), obj_name, paste0(callstack_name, ":", obj_name))
   
   vals_required <- unique(vals_required)
   missing_vals <- setdiff(vals_required, df[[varname]])
   if(length(missing_vals)) {
      on.exit(return(missing_vals))
      stop(paste(display_name, "missing values from", varname, ":", toString(missing_vals)))
   }
   if(verbose) message(paste(display_name, "passing assert_required_values -", varname))
   if(v_verbose) message(toString(vals_required))
}

get_object_name_from_call <- function() {
   
   object_name <- tryCatch(
      {
         call_stack <- sys.calls()
         
         
         calling_expr <- call_stack[[length(call_stack) - 1]]
         
         object_name <- deparse(calling_expr[[2]])
      },
      error = function(e){
         "object_name_failure"
      }
   )
   return(object_name)
}

