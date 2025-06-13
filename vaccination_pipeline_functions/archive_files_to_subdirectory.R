








#' @param root_dir [path] The directory to archive - all files will be moved to

#' @param method [chr] The method to use for archiving - supports c("zip", "subfolder")

#' @return none
#' @export

#' @examples
archive_files_to_subdirectory <- function(root_dir, method = "subfolder"){
   
   
   if(!require(data.table)) stop("This function requires the 'data.table' package")
   if(!(dir.exists(root_dir))) {message(root_dir, "does not exist, exiting."); return()}
   valid_methods <- c("subfolder", "zip")
   if(!(method %in% valid_methods)) stop("Supply a suported method: ",  toString(valid_methods))
   stopifnot(length(method) == 1)
   
   
   
   f_table <- as.data.table(file.info(list.files(root_dir, full.names = TRUE)), keep.rownames = TRUE)
   
   f_table <- f_table[!grepl("\\.zip$|\\.tar", rn, ignore.case = TRUE), ]
   
   message("Archiving old files with method = ", method)
   archive_dir            <- file.path(root_dir, paste0("archive", format(Sys.time(), "_%Y%m%d_%H%M%S")))
   fnames_from            <- f_table[isdir == FALSE]$rn
   files_to_archive_exist <- any(f_table$isdir == FALSE)
   
   if (method == "subfolder" & files_to_archive_exist) {
      
      message("Archiving old files to ", archive_dir)
      
      dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)
      fnames_to    <- file.path(archive_dir, basename(fnames_from))
      success_mask <- file.rename(from = fnames_from, to = fnames_to)
      
      if (!all(success_mask)) {
         message("Archived these files:\n", paste0(fnames_from[success_mask], collapse = "\n"))
         stop("Failed to archive these file:\n", paste0(fnames_from[!success_mask], collapse = "\n"))
      }
      
   } else if (method == "zip" & files_to_archive_exist) {
      
      archive_dir <- paste0(archive_dir, ".zip")
      
      message("Archiving old files to ", archive_dir)
      
      which_path_status <- system(SYSTEM_COMMAND)
      if (!is.null(attr(which_path_status, "status"))) {
         stop("The 'zip' utility is not installed on this system.")
      }
      
      
      
      status_code <- zip(zipfile = archive_dir, files = fnames_from, flags = "-jm")
      message("Setting archive directory permissions to 775.")
      Sys.chmod(archive_dir, "775", use_umask = FALSE)
      
      if (!status_code %in% c(0) ) {
         stop("Failed to archive some files - see:\n     ", root_dir, "\n     ", archive_dir)
      }
      
   } else {
      message("No files to archive in ", root_dir, " - exiting.")
   }
}