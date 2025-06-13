





#' @param root_dir [path] The directory to archive - all files will be _copied_ to


#' @return [none]
#' @export

#' @examples
archive_files_to_subdirectory <- function(root_dir){
   
   
   if(!require(data.table)) stop("This function requires the 'data.table' package")
   if(!(dir.exists(root_dir))) {message(root_dir, "does not exist, exiting."); return()}
   
   
   
   f_table <- as.data.table(file.info(list.files(root_dir, full.names = TRUE)), keep.rownames = TRUE)
   
   if (any(f_table$isdir == FALSE)) {
      archive_dir <- file.path(root_dir, paste0("archive", format(Sys.time(), "_%Y%m%d_%H%M%S")))
      
      message("Archiving old files to ", archive_dir)
      
      dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)
      fnames_from  <- f_table[isdir == FALSE]$rn
      fnames_to    <- file.path(archive_dir, basename(fnames_from))
      success_mask <- file.copy(from = fnames_from, to = fnames_to)
      if (!all(success_mask)) {
         message("Archived these files:\n", paste0(fnames_from[success_mask], collapse = "\n"))
         stop("Failed to archive these file:\n", paste0(fnames_from[!success_mask], collapse = "\n"))
      }
   }
}









#' @param root_dir [chr] The directory to archive - all files will be _copied_ to the archive folder. 

#' @return [none]
#' @export

#' @examples
archive_files_to_directory <- function(root_dir){
   
   
   if(!require(data.table)) stop("This function requires the 'data.table' package")
   if(!(dir.exists(root_dir))) {message(root_dir, "does not exist, exiting."); return()}
   
   
   
   f_table <- as.data.table(file.info(list.files(root_dir, full.names = TRUE)), keep.rownames = TRUE)
   
   if (any(f_table$isdir == FALSE)) {
      archive_dir <- paste0(root_dir, "_archive", format(Sys.time(), "_%Y%m%d_%H%M%S"))
      
      message("Archiving old files to ", archive_dir)
      
      dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)
      fnames_from  <- f_table[isdir == FALSE]$rn
      fnames_to    <- file.path(archive_dir, basename(fnames_from))
      success_mask <- file.copy(from = fnames_from, to = fnames_to)
      if (!all(success_mask)) {
         message("Archived these files:\n", paste0(fnames_from[success_mask], collapse = "\n"))
         stop("Failed to archive these file:\n", paste0(fnames_from[!success_mask], collapse = "\n"))
      }
   }
}



#' @param archive_dir [chr] The directory to restore from
#' @param restore_dir [chr] The directory to restore to

#' @return [none]
#' @export

#' @examples
restore_from_archive_directory <- function(archive_dir, restore_dir){
   
   
   if(!require(data.table)) stop("This function requires the 'data.table' package")
   if(!(dir.exists(archive_dir))) {message(archive_dir, "does not exist, exiting."); return()}
   if(!(dir.exists(restore_dir))) {message(restore_dir, "does not exist, exiting."); return()}
   if(!grepl("_archive", archive_dir)) stop("This function is only for directories with '_archive' in the name")
   
   
   
   f_table <- as.data.table(file.info(list.files(archive_dir, full.names = TRUE)), keep.rownames = TRUE)
   
   if (any(f_table$isdir == FALSE)) {
      message("Restoring old files from ", archive_dir, " to ", restore_dir)
      
      fnames_from  <- f_table[isdir == FALSE]$rn
      fnames_to    <- file.path(restore_dir, basename(fnames_from))
      success_mask <- file.copy(from = fnames_from, to = fnames_to, overwrite = TRUE)
      if (!all(success_mask)) {
         message("Restored these files:\n", paste0(fnames_from[success_mask], collapse = "\n"))
         stop("Failed to restore these file:\n", paste0(fnames_from[!success_mask], collapse = "\n"))
      }
   }
}