



#' @param path_current [chr] full path with file name and extension of the file to be archived
#' @param dir_archive [chr] full path to the directory where the file will be archived

#' @return [chr] full path to the archived file
#' @export

#' @examples
archive_single_file <- function(path_current, dir_archive = file.path(dirname(path_current), "archive")){
   
   if(!file.exists(path_current)){
      stop("The file to archive does not exist: ", path_current)
   }
   
   dir.create(dir_archive, showWarnings = FALSE, recursive = TRUE)
   file_name <- basename(path_current)
   ext <- paste0(".", sub(".*\\.", "", file_name))
   file_basename <- sub(ext, "", file_name)
   
   
   path_archive <- file.path(dir_archive, paste0(file_basename, format(Sys.time(), "_%Y%m%d%H%M"), ext))
   file.copy(path_current, path_archive)
   
   
   return(path_archive)
}