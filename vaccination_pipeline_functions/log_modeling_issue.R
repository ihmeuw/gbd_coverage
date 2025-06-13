





#' @param output_root [chr] directory path
#' @param user_message [chr] length 1 character vector

#' @return [none] writes to disk + std_err
#' @export

#' @examples
log_modeling_issue <- function(output_root, user_message){
   
   stopifnot(dir.exists(output_root))
   stopifnot(length(user_message) == 1)
   
   
   write(user_message, file.path(output_root, "00_MODELING_ISSUE_FLAGS.txt"), append = TRUE)
   
   message(user_message)
}