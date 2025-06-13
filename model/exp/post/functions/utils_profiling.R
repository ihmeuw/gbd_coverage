
get_maxrss <- function(job_name, time){
   maxrss <- system2(SYSTEM_COMMAND), stdout=TRUE)
   return(maxrss)
}
get_elapsed_time <- function(job_name, time){
   elapsed <- system2(SYSTEM_COMMAND), stdout=TRUE)
}