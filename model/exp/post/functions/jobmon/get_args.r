#' @title Get arguments for argparse

CLUSTER_NAME

#' @return vector of arguments as characters in the order passed to sbatch
get_args <- function() {
  
  args <- commandArgs(TRUE)
  

  if (is.null(args) || length(args) == 0) {
    
    
    
    
    
    
    
    
    
    
    args <- commandArgs(FALSE)
    idx <- match("--no-save", args)
    if (is.na(idx)) {
      message(sprintf("--no-save not detected in commandArgs() - cannot determine CLI args from '%s'", paste(commandArgs(), collapse = " ")))
      args <- character(0)
    } else if (idx == length(args)) {
      
      args <- character(0)
    } else {
      
      args <- args[-seq_len(idx + 1)]
    }
  }
  return(args)
}
