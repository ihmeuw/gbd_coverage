


#' @param ... additional arguments for `scale_fill_gradientn()`

scale_fill_vaccine <- function(...) {
  
  
  
  
  
  
  
  
  
  
  vals <- c(1.0,       0.8,        0.53,      0.47,      0.4,       0.25,      0.10,      0.000000)
  cols <- c("
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(scale_fill_gradientn(colors = cols, values = vals, 
                              breaks = c(0,0.2,0.4,0.6,0.8,1),
                              limits = c(0,1),
                              labels = scales::percent,
                              ...))
}