logit <- function(x) {
   log(x/(1-x))
}

invlogit <- function(x) {
   exp(x)/(1+exp(x))
}