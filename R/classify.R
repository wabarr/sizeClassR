#' Classify body masses into size classes
#'
#' @param sizes vector of body sizes to classify, units are assumed to be kg
#' @param method method of classification, default is Brain
#'
#' @return
#' @export
#'
#' @examples
#' x <- runif(10, min=10, max=1500)
#'x[11] <- NA
#'x[12] <- 80
#'classify(x)

classify <- function(sizes, method="Brain") {
  if(mean(sizes, na.rm=TRUE)>5000){
    warning("Are you sure your units are in kg? Please double check. This appears to be a mightly large group of animals!")
  }
  maxima <- c(0,18,80,350,1000,180000)
  ##note: upper limit from body mass of blue whale
  classes <- cut(sizes, breaks=maxima, labels = FALSE, include.lowest = TRUE, right=TRUE)
  return(classes)
}

