#' Classify body masses into size classes
#'
#' @param sizes vector of body sizes to classify, units are assumed to be kg
#' @param method method of classification, default is Brain
#' @value A vector of length `sizes` with an integer size class categorization for each element in `sizes`. Available methods are "Brain", refering to Brain's (1980) size classes and "LewisWerdelin" refering to Lewis and Werdelin's (2007) carnivore size classes.
#' @return
#' @export
#'
#' @examples
#' x <- runif(10, min=10, max=1500)
#' x[11] <- NA
#' x[12] <- 80
#' classify(x, method="Brain")

classify <- function(sizes, method) {
  if(mean(sizes, na.rm=TRUE)>5000){
    warning("Are you sure your units are in kg? Please double check. This appears to be a mightly large group of animals!")
  }
  if(!method %in% c("Brain","LewisWerdelin")) stop(simpleError("Unknown body size classification method"))
  maxMammalMass <- 180000
  ## this is approximate mass of Blue Whale
  if(method=="Brain") cutpoints <- c(0,18,80,350,1000,maxMammalMass)
  if(method=="LewisWerdelin") cutpoints <- c(0,10,21,100, maxMammalMass)

  classes <- cut(sizes, breaks=cutpoints, labels = FALSE)
  return(classes)
}

