#'
#' Routine to reduce memory consumption of objects
#' 
#' @description Model estimation routines on large data set might produce
#' large output data tables that are not needed in subsequent analysis
#' but are a substantial burden to memory consumption. Trim routines
#' remove unnecessary data table and reduce memory usage of output objects
#' 
#' @param trimObject Object to be trimmed: It could be the
#' output of a statistical routine like glm or lm, or the
#' output of user-defined statistical functions; Depending
#' on the input object's class, the suitable trim function
#' will be called
#' @noRd
csslr.utils.trim <- function(trimObject, ...) {
  UseMethod("csslr.utils.trim")
}

#' @noRd
csslr.utils.trim.default <- function(trimObject,...) {
  theClass <- class(trimObject)
  theMessage <- paste0("No csslr.utils.trim function is implemented for objects of type ", theClass)
  stop(theMessage)
}

#' Routine to trim glm output keeping the predict functionality
#' 
#' See also: http://www.r-bloggers.com/trimming-the-fat-from-glm-models-in-r/
#' @noRd
csslr.utils.trim.glm <- function(trimObject) {
  trimObject$residuals <- c()
  trimObject$fitted.values <- c()
  trimObject$effects <- c()
  trimObject$qr$qr <- c()  
  trimObject$family$variance <- c()
  trimObject$family$dev.resids <- c()
  trimObject$family$aic <- c()
  trimObject$family$validmu <- c()
  trimObject$family$simulate <- c()
  trimObject$linear.predictors <- c()
  trimObject$weights <- c()
  trimObject$prior.weights <- c()
  trimObject$na.action <- c()
  trimObject$data <- c()
  trimObject$model <- c()
  trimObject$y <- c()
  attr(trimObject$terms, ".Environment") <- c()
  attr(trimObject$formula, ".Environment") <- c()
  
  return(trimObject)
}

#' Routine to trim roc output to reduce memory but keeping the ability to run roc.test
#' @noRd
csslr.utils.trim.roc <- function(trimObject, trim4plot=FALSE) {
  trimObject$cases <- c()
  trimObject$controls <- c()
  if (trim4plot == FALSE) {
    trimObject$sensitivities <- c()
    trimObject$oneMinusSpecificities <- c()
  }

  return(trimObject)
}

#' Routine to trim roc.test output to reduce memory
#' @noRd
csslr.utils.trim.htest <- function(trimObject) {
  trimObject$roc1 <- csslr.utils.trim(trimObject$roc1)
  trimObject$roc2 <- csslr.utils.trim(trimObject$roc2)

  return(trimObject)
}

#' Routine to trim speedglm objects
#' @noRd
csslr.utils.trim.speedglm <- function(trimObject) {
  trimObject <- csslr.utils.trim.glm(trimObject)

  return(trimObject)
}
