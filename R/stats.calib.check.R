#'
#' Spiegelhalter test on calibration quality of a single probability forecast
#' 
#' @description This routine implements the test of Spiegelhalter (Spiegelhalter D. J. (1986),
#' Probabilistic prediction in patient management and clinical trials, Statistics
#' in Medicine 5(5), 421–433). The null hypothesis is that the MSE is equal to
#' its expected value. If the null can be rejected this is a strong indication
#' that a model is poorly specified
#' 
#' @param indicators Vector of 0/1 values where 0 is a "good" observation
#' and "1" models a bad observation
#' @param probabilities Probability forecasts of a model
#' @return List with MSE, expected MSE for the probability forecast together
#' with test statistic and p-value of the Spiegelhalter test
#' 
#' @examples
#'
#' set.seed(123)
#' DT.data <- data.table(Response = c(rep(0,100),rep(1,100)))
#' DT.data[Response == 0, Variable := rnorm(100, 1.0, 1.0)]
#' DT.data[Response == 1, Variable := rnorm(100, -1.0, 1.0)]
#' 
#' responseVec <- DT.data[, Response]
#' variableVec <- DT.data[, Variable]
#' 
#' calibCheck <- csslr.stats.calib.check(responseVec, variableVec)
#' 
#' print(calibCheck$MSE)
#' print(calibCheck$EMSE)
#' print(calibCheck$p.value)
#'
#' @export
csslr.stats.calib.check <- function(indicators, probabilities) {
  numValues <- 1.0 * length(indicators)

  # Check for NAs and Infs
  if ((any(is.na(indicators)) | any(is.infinite(indicators))) == TRUE)
    stop("At least one of the indicators is NA or Inf")
  if ((any(is.na(probabilities)) | any(is.infinite(probabilities))) == TRUE)
    stop("At least one of the probabilities is NA or Inf")
  if (length(probabilities) != numValues)
    stop("Dimensions of indicators and probabilities are different")
  `%notin%` <- Negate(`%in%`)
  if (any(indicators %notin% c(0,1)))
    stop("At least on of the indicators is not 0 or 1")

  # Make double to avoid overflow with large integer numbers
	numValues <- 1.0 * numValues

	mse <- t(indicators - probabilities) %*% (indicators - probabilities)
	mse <- mse / numValues
	eMse <- t(probabilities) %*% (1 - probabilities)
	eMse <- eMse / numValues
	temp1 <- probabilities * (1.0 - probabilities)
	temp2 <- (1.0 - 2.0 * probabilities) * (1.0 - 2.0 * probabilities)
	varMse <- t(temp1) %*% temp2
	varMse <- varMse / numValues / numValues

	if (varMse > 0.0)
		statistic <- (mse - eMse) / sqrt(varMse)
	else {
		if ((mse - eMse) == 0.0)
			statistic = 0.0
		else
			statistic = Inf
	}
	if (statistic > 0.0) {
		pValue <- 2.0 * (1.0 - pnorm(statistic))
	} else {
		pValue <- 2.0 * pnorm(statistic)
	}

	ret = list()
	ret[["MSE"]] = mse;
	ret[["EMSE"]] = eMse;
	ret[["SMSE"]] = sqrt(varMse);
	ret[["statistic"]] = statistic;
	ret[["p.value"]] = pValue;

	class(ret) <- "csslr.stats.calib.check"
	
	return(ret)
}
