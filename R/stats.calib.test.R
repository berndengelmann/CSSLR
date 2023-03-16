#'
#' Redelmeier test on comparing the MSE of two different probability forecasts
#' 
#' @description This routine implements the Redelmeier et al (Redelmeier D. A.,
#' Bloch D. A. & Hickam D. H. (1991), Assessing predictive accuracy: How to compare
#' Brier scores’, Journal of Clinical Epidemiology 44(11), 1141–1146) test on the
#' difference of MSE for two probability forecasts. The null hypothesis is that the
#' MSE of both probability forecasts is equal
#' 
#' @param indicators Vector of 0/1 values where 0 is a "good" observation
#' and "1" models a bad observation
#' @param probabilities1 Probability forecasts of Model 1
#' @param probabilities2 Probability forecasts of Model 2
#' @return List with MSE, expected MSE for both probability forecasts together
#' with test statistic and p-value of the Redelmeier et al test
#' @export
csslr.stats.calib.test <- function(indicators, probabilities1, probabilities2) {
  numValues <- 1.0 * length(indicators)

  # Check for NAs and Infs
  if ((any(is.na(indicators)) | any(is.infinite(indicators))) == TRUE)
    stop("At least one of the indicators is NA or Inf")
  if ((any(is.na(probabilities1)) | any(is.infinite(probabilities1))) == TRUE)
    stop("At least one of the probabilities1 is NA or Inf")
  if ((any(is.na(probabilities2)) | any(is.infinite(probabilities2))) == TRUE)
    stop("At least one of the probabilities2 is NA or Inf")
  if (length(probabilities1) != numValues)
    stop("Dimensions of indicators and probabilities1 are different")
  if (length(probabilities2) != numValues)
    stop("Dimensions of indicators and probabilities2 are different")
  `%notin%` <- Negate(`%in%`)
  if (any(indicators %notin% c(0,1)))
    stop("At least on of the indicators is not 0 or 1")

  # Make double to avoid overflow with large integer numbers
	numValues <- 1.0 * numValues

	mse1 <- t(indicators - probabilities1) %*% (indicators - probabilities1)
	mse1 <- mse1 / numValues
	mse2 <- t(indicators - probabilities2) %*% (indicators - probabilities2)
	mse2 <- mse2 / numValues
	eMse1 <- t(probabilities1) %*% (1 - probabilities1)
	eMse1 <- eMse1 / numValues
	eMse2 <- t(probabilities2) %*% (1 - probabilities2)
	eMse2 <- eMse2 / numValues
	mseDiff <- t(probabilities1) %*% probabilities1 - t(probabilities2) %*% probabilities2 -
	  2.0 * t(probabilities1 - probabilities2) %*% indicators
	temp1 <- (probabilities1 - probabilities2) * (probabilities1 - probabilities2)
	temp2 <- (probabilities1 + probabilities2) * (2.0 - probabilities1 - probabilities2)
	varMseDiff <- t(temp1) %*% temp2

	if (varMseDiff > 0.0)
	  statistic <- mseDiff / sqrt(varMseDiff)
	else {
	  if (mseDiff == 0.0)
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
	ret[["MSE1"]] = mse1;
	ret[["EMSE1"]] = eMse1;
	ret[["MSE2"]] = mse2;
	ret[["EMSE2"]] = eMse2;
	ret[["statistic"]] = statistic;
	ret[["p.value"]] = pValue;

	class(ret) <- "csslr.stats.calib.test"

	return(ret)
}
