#'
#' Routine for comparing two logistic regression models
#' 
#' @description This routine compares two logistic regression model. It computes the
#' mean probability forecast of each model, performs tests on difference
#' in AUROC and MSE using the tests in pROC and csslr.stats
#' 
#' @param lr1 Logistic regression Model 1 (provided either as glm object or formula)
#' @param lr2 Logistic regression Model 2 (provided either as glm object or formula)
#' @param DT.data Data set containing all variables needed for model estimation
#' @return Object containing the estimation results of the individual models,
#' information criteria and p-values of tests for difference in AUROC and MSE
#'
#' @examples
#'
#' set.seed(123)
#' DT.data <- data.table(Response = c(rep(0,100),rep(1,100)))
#' DT.data[Response == 0, Strong1 := rnorm(100, 1.0, 1.0)]
#' DT.data[Response == 1, Strong1 := rnorm(100, -1.0, 1.0)]
#' DT.data[Response == 0, Strong2 := rnorm(100, 1.0, 1.0)]
#' DT.data[Response == 1, Strong2 := rnorm(100, -1.0, 1.0)]
#' DT.data[Response == 0, Weak1 := rnorm(100, 0.5, 1.0)]
#' DT.data[Response == 1, Weak1 := rnorm(100, -0.5, 1.0)]
#' DT.data[Response == 0, Weak2 := rnorm(100, 0.5, 1.0)]
#' DT.data[Response == 1, Weak2 := rnorm(100, -0.5, 1.0)]
#' DT.data[Response == 0, Random1 := rnorm(100, 0.0, 1.0)]
#' DT.data[Response == 1, Random1 := rnorm(100, 0.0, 1.0)]
#' DT.data[Response == 0, Random2 := rnorm(100, 0.0, 1.0)]
#' DT.data[Response == 1, Random2 := rnorm(100, 0.0, 1.0)]
#' 
#' modelFormula1 <- Response ~ Strong1 + Strong2 + Random2
#' modelFormula2 <- Response ~ Strong2 + Weak1 + Weak2 + Random1
#'
#' modelComparison <- csslr.model.compare.lr(modelFormula1, modelFormula2, DT.data)
#'
#' # Print the outcome
#' print(modelComparison)
#'
#' @export
csslr.model.compare.lr <- function(...) {
  UseMethod("csslr.model.compare.lr")
}

# Default routine computing models from formulas it necessary
#' @export
csslr.model.compare.lr.default <- function(lr1, lr2, DT.data) {
  # Definition of the glm function
  if (getOption('csslr.speed.glm') == FALSE) {
    glmFunction <- 'glm'
  } else {
    glmFunction <- 'speedglm'
  }

  # Check if lr1 is a formula
  if (class(lr1)[1] == "formula") {
    modelFormula <- lr1
    lr1 <- get(glmFunction)(lr1, data=DT.data, family=binomial(link='logit'), y=FALSE, model=FALSE)
    lr1$formula <- modelFormula
  }
  # Check if lr2 is a formula
  if (class(lr2)[1] == "formula") {
    modelFormula <- lr2
    lr2 <- get(glmFunction)(lr2, data=DT.data, family=binomial(link='logit'), y=FALSE, model=FALSE)
    lr2$formula <- modelFormula
  }

  compareResults <- csslr.model.compare.lr_model(lr1, lr2, DT.data)
  return(compareResults)
}

#' Routine for comparing two glm objects that are outcomes of logistic regressions
#' @noRd
csslr.model.compare.lr_model <- function(glmObject1, glmObject2, DT.data) {
  if (class(glmObject1)[1] != "glm" & class(glmObject1)[1] != "speedglm")
    stop("glmObject1 is not an object of the class glm or speedglm")
  if (class(glmObject2)[1] != "glm" & class(glmObject2)[1] != "speedglm")
    stop("glmObject2 is not an object of the class glm or speedglm")

  responseVar1 <- as.character(glmObject1$formula[[2]])
  responseVar2 <- as.character(glmObject2$formula[[2]])
  if (responseVar1 != responseVar2)
    stop("The glm models glmObject1 and glmObject2 have different response variables")

  compareResults <- list()

  temp0 <- DT.data[, get(responseVar1)]
  temp1 <- DT.data[, predict(glmObject1, type="response", DT.data)]
  temp2 <- DT.data[, predict(glmObject2, type="response", DT.data)]

  # Remove NAs to ensure the following algorithms run smoothly
  temp1 <- temp1[!is.na(temp1) & !is.na(temp2)]
  temp2 <- temp2[!is.na(temp1) & !is.na(temp2)]

  compareResults[["mean.prob"]] <- list()
  compareResults[["mean.prob"]][["lr.model1"]] <- mean(temp1)
  compareResults[["mean.prob"]][["lr.model2"]] <- mean(temp2)

  tempRoc1 <- pROC::roc(temp0, temp1, quiet = TRUE)
  tempRoc2 <- pROC::roc(temp0, temp2, quiet = TRUE)
  tempRocTest <- pROC::roc.test(tempRoc1, tempRoc2, method = 'delong', quiet = TRUE)
  tempRocTest <- csslr.utils.trim(tempRocTest)
  if (getOption('csslr.use.ar') == TRUE) {
    tempRocTest$roc1$ar <- 2 * tempRocTest$roc1$auc - 1
    tempRocTest$roc2$ar <- 2 * tempRocTest$roc2$auc - 1
  }
  compareResults[["roc.test"]] <- tempRocTest

  tempCalib1 <- csslr.stats.calib.check(temp0, temp1)
  tempCalib2 <- csslr.stats.calib.check(temp0, temp2)
  compareResults[["calib"]] <- list()
  compareResults[["calib"]][["lr.model1"]] <- tempCalib1
  compareResults[["calib"]][["lr.model2"]] <- tempCalib2
  tempCalibTest <- csslr.stats.calib.test(temp0, temp1, temp2)
  compareResults[["calib.test"]] <- tempCalibTest
  
  model1.aic <- AIC(glmObject1)
  model1.bic <- BIC(glmObject1)
  model2.aic <- AIC(glmObject2)
  model2.bic <- BIC(glmObject2)
  compareResults[["ic"]][["lr.model1"]] <- data.frame(aic=model1.aic, bic=model1.bic)
  compareResults[["ic"]][["lr.model2"]] <- data.frame(aic=model2.aic, bic=model2.bic)

  compareResults[['summary']][['lr.model1']] <- summary(glmObject1)
  compareResults[['summary']][['lr.model2']] <- summary(glmObject2)
  glmObject1 <- csslr.utils.trim(glmObject1)
  glmObject2 <- csslr.utils.trim(glmObject2)
  compareResults[["full.model"]][['lr.model1']] <- glmObject1
  compareResults[["full.model"]][['lr.model2']] <- glmObject2

  class(compareResults) <- "csslr.model.compare.lr"

  gc()
  return(compareResults)
}
