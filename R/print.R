
#' @export
print.csslr.model.analysis.lr.calib <- function(object, ...) {
  listNames <- names(object)

  df <- data.frame()
  for (name in listNames) {
    mse <- object[[name]]$MSE
    emse <- object[[name]]$EMSE
    statistic <- object[[name]]$statistic
    p.value <- object[[name]]$p.value
    df <- rbind(df, c(mse, emse, statistic, p.value))
  }
  listNames[1] <- "Full"
  for (i in 2:length(listNames))
    listNames[i] <- paste("without ", listNames[i], sep="")
  df$temp <- listNames
  df <- df[, c(5,1,2,3,4)]
  colnames(df) <- c('Model','MSE','EMSE','Test Statistic','p-value')

  cat("\nResults of the calibration check:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.calib.test <- function(object, ...) {
  listNames <- names(object)

  df <- data.frame()
  for (name in listNames) {
    statistic <- object[[name]]$statistic
    p.value <- object[[name]]$p.value
    df <- rbind(df, c(statistic, p.value))
  }
  df$temp <- listNames
  df <- df[, c(3,1,2)]
  colnames(df) <- c('Excluded Variable','Test Statistic','p-value')

  cat("\nResults of the incremental calibration test:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.glm.lr.test <- function(object, ...) {
  listNames <- names(object)

  df <- data.frame()
  for (name in listNames) {
    statistic <- object[[name]]$Chisq[2]
    p.value <- object[[name]]$`Pr(>Chisq)`[2]
    df <- rbind(df, c(statistic, p.value))
  }
  df$temp <- listNames
  df <- df[, c(3,1,2)]
  colnames(df) <- c("Excluded Variable", "Test Statistic", "p-value")
  
  cat("\nResults of the incremental Likelihood Ratio tests:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.roc <- function(object, ...) {
  listNames <- names(object)

  df <- data.frame()
  for (name in listNames) {
    if (getOption('csslr.use.ar') == FALSE) {
      powerMeasure <- object[[name]]$auc
    } else {
      powerMeasure <- object[[name]]$ar
    }
    numBads <- object[[name]]$numCases
    numGoods <- object[[name]]$numControls
    df <- rbind(df, c(powerMeasure, numBads, numGoods))
  }
  listNames[1] <- 'Full'
  for (i in seq(2, length(listNames), length.out = length(listNames) - 1))
    listNames[i] <- paste("without ", listNames[i], sep="")
  df$temp <- listNames
  df <- df[, c(4,1,2,3)]
  if (getOption('csslr.use.ar') == FALSE) {
    colnames(df) <- c('Model','AUC','#Bads','#Goods')
    cat("\nArea below the ROC curve:\n")
  } else {
    colnames(df) <- c('Model','AR','#Bads','#Goods')
    cat("\nAccuracy Ratio:\n")
  }

  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.ic <- function(object, ...) {
  listNames <- names(object)

  df <- data.frame()
  for (name in listNames) {
    aic <- object[[name]]$aic
    bic <- object[[name]]$bic
    df <- rbind(df, c(aic, bic))
  }
  listNames[1] <- "Full"
  for (i in seq(2, length(listNames), length.out = length(listNames) - 1))
    listNames[i] <- paste("without ", listNames[i], sep="")
  df$temp <- listNames
  df <- df[, c(3,1,2)]
  colnames(df) <- c('Model','AIC','BIC')

  cat("\nInformation Criteria:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.roc.test <- function(object, ...) {
  listNames <- names(object)

  df <- data.frame()
  for (i in seq(2, length(listNames), length.out = length(listNames) - 1)) {
    name <- listNames[i]
    statistic <- object[[name]]$statistic
    p.value <- object[[name]]$p.value
    df <- rbind(df, c(statistic, p.value))
  }
  if (length(listNames) > 1) {
    df$temp <- listNames[2:length(listNames)]
    df <- df[, c(3,1,2)]
    colnames(df) <- c('Excluded Variable','Test Statistic','p-value')
  }

  cat("\nResults of the incremental discriminative power test:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.compare.lr <- function(object, ...) {
  cat("\nResults of the model comparison:\n")
  cat("Mean probability model 1: ", object[["mean.prob"]][["lr.model1"]], "\n", sep="")
  cat("Mean probability model 2: ", object[["mean.prob"]][["lr.model2"]], "\n", sep="")
  if (!is.null(object[["ic"]][["glm.model1"]]$aic)) {
    cat("AIC model 1: ", object[["ic"]][["lr.model1"]]$aic, "\n", sep="")
    cat("AIC model 2: ", object[["ic"]][["lr.model2"]]$aic, "\n", sep="")
    cat("BIC model 1: ", object[["ic"]][["lr.model1"]]$bic, "\n", sep="")
    cat("BIC model 2: ", object[["ic"]][["lr.model2"]]$bic, "\n", sep="")
  }
  if (getOption('csslr.use.ar') == FALSE) {
    cat("Area below the ROC curve model 1: ", object[["roc.test"]]$roc1$auc, "\n", sep="")
    cat("Area below the ROC curve model 2: ", object[["roc.test"]]$roc2$auc, "\n", sep="")
    cat("Test on difference of AUC: ", object[["roc.test"]]$statistic, " (test statistic), ",
        object[["roc.test"]]$p.value, " (p-value)\n", sep="")
  } else {
    cat("Accuracy ratio of model 1: ", object[["roc.test"]]$roc1$ar, "\n", sep="")
    cat("Accuracy ratio of model 2: ", object[["roc.test"]]$roc2$ar, "\n", sep="")
    cat("Test on difference of AR: ", object[["roc.test"]]$statistic, " (test statistic), ",
        object[["roc.test"]]$p.value, " (p-value)\n", sep="")
  }
  cat("Calibration check for model 1: ", object[["calib"]][["lr.model1"]]$MSE, " (MSE), ",
      object[["calib"]][["lr.model1"]]$EMSE, " (EMSE), ", object[["calib"]][["lr.model1"]]$statistic,
      " (test statistic), ", object[["calib"]][["lr.model1"]]$p.value, " (p-value)\n", sep="")
  cat("Calibration check for model 2: ", object[["calib"]][["lr.model2"]]$MSE, " (MSE), ",
      object[["calib"]][["lr.model2"]]$EMSE, " (EMSE), ", object[["calib"]][["lr.model2"]]$statistic,
      " (test statistic), ", object[["calib"]][["lr.model2"]]$p.value, " (p-value)\n", sep="")
  cat("Test on difference of MSE: ", object[["calib.test"]]$statistic, " (test statistic), ",
      object[["calib.test"]]$p.value, " (p-value)\n", sep="")
}

#' @export
print.csslr.stats.roc <- function(object, ...) {
  if (getOption('csslr.use.ar') == FALSE) {
    cat("\nArea below the ROC curve:\n", object$auc)
  } else {
    cat("\nAccuracy Ratio:\n", 2.0 * object$auc - 1.0)
  }
}
