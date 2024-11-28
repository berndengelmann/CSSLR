#'
#' Routine for performing incremental analysis of a logistic regression model
#' 
#' @description This routine performs an incremental analysis of a logistic
#' regression model. Each variable is removed one-by-one and the impact of each
#' variable on the full model is measured in terms of information criteria, area
#' under the ROC curve and MSE. Furthermore, variance inflation factors are
#' computed to allow a check for multi-collinearity
#' 
#' @param modelFormula Model for which the incremental analysis should be performed
#' @param DT.data Data table containing all variables needed for model estimation
#' @param panelDataIdentifier For panel data sets a variable identifying data sets
#' belonging to the same entity can be defined. This leads to adjustments in the
#' p-values for testing the significance of model coefficients
#' @param roc If TRUE area under the ROC curve together with a test on difference
#' of AUROC between the full and the reduced models is performed
#' @param calib If TRUE MSE is computed together with a calibration check and the
#' test on difference of MSE between the full and the reduced models
#' @param ic If TRUE the information criteria AIC and BIC are computed for the
#' full and the reduced models
#' @param lr If TRUE likelihood ratio tests for model coefficients are performed
#' in addition to the Wald test that is carried out by default
#' @param quiet If TRUE output on the progress of the function is suppressed
#' @param incrementVariable If TRUE the incremental test is performed on variable
#' level not model part level. This means, if a variable occurs in multiple
#' parts of a model formula, e.g. due to interactions, it is completely
#' removed to evaluate its impact
#' @param fullModelTrim Trim the glm model results object to save memory but still
#' be able to do predictions with the model
#' @param dataValidation If true, checks for NA and Inf in the data are performed
#' and invalid data sets are removed; if FALSE it is assumed that input data is clean
#' @return A list of tables containing the outcome of the various diagnostics
#' for the full and the reduced models
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
#' modelFormula <- Response ~ Strong1 + Strong2 + Random2
#' modelResults <- csslr.model.analysis.lr(modelFormula, DT.data)
#' 
#' # Print model summary
#' print(modelResults$model.summary)
#' 
#' # Predict probability of being "bad"
#' badProbs <- predict(modelResults$full.model, type = 'response', DT.data)
#' 
#' # Incremental Analysis
#' print(modelResults$ic)
#' print(modelResults$roc)
#' print(modelResults$roc.test)
#' print(modelResults$calib)
#' print(modelResults$calib.test)
#' 
#' @export
csslr.model.analysis.lr <- function(modelFormula, DT.data, panelDataIdentifier='',
                                    roc = TRUE, calib = TRUE, ic = TRUE, lr = FALSE,
                                    quiet = FALSE, incrementVariable = TRUE,
                                    fullModelTrim = TRUE, dataValidation = TRUE) {
  modelResults <- list()

  # Definition of the glm function
  if (getOption('csslr.speed.glm') == FALSE) {
    glmFunction <- 'glm'
  } else {
    glmFunction <- 'speedglm'
  }

  # Parsing the model formula
  responseVar <- rlang::f_lhs(modelFormula)
  allVariables <- all.vars(modelFormula)[-1]

  # In incremental analysis variables are removed one-by-one using grepl
  # This will not work if variable like Var1, Var10, Var100 are included in the data
  # The purpose of this part is to identify variables with names included
  # in other names to identify the correct parts of the model formula that have to be
  # changed during an incremental analysis
  includedVariables <- list()
  for (vbl in incrementVariable) {
    includedVariables[[vbl]] <- c()
    for (vbl2 in allVariables) {
      if (vbl != vbl2 & grepl(vbl, vbl2, fixed = T) == TRUE) {
        includedVariables[[vbl]] <- c(includedVariables[[vbl]], vbl2)
      }
    }
  }

  # Removing NA's to make sure all applications run through
  if (dataValidation == TRUE) {
    for (vbl in allVariables) {
      DT.data <- DT.data[!is.infinite(get(vbl))]
      DT.data <- DT.data[!is.na(get(vbl))]
    }
  }

  # Check if these is still some data left
  if (nrow(DT.data) == 0)
    stop('After removing NA and Inf of all variables in the model formula the data set is empty')
  
  # Workaround to prevent an error in vif (not finding DT.data)
  .GlobalEnv$.csslrEnvnmt <- new.env()
  .GlobalEnv$.csslrEnvnmt$dt <- DT.data
  .GlobalEnv$.csslrEnvnmt$fmla <- as.formula(modelFormula)

  # Estimation of the full model
  if (quiet == FALSE)
    print("Estimation of the full model...")
  fullModel <- glm(formula=.csslrEnvnmt$fmla, data=.csslrEnvnmt$dt, family=binomial(link="logit"), y=FALSE, model=FALSE)

  # Model summary and model trimming (Store model formula separately because it gets lost otherwise)
  # Adjust the model summary for the panel structure of the data set if necessary
  modelResults[["model.summary"]] <- summary(fullModel)
  if (panelDataIdentifier != '') {
    # Adjust standard error
    adjCov <- sandwich::vcovPC(fullModel, cluster = DT.data[, get(panelDataIdentifier)])
    ct <- lmtest::coeftest(fullModel, vcov = adjCov)
    modelResults[["model.summary"]]$coefficients[,2:4] <- ct[,2:4]
  }
  modelResults[["model.summary"]]$call$formula <- modelFormula
  modelResults[["model.summary"]]$deviance.resid <- NA
  fullModel.aic <- AIC(fullModel)
  fullModel.bic <- BIC(fullModel)
  # VIF Calculation is pointless for models with one variable
  if (length(allVariables) > 1) {
    tryCatch ({
      fullModel.vif <- "VIF calculation was unsuccessful"
      fullModel.vif <- car::vif(fullModel)
      # Data transformation necessary of fullModel.vif is not of class matrix
      if (class(fullModel.vif)[1] == "numeric") {
        tempVif <- data.frame(fullModel.vif)
        colnames(tempVif) <- c('GVIF')
        tempVif$Df <- 1
        tempVif$GVIVTEMP <- tempVif$GVIF^(1/2)
        colnames(tempVif) <- c('GVIF', 'Df', 'GVIF^(1/(2*Df))')
        fullModel.vif <- data.table(tempVif)
      } else {
        fullModel.vif <- data.table(fullModel.vif)
      }
    }, error = function(e) {
      fullModel.vif <- e
    }, finally = {
      # nothing
    })
  } else {
    fullModel.vif <- "No meaningful VIF can be computed for models with one variable only"
  }
  
  # After completion of vif, the new environment is no longer needed
  rm(.csslrEnvnmt, envir = .GlobalEnv)

  # Saving the output
  if (lr == FALSE & fullModelTrim == TRUE) {
    fullModel <- csslr.utils.trim(fullModel)
  }
  modelResults[["full.model"]] <- fullModel
  modelResults[["vif"]] <- fullModel.vif

  if (ic == TRUE) {
    modelResults[["ic"]] <- list()
    modelResults[["ic"]][["full.model"]] <- data.frame(aic=fullModel.aic, bic=fullModel.bic)
    class(modelResults[["ic"]]) <- "csslr.model.analysis.lr.ic"
  }

  # Definition of the appropriate increments for incremental analysis
  if (incrementVariable == TRUE) {
    increments <- allVariables
  } else {
    increments <- attr(fullModel$terms, "term.labels")
  }

  # Predictions of the full model and the reduced models
  if ((roc== TRUE) | (calib==TRUE) | (ic==TRUE) | (lr==TRUE)) {
    if (quiet == FALSE)
      print("Predictions of the reduced models...")

    DT.temp <- data.table(predict(fullModel, type="response", DT.data))
    fullModelName <- "full.model"
    setnames(DT.temp, "V1", fullModelName)
    modelNames <- c(fullModelName)

    # Likelihood ratio tests output object
    if (lr == TRUE) {
      modelResults[["lr.test"]] <- list()
      class(modelResults[["lr.test"]]) <- "csslr.model.analysis.lr.lr.test"
    }

    for (increment in increments) {
      # No automatic solution for the case of one variable only
      if (length(increments) > 1) {
        modelComponents <- attr(fullModel$terms, "term.labels")
        if (incrementVariable == F & grepl(':', increment, fixed = T)) {
          reducedComponents <- modelComponents[modelComponents != increment.tmp]
        } else {
          reducedComponents <- modelComponents[!grepl(increment, modelComponents, fixed = T)]
          if (increment %in% names(includedVariables)) {
            keepVariables <- includedVariables[[increment]]
            for (vbl in keepVariables) {
              keepComponents <- modelComponents[grepl(vbl, modelComponents, fixed = T)]
              reducedComponents <- unique(c(reducedComponents, keepComponents))
            }
          }
        }
        # Make sure the formula is not empty
        if (length(reducedComponents) == 0) {
          reducedComponents <- c('1')
        }
        reducedFormula <- as.formula(paste(c(responseVar, paste(reducedComponents, collapse = ' + ')), collapse = ' ~ '))
      } else {
        reducedFormula <- as.formula(paste(c(responseVar, '1'), collapse = ' ~ '))
      }
      reducedModel <- get(glmFunction)(as.formula(reducedFormula), data=DT.data, family=binomial(link="logit"), y=FALSE, model=FALSE)
      if (ic == TRUE) {
        reducedModel.aic <- AIC(reducedModel)
        reducedModel.bic <- BIC(reducedModel)
        modelResults[["ic"]][[increment]] <- data.frame(aic=reducedModel.aic, bic=reducedModel.bic)
      }
      # Predictions are needed for AUC and MSE only
      if ((roc == TRUE) | (calib == TRUE)) {
        DT.temp[, tempName := predict(reducedModel, type="response", DT.data)]
        setnames(DT.temp, "tempName", increment)
        modelNames <- c(modelNames, increment)
      }
      # Carry out the lr test if it is needed
      if (lr == TRUE) {
        lrTestResult <- lrtest(fullModel, reducedModel)
        modelResults[["lr.test"]][[increment]] <- lrTestResult
      }
      rm(reducedModel)
    }
  }

  # Save the reduced full model for later predictions
  # If no lr tests are needed it has been saved already
  if (lr == TRUE) {
    fullModel <- csslr.utils.trim(fullModel)
    modelResults[["full.model"]] <- fullModel
  }

  # ROC analysis
  if (roc == TRUE) {
    if (quiet == FALSE)
      print("ROC analysis...")
    modelResults[["roc"]] <- list()
    modelResults[["roc.test"]] <- list()
    modelRoc <- pROC::roc(DT.data[, get(responseVar)], DT.temp[, get(modelNames[1])], quiet = TRUE)
    modelRoc$numCases <- length(modelRoc$cases)
    modelRoc$numControls <- length(modelRoc$controls)
    saveModelRoc <- csslr.utils.trim(modelRoc, trim4plot = TRUE)
    if (getOption('csslr.use.ar') == TRUE) {
      saveModelRoc$ar <- 2.0 * saveModelRoc$auc - 1.0
      saveModelRoc$auc <- NULL
    }
    modelResults[["roc"]][[modelNames[1]]] <- saveModelRoc
    modelResults[["roc.test"]][[modelNames[1]]] <- saveModelRoc
    for (i in seq(2, length(modelNames), length.out = max(0, length(modelNames) - 1))) {
      tempRoc <- pROC::roc(DT.data[, get(responseVar)], DT.temp[, get(modelNames[i])], quiet = TRUE)
      tempRoc$numCases <- length(tempRoc$cases)
      tempRoc$numControls <- length(tempRoc$controls)
      tempRocTest <- pROC::roc.test(modelRoc, tempRoc, method = 'delong', quiet = TRUE)
      tempRoc <- csslr.utils.trim(tempRoc)
      tempRocTest <- csslr.utils.trim(tempRocTest)
      if (getOption('csslr.use.ar') == TRUE) {
        tempRoc$ar <- 2.0 * tempRoc$auc - 1.0
        tempRoc$auc <- NULL
        tempRocTest$roc1$ar <- 2.0 * tempRocTest$roc1$auc - 1.0
        tempRocTest$roc1$auc <- NULL
        tempRocTest$roc2$ar <- 2.0 * tempRocTest$roc2$auc - 1.0
        tempRocTest$roc2$auc <- NULL
      }
      modelResults[["roc"]][[modelNames[i]]] <- tempRoc
      modelResults[["roc.test"]][[modelNames[i]]] <- tempRocTest
    }
    rm(modelRoc)
    class(modelResults[["roc"]]) <- "csslr.model.analysis.lr.roc"
    class(modelResults[["roc.test"]]) <- "csslr.model.analysis.lr.roc.test"
  }

  # MSE analysis
  if (calib == TRUE) {
    if (quiet == FALSE)
      print("MSE analysis...")
    modelResults[["calib"]] <- list()
    modelResults[["calib.test"]] <- list()
    modelCalib <- csslr.stats.calib.check(DT.data[, get(responseVar)], DT.temp[, get(modelNames[1])])
    modelResults[["calib"]][[modelNames[1]]] <- modelCalib
    for (i in seq(2, length(modelNames), length.out = max(0, length(modelNames) - 1))) {
      tempCalib <- csslr.stats.calib.check(DT.data[, get(responseVar)], DT.temp[, get(modelNames[i])])
      tempCalibTest <- csslr.stats.calib.test(DT.data[, get(responseVar)], DT.temp[, get(modelNames[1])], DT.temp[, get(modelNames[i])])
      modelResults[["calib"]][[modelNames[i]]] <- tempCalib
      modelResults[["calib.test"]][[modelNames[i]]] <- tempCalibTest
    }
    class(modelResults[["calib"]]) <- "csslr.model.analysis.lr.calib"
    class(modelResults[["calib.test"]]) <- "csslr.model.analysis.lr.calib.test"
  }

  class(modelResults) <- "csslr.model.analysis.lr"

  gc()
  return(modelResults)
}
