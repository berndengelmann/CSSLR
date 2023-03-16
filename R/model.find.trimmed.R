#'
#' Routine to trim models in a selection step
#'
#' @description Routine to identify variable that might be removed from models in case they became
#' less important after more variable have been selected
#' 
#' @param modelList List of model that are tested for trimming
#' @param topModels List containing AUC and MSE together with formulas of the best models
#' @param modelsTestedList List of models that already have been evaluated in previous steps;
#' this list created merely for performance reasons to avoid multiple testing of the same model
#' @param DT.data Data set containing the response variable and all independent variables
#' @param selectionMode Variables that controls when a model should be considered as an
#' improvement over a reference model: Either AUC and MSE have to show an improvement ('AUC_and_MSE')
#' or, alternatively, one has to show an improvement and the other no deterioration ('AUC_or_MSE')
#' @param pAUCTrim When removing a variable, AUC is considered as improved if its value has increased and
#' the p-value of the AUC-test is below pAUCTrim
#' @param pMSETrim When removing a variable, MSE is considered as improved if its value has decreased and
#' the p-value of the MSE-test is below pMSETrim
#' @param debugLevel Parameter that controls printing on screen when running the selection algorithm (0: No output
#' on screen; 1: Limited output showing the current progress step; 2: Detailed output of the model currently analyzed)
#' @return A list containing three items: A list of trimmed (or left unchanged, depending on the outcome
#' of the tests) models an update of modelsTestedList, and a data.table containing the results of the tests
#' and decisions about all models from the list
#' @noRd
csslr.model.find.trimmed <- function(modelList, topModels, modelsTestedList, DT.data, selectionMode,
                                     pAUCTrim, pMSETrim, debugLevel) {
  DT.report <- data.table()
  trimmedModels <- c()
  for (theFormula in modelList) {
    if (debugLevel > 1) {
      formulaString <- paste(deparse(theFormula, width.cutoff = 500), collapse="")
      print(paste0('Test for Trimming: ', formulaString))
    }
    responseVar <- rlang::f_lhs(theFormula)
    numVars <- length(all.vars(theFormula)[-1])
    if (numVars == 1) {
      trimCompleted <- TRUE
      trimDecision <- 'Unchanged'
    } else {
      theModel <- csslr.model.analysis.lr(theFormula, DT.data, lr=F, calib=T, roc=T, ic=F, quiet=T, dataValidation = F)
      trimCompleted <- FALSE
      trimDecision <- 'Unchanged'
      while (trimCompleted == FALSE) {
        numVars <- length(theModel$roc.test) - 1
        varNames <- all.vars(theFormula)[-1]
        aucModel <- as.numeric(theModel$roc$full.model$auc)
        mseModel <- as.numeric(theModel$calib$full.model$MSE)
        # Check if model can be improved by removing a variable
        removeIdx <- 0
        for (i in 1:numVars) {
          auc <- as.numeric(theModel[['roc']][[varNames[i]]][['auc']])
          pValAUC <- as.numeric(theModel[['roc.test']][[varNames[i]]][['p.value']])
          mse <- as.numeric(theModel[['calib']][[varNames[i]]][['MSE']])
          pValMSE <- as.numeric(theModel[['calib.test']][[varNames[i]]][['p.value']])
          noAUCImprovement <- FALSE
          noMSEImprovement <- FALSE
          if (auc > aucModel | (auc < aucModel & pValAUC > pAUCTrim)) {
            noAUCImprovement <- TRUE
          }
          if (mse < mseModel | (mse > mseModel & pValMSE > pMSETrim)) {
            noMSEImprovement <- TRUE
          }
          if (selectionMode == 'AUC_or_MSE' & (noAUCImprovement == TRUE & noMSEImprovement == TRUE)) {
            removeIdx <- i
            break
          }
          if (selectionMode == 'AUC_and_MSE' & (noAUCImprovement == TRUE | noMSEImprovement == TRUE)) {
            removeIdx <- i
            break
          }
        }
        if (removeIdx == 0) {
          trimCompleted <- TRUE
        } else {
          reducedVars <- varNames[varNames[removeIdx] != varNames]
          reducedVars <- ifelse(grepl('[^[:alnum:]]', reducedVars), paste0('`', reducedVars, '`'), reducedVars)
          reducedVars <- sort(reducedVars)
          theFormula <- as.formula(paste(c(responseVar, paste(reducedVars, collapse = ' + ')), collapse = ' ~ '))
          theModel <- csslr.model.analysis.lr(theFormula, DT.data, lr=F, calib=T, roc=T, ic=F, quiet=T, dataValidation = F)
          trimDecision <- 'Trimmed'
          if (length(reducedVars) == 1) {
            trimCompleted <- TRUE
          }
          # Check whether top models have changed
          if (auc > topModels[['AUC']]) {
            topModels[['AUC']] <- auc
            topModels[['AUC_Formula']] <- theFormula
          }
          if (mse < topModels[['MSE']]) {
            topModels[['MSE']] <- mse
            topModels[['MSE_Formula']] <- theFormula
          }
        }
      }
    }
    theString <- paste(deparse(theFormula, width.cutoff = 500), collapse="")
    if (!(theString %in% modelsTestedList)) {
      modelsTestedList <- c(modelsTestedList, theString)
    }
    trimmedModels <- c(trimmedModels, theFormula)
    DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theFormula, width.cutoff = 500), collapse="")),
                          Decision = trimDecision)
    DT.report <- rbind(DT.report, DT.temp)
  }
  
  returnList <- list()
  returnList[['ModelsTrimmed']] <- trimmedModels
  returnList[['ModelsTested']] <- modelsTestedList
  returnList[['Report']] <- DT.report
  returnList[['TopModels']] <- topModels

  return(returnList)
}
