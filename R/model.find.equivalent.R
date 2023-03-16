#'
#' Routine to identify equivalent models in a selection step
#'
#' @description Routine to identify models that are equivalent to one or two models that have
#' been identifies as leading models in terms of AUC and MSE
#' 
#' @param modelsCandidates List with model formulas; this vector contains all
#' candidate models that are compared with the leading models in terms of AUC and MSE
#' @param modelsLeading List containing one or two models that were identified
#' as most superior models in terms of AUC and MSE
#' @param DT.data Data set containing the response variable and all independent variables 
#' @param pAUC When testing for equivalence in AUC, the p-value of the AUC-test has to be
#' greater than pAUC to conclude that two models are equivalent with respect to AUC
#' @param pMSE When testing for equivalence in MSE, the p-value of the MSE-test has to be
#' greater than pMSE to conclude that two models are equivalent with respect to MSE
#' @param maxEquivalentModels If p-values are set rather high, the number of models selected might become very
#' high; to keep the outcome and computational time manageable, a maximum number of equivalent models selected
#' in each selection step can be capped by this parameter
#' @param debugLevel Parameter that controls printing on screen when running the selection algorithm (0: No output
#' on screen; 1: Limited output showing the current progress step; 2: Detailed output of the model currently analyzed)
#' @return A list containing two items: A list of models that are identified as equivalent
#' and a data.table containing the results of the tests and decision about a candidate model
#' @noRd
csslr.model.find.equivalent <- function(modelsCandidates, modelsLeading, DT.data, pAUC, pMSE,
                                        maxEquivalentModels, debugLevel) {
  leadingModelsIncluded <- FALSE
  DT.report <- data.table()
  modelsEquivalent = modelsLeading
  for (i in 1:length(modelsCandidates)) {
    theCandidate <- modelsCandidates[[i]]
    if (debugLevel > 1) {
      candidateString <- paste(deparse(theCandidate, width.cutoff = 500), collapse="")
      print(paste0('Test for Equivalent: ', candidateString))
    }
    equivalOutcome <- c()
    modelComps <- list()
    for (j in 1:length(modelsLeading)) {
      theReference <- modelsLeading[[j]]
      modelComp <- csslr.model.compare.lr(theCandidate, theReference, DT.data)
      modelComps[[j]] <- modelComp
      aucEval <- ''
      if (modelComp$roc.test$roc1$auc >= modelComp$roc.test$roc2$auc & modelComp$roc.test$p.value <= pAUC) {
        aucEval <- 'superior'
      } else if (modelComp$roc.test$roc1$auc <= modelComp$roc.test$roc2$auc & modelComp$roc.test$p.value <= pAUC) {
        aucEval <- 'inferior'
      } else {
        aucEval <- 'equivalent'
      }
      mseEval <- ''
      if (modelComp$calib.test$MSE1 >= modelComp$calib.test$MSE2 & modelComp$calib.test$p.value <= pMSE) {
        mseEval <- 'inferior'
      } else if (modelComp$calib.test$MSE1 <= modelComp$calib.test$MSE2 & modelComp$calib.test$p.value <= pMSE) {
        mseEval <- 'superior'
      } else {
        mseEval <- 'equivalent'
      }
      if (aucEval == 'superior' | mseEval == 'superior' | (aucEval == 'equivalent' & mseEval == 'equivalent')) {
        equivalOutcome <- c(equivalOutcome, 'equivalent')
      } else {
        equivalOutcome <- c(equivalOutcome, 'inferior')
      }
      if (leadingModelsIncluded == FALSE) {
        if (length(modelsLeading) == 1) {
          DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theReference, width.cutoff = 500), collapse="")),
                                AUC = as.numeric(modelComp$roc.test$roc2$auc),
                                `AUC1-test p-value` = NA_real_,
                                MSE = as.numeric(modelComp$calib.test$MSE2),
                                `MSE1-test p-value` = NA_real_,
                                Decision = 'Leading')
        } else {
          DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theReference, width.cutoff = 500), collapse="")),
                                AUC = as.numeric(modelComp$roc.test$roc2$auc),
                                `AUC1-test p-value` = NA_real_,
                                `AUC2-test p-value` = NA_real_,
                                MSE = as.numeric(modelComp$calib.test$MSE2),
                                `MSE1-test p-value` = NA_real_,
                                `MSE2-test p-value` = NA_real_,
                                Decision = 'Leading')
        }
        DT.report <- rbind(DT.report, DT.temp)
        if (j == length(modelsLeading)) {
          leadingModelsIncluded <- TRUE
        }
      }
    }
    if (all(equivalOutcome == 'equivalent')) {
      modelsEquivalent[[length(modelsEquivalent) + 1]] <- theCandidate
      if (length(modelsLeading) == 1) {
        DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theCandidate, width.cutoff = 500), collapse="")),
                              AUC = as.numeric(modelComp$roc.test$roc1$auc),
                              `AUC1-test p-value` = as.numeric(modelComp$roc.test$p.value),
                              MSE = as.numeric(modelComp$calib.test$MSE1),
                              `MSE1-test p-value` = as.numeric(modelComp$calib.test$p.value),
                              Decision = 'Equivalent')
      } else {
        DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theCandidate, width.cutoff = 500), collapse="")),
                              AUC = as.numeric(modelComps[[1]]$roc.test$roc1$auc),
                              `AUC1-test p-value` = as.numeric(modelComps[[1]]$roc.test$p.value),
                              `AUC2-test p-value` = as.numeric(modelComps[[2]]$roc.test$p.value),
                              MSE = as.numeric(modelComps[[1]]$calib.test$MSE1),
                              `MSE1-test p-value` = as.numeric(modelComps[[1]]$calib.test$p.value),
                              `MSE2-test p-value` = as.numeric(modelComps[[2]]$calib.test$p.value),
                              Decision = 'Equivalent')
      }
      DT.report <- rbind(DT.report, DT.temp)
    } else {
      if (length(modelsLeading) == 1) {
        DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theCandidate, width.cutoff = 500), collapse="")),
                              AUC = as.numeric(modelComp$roc.test$roc1$auc),
                              `AUC1-test p-value` = as.numeric(modelComp$roc.test$p.value),
                              MSE = as.numeric(modelComp$calib.test$MSE1),
                              `MSE1-test p-value` = as.numeric(modelComp$calib.test$p.value),
                              Decision = 'Rejected')
      } else {
        DT.temp <- data.table(Model = gsub('`', '', paste(deparse(theCandidate, width.cutoff = 500), collapse="")),
                              AUC = as.numeric(modelComps[[1]]$roc.test$roc1$auc),
                              `AUC1-test p-value` = as.numeric(modelComps[[1]]$roc.test$p.value),
                              `AUC2-test p-value` = as.numeric(modelComps[[2]]$roc.test$p.value),
                              MSE = as.numeric(modelComps[[1]]$calib.test$MSE1),
                              `MSE1-test p-value` = as.numeric(modelComps[[1]]$calib.test$p.value),
                              `MSE2-test p-value` = as.numeric(modelComps[[2]]$calib.test$p.value),
                              Decision = 'Rejected')
      }
      DT.report <- rbind(DT.report, DT.temp)
    }
  }
  
  if (length(modelsEquivalent) > maxEquivalentModels) {
    # Reduce the number of equivalent models and adjust the report
    DT.lead <- DT.report[Decision == 'Leading']
    DT.equiv <- DT.report[Decision == 'Equivalent']
    DT.equivAUC <- setorder(copy(DT.equiv), -`AUC1-test p-value`)
    if ('MSE2-test p-value' %in% names(DT.report)) {
      DT.equivMSE <- setorder(copy(DT.equiv), -`MSE2-test p-value`)
    } else {
      DT.equivMSE <- setorder(copy(DT.equiv), -`MSE1-test p-value`)
    }
    equivModelString <- DT.lead[, Model]
    modelsCounter <- length(equivModelString)
    for (i in 1:(nrow(DT.equivAUC))) {
      equivModelString <- c(equivModelString, DT.equivAUC[, Model][i])
      equivModelString <- c(equivModelString, DT.equivMSE[, Model][i])
      equivModelString <- unique(equivModelString)
      if (length(equivModelString) >= maxEquivalentModels) {
        break
      }
    }
    DT.report[!(Model %in% equivModelString), Decision := 'Removed (#Models Limit)']
    modelsEquivalent <- list()
    for (i in 1:length(equivModelString)) {
      modelsEquivalent[[i]] <- as.formula(equivModelString[i])
    }
  }
  
  returnList <- list()
  returnList[['ModelsEquivalent']] <- modelsEquivalent
  returnList[['Report']] <- DT.report
  
  return(returnList)
}
