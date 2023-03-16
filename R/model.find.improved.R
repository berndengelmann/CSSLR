#'
#' Routine to find improved models in a selection step
#'
#' @description Routine to identify models that improve an existing model (modelStart) by
#' adding variables one-by-one from a list of candidate variables to this model
#' 
#' @param modelStart Model that is aimed to be improved by adding an additional variable
#' @param topModels List containing AUC and MSE together with formulas of the best models
#' @param modelsTestedList List of models that already have been evaluated in previous steps;
#' this list created merely for performance reasons to avoid multiple testing of the same model
#' @param DT.data Data set containing the response variable and all independent variables
#' @param selectionMode Variables that controls when a model should be considered as an
#' improvement over modelStart: Either AUC and MSE have to show an improvement ('AUC_and_MSE')
#' or, alternatively, one has to show an improvement and the other no deterioration ('AUC_or_MSE')
#' @param pCoeff When testing for significance of coefficients, the p-value has to be below
#' pCoeff to consider a model as improved
#' @param vifCrit When computing variance inflation factors, all VIFs have to be below vifCrit
#' to consider a model as improved
#' @param pCalib When performing the Spiegelhalter calibration test, the p-value has to be larger
#' than pCalib to consider a model as improved
#' @param pAUC When testing for difference in AUC after including a variable, the p-value has to be
#' below pAUC to consider the extended model as improved
#' @param pMSE When testing for difference in MSE after including a variable, the p-value has to be
#' below pMSE to consider the extended model as improved
#' @param applyAIC Apply the AIC criterion when deciding about model improvement (TRUE); ignore AIC (FALSE)
#' @param applyBIC Apply the BIC criterion when deciding about model improvement (TRUE); ignore BIC (FALSE)
#' @param panelDataIdentifier In panel data sets, multiple data sets are recorded at different points in time
#' but belong to the same entity, e.g., a company or a person. The panelDataIdentifier is the ID / Name that
#' identifies the entity
#' @param debugLevel Parameter that controls printing on screen when running the selection algorithm (0: No output
#' on screen; 1: Limited output showing the current progress step; 2: Detailed output of the model currently analyzed)
#' @return A list containing three items: A list of models that are identified as improvements over
#' modelStart, an update of modelsTestedList, and a data.table containing the results of the tests
#' and decisions about all candidate models
#' @noRd
csslr.model.find.improved <- function(modelStart, topModels, modelsTestedList,
                                      DT.data, selectionMode, selectionVariables,
                                      pCoeff, vifCrit, pCalib, pAUC, pMSE,
                                      applyAIC, applyBIC, panelDataIdentifier, debugLevel) {
  # Definition of the glm function
  if (getOption('csslr.speed.glm') == FALSE) {
    glmFunction <- 'glm'
  } else {
    glmFunction <- 'speedglm'
  }
  
  # Parsing the model formula
  responseVar <- rlang::f_lhs(modelStart)
  allVariables <- all.vars(modelStart)[-1]
  
  # Compute AIC, BIC, AUC, MSE for modelStart
  modelStartGlm <- get(glmFunction)(modelStart, data=DT.data, family=binomial(link='logit'), y=FALSE, model=FALSE)
  AIC.start <- as.numeric(AIC(modelStartGlm))
  BIC.start <- as.numeric(BIC(modelStartGlm))
  PredictVec.start <- predict(modelStartGlm, type = 'response', DT.data)
  ResponseVec <- DT.data[, get(responseVar)]
  roc.start <- pROC::roc(ResponseVec, PredictVec.start, quiet = TRUE)
  names.start <- rownames(coef(summary(modelStartGlm)))
  
  DT.report <- data.table()
  candidateVariables <- selectionVariables[!(selectionVariables %in% all.vars(modelStart))]
  
  modelsImproved <- c()
  for (crd in candidateVariables) {
    modelVars <- c(allVariables, crd)
    # Check for special characters
    modelVars <- sort(modelVars)
    modelVars <- ifelse(grepl('[^[:alnum:]]', modelVars), paste0('`', modelVars, '`'), modelVars)
    candidateFormula <- as.formula(paste(c(responseVar, paste(modelVars, collapse = ' + ')), collapse = ' ~ '))
    candidateString <- paste(deparse(candidateFormula, width.cutoff = 500), collapse="")
    if (!(candidateString %in% modelsTestedList) == TRUE) {
      if (debugLevel > 1) {
        print(paste0('Test for Improvement: ', candidateString))
      }
      candidateModel <- csslr.model.analysis.lr(candidateFormula, DT.data, panelDataIdentifier = panelDataIdentifier,
                                                lr = FALSE, roc = FALSE, calib = FALSE, ic = FALSE,
                                                quiet = TRUE, fullModelTrim = FALSE, dataValidation = FALSE)
      isImproved <- TRUE
      modelDecision <- 'Selected'
      coeffNames <- rownames(coef(candidateModel$model.summary))
      addNames <- coeffNames[!(coeffNames %in% names.start)]
      addIdx <- which(coeffNames %in% addNames)
      minCoeff <- as.numeric(min(coef(candidateModel$model.summary)[addIdx,4]))
      if (minCoeff > pCoeff) {
        isImproved <- FALSE
        modelDecision <- 'Rejected (Coeff)'
      }
      AIC.candidate <- as.numeric(AIC(candidateModel$full.model))
      if (AIC.start <= AIC.candidate & applyAIC == TRUE) {
        isImproved <- FALSE
        modelDecision <- 'Rejected (AIC)'
      }
      BIC.candidate <- as.numeric(BIC(candidateModel$full.model))
      if (BIC.start <= BIC.candidate & applyBIC == TRUE) {
        isImproved <- FALSE
        modelDecision <- 'Rejected (BIC)'
      }
      if (class(candidateModel$vif)[1] != 'character') {
        if (max(candidateModel$vif$`GVIF^(1/(2*Df))` >= vifCrit)) {
          isImproved <- FALSE
          modelDecision <- 'Rejected (VIF)'
        }
      } else {
        # Error comes from aliased coefficients in the model
        if (candidateModel$vif == 'VIF calculation was unsuccessful') {
          isImproved <- FALSE
          modelDecision <- 'Rejected (VIF)'
        }
      }
      PredictVec.candidate <- predict(candidateModel$full.model, type = 'response', DT.data)
      roc.candidate <- pROC::roc(ResponseVec, PredictVec.candidate, quiet = TRUE)
      rocTest <- pROC::roc.test(roc.start, roc.candidate, method = 'delong', quiet = TRUE)
      mseCheck <- csslr.stats.calib.check(ResponseVec, PredictVec.candidate)
      if (mseCheck$p.value < pCalib) {
        isImproved <- FALSE
        modelDecision <- 'Rejected (Calib)'
      }
      aucImproved <- TRUE
      if (as.numeric(rocTest$p.value) >= pAUC | as.numeric(rocTest$roc1$auc) >= as.numeric(rocTest$roc2$auc)) {
        if (selectionMode == 'AUC_and_MSE') {
          isImproved <- FALSE
          modelDecision <- 'Rejected (AUC)'
        } else {
          aucImproved <- FALSE
        }
      }
      calibTest <- csslr.stats.calib.test(ResponseVec, PredictVec.start, PredictVec.candidate)
      if (as.numeric(calibTest$p.value) >= pMSE | as.numeric(calibTest$MSE1) <= as.numeric(calibTest$MSE2)) {
        if (selectionMode == 'AUC_and_MSE' | aucImproved == FALSE) {
          isImproved <- FALSE
          if (aucImproved == FALSE) {
            modelDecision <- 'Rejected (AUC)'
          } else {
            modelDecision <- 'Rejected (MSE)'
          }
        }
      }
      if (isImproved == TRUE) {
        modelsImproved <- c(modelsImproved, candidateFormula)
        # Check if top models should be replaced
        modelAUC <- as.numeric(rocTest$roc2$auc)
        modelMSE <- as.numeric(calibTest$MSE2)
        if (modelAUC > topModels[['AUC']]) {
          topModels[['AUC']] <- modelAUC
          topModels[['AUC_Formula']] <- candidateFormula
        }
        if (modelMSE < topModels[['MSE']]) {
          topModels[['MSE']] <- modelMSE
          topModels[['MSE_Formula']] <- candidateFormula
        }
      }
      DT.report.temp <- data.table(Model = gsub('`', '', paste(deparse(candidateFormula, width.cutoff = 500), collapse="")),
                                   `Coefficient p-value` = minCoeff,
                                   AIC = AIC.candidate,
                                   BIC = BIC.candidate,
                                   AUC = as.numeric(rocTest$roc2$auc),
                                   `AUC-test p-value` = as.numeric(rocTest$p.value),
                                   MSE = as.numeric(calibTest$MSE2),
                                   `MSE-test p-value` = as.numeric(calibTest$p.value),
                                   Decision = modelDecision)
      DT.report <- rbind(DT.report, DT.report.temp)
      modelsTestedList <- c(modelsTestedList, candidateString)
    }
  }
  
  returnList <- list()
  returnList[['ModelsImproved']] <- modelsImproved
  returnList[['ModelsTested']] <- modelsTestedList
  returnList[['Report']] <- DT.report
  returnList[['TopModels']] <- topModels

  return(returnList)
}
