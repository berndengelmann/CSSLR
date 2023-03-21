#'
#' Routine to run automated logistic regression model selection
#' 
#' @description This function implements the core CSSLR variable selection algorithm which
#' is described in Engelmann B., Comprehensive Stepwise Selection for Logistic Regression
#' 
#' @param response Column name of the response variable contained in DT.data
#' @param DT.data Data set containing the response variable and all independent variables
#' @param selectionMode Variables that controls when a model should be considered as an
#' improvement over a reference model: Either AUC and MSE have to show an improvement ('AUC_and_MSE')
#' or, alternatively, one has to show an improvement and the other no deterioration ('AUC_or_MSE')
#' @param selectionVariables List of variable in DT.data from which the logistic regression model
#' should be built
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
#' @param pAUCTrim When removing a variable, AUC is considered as improved if its value has increased and
#' the p-value of the AUC-test is below pAUCTrim
#' @param pMSETrim When removing a variable, MSE is considered as improved if its value has decreased and
#' the p-value of the MSE-test is below pMSETrim
#' @param pAUCEquiv When comparing two models, they are considered as equivalent with respect to AUC
#' when the p-value of the AUC-test is above pAUCEquiv
#' @param pMSEEquiv When comparing two models, they are considered as equivalent with respect to MSE
#' when the p-value of the MSE-test is above pMSEEquiv
#' @param applyAIC Apply the AIC criterion when deciding about model improvement (TRUE); ignore AIC (FALSE)
#' @param applyBIC Apply the BIC criterion when deciding about model improvement (TRUE); ignore BIC (FALSE)
#' @param panelDataIdentifier In panel data sets, multiple data sets are recorded at different points in time
#' but belong to the same entity, e.g., a company or a person. The panelDataIdentifier is the ID / Name that
#' identifies the entity
#' @param maxSelectionSteps Maximum number of selection steps performed if no termination criterion is met earlier
#' @param maxEquivalentModels If p-values are set rather high, the number of models selected might become very
#' high; to keep the outcome and computational time manageable, a maximum number of equivalent models selected
#' in each selection step can be capped by this parameter
#' @param skipTrimming Model trimming is the least important step in the model selection but might
#' consume a significant proportion of computational time especially for large data sets. This
#' parameter allows turning trimming off if it is TRUE and running trimming if it is FALSE
#' @param debugLevel Parameter that controls printing on screen when running the selection algorithm (0: No output
#' on screen; 1: Limited output showing the current progress step; 2: Detailed output of the model currently analyzed)
#' @return A list containing three elements: A list of leading model(s), a second list
#' of models that are equivalent to the leading models (might be empty), an extensive
#' list containing data.tables reporting every selection step and it outcome
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
#' selectionVariables <- names(DT.data)[-1]
#' SelectionOutput <- csslr.model.selection('Response', DT.data, selectionVariables)
#'
#' # Print the selected leading model(s)
#' print(SelectionOutput[['ModelsLeading']])
#'
#' # Print all equivalent models (may include more than the leading models)
#' print(SelectionOutput[['Equivalent']])
#'
#' @export
csslr.model.selection <- function(response, DT.data, selectionVariables, selectionMode = 'AUC_or_MSE',
                                  pCoeff = 0.05, vifCrit = 5.00, pCalib = 0.50, pAUC = 0.05,
                                  pMSE = 0.05, pAUCTrim = 0.025, pMSETrim = 0.025,
                                  pAUCEquiv = 0.10, pMSEEquiv = 0.10, applyAIC = TRUE, applyBIC = TRUE,
                                  panelDataIdentifier = '', maxSelectionSteps = 10, maxEquivalentModels = 10,
                                  skipTrimming = FALSE, debugLevel = 0) {
  # Consistency checks
  DT.data <- data.table(DT.data)
  if (class(DT.data)[1] != 'data.table') {
    stop('Dataset has to be convertible to a data table')
  }
  if (!(response %in% names(DT.data))) {
    stop(paste0(response, ' is not contained in the dataset'))
  }
  if (!selectionMode %in% c('AUC_and_MSE', 'AUC_or_MSE')) {
    stop(paste0('The variable selectionMode is ', selectionMode, '. It has to be either AUC_and_MSE or AUC_or_MSE'))
  }
  notInData <- selectionVariables[!(selectionVariables %in% names(DT.data))]
  if (length(notInData) > 0) {
    stop(paste0('The variables ', notInData, ' are not contained in the dataset but selected for models estimation'))
  }
  csslr.utils.helpers.inRange(pCoeff, 0.0, 1.0, 'Critical coefficient p-value pCoeff')
  csslr.utils.helpers.inRange(vifCrit, 1.0, Inf, 'Critical variance inflation factors limit vifCrit')
  csslr.utils.helpers.inRange(pCalib, 0.0, 1.0, 'Critical calibration check p-value pCalib')
  csslr.utils.helpers.inRange(pAUC, 0.0, 1.0, 'Critical AUC test for improvement p-value value pAUC')
  csslr.utils.helpers.inRange(pMSE, 0.0, 1.0, 'Critical MSE test for improvement p-value pMSE')
  csslr.utils.helpers.inRange(pAUCTrim, 0.0, 1.0, 'Critical AUC test for trimming p-value pAUCTrim')
  csslr.utils.helpers.inRange(pMSETrim, 0.0, 1.0, 'Critical MSE test for trimming p-value pMSETrim')

  # Sort variables for selection alphabetically to find them easier in the report
  selectionVariables <- sort(selectionVariables)
  
  if (grepl('[^[:alnum:]]', response) == TRUE) {
    startModel <- paste0('`', response, '` ~ 1')
  } else {
    startModel <- paste0(response, ' ~ 1')
  }

  # Define a list structure containing the best models in terms of AUC and MSE
  topModels <- list()
  topModels[['AUC']] <- 0.5
  topModels[['AUC_Formula']] <- startModel
  topModels[['MSE']] <- 1.0
  topModels[['MSE_Formula']] <- startModel

  modelsEquivalent <- list()
  modelsEquivalent[[1]] <- as.formula(startModel)
  modelsLeadingOld <- modelsEquivalent
  modelsLeading <- modelsEquivalent

  modelsTestedList <- c()
  selectionReport <- list()
  for (selectStep in 1:maxSelectionSteps) {
    if (debugLevel > 0) {
      print(paste0('Selection Step: ', selectStep))
    }
    selectionStepReport <- list()
    DT.report.improved <- data.table()
    improvementFound <- FALSE
    modelsImproved <- list()
    if (debugLevel > 0) {
      print(paste0('  Find improved models...'))
    }
    for (modelNum in 1:length(modelsEquivalent)) {
      # STEP I: Find a list of improved models
      modelStart <- modelsEquivalent[[modelNum]]
      resultsImproved <- csslr.model.find.improved(modelStart, topModels, modelsTestedList,
                                                   DT.data, selectionMode, selectionVariables,
                                                   pCoeff, vifCrit, pCalib, pAUC, pMSE,
                                                   applyAIC, applyBIC, panelDataIdentifier, debugLevel)
      modelsTemp <- resultsImproved[['ModelsImproved']]
      modelsTestedList <- resultsImproved[['ModelsTested']]
      topModels <- resultsImproved[['TopModels']]
      DT.report.temp <- resultsImproved[['Report']]
      if (length(modelsTemp) > 0) {
        improvementFound <- TRUE
        modelsImproved <- c(modelsImproved, modelsTemp)
      } else {
        modelsImproved <- c(modelsImproved, modelStart)
      }
      DT.report.improved <- rbind(DT.report.improved, DT.report.temp)
    }
    
    DT.report.improved <- unique(DT.report.improved)
    selectionStepReport[['IMPROVED']] <- DT.report.improved
    
    # TERMINATION Criterion I: Stop if no improved models are found
    if (improvementFound == FALSE) {
      selectionReport[[paste0('Step',selectStep)]] <- selectionStepReport
      returnList <- list()
      returnList[['ModelsLeading']] <- csslr.utils.helpers.fList2C(modelsLeading)
      returnList[['ModelsSelected']] <- csslr.utils.helpers.fList2C(modelsEquivalent)
      returnList[['SelectionReport']] <- selectionReport
      return(returnList)
    }

    # STEP II: Trim the selected models if necessary
    if (skipTrimming == TRUE) {
      modelsTrimmed = modelsImproved
    } else {
      if (debugLevel > 0) {
        print(paste0('  Trim improved models...'))
      }
      resultsTrim <- csslr.model.find.trimmed(modelsImproved, topModels, modelsTestedList,
                                              DT.data, selectionMode,
                                              pAUCTrim, pMSETrim, debugLevel)
      modelsTrimmed <- resultsTrim[['ModelsTrimmed']]
      modelsTestedList <- resultsTrim[['ModelsTested']]
      DT.report.trim <- resultsTrim[['Report']]
      topModels <- resultsTrim[['TopModels']]
      selectionStepReport[['TRIM']] <- DT.report.trim
    }

    # STEP III: Find the leading among the improved models
    if (debugLevel > 0) {
      print(paste0('  Identify leading models...'))
    }
    resultsLeading <- csslr.model.find.leading(topModels, DT.data, pAUC, pMSE, debugLevel)
    modelsLeading <- resultsLeading[['ModelsLeading']]
    DT.report.leading <- resultsLeading[['Report']]
    selectionStepReport[['LEADING']] <- DT.report.leading
    
    # STEP IV: Find the set of models that is equivalent to the leading model(s)
    removeIdx <- c()
    modelsCandidates <- modelsTrimmed
    for (i in 1:length(modelsCandidates)) {
      varNames <- all.vars(modelsCandidates[[i]])
      for (j in 1:length(modelsLeading)) {
        leadingNames <- all.vars(modelsLeading[[j]])
        if (length(varNames) == length(leadingNames) & all(varNames %in% leadingNames)) {
          removeIdx <- c(removeIdx, i)
        }
      }
    }
    if (length(removeIdx) > 0) {
      removeIdx <- unique(removeIdx)
      modelsCandidates <- modelsCandidates[-removeIdx]
    }
    
    if (length(modelsCandidates) > 0) {
      if (debugLevel > 0) {
        print(paste0('  Identify equivalent models...'))
      }
      resultsEquivalent <- csslr.model.find.equivalent(modelsCandidates, modelsLeading, DT.data,
                                                       pAUCEquiv, pMSEEquiv, maxEquivalentModels,
                                                       debugLevel)
      modelsEquivalent <- resultsEquivalent[['ModelsEquivalent']]
      DT.report.equivalent <- resultsEquivalent[['Report']]
    } else {
      modelsEquivalent <- modelsLeading
      DT.report.equivalent <- DT.report.leading
    }
    selectionStepReport[['EQUIVALENT']] <- DT.report.equivalent
    
    selectionReport[[paste0('Step',selectStep)]] <- selectionStepReport
    
    # TERMINATION Criterion II: Stop if leading models did not improve in one step
    if (length(modelsLeading) == length(modelsLeadingOld)) {
      terminate <- FALSE
      leadChar1 <- paste(deparse(modelsLeading[[1]], width.cutoff = 500), collapse="")
      leadOldChar1 <- paste(deparse(modelsLeadingOld[[1]], width.cutoff = 500), collapse="")
      if (length(modelsLeading) == 1 & leadChar1 == leadOldChar1) {
        terminate <- TRUE
      } else if (length(modelsLeading) == 2) {
        leadChar2 <- paste(deparse(modelsLeading[[2]], width.cutoff = 500), collapse="")
        leadOldChar2 <- paste(deparse(modelsLeadingOld[[2]], width.cutoff = 500), collapse="")
        if ((leadChar1 == leadOldChar1 & leadChar2 == leadOldChar2) |
            (leadChar1 == leadOldChar2 & leadChar2 == leadOldChar1)) {
          terminate <- TRUE
        }
      }
      if (terminate == TRUE) {
        returnList <- list()
        returnList[['ModelsLeading']] <- csslr.utils.helpers.fList2C(modelsLeading)
        returnList[['ModelsSelected']] <- csslr.utils.helpers.fList2C(modelsEquivalent)
        returnList[['SelectionReport']] <- selectionReport
        return(returnList)
      }
    }
    modelsLeadingOld <- modelsLeading
    gc()
  }

  returnList <- list()
  returnList[['ModelsLeading']] <- csslr.utils.helpers.fList2C(modelsLeading)
  returnList[['ModelsSelected']] <- csslr.utils.helpers.fList2C(modelsEquivalent)
  returnList[['SelectionReport']] <- selectionReport
  return(returnList)
}
