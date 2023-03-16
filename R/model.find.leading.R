#'
#' Routine to identify leading models in a selection step
#'
#' Routine to identify leading models among the top models which are the models
#' with the highest AUC and lowest MSE; if there is one model dominating both categories this
#' is the leading model. If there are two models it is tested whether one model is significantly
#' better in one category and equivalent in the second; if yes, this model is identified as leading
#' model, otherwise two leading models are returned
#' 
#' @param topModels List containing AUC and MSE together with formulas of the best models
#' @param DT.data Data set containing the response variable and all independent variables 
#' @param pAUC When testing for superiority in AUC, the p-value of the AUC-test has to be
#' lower than pAUC to conclude that one model is superior with respect to AUC
#' @param pMSE When testing for superiority in MSE, the p-value of the MSE-test has to be
#' lower than pMSE to conclude that one model is superior with respect to MSE
#' @param debugLevel Parameter that controls printing on screen when running the selection algorithm (0: No output
#' on screen; 1: Limited output showing the current progress step; 2: Detailed output of the model currently analyzed)
#' @return A list containing two items: A list of models that are identified as leading
#' and a data.table containing the results of the tests and decisions made
#' @noRd
csslr.model.find.leading <- function(topModels, DT.data, pAUC, pMSE, debugLevel) {

  model1 <- paste(deparse(topModels[['AUC_Formula']], width.cutoff = 500), collapse="")
  model2 <- paste(deparse(topModels[['MSE_Formula']], width.cutoff = 500), collapse="")
  
  tempModels <- unique(c(model1,model2))

  DT.report <- data.table()
  leadingModels <- list()
  if (length(tempModels) == 2) {
    # Run model comparison and check p-values
    modelComp <- csslr.model.compare.lr(as.formula(tempModels[1]), as.formula(tempModels[2]), DT.data)
    DT.temp <- data.table(Model = model1, AUC = as.numeric(modelComp$roc.test$roc1$auc),
                          MSE = as.numeric(modelComp$calib.test$MSE1), Decision = NA_character_)
    DT.report <- rbind(DT.report, DT.temp)
    DT.temp <- data.table(Model = model2, AUC = as.numeric(modelComp$roc.test$roc2$auc),
                          MSE = as.numeric(modelComp$calib.test$MSE2), Decision = NA_character_)
    DT.report <- rbind(DT.report, DT.temp)
    if (modelComp$roc.test$p.value < pAUC & modelComp$calib.test$p.value >= pMSE) {
      leadingModels[[1]] <- as.formula(tempModels[1])
      DT.report[Model == tempModels[1], Decision := 'Leading']
    } else if (modelComp$roc.test$p.value >= pAUC & modelComp$calib.test$p.value < pMSE) {
      leadingModels[[1]] <- as.formula(tempModels[2])
      DT.report[Model == tempModels[2], Decision := 'Leading']
    } else {
      leadingModels[[1]] <- as.formula(tempModels[1])
      leadingModels[[2]] <- as.formula(tempModels[2])
      DT.report[Model == tempModels[1], Decision := 'Leading']
      DT.report[Model == tempModels[2], Decision := 'Leading']
    }
  } else {
    leadingModels[[1]] = as.formula(tempModels)
    DT.temp <- data.table(Model = model1, AUC = topModels[['AUC']], MSE = topModels[['MSE']], Decision = 'Leading')
    DT.report <- rbind(DT.report, DT.temp)
  }
  
  DT.report[, Model := gsub('`', '', Model)]
  
  returnList <- list()
  returnList[['ModelsLeading']] <- leadingModels
  returnList[['Report']] <- DT.report
  
  return(returnList)
}
