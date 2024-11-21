
#' Routine to validate input data and stop the code when failing
#' 
#' @param input Input variable / vector of numeric data 
#' @param minVal minVal minimum admissible value
#' @param maxVal maxVal maximum admissible value
#' @param errorName Name of the variable that should be used in the error message when check fails
#' @param strict TRUE if minVal < input < maxVal, FALSE if minVal <= input <= maxVal
#' @noRd
csslr.utils.helpers.inRange <- function(input, minVal, maxVal, errorName, strict = FALSE) {
  if (is.numeric(input) == FALSE) {
    stop('csslr.utils.helpers.inRange is called with non-numeric data')
  }
  if (is.numeric(minVal) == FALSE) {
    stop('csslr.utils.helpers.inRange is called with a non-numeric minimum value')
  }
  if (is.numeric(maxVal) == FALSE) {
    stop('csslr.utils.helpers.inRange is called with non-numeric maximum value')
  }
  
  if (strict == FALSE) {
    allGood <- ifelse(minVal <= input & input <= maxVal, TRUE, FALSE)
  } else {
    allGood <- ifelse(minVal < input & input < maxVal, TRUE, FALSE)
  }
  if (any(allGood == FALSE)) {
    stop(paste0(errorName, ' is not between ', minVal, ' and ', maxVal))
  }
}

#' @noRd
csslr.utils.helpers.fList2C <- function(formulaList) {
  characterList <- list()
  for (i in 1:length(formulaList)) {
    characterFormula <- paste(deparse(formulaList[[i]], width.cutoff = 500), collapse="")
    characterList[[i]] <- characterFormula
  }
  return(characterList)
}

#' @noRd
csslr.utils.helpers.checkDT <- function(variable, DT.data, fctName) {
  `%notin%` <- Negate(`%in%`)

  if (class(DT.data)[1] != "data.table")
    stop(paste0('Error in ', functionName, ': The data object is not of the class data.table'))

  if (is.character(variable) == FALSE)
    stop(paste0('Error in ', functionName, ': The column name', variable, ' is not of type character'))

  if (variable %notin% names(DT.data))
    stop(paste0('Error in ', functionName, ': There is no column with name ', variable, ' in the data.table object'))
}
