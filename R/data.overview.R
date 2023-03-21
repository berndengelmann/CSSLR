#'
#' Routine to generate an overview of the data set
#' 
#' @description The routine computes AUC and MSE for all variables in the data set;
#' this helps in identifying the subset of variables that should be input into the CSSLR
#' algorithm and the complimentary set of variables that could be excluded
#' 
#' @param response Column name of the response variable contained in DT.data
#' @param DT.data Data set containing the response variable and all independent variables 
#' @return A data.table containing all variables and their AUC and MSE values
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
#' DT.overview <- csslr.data.overview('Response', DT.data)
#'
#' print(DT.overview)
#'
#' @export
csslr.data.overview <- function(response, DT.data) {
  # Constant that determines if categorical variables should be excluded
  maxNumberCategories <- 25

  # Consistency checks
  DT.data <- data.table(DT.data)
  if (class(DT.data)[1] != 'data.table') {
    stop('Dataset has to be convertible to a data table')
  }
  if (!(response %in% names(DT.data))) {
    stop(paste0(response, ' is not contained in the dataset'))
  }

  # Definition of the glm function
  if (getOption('csslr.speed.glm') == FALSE) {
    glmFunction <- 'glm'
  } else {
    glmFunction <- 'speedglm'
  }

  allNames <- names(DT.data)
  allNames <- allNames[allNames != response]

  pb = txtProgressBar(min = 0, max = length(allNames), initial = 0)
  DT.overview <- data.table()
  for (i in 1:length(allNames)) {
    msg <- 'ok'
    setTxtProgressBar(pb,i)
    # Determine the data type
    if ((is.character(DT.data[, get(allNames[i])]) == TRUE) |
        any(class(DT.data[, allNames[i]]) %in% c('factor', 'yearqtr'))) {
      type <- 'categoric'
    } else if (is.numeric(DT.data[, get(allNames[i])]) == TRUE) {
      type <- 'numeric'
    } else {
      type <- 'unknown'
    }
    # Catch the response variable
    if (allNames[i] == response) {
      msg <- 'response variable'
    }
    # Determine missing values
    numMissing <- nrow(DT.data[is.na(get(allNames[i]))])
    # Continue the analysis on non-missing data
    DT.temp <- DT.data[!is.na(get(allNames[i]))]
    # Is there still data available for modelling?
    if (nrow(DT.temp) == 0) {
      msg <- "all data is NA"
    } else {
      if (type == 'categoric') {
        # Determine the number of categories
        tab <- table(DT.temp[, get(allNames[i])])
        # Error if the number of categories is more than maxNumberCategories
        if (length(tab) > maxNumberCategories) {
          msg <- paste0("variable has more than ", maxNumberCategories, " categories")
        }
      }
      # If all values are identical the variable is useless
      if (length(unique(DT.temp[,get(allNames[i])])) == 1) {
        msg <- 'variable has identical values'
      }
    }
    aucVal <- NA_real_
    mseVal <- NA_real_
    if (msg == 'ok') {
      # Ensure special characters are treated correctly
      if (grepl('[^[:alnum:]]', response) == TRUE) {
        theFormula <- paste0('`', response, '` ~ `', allNames[i], '`')
      } else {
        theFormula <- paste0(response, ' ~ ', allNames[i])
      }
      # Estimate a model and predict PDs in-sample
      theModel <- get(glmFunction)(as.formula(theFormula), DT.temp, family = binomial('logit'))
      responseVec <- DT.temp[, get(response)]
      pdVec <- predict(theModel, type = 'response', data = DT.temp)
      roc <- pROC::roc(responseVec, pdVec, quiet = TRUE)
      mse <- csslr.stats.calib.check(responseVec, pdVec)
      aucVal <- as.numeric(roc$auc)
      mseVal <- as.numeric(mse$MSE)
    }
    DT.insert <- data.table(Variable = allNames[i], Missing = numMissing,
                            AUC = aucVal, MSE = mseVal, Message = msg)
    DT.overview <- rbind(DT.overview, DT.insert)
  }

  gc()
  return(DT.overview)
}
