#'
#' Routine to compute a bucket overview of a single variable in a data set
#' 
#' @description This routine splits a variable into buckets and computes an overview table
#'              containing the mean response and the number of observations. For 0/1 response
#'              variables in addition Weight-of-Evidence / Likelihood Ratio is computed
#'              for each bucket. In the case of numeric
#'              variable observations are either split into equally sized buckets or
#'              user-defined bucket limits can be defined. The user has the option to include
#'              the bucketed variable into the data table. For categorical variables or
#'              numeric variables with a small number of distinct values, the buckets
#'              are defined by the available categories and no bucketing algorithm is needed
#'
#' @param response Column name of the response variable contained in DT.data
#' @param variable Columns name of the variable that should be bucketed
#' @param DT.data data.table object containing both response and variable
#' @param method Bucketing method (only relevant for numeric variables); admissible values are "equalcut"
#'               which would distribute the data into roughly equally sized buckets and "userdefined"
#'               which would utilize the user-defined bucket limits for bucketing (default: "equalcut")
#' @param DT.cut An optional alternative data set that could be used to define buckets when the
#'               method "equalcut" is applied. This could be useful, e.g. in cases where a mising values
#'               imputation algorithm distorts splitting data into equally sized buckets (default: NULL)
#' @param numberBins Number of buckets that should be created (only relevant for numerical variables in
#'                   combination with the "equalcut" method). Default: 10
#' @param userDefinedBins A numeric vector containing the boundaries of buckets (only relevant for numerical
#'                        variables in combination with the "userdefined" method) 
#' @param bucketAverageMethod In the summary table, an average value of the variable is created;
#'                            It can be controlled if this should be done using "mean" or "median"
#' @param value.lowest Lower boundary of the lowest bucket (only relevant for numeric variables in
#'                     combination with the "equalcut" method). Default: 0.0
#' @param value.highest Upper boundary of the highest bucket (only relevant for numeric variables in
#'                      combination with the "equalcut" method). Default: +Inf
#' @param includeBinnedVariable Flag which controls whether the bucketed variable should be included
#'                              into DT.data (Default: FALSE)
#' @param variable.bin Name of the bucketed variable in case it is included into the data set. If no variable
#'                     name is provided, a variable name is created automatically using the ending .bin, where
#'                     a check if performed if the variable exists in the data already (Default: NULL)
#' @return A data.table containing the summary of the bucketing
#' 
#' @examples
#' 
#' set.seed(123)
#' DT.data <- data.table(Response = c(rep(0,100),rep(1,100)))
#' DT.data[Response == 0, Variable := rnorm(100, 1.0, 1.0)]
#' DT.data[Response == 1, Variable := rnorm(100, -1.0, 1.0)]
#' 
#' bucket1 <- csslr.data.bucketing("Response", "Variable", DT.data)
#' bucket2 <- csslr.data.bucketing("Response", "Variable", DT.data, method = "userdefined",
#'                                 userDefinedBins = c(-2.0,-1.0,0.0,1.0,2.0))
#'  
#' @export
csslr.data.bucketing <- function(response, variable, DT.data, method = "equalcut", DT.cut = NULL,
                                 numberBins = 10, userDefinedBins = NULL, bucketAverageMethod = "median",
                                 value.lowest = 0.0, value.highest = Inf,
                                 includeBinnedVariable = FALSE, variable.bin = NULL) {

  # DT.data has to be a data.table and contain a column named variable and response
  csslr.utils.helpers.checkDT(response, DT.data, 'csslr.data.bucketing')
  csslr.utils.helpers.checkDT(variable, DT.data, 'csslr.data.bucketing')

  # If the binned variable should be included, carry out the following logic:
  # - If a name for the binned variable is provided, check if the name already exists in the data and if yes stop the code
  # - If no name is provided, auto-generate a name for the binned variable
  if (includeBinnedVariable == TRUE) {
    if (is.null(variable.bin) == FALSE) {
      if (is.character(variable.bin) == FALSE) {
        stop(paste0('The value for variable.bin', variable.bin, ' is not a character'))
      }
      if (variable.bin %in% names(DT.data))
        stop(paste0("The column ", variable.bin, " is already contained in the data"))
    } else {
      variable.temp <- paste0(variable, ".bin")
      extraCounter <- 1
      while (variable.temp %in% names(DT.data)) {
        variable.temp <- paste0(variable.bin, extraCounter)
        extraCounter <- extraCounter + 1
      }
      variable.bin <- variable.temp
    }
  }

  responseVec <- DT.data[, get(response)]
  if (!(class(responseVec) %in% c("integer", "numeric"))) {
    stop(paste0("The column", response, "has to be numeric or integer"))
  }
  isProbability <- FALSE
  if (all(responseVec %in% c(0,1))) {
    isProbability <- TRUE
  }

  # Check the variable type and call the appropriate bucketing function
  variableVec <- DT.data[, get(variable)]
  if (class(variableVec) == "numeric") {
    numValues <- length(unique(variableVec))
    if (numValues <= numberBins) {
      # Run the categoric function, since the number of distinct values is small
      variableVec <- factor(variableVec, levels = sort(unique(variableVec)))
      res <- csslr.data.bucketing.c(responseVec, variableVec, isProbability)
    } else {
      if (!is.null(DT.cut)) {
        csslr.utils.helpers.checkDT(variable, DT.cut, 'csslr.data.bucketing')
        subset4cutVec <- DT.cut[, get(variable)]
      } else {
        subset4cutVec <- NULL
      }
      res <- csslr.data.bucketing.n(responseVec, variableVec, method, subset4cutVec,
                                    numberBins, userDefinedBins, bucketAverageMethod,
                                    isProbability, value.lowest, value.highest,
                                    includeBinnedVariable, variable.bin)
    }
  } else {
    res <- csslr.data.bucketing.c(responseVec, variableVec, isProbability)
  }
  
  if (includeBinnedVariable == TRUE & !is.null(res[['BinnedVec']])) {
    DT.data[, eval(variable.bin) := res[['BinnedVec']]]
  }

  result <- res[['Summary']]
  if ("V1.bin" %in% names(result)) {
    setnames(result, "V1.bin", paste0(variable, ".bin"))
    setnames(result, "V1", paste0(variable, "_", bucketAverageMethod))
  } else {
    setnames(result, "V1", variable)
  }
  if ("Mean_R" %in% names(result)) {
    setnames(result, "Mean_R", paste0("Mean_", response))
  }

  return(result)
}

# Bucketing function for numeric data
csslr.data.bucketing.n <- function(responseVec, variableVec, method, subset4cutVec,
                                   numberBins, userDefinedBins, bucketAverageMethod,
                                   isProbability, value.lowest, value.highest,
                                   includeBinnedVariable, variable.bin) {
  `%notin%` <- Negate(`%in%`)

  useWeightOfEvidence = getOption('csslr.use.woe')

  # Check if an admissible method was selected
  if (method %notin% c("equalcut", "userdefined"))
    stop("Admissible values for method are 'equalcut' and 'userdefined'")

  DT.data <- data.table(V1 = variableVec, R = responseVec)

  if (method == "equalcut") {
    if (is.null(subset4cutVec) == TRUE) {
      DT.cut <- data.table(V1 = variableVec)
    } else {
      DT.cut <- data.table(V1 = subset4cutVec)
    }
    # Build the binned variable on the data that should determine the split
    DT.cut <- DT.cut[, V1.bin := cut_number(DT.cut[, V1], numberBins, closed = "right")]
    # Change the boundaries of the factors to value boundaries to be more generic
    listFactors <- levels(DT.cut[, V1.bin])
    temp1 <- strsplit(as.character(listFactors[1]), ",")
    # Check for the suitability of the lower boundary
    minValue <- substr(temp1[[1]][1], 2, nchar(temp1[[1]][1]))
    if (value.lowest > as.numeric(minValue)) {
      warning(paste0('The lowest value in the data ', minValue, ' is below value.lowest. Replacing this function parameter by -Inf'))
      value.lowest <- -Inf
    }
    newFactor <- paste("[", as.character(value.lowest), ",", temp1[[1]][2], sep="")
    theFactorLevel <- listFactors[1]
    DT.cut <- DT.cut[V1.bin == theFactorLevel, V1.bin := newFactor]
    levels(listFactors) <- levels(DT.cut[, V1.bin])
    temp2 <- strsplit(as.character(listFactors[length(listFactors)]), ",")
    # Check the suitability of the upper boundary
    maxValue <- substr(temp2[[1]][2], 1, nchar(temp2[[1]][2])-1)
    if (value.highest < as.numeric(maxValue)) {
      warning(paste0('The largest value in the data ', maxValue, ' is above value.highest. Replacing this function parameter by Inf'))
      value.highest <- Inf
    }
    newFactor <- paste(temp2[[1]][1], ",", as.character(value.highest), "]", sep="")
    theFactorLevel <- listFactors[length(listFactors)]
    DT.cut <- DT.cut[V1.bin == theFactorLevel, V1.bin := newFactor]
    DT.cut <- DT.cut[, V1.bin := droplevels(V1.bin)]
    # Do the mapping of variableVec data into the bins again for the full data set
    # Use a rounding error parameter because cut_number gets <= and >= sometimes wrong
    eps <- 1.0e-8
    listFactors <- levels(DT.cut[, V1.bin])
    for (factor in listFactors) {
      temp <- strsplit(as.character(factor), ",")
      lowerBracket <- substring(temp[[1]][1], 1, 1)
      lower <- as.numeric(substring(temp[[1]][1], 2))
      upper <- as.numeric(substr(temp[[1]][2], 1, nchar(temp[[1]][2])-1))
      if (lowerBracket == '[') {
        DT.data[lower <= V1 & V1 <= (upper*(1+eps)), V1.bin := factor]
      } else {
        DT.data[(lower*(1+eps)) < V1 & V1 <= (upper*(1+eps)), V1.bin := factor]
      }
    }
  } else {
    # If the user-defined bins are NULL, it is assumed that variable.bin already exists in the data set
    if (is.null(userDefinedBins) & (variable.bin %notin% names(DT.data)))
      stop(paste0("userDefinedBins do not exist and ", variable.bin, " is not contained in the data"))

    if (is.null(userDefinedBins) == FALSE) {
      # Check whether user-defined bins are a numerical vector
      if (is.vector(userDefinedBins) == FALSE)
        stop("userDefinedBins must be a vector with numerical entries only")
      if (is.double(userDefinedBins) == FALSE)
        stop("userDefinedBins must be a vector with numerical entries only")

      userDefinedBins <- sort(userDefinedBins)

      if (value.lowest > userDefinedBins[1]) {
        warning(paste0('The lowest value of the user-defined bins ', userDefinedBins[1],
                       ' is below value.lowest. Replacing this function parameter by -Inf'))
        value.lowest <- -Inf
      }
      if (value.highest < userDefinedBins[length(userDefinedBins)]) {
        warning(paste0('The highest value of the user-defined bins ', userDefinedBins[length(userDefinedBins)],
                       ' is above value.highest. Replacing this function parameter by Inf'))
        value.highest <- Inf
      }

      # Constant will be used in testing for equality to avoid numerical inaccuracies
      eps <- 1.0e-8
      DT.data[, V1.bin := NA_character_]
      for (i in 1:(length(userDefinedBins) + 1)) {
        if (i == 1) {
          theFactor <- paste("[", value.lowest, ",", userDefinedBins[1], "]", sep="")
          DT.data[value.lowest <= V1 & V1 <= (userDefinedBins[1]*(1+eps)), V1.bin := theFactor]
        } else if (i == (length(userDefinedBins) + 1)) {
          theFactor <- paste("(", userDefinedBins[length(userDefinedBins)], ",", value.highest, "]", sep="")
          DT.data[(userDefinedBins[length(userDefinedBins)]*(1+eps)) < V1 & V1 <= value.highest, V1.bin := theFactor]
        } else {
          theFactor <- paste("(", userDefinedBins[i-1], ",", userDefinedBins[i], "]", sep="")
          DT.data[(userDefinedBins[i-1]*(1+eps)) < V1 & V1 <= (userDefinedBins[i]*(1+eps)), V1.bin := theFactor]
        }
      }
      # Final sanity check
      if (any(is.na(DT.data[!is.na(V1), V1.bin])))
        stop("There are non-NA values of the risk driver that could not be binned. Wrong inputs for value.lowest or value.highest?")
    }
  }

  # Generate the overview table for output
  tab.list <- DT.data[, list(mean(R), get(bucketAverageMethod)(V1), .N), by = V1.bin]
  DT.temp <- data.table(tab.list[[1]])
  setnames(DT.temp, 1, "V1.bin")
  DT.temp[, Observations := tab.list[[4]]]
  DT.temp[, V1 := tab.list[[3]]]
  if (isProbability == TRUE) {
    DT.temp[, Probability := tab.list[[2]]]
    if (useWeightOfEvidence == FALSE) {
      DT.temp[, LogOdds := log(Probability / (1 - Probability))]
    } else {
      variableCats <- DT.temp[, V1.bin]
      pGood <- c()
      pBad <- c()
      for (i in 1:nrow(DT.temp)) {
        if (!is.na(variableCats[i])) {
          pGoodTemp <- sum(DT.data[V1.bin == variableCats[i], 1.0 - R]) / nrow(DT.data)
          pBadTemp <- sum(DT.data[V1.bin == variableCats[i], R]) / nrow(DT.data)
        } else {
          pGoodTemp <- sum(DT.data[is.na(V1), 1.0 - R]) / nrow(DT.data)
          pBadTemp <- sum(DT.data[is.na(V1), R]) / nrow(DT.data)
        }
        pGood <- c(pGood, pGoodTemp)
        pBad <- c(pBad, pBadTemp)
      }
      pGoodFull <- mean(DT.data[, 1.0 - R])
      pBadFull <- mean(DT.data[, R])
      DT.temp[, WoE := log(pGood / pBad) - log(pGoodFull / pBadFull)]
    }
  } else {
    DT.temp[, Mean_R := tab.list[[2]]]
  }

  setkeyv(DT.temp, "V1")
  DT.temp <- droplevels(DT.temp)

  # Fix the sorting of the newly created variable to avoid inconsistencies across platforms
  listFactors <- DT.temp[, V1.bin]
  DT.data <- DT.data[, V1.bin := factor(DT.data[, V1.bin], levels = listFactors, ordered = F)]

  res <- list()
  res[['BinnedVec']] <- DT.data[, V1.bin]
  res[['Summary']] <- DT.temp

  return(res)
}

csslr.data.bucketing.c <- function(responseVec, variableVec, isProbability) {

  useWeightOfEvidence = getOption('csslr.use.woe')
  
  DT.data <- data.table(V1 = variableVec, R = responseVec)

  Tab.list <- DT.data[, list(mean(R), .N), by = V1]

  DT.temp <- data.table(Tab.list[[1]])
  setnames(DT.temp, 1, "V1")
  DT.temp[, Observations := Tab.list[[3]]]
  if (isProbability == TRUE) {
    DT.temp[, Probability := Tab.list[[2]]]
    if (useWeightOfEvidence == FALSE) {
      DT.temp[, LogOdds := log(Probability / (1 - Probability))]
    } else {
      variableCats <- DT.temp[, V1]
      pGood <- c()
      pBad <- c()
      for (i in 1:nrow(DT.temp)) {
        if (is.na(variableCats[i]) == TRUE) {
          pGoodTemp <- sum(DT.data[is.na(V1), 1.0 - R]) / nrow(DT.data)
          pBadTemp <- sum(DT.data[is.na(V1), R]) / nrow(DT.data)
        } else {
          pGoodTemp <- sum(DT.data[V1 == variableCats[i], 1.0 - R]) / nrow(DT.data)
          pBadTemp <- sum(DT.data[V1 == variableCats[i], R]) / nrow(DT.data)
        }
        pGood <- c(pGood, pGoodTemp)
        pBad <- c(pBad, pBadTemp)
      }
      pGoodFull <- mean(DT.data[, 1.0 - R])
      pBadFull <- mean(DT.data[, R])
      DT.temp[, WoE := log(pGood / pBad) - log(pGoodFull / pBadFull)]
    }
  } else {
    varname <- paste("mean", response, sep=".")
    DT.temp[, eval(varname) := Tab.list[[2]]]
  }

  setorder(DT.temp, "V1")

  res <- list()
  res[['BinnedVec']] <- NULL
  res[['Summary']] <- DT.temp

  return(res)
}
