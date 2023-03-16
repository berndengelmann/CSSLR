
library(CSSLR)
library(data.table)
library(glmnet)
library(MASS)

rm(list=ls())

source('./Tests/MyStepwiseGLM.R')

set.seed(123456)

##########################################################################
##### Table 3: allRandom <- FALSE, lessStrong <- FALSE, noStrong <- FALSE
##### Table 4: allRandom <- FALSE, lessStrong <- TRUE, noStrong <- FALSE
##### Table 5: allRandom <- FALSE, lessStrong <- TRUE, noStrong <- TRUE
##### Table 6: allRandom <- TRUE, lessStrong <- TRUE, noStrong <- TRUE
allRandom <- TRUE
lessStrong <- TRUE
noStrong <- TRUE
numberSimulations <- 1000

report <- data.table()
for (simRun in 1:numberSimulations) {

  ###################################################################################################
  #### Generate a simulated data set to test the model selection
  
  numGenerate <- 500
  
  response <- c(rep(0,numGenerate),rep(1,numGenerate))
  
  DT.data <- data.table(Response = response)
  if (allRandom == FALSE) {
    if (noStrong == FALSE) {
      # Generation of strong variables
      for (i in 1:3) {
        if (lessStrong == FALSE) {
          DT.data[Response == 0, eval(paste0('Strong',i)) := rnorm(numGenerate, 1.0, 1.0)]
          DT.data[Response == 1, eval(paste0('Strong',i)) := rnorm(numGenerate, -1.0, 1.0)]
        } else {
          DT.data[Response == 0, eval(paste0('Strong',i)) := rnorm(numGenerate, 0.30, 1.0)]
          DT.data[Response == 1, eval(paste0('Strong',i)) := rnorm(numGenerate, -0.30, 1.0)]
        }
      }
    }
    # Generation of weak variables
    for (i in 1:3) {
      if (lessStrong == FALSE) {
        DT.data[Response == 0, eval(paste0('Weak',i)) := rnorm(numGenerate, 0.5, 1.0)]
        DT.data[Response == 1, eval(paste0('Weak',i)) := rnorm(numGenerate, -0.5, 1.0)]
      } else {
        DT.data[Response == 0, eval(paste0('Weak',i)) := rnorm(numGenerate, 0.15, 1.0)]
        DT.data[Response == 1, eval(paste0('Weak',i)) := rnorm(numGenerate, -0.15, 1.0)]
      }
    }
    # Generation of random variables
    if (noStrong == FALSE) {
      for (i in 1:14) {
        DT.data[Response == 0, eval(paste0('zRandom',i)) := rnorm(numGenerate, 0.0, 1.0)]
        DT.data[Response == 1, eval(paste0('zRandom',i)) := rnorm(numGenerate, 0.0, 1.0)]
      }
    } else {
      for (i in 1:17) {
        DT.data[Response == 0, eval(paste0('zRandom',i)) := rnorm(numGenerate, 0.0, 1.0)]
        DT.data[Response == 1, eval(paste0('zRandom',i)) := rnorm(numGenerate, 0.0, 1.0)]
      }
    }
  } else {
    # Generation of random variables
    for (i in 1:20) {
      DT.data[Response == 0, eval(paste0('zRandom',i)) := rnorm(numGenerate, 0.0, 1.0)]
      DT.data[Response == 1, eval(paste0('zRandom',i)) := rnorm(numGenerate, 0.0, 1.0)]
    }
  }
  
  
  ###################################################################################################
  #### Run the model selection
  
  print(simRun)
  
  # CSSLR: Parameter set 1
  selectionMode <- 'AUC_or_MSE'
  pCoeff <- 0.05
  pAUCTrim <- 0.025
  pMSETrim <- 0.025
  vifCrit <- 5.0
  pCalib <- 0.50
  pAUC <- 0.05
  pMSE <- 0.05
  SelectionOutput1a <- csslr.model.selection('Response', DT.data, names(DT.data)[-1], selectionMode,
                                             pCoeff, vifCrit, pCalib, pAUC, pMSE, pAUCTrim, pMSETrim, debugLevel = 0)
  csslrVars1a <- sort(all.vars(as.formula(SelectionOutput1a$ModelsLeading[[1]]))[-1])
  print(cat('CSSLR 1a:', paste(csslrVars1a, collapse = ' '), ' '))
  
  selectionMode <- 'AUC_and_MSE'
  SelectionOutput1b <- csslr.model.selection('Response', DT.data, names(DT.data)[-1], selectionMode,
                                             pCoeff, vifCrit, pCalib, pAUC, pMSE, pAUCTrim, pMSETrim, debugLevel = 0)
  csslrVars1b <- sort(all.vars(as.formula(SelectionOutput1b$ModelsLeading[[1]]))[-1])
  print(cat('CSSLR 1b:', paste(csslrVars1b, collapse = ' '), ' '))
  
  # CSSLR: Parameter set 2
  selectionMode <- 'AUC_or_MSE'
  pCoeff <- 0.05
  pAUCTrim <- 0.025
  pMSETrim <- 0.025
  vifCrit <- 5.0
  pCalib <- 0.10
  pAUC <- 0.10
  pMSE <- 0.10
  SelectionOutput2a <- csslr.model.selection('Response', DT.data, names(DT.data)[-1], selectionMode,
                                             pCoeff, vifCrit, pCalib, pAUC, pMSE, pAUCTrim, pMSETrim, debugLevel = 0)
  csslrVars2a <- sort(all.vars(as.formula(SelectionOutput2a$ModelsLeading[[1]]))[-1])
  print(cat('CSSLR 2a:', paste(csslrVars2a, collapse = ' '), ' '))

  selectionMode <- 'AUC_and_MSE'
  SelectionOutput2b <- csslr.model.selection('Response', DT.data, names(DT.data)[-1], selectionMode,
                                             pCoeff, vifCrit, pCalib, pAUC, pMSE, pAUCTrim, pMSETrim, debugLevel = 0)
  csslrVars2b <- sort(all.vars(as.formula(SelectionOutput2b$ModelsLeading[[1]]))[-1])
  print(cat('CSSLR 2b:', paste(csslrVars2b, collapse = ' '), ' '))

  # MASS  
  massGlm <- glm(Response ~ ., family = binomial('logit'), data = DT.data)
  massStep <-  stepAIC(massGlm, trace = FALSE)
  massVars <- sort(names(massStep$coefficients)[-1])
  print(cat('    MASS:', paste(massVars, collapse = ' '), ' '))
  
  # My.stepwise
  myStep <- MyStepwiseGLM('Response', names(DT.data)[-1], data = DT.data, sle = 0.05, sls = 0.05, myfamily = 'binomial')
  myStepVars <- sort(names(myStep$coefficients)[-1])
  print(cat('  MYSTEP:', paste(myStepVars, collapse = ' '), ' '))

  # LASSO
  x <- Matrix(as.matrix(DT.data[, names(DT.data)[-1], with=F]))
  y <- Matrix(as.matrix(DT.data[, 'Response', with=F]))
  lasso <- glmnet(x, y, family = 'binomial', alpha = 0.999)
  cv.lasso <- cv.glmnet(x, y, family = 'binomial', alpha = 0.999)
  lambda.lasso1 <- cv.lasso$lambda.1se
  coef.lasso1 <- coef(lasso, s = lambda.lasso1, exact = FALSE)
  lasso1Coeff <- coef.lasso1[,1]
  lasso1Names <- names(lasso1Coeff[lasso1Coeff != 0.0])
  lasso1Names <- lasso1Names[lasso1Names != '(Intercept)']
  print(cat('  LASSO1:', paste(lasso1Names, collapse = ' '), ' '))

  lambda.lasso2 <- cv.lasso$lambda.min
  coef.lasso2 <- coef(lasso, s = lambda.lasso2, exact = FALSE)
  lasso2Coeff <- coef.lasso2[,1]
  lasso2Names <- names(lasso2Coeff[lasso2Coeff != 0.0])
  lasso2Names <- lasso2Names[lasso2Names != '(Intercept)']
  print(cat('  LASSO2:', paste(lasso2Names, collapse = ' '), ' '))
  
  # Prepare summary report
  strongVars <- paste0('Strong', c(1:3))
  weakVars <- paste0('Weak', c(1:3))
  randomVars <- paste0('zRandom', c(1:20))
  numStrong.csslr1a <- length(csslrVars1a[csslrVars1a %in% strongVars])
  numWeak.csslr1a <- length(csslrVars1a[csslrVars1a %in% weakVars])
  numRandom.csslr1a <- length(csslrVars1a[csslrVars1a %in% randomVars])
  numStrong.csslr1b <- length(csslrVars1b[csslrVars1b %in% strongVars])
  numWeak.csslr1b <- length(csslrVars1b[csslrVars1b %in% weakVars])
  numRandom.csslr1b <- length(csslrVars1b[csslrVars1b %in% randomVars])
  numStrong.csslr2a <- length(csslrVars2a[csslrVars2a %in% strongVars])
  numWeak.csslr2a <- length(csslrVars2a[csslrVars2a %in% weakVars])
  numRandom.csslr2a <- length(csslrVars2a[csslrVars2a %in% randomVars])
  numStrong.csslr2b <- length(csslrVars2b[csslrVars2b %in% strongVars])
  numWeak.csslr2b <- length(csslrVars2b[csslrVars2b %in% weakVars])
  numRandom.csslr2b <- length(csslrVars2b[csslrVars2b %in% randomVars])
  numStrong.mass <- length(massVars[massVars %in% strongVars])
  numWeak.mass <- length(massVars[massVars %in% weakVars])
  numRandom.mass <- length(massVars[massVars %in% randomVars])
  numStrong.mystep <- length(myStepVars[myStepVars %in% strongVars])
  numWeak.mystep <- length(myStepVars[myStepVars %in% weakVars])
  numRandom.mystep <- length(myStepVars[myStepVars %in% randomVars])
  numStrong.lasso1 <- length(lasso1Names[lasso1Names %in% strongVars])
  numWeak.lasso1 <- length(lasso1Names[lasso1Names %in% weakVars])
  numRandom.lasso1 <- length(lasso1Names[lasso1Names %in% randomVars])
  numStrong.lasso2 <- length(lasso2Names[lasso2Names %in% strongVars])
  numWeak.lasso2 <- length(lasso2Names[lasso2Names %in% weakVars])
  numRandom.lasso2 <- length(lasso2Names[lasso2Names %in% randomVars])
  DT.temp <- data.table(SimRun = simRun,
                        CSSLR1a_Strong = numStrong.csslr1a, CSSLR1a_Weak = numWeak.csslr1a, CSSLR1a_Random = numRandom.csslr1a,
                        CSSLR1b_Strong = numStrong.csslr1b, CSSLR1b_Weak = numWeak.csslr1b, CSSLR1b_Random = numRandom.csslr1b,
                        CSSLR2a_Strong = numStrong.csslr2a, CSSLR2a_Weak = numWeak.csslr2a, CSSLR2a_Random = numRandom.csslr2a,
                        CSSLR2b_Strong = numStrong.csslr2b, CSSLR2b_Weak = numWeak.csslr2b, CSSLR2b_Random = numRandom.csslr2b,
                        MASS_Strong = numStrong.mass, MASS_Weak = numWeak.mass, MASS_Random = numRandom.mass,
                        MYSTEP_Strong = numStrong.mystep, MYSTEP_Weak = numWeak.mystep, MYSTEP_Random = numRandom.mystep,
                        Lasso1_Strong = numStrong.lasso1, Lasso1_Weak = numWeak.lasso1, Lasso1_Random = numRandom.lasso1,
                        Lasso2_Strong = numStrong.lasso2, Lasso2_Weak = numWeak.lasso2, Lasso2_Random = numRandom.lasso2)
  report <- rbind(report, DT.temp)
}
