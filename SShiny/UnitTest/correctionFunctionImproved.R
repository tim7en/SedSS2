library (lmtest)
source ('convMixed.R')
datas <- read.csv("AllenSource.csv")
target <- read.csv ('AllenTarget.csv')

zero_const <- 0.001
neg_glob <- ('x/1000+1, (x-1)*1000')
standardDev <- 3
p.Val <- 0.05
R2 <- 0.7
drops <- NULL
corFor <- c('D50','TOC')
mixed <- 'N15'

mixedData <- rbind (datas, target)
negatives <- colnames(mixedData)[-c(1, 2)][sapply(mixedData[, -c(1, 2)], function(x) min(x)) <= 0]
positives <- colnames(mixedData)[-c(1, 2)][sapply(mixedData[, -c(1, 2)], function(x) max(x)) >= 0]
mixed <- negatives[which(negatives %in% positives)]
negatives <- negatives[which (!negatives %in% mixed)]
datasPositiveOnly <- convert (mixedData, neg_glob, zero_const)
datasOriginal <- inverse_neg_glob (datasPositiveOnly, neg_glob, c(mixed))
datasOriginal[c(negatives)] <- datasOriginal[(negatives)]* -1
src <- datasOriginal[1:nrow(datas),]
trg <- datasOriginal[(nrow(datas)+1):nrow(datasOriginal),]





datas <- convert (datas, neg_glob, zero_const)
target <- convert (target, neg_glob, zero_const)
# function used to extract p value of equation
lmp <- function(modelobject) { # From stackexchange
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  return(p)
}

correct.improved <- function(dat, corfor, drop, dev, R2, alpha) {
  library(parallel)
  cl <- detectCores() - 1
  # Initiate clustering
  cl <- makeCluster(cl)

  clusterEvalQ(cl, {
    ## set up each worker.  Could also use clusterExport()
    library(lmtest)
    library(car)
  })


  # Create possible combination of formulas (7^3 possible combinations)
  eq1 <- c("(conc)", "log(conc)", "I(conc^2)", "I(conc^(1/2))", "I(conc^(1/3))", "I(conc^(-1))")
  eq2 <- c("var1", "log(var1)", "I(var1^2)", "I(var1^(1/2))", "I(var1^(1/3))", "I(var1^(-1))")
  eq3 <- c("var2", "log(var2)", "I(var2^2)", "I(var2^(1/2))", "I(var2^(1/3))", "I(var2^(-1))")


  # Multiple regression
  eqComb <- expand.grid(eq1, eq2, eq3)
  indp <- paste(eqComb$Var2, eqComb$Var3, sep = "*")
  multipleRegression <- paste(eqComb$Var1, indp, sep = "~")


  # Simple regression
  eqSize <- expand.grid(eq1, eq2)
  simpleRegression <- paste(eqSize$Var1, eqSize$Var2, sep = "~")


  clusterExport(cl, list("lmp"))

  # Evaluate multiple regression w size & toc
  # x is an array of functions to evaluate
  # conc is the response variable
  # y is a list of predictors
  custom_lmEval <- function(x, conc, y, R2.adj, residualsDiv, alphalevel) {
    if (length(y) > 1) {
      for (i in seq(1, length(y))) {
        assign(paste0("var", i), as.numeric(unlist(y[[i]])))
      }
      myEquation <- step(lm(eval(parse(text = x))), trace = 0) # forward stepwise model selection
    } else {
      var1 <- unlist(y)
      myEquation <- lm(eval(parse(text = x))) # simple regression
    }

    if (length(myEquation$coefficients) == 1) {
      return(NA)
    }

    mydw <- dwtest(myEquation) # Durbin Watson Test autocorrelation of disturbances
    mybp <- bptest(myEquation) # Heteroscadastisity Breusch-Pagan test studentized Breusch-Pagan test (package lmtest)
    myresp <- shapiro.test(myEquation$residuals)$p.value

    if (ncol(myEquation$model) > 2) {
      myvif <- vif(myEquation) # variance inflation factors (if vif >10 there is a multicollinearity)
    } else {
      myvif <- 0 # else just assume as 0
    }


    myCooksDistance <- cooks.distance(myEquation) # cooks distance of residuals, detection of outliers (3 or more deviations)

    lmSum <- summary(myEquation)
    lmPvalue <- lmp(myEquation)

    if (lmPvalue > alphalevel) { # significance of equation
      return(NA)
    } else if (myresp < 2 * alphalevel) { # normality of residuals from Royston (1995)
      return(NA)
    } else if (any(myCooksDistance > 1)) { # cooks distance, impact of certain measure in model
      return(NA)
    } else if (lmSum$r.squared < R2.adj) { # pearson correlation sq
      return(NA)
    } else if (mydw$p.value < alphalevel) { # durbin watson test of autocorrelation
      return(NA)
    } else if (mybp$p.value < alphalevel) { # breusch-pagan test of heteroscadastisity
      return(NA)
    } else if (any(myvif > 10)) { # default VIF 10 #variance inflation factor
      return(NA)
    } else {
      if (length (y) >1){
        return(c(lmSum$r.squared, myresp, lmPvalue, mydw$p.value, mybp$p.value, as.character(myEquation$call)[2])) #stepwise selected equation
      } else {
        return(c(lmSum$r.squared, myresp, lmPvalue, mydw$p.value, mybp$p.value, x)) #equation from the table
      }
    }
  }

  # evaluate regressions (can be parallel or in sequence) #can be adjusted for mac or windows
  applyRegression <- function(x, y, deviates, R2, alphalevels) {
    if (length(y) > 1) {
      var1 <- as.numeric(unlist(y[[1]]))
      var2 <- as.numeric(unlist(y[[2]]))
      conc <- x
      #output <- sapply (multipleRegression, custom_lmEval, conc, list(var1, var2), residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
      output <- parSapply(cl, multipleRegression, custom_lmEval, conc, list(var1, var2), residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
      return(output)
    } else {
      var1 <- unlist(y)
      conc <- x
      #output <- sapply ( simpleRegression, custom_lmEval, conc, list(var1), residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
      output <- parSapply(cl, simpleRegression, custom_lmEval, conc, list(var1), residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
      return(output)
    }
  }


  correction <- function(input, corFor, drops, deviates, R2, alphalevel) {
    x <- input[, which(!names(input) %in% drops)]
    uniqueSource <- unique(x[, 2])
    outputDataFrame <- NULL

    for (i in uniqueSource) {
      datas <- x[which(x[, 2] == i), ]
      if (length(corFor) > 1) {
        listCorFor <- list(datas[corFor[1]], datas[corFor[2]]) # list of two independent variables to correct for
      } else {
        listCorFor <- list(datas[corFor]) # list of only one variable to correct for
      }
      for (it in seq(5, ncol(datas))) {
        myoutput <- applyRegression(datas[, it], y = listCorFor, deviates = deviates, R2 = R2, alphalevels = alphalevel)
        outputDataFrame <- rbind(outputDataFrame, cbind(i, unlist(myoutput), names(datas)[it], paste(corFor, collapse = "*")))
      }
    }

    outputDataFrame <- data.frame(na.omit(outputDataFrame))
    rownames(outputDataFrame) <- NULL
    names(outputDataFrame) <- c("SourceType", "Param", "Response", "Predictor")
    
    if (nrow (outputDataFrame) == 0){
      return (NULL)
    }

    parnames <- c("R2", "p-residuals", "p-eq", "p-dw", "p-bp", "Equation")
    outputDataFrame$parNames <- rep(parnames, nrow(outputDataFrame) / length(parnames))
    R2.df <- outputDataFrame[which(outputDataFrame$parNames == "R2"), ]
    Res.df <- outputDataFrame[which(outputDataFrame$parNames == "p-residuals"), ]
    Eqp.df <- outputDataFrame [which(outputDataFrame$parNames == "p-eq"), ]
    Dw.df <- outputDataFrame[which(outputDataFrame$parNames == "p-dw"), ]
    Dp.df <- outputDataFrame[which(outputDataFrame$parNames == "p-bp"), ]
    Eq.df <- outputDataFrame[which(outputDataFrame$parNames == "Equation"), ]
    outputDF <- data.frame(as.character(R2.df$SourceType), as.character(R2.df$Response), as.character(R2.df$Predictor), as.character(Eq.df$Param), Eqp.df$Param, R2.df$Param, Res.df$Param, Dw.df$Param, Dp.df$Param, stringsAsFactors = FALSE)
    names(outputDF) <- c("SourceType", "Response", "Predictor", "Equations", "p-model", "PearsonR2", "p-residuals", "p-dw", "p-bp")
    outputDF[, -c(1:4)] <- apply(outputDF[, -c(1:4)], 2, as.character)
    outputDF[, -c(1:4)] <- apply(outputDF[, -c(1:4)], 2, as.numeric)
    outputDF[, -c(1:4)] <- apply(outputDF[, -c(1:4)], 2, round, digits = 3)
    uniqueResponse <- unique(outputDF$Response)

    # remove duplicates that are result of stepwise model selection
    resultDF <- NULL

    for (i in uniqueResponse) {
      datasSubset <- outputDF[which(outputDF$Response %in% i), ]
      datasSubset <- datasSubset[!duplicated(datasSubset$Equations), ]
      resultDF <- rbind(resultDF, datasSubset)
    }
    return(resultDF)
    stopCluster(cl)
    closeAllConnections()
  }
  resultDF <- correction(dat, corfor, drops = drop, deviates = dev, R2 = R2, alphalevel = alpha)
  closeAllConnections()
  return(resultDF)
}

myresult <- correct.improved(datas,  corFor, drops, standardDev, R2, p.Val)