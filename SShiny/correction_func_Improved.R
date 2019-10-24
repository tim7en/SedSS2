library(dplyr)
source ('convMixed.R')

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
      #print ('normality of residu')
      return(NA)
    } else if (any(myCooksDistance > 1)) { # cooks distance, impact of certain measure in model
      #print ('cooks dist')
      return(NA)
    } else if (lmSum$r.squared < R2.adj) { # pearson correlation sq
      #print ('r2-adj')
      return(NA)
    } else if (mydw$p.value < alphalevel) { # durbin watson test of autocorrelation
      #print ('DW test')
      return(NA)
    } else if (mybp$p.value < alphalevel) { # breusch-pagan test of heteroscadastisity
      #print ('BP test')
      return(NA)
    } else if (any(myvif > 10)) { # default VIF 10 #variance inflation factor
      #print ('VIF test')
      return(NA)
    } else {
      if (length (y) >1){
        return(c(lmSum$adj.r.squared, myresp, lmPvalue, mydw$p.value, mybp$p.value, as.character(myEquation$call)[2])) #stepwise selected equation
      } else {
        return(c(lmSum$adj.r.squared, myresp, lmPvalue, mydw$p.value, mybp$p.value, x)) #equation from the table
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
      output <- parSapply(cl, multipleRegression, custom_lmEval, conc, list(var1, var2),  R2.adj = R2, residualsDiv = deviates, alphalevel = alphalevels)
      return(output)
    } else {
      var1 <- unlist(y)
      conc <- x
      #output <- sapply (simpleRegression, custom_lmEval, conc, list(var1), residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
      output <- parSapply(cl, simpleRegression, custom_lmEval, conc, list(var1), R2.adj = R2, residualsDiv = deviates, alphalevel = alphalevels)
      return(output)
    }
  }
  
  
  correction <- function(input, corFor, drops, deviates, R2, alphalevel) {
    x <- input[, which(!names(input) %in% drops)]
    uniqueSource <- unique(x[, 2])
    outputDataFrame <- NULL
    
    for (i in uniqueSource) {
      #print (i)
      
      datas <- x[which(x[, 2] == i), ]
      if (length(corFor) > 1) {
        listCorFor <- list(datas[corFor[1]], datas[corFor[2]]) # list of two independent variables to correct for
      } else {
        listCorFor <- list(datas[corFor]) # list of only one variable to correct for
      }
      for (it in seq(5, ncol(datas))) { #start from column 5
        myoutput <- applyRegression(datas[, it], y = listCorFor, deviates = deviates, R2 = R2, alphalevels = alphalevel)


          tryCatch({
            if (is.null(nrow(myoutput))){
              outputDataFrame <- rbind(outputDataFrame, cbind(i, unlist(myoutput), names(datas)[it], paste(corFor, collapse = "*")))
            } else {
              if (dim(myoutput)[2]>4){
                for (it2 in seq (1, ncol(myoutput))){
                  outputDataFrame <- rbind(outputDataFrame, cbind(i, myoutput[,it2], names(datas)[it], paste(corFor, collapse = "*")))
                }
                #newdat <- outputDataFrame
              } else {
                outputDataFrame <- rbind(outputDataFrame, cbind(i, unlist(myoutput), names(datas)[it], paste(corFor, collapse = "*")))
              }
            }
          }, error = function (e){
            
          })
          
      }
      #print (dim(outputDataFrame))
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
    
    stopCluster(cl)
    closeAllConnections()
    return(resultDF)
  }
  resultDF <- correction(dat, corfor, drops = drop, deviates = dev, R2 = R2, alphalevel = alpha)
  closeAllConnections()
  rownames(resultDF) <- NULL
  return(resultDF)
}



correctionIntegrityCheck <- function (x, target, myresult, corFor, drops){
  correctionList <- list ()
  drops <- drops[[2]]
  
  for (j in seq(1, nrow(target))) {
    
    myresult$OverCorrection <- FALSE
    for (i in seq(1, nrow(myresult))) {
      #check my result i, if the target and source match, skip current part or 
      #assign to myresult$OverCorrection <-TRUE value
      
      if ((target[j,1] %in% drops[,1]) && (myresult[i,2] %in% drops[,2])){
        myresult$OverCorrection[i] <- TRUE
        next
      }
      
      
      datas <- x[which (x[,2] %in% myresult[i,1]),] #subset of the source type
      conc <- as.numeric(unlist(datas[myresult[i, 2]])) #element to change
      
      if (length(corFor) > 1) {
        var1 <- as.numeric(unlist(datas[corFor[1]]))
        var2 <- as.numeric(unlist(datas[corFor[2]]))
        myfit <- lm(eval(parse(text = myresult[i, 4])))
        
        if (ncol(myfit$model)>2){
          Yi <- myfit$model[, 1] #concentration transformed
          Si <- myfit$model[, 2] #var 1 transformed
          Ti <- myfit$model[, 3] #var 2 transformed
        } else if (ncol(myfit$model == 2)){
          Yi <- myfit$model[,1] #concentration transformed
          Si <- myfit$model[,2] #var 1 transformed
        }
        
        var1 <- as.numeric(unlist(target[j,corFor[1]])) #get the value from the target (1)
        var2 <- as.numeric(unlist(target[j,corFor[2]])) 
        conc <- as.numeric(unlist(target[j, myresult[i,2]])) #get the concentration of the element from the targeet
        myfitTarget <- lm(eval(parse(text = myresult[i, 4]))) #fit and transform the data
        
        if (ncol(myfitTarget$model)>2){
          St <- myfitTarget$model[,2] #var 1 transformed
          Tt <- myfitTarget$model[,3] #var 2 transformed
        } else if (ncol(myfit$model == 2)){ 
          St <- myfitTarget$model[,2] #only var 1 transformed
        }
        
        if (length(myfit$coefficients) > 3) {
          SlopeVar1 <- myfit$coefficients[2] #get the slope of equation for var 1
          SlopeVar2 <- myfit$coefficients[3] #get slope of equation for var 2
          SlopeVar1Var2 <- myfit$coefficients[4] # interaction term get interaction 
          corrected <- Yi- ((Si-St)*SlopeVar1+(Ti-Tt)*SlopeVar2+((Si*Ti)-(St*Tt))*SlopeVar1Var2)
        } else if (length(myfit$coefficients) == 3) {
          SlopeVar1 <- myfit$coefficients[2]
          SlopeVar2 <- myfit$coefficients[3]
          #Yi- ((Si-St)*SlopeVar1+(Ti-Tt)*SlopeVar2)
          corrected <- Yi- ((Si-St)*SlopeVar1+(Ti-Tt)*SlopeVar2)
        } else if (length(myfit$coefficients) == 2){
          SlopeVar1 <- myfit$coefficients[2]
          corrected <- Yi - (Si-St)*SlopeVar1
        }
        
        tname <- colnames(myfit$model[1])
        if ( tname == "log(conc)" ){
          corrected <- exp (corrected)
        } else if (tname == "I(conc^2)"){
          corrected <- sqrt (corrected)
        } else if (tname == "I(conc^(1/2))"){
          corrected <- corrected^2
        } else if (tname == "I(conc^(1/3))"){
          corrected <- corrected^3
        } else if (tname == "I(conc^(-1))"){
          corrected <- 1/corrected
        }
        
        if (any(is.na(corrected))){
          myresult$OverCorrection[i] <- TRUE
        } else if (any(is.nan(corrected))){
          myresult$OverCorrection[i] <- TRUE
        } else if (any(corrected<0)){
          myresult$OverCorrection[i] <- TRUE
        }
        
      } else {
        var1 <- as.numeric(unlist(datas[corFor[1]]))
        myfit <- lm(eval(parse(text = myresult[i, 4])))
        Yi <- myfit$model[,1]
        Si <- myfit$model[,2]
        var1 <- as.numeric(unlist(target[j,corFor[1]]))
        conc <- as.numeric(unlist(target[j, myresult[i,2]]))
        myfitT <- lm(eval(parse(text = myresult[i, 4])))
        St <- myfitT$model[,2]
        SlopeVar1 <- myfit$coefficients[2]
        corrected <- Yi - (Si-St)*SlopeVar1
        
        tname <- colnames(myfit$model[1])
        if ( tname == "log(conc)" ){
          corrected <- exp (corrected)
        } else if (tname == "I(conc^2)"){
          corrected <- sqrt (corrected)
        } else if (tname == "I(conc^(1/2))"){
          corrected <- corrected^2
        } else if (tname == "I(conc^(1/3))"){
          corrected <- corrected^3
        } else if (tname == "I(conc^(-1))"){
          corrected <- 1/corrected
        }
        
        if (any(is.na(corrected))){
          myresult$OverCorrection[i] <- TRUE
        } else if (any(is.nan(corrected))){
          myresult$OverCorrection[i] <- TRUE
        } else if (any(corrected<0)){
          myresult$OverCorrection[i] <- TRUE
        }
      }
    }
    correctionList[[j]] <- myresult
  }
  return (correctionList)
}


selectedCorrectionTabs <- function (correctionList, target, corFor){
  correctionList <- correctionList
  # correctionList
  for (i in seq(1, length(correctionList))) {
    correctionList[[i]] <- correctionList[[i]][which(correctionList[[i]]$OverCorrection %in% FALSE), ]
    f <- correctionList[[i]]
    f <- f %>%
      dplyr::group_by(SourceType, Response) %>%
      dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
    
    datas <- data.frame (f)
    
    if (corFor == 'rawM'){
      origRaw <- c('(conc) ~ var1','(conc)~var1', '(conc) ~ var2', '(conc)~var2',
                   '(conc) ~ var1 + var2', '(conc) ~ var1 * var2')
      
      eq1 <- c('(conc)')
      eq2 <- c("var1", "log(var1)", "(var1^2)", "(var1^(1/2))", "(var1^(1/3))", "(var1^(-1))")
      eq3 <- c("var2", "log(var2)", "(var2^2)", "(var2^(1/2))", "(var2^(1/3))", "(var2^(-1))")
      eqComb <- expand.grid(eq1, eq2, eq3)
      indp <- paste(eqComb$Var2, eqComb$Var3, sep = "*")
      reg.eq <- paste(eqComb$Var1, indp, sep = "~")
      eqSize <- expand.grid(eq1, eq2)
      regSize.eq <- paste(eqSize$Var1, eqSize$Var2, sep = "~")
      myoptions <- c(origRaw, reg.eq, regSize.eq)
      
      datas$rank[which (datas$Equations %in% myoptions)] <- 1
      myrep <- datas[datas$rank == 1,]
      
      
      
      tabUnique <- unique (myrep[c('SourceType','Response')])
      for (j in seq( 1, nrow(tabUnique))){
        myindx <- which (myrep$SourceType %in% tabUnique$SourceType[j] & myrep$Response %in% tabUnique$Response[j])
        if (length(myindx)>1){
          myrep$rank[myindx[which(!myrep$Equations[myindx] %in% myoptions)]]<-0
          myrep <- myrep[which(myrep$rank ==1),]
        }
      }
      
      for (j in seq( 1, nrow(tabUnique))){
        myindx <- which (myrep$SourceType %in% tabUnique$SourceType[j] & myrep$Response %in% tabUnique$Response[j])
        if (myrep$Equations[myindx] %in% origRaw){
          myrep$rank[myindx[which(!myrep$Equations[myindx] %in% origRaw)]]<-0
          myrep <- myrep[which(myrep$rank ==1),]
        }
      }
      
      myrep <- myrep%>%
        dplyr::group_by(SourceType, Response) %>%
        dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
      
      f <-data.frame(myrep)
    }
    correctionList[[i]] <- data.frame(f[which(f$rank == 1), ])
  }

  
  selectedData <- NULL
  for (i in seq (1, length(correctionList))){
    datTab <- cbind (as.character(target[i,1]), correctionList[[i]])
    selectedData <- rbind (selectedData, datTab)
  }
  names (selectedData)[1] <- ('TARGET')
  selectedData <- selectedData[,-ncol(selectedData)]
  selectedData <- selectedData[,-ncol(selectedData)]
  return (selectedData)
}


selectedCorrectionTabs2 <- function (correctionList, target){
  correctionList <- correctionList
  # correctionList
  for (i in seq(1, length(correctionList))) {
    correctionList[[i]] <- correctionList[[i]][which(correctionList[[i]]$OverCorrection %in% FALSE), ]
    f <- correctionList[[i]]
    f <- f %>%
      dplyr::group_by(SourceType, Response) %>%
      dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
    
    datas <- data.frame (f)
    
    origRaw <- c('(conc) ~ var1','(conc)~var1', '(conc) ~ var2', '(conc)~var2',
                   '(conc) ~ var1 + var2', '(conc) ~ var1 * var2')
    
    eq1 <- c('(conc)')
    eq2 <- c("var1", "log(var1)", "(var1^2)", "(var1^(1/2))", "(var1^(1/3))", "(var1^(-1))")
    eq3 <- c("var2", "log(var2)", "(var2^2)", "(var2^(1/2))", "(var2^(1/3))", "(var2^(-1))")
    eqComb <- expand.grid(eq1, eq2, eq3)
    indp <- paste(eqComb$Var2, eqComb$Var3, sep = "*")
    reg.eq <- paste(eqComb$Var1, indp, sep = "~")
    eqSize <- expand.grid(eq1, eq2)
    regSize.eq <- paste(eqSize$Var1, eqSize$Var2, sep = "~")
    myoptions <- c(origRaw, reg.eq, regSize.eq)
    
    datas$rank[which (datas$Equations %in% myoptions)] <- 1
    myrep <- datas[datas$rank == 1,]
    
    
    
    tabUnique <- unique (myrep[c('SourceType','Response')])
    for (j in seq( 1, nrow(tabUnique))){
      myindx <- which (myrep$SourceType %in% tabUnique$SourceType[j] & myrep$Response %in% tabUnique$Response[j])
      if (length(myindx)>1){
        myrep$rank[myindx[which(!myrep$Equations[myindx] %in% myoptions)]]<-0
        myrep <- myrep[which(myrep$rank ==1),]
      }
    }
    
    for (j in seq( 1, nrow(tabUnique))){
      myindx <- which (myrep$SourceType %in% tabUnique$SourceType[j] & myrep$Response %in% tabUnique$Response[j])
      if (myrep$Equations[myindx] %in% origRaw){
        myrep$rank[myindx[which(!myrep$Equations[myindx] %in% origRaw)]]<-0
        myrep <- myrep[which(myrep$rank ==1),]
      }
    }

    myrep <- myrep%>%
      dplyr::group_by(SourceType, Response) %>%
      dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
    
    f <- data.frame(myrep)
    correctionList[[i]] <- data.frame(f[which(f$rank == 1), ])
  }
  return (correctionList)
}

getListCorrectedData <- function (correctionList, target, x, corFor, pickModel, neg_glob){
  
  if (pickModel == 'bestM'){
    for (i in seq(1, length(correctionList))) {
      correctionList[[i]] <- correctionList[[i]][which(correctionList[[i]]$OverCorrection %in% FALSE), ]
      f <- correctionList[[i]]
      f <- f %>%
        dplyr::group_by(SourceType, Response) %>%
        dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
      correctionList[[i]] <- data.frame(f[which(f$rank == 1), ])
    }
  } else {
    correctionList <- selectedCorrectionTabs2(correctionList, target)
  }

  correctedDataFrames <- list ()
  
  for (j in seq(1, nrow(target))) { # for each target
    myresult <- correctionList[[j]]
    transformedResultDf <- x
    
    
    for (it in seq(1, nrow(myresult))) {
      datas <- x[which(x[, 2] %in% myresult[it, 1]), ] # subset of the source type
      conc <- as.numeric(unlist(datas[myresult[it, 2]]))
      
      if (length(corFor) > 1) {
        var1 <- as.numeric(unlist(datas[corFor[1]]))
        var2 <- as.numeric(unlist(datas[corFor[2]]))
        myfit <- lm(eval(parse(text = myresult[it, 4])))
        
        if (ncol(myfit$model) > 2) {
          Yi <- myfit$model[, 1]
          Si <- myfit$model[, 2]
          Ti <- myfit$model[, 3]
        } else if (ncol(myfit$model == 2)) {
          Yi <- myfit$model[, 1]
          Si <- myfit$model[, 2]
        }
        
        var1 <- as.numeric(unlist(target[j, corFor[1]]))
        var2 <- as.numeric(unlist(target[j, corFor[2]]))
        conc <- as.numeric(unlist(target[j, myresult[it, 2]]))
        myfitT <- lm(eval(parse(text = myresult[it, 4])))
        
        if (ncol(myfitT$model) > 2) {
          St <- myfitT$model[, 2]
          Tt <- myfitT$model[, 3]
        } else if (ncol(myfit$model == 2)) {
          St <- myfitT$model[, 2]
        }
        
        if (length(myfit$coefficients) > 3) {
          SlopeVar1 <- myfit$coefficients[2]
          SlopeVar2 <- myfit$coefficients[3]
          SlopeVar1Var2 <- myfit$coefficients[4] # interaction term
          corrected <- Yi - ((Si - St) * SlopeVar1 + (Ti - Tt) * SlopeVar2 + ((Si * Ti) - (St * Tt)) * SlopeVar1Var2)
        } else if (length(myfit$coefficients) == 3) {
          SlopeVar1 <- myfit$coefficients[2]
          SlopeVar2 <- myfit$coefficients[3]
          corrected <- Yi - ((Si - St) * SlopeVar1 + (Ti - Tt) * SlopeVar2)
        } else if (length(myfit$coefficients) == 2) {
          SlopeVar1 <- myfit$coefficients[2]
          corrected <- Yi - (Si - St) * SlopeVar1
        }
        
        tname <- colnames(myfit$model[1])
        if (tname == "log(conc)") {
          corrected <- exp(corrected)
        } else if (tname == "I(conc^2)") {
          corrected <- sqrt(corrected)
        } else if (tname == "I(conc^(1/2))") {
          corrected <- corrected^2
        } else if (tname == "I(conc^(1/3))") {
          corrected <- corrected^3
        } else if (tname == "I(conc^(-1))") {
          corrected <- 1 / corrected
        } else {}
        
        transformedResultDf[which(transformedResultDf[,2] %in% myresult[it, 1]),myresult[it, 2]] <- corrected
        
      } else {
        var1 <- as.numeric(unlist(datas[corFor[1]]))
        myfit <- lm(eval(parse(text = myresult[it, 4])))
        Yi <- myfit$model[, 1]
        Si <- myfit$model[, 2]
        var1 <- as.numeric(unlist(target[j, corFor[1]]))
        conc <- as.numeric(unlist(target[j, myresult[it, 2]]))
        myfitT <- lm(eval(parse(text = myresult[it, 4])))
        St <- myfitT$model[, 2]
        
        SlopeVar1 <- myfit$coefficients[2]
        corrected <- Yi - (Si - St) * SlopeVar1
        tname <- colnames(myfit$model[1])
        
        if (tname == "log(conc)") {
          corrected <- exp(corrected)
        } else if (tname == "I(conc^2)") {
          corrected <- sqrt(corrected)
        } else if (tname == "I(conc^(1/2))") {
          corrected <- corrected^2
        } else if (tname == "I(conc^(1/3))") {
          corrected <- corrected^3
        } else if (tname == "I(conc^(-1))") {
          corrected <- 1 / corrected
        } else {}
        
        
        transformedResultDf[which(transformedResultDf[,2] %in% myresult[it, 1]),myresult[it, 2]] <- corrected
      }
    }
    correctedDataFrames[[j]] <- transformedResultDf
  }
  
  for (i in seq (1, length(correctedDataFrames))){
    correctedDataFrames[[i]] <- inverse_neg_glob (correctedDataFrames[[i]], neg_glob, c(mixed))
    #correctedDataFrames[[i]][c(negatives)] <- correctedDataFrames[[i]][c(negatives)]* -1
  }
  correctedDataFrames
}


correctOneVar <- function (datas,target, corFor, element, myresult){
  if (length(corFor) > 1) {
    var1 <- as.numeric(unlist(datas[corFor[1]]))
    var2 <- as.numeric(unlist(datas[corFor[2]]))
    conc <- as.numeric(unlist(datas[element]))
    myfit <- lm(eval(parse(text = myresult)))
    
    if (ncol(myfit$model) > 2) {
      Yi <- myfit$model[, 1]
      Si <- myfit$model[, 2]
      Ti <- myfit$model[, 3]
    } else if (ncol(myfit$model == 2)) {
      Yi <- myfit$model[, 1]
      Si <- myfit$model[, 2]
    }
    
    var1 <- as.numeric(unlist(target[, corFor[1]]))
    var2 <- as.numeric(unlist(target[, corFor[2]]))
    conc <- as.numeric(unlist(target[, element]))
    myfitT <- lm(eval(parse(text = myresult))) #same for target
    
    if (ncol(myfitT$model) > 2) {
      St <- myfitT$model[, 2]
      Tt <- myfitT$model[, 3]
    } else if (ncol(myfit$model == 2)) {
      St <- myfitT$model[, 2]
    }
    
    if (length(myfit$coefficients) > 3) {
      SlopeVar1 <- myfit$coefficients[2]
      SlopeVar2 <- myfit$coefficients[3]
      SlopeVar1Var2 <- myfit$coefficients[4] # interaction term
      corrected <- Yi - ((Si - St) * SlopeVar1 + (Ti - Tt) * SlopeVar2 + ((Si * Ti) - (St * Tt)) * SlopeVar1Var2)
    } else if (length(myfit$coefficients) == 3) {
      SlopeVar1 <- myfit$coefficients[2]
      SlopeVar2 <- myfit$coefficients[3]
      corrected <- Yi - ((Si - St) * SlopeVar1 + (Ti - Tt) * SlopeVar2)
    } else if (length(myfit$coefficients) == 2) {
      SlopeVar1 <- myfit$coefficients[2]
      corrected <- Yi - (Si - St) * SlopeVar1
    }
    
    tname <- colnames(myfit$model[1])
    if (tname == "log(conc)") {
      corrected <- exp(corrected)
    } else if (tname == "I(conc^2)") {
      corrected <- sqrt(corrected)
    } else if (tname == "I(conc^(1/2))") {
      corrected <- corrected^2
    } else if (tname == "I(conc^(1/3))") {
      corrected <- corrected^3
    } else if (tname == "I(conc^(-1))") {
      corrected <- 1 / corrected
    }
  } else {
    var1 <- as.numeric(unlist(datas[corFor[1]]))
    myfit <- lm(eval(parse(text = myresult)))
    
    Yi <- myfit$model[, 1]
    Si <- myfit$model[, 2]
    var1 <- as.numeric(unlist(target[, corFor]))
    conc <- as.numeric(unlist(target[, element]))
    
    myfitT <- lm(eval(parse(text = myresult)))
    St <- myfitT$model[, 2]
    SlopeVar1 <- myfit$coefficients[2]
    corrected <- Yi - (Si - St) * SlopeVar1
    tname <- colnames(myfit$model[1])
    
    if (tname == "log(conc)") {
      corrected <- exp(corrected)
    } else if (tname == "I(conc^2)") {
      corrected <- sqrt(corrected)
    } else if (tname == "I(conc^(1/2))") {
      corrected <- corrected^2
    } else if (tname == "I(conc^(1/3))") {
      corrected <- corrected^3
    } else if (tname == "I(conc^(-1))") {
      corrected <- 1 / corrected
    }
  }
  return (corrected)
}
