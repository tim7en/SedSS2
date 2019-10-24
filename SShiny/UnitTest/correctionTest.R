library (lmtest)
library (car)

x <- read.csv ('correctionData.csv')
y <- read.csv ('correctionTarget.csv')


# Create possible combination of formulas (7^3 possible combinations)
eq1 <- c("(conc)", "log(conc)", "I(conc^2)", "I(conc^(1/2))", "I(conc^(1/3))", "I(conc^(-1))")
eq2 <- c("var1", "log(var1)", "I(var1^2)", "I(var1^(1/2))", "I(var1^(1/3))", "I(var1^(-1))")
eq3 <- c("var2", "log(var2)", "I(var2^2)", "I(var2^(1/2))", "I(var2^(1/3))", "I(var2^(-1))")


#Multiple regression
eqComb <- expand.grid(eq1, eq2, eq3)
indp <- paste(eqComb$Var2, eqComb$Var3, sep = "*")
multipleRegression <- paste(eqComb$Var1, indp, sep = "~")


#Simple regression
eqSize <- expand.grid(eq1, eq2)
simpleRegression <- paste(eqSize$Var1, eqSize$Var2, sep = "~")


#Evaluate multiple regression w size & toc
#x is an array of functions to evaluate
#conc is the response variable
#y is a list of predictors
custom_lmEval <- function (x, conc, y, R2.adj, residualsDiv, alphalevel) {

  if (length(y)>1){
    for (i in seq (1, length(y))){
      assign(paste0('var',i), y[[i]])
    }
    print (class(var1))
    print (var2)
    myEquation <- step(lm(eval(parse(text = x))), trace = 0) #forward stepwise model selection
  } else {
    var1 <- unlist(y)
    myEquation <- lm(eval(parse(text = x))) #simple regression
  }

  mydw <- dwtest (myEquation) #Durbin Watson Test autocorrelation of disturbances
  mybp <- bptest(myEquation) #Heteroscadastisity Breusch-Pagan test studentized Breusch-Pagan test (package lmtest)
  myresp <- shapiro.test(myEquation$residuals)$p.value
    
  if (ncol (myEquation$model) > 2){
    myvif <- vif(myEquation) #variance inflation factors (if vif >10 there is a multicollinearity)
  } else {
    myvif <- 0 #else just assume as 0
  }
  
  myCooksDistance <- cooks.distance(myEquation) #cooks distance of residuals, detection of outliers (3 or more deviations)

  lmSum <- summary (myEquation)
  lmPvalue<- lmSum$coefficients[,4]
  
  if (lmPvalue > alphalevel){ #significance of equation
    return (NA)
  } else if (myresp < 2*alphalevel) {  #normality of residuals from Royston (1995)
    return (NA)
  } else if ( any(myCooksDistance > 1)){ #cooks distance, impact of certain measure in model
    return (NA)
  } else if (lmSum$r.squared < R2.adj){ #pearson correlation sq
    return (NA)
  } else if (mydw$p.value < alphalevel) { #durbin watson test of autocorrelation
    return (NA)
  } else if (mybp$p.value < alphalevel) { #breusch-pagan test of heteroscadastisity
    return (NA)
  } else if (any(myvif > 10)){ #default VIF 10 #variance inflation factor
    return (NA)
  } else {
    print (myEquation$coefficients)
    return (c(lmSum$r.squared,myresp, mydw$p.value, mybp$p.value, as.character(myEquation$call)))
  }
}


#evaluate regressions (can be parallel or in sequence) #can be adjusted for mac or windows
applyRegression <- function (x, y, deviates, R2, alphalevels) {
  if (length (y) > 1){
    var1 <- y[[1]]
    var2 <- y[[2]]
    conc <- x
    output <- sapply (multipleRegression, custom_lmEval, conc, list(var1, var2),residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
    return (output)
  } else {
    var1 <- unlist(y)
    conc <- x
    output <- sapply (simpleRegression, custom_lmEval, conc, list(var1),residualsDiv = deviates, R2.adj = R2, alphalevel = alphalevels)
    return (output)
  }
}


uniSource <- unique (x[,2])
outputDataFrame <- NULL
listOutputDataFrame <- NULL

for (i in uniSource){
  datas <- x[which(x[,2] == i),]
  
  myoutput <- applyRegression(datas$Potassiumgg, list(datas$D50, datas$TOC), 3, 0.7, 0.05)
  dat <- na.omit(cbind (i, myoutput))
  
  if (nrow(dat)<1){
    myoutput <- applyRegression(datas$Potassiumgg, list(datas$D50), 3, 0.7, 0.05)
    dat <- na.omit(cbind (i, myoutput))
    if (nrow(dat)<1){
      myoutput <- applyRegression(datas$Potassiumgg, list(datas$TOC), 3, 0.7, 0.05)
      dat <- na.omit(cbind (i, myoutput))
      if (nrow(dat)>1){
        outputDataFrame <- rbind (outputDataFrame,(cbind (i, unlist(myoutput), 'var2')))
      }
    } else {
      outputDataFrame <- rbind (outputDataFrame,(cbind (i, unlist(myoutput), 'var1')))
    }
  } else {
    outputDataFrame <- rbind (outputDataFrame,(cbind (i, unlist(myoutput), 'var1*var2')))
  }
}

outputDataFrame <- data.frame (na.omit(outputDataFrame))
rownames (outputDataFrame) <- NULL
names (outputDataFrame) <- c('SourceType', 'Pars', 'Equation')

outputDataFrame$formula <- multipleRegression
outputDataFrame <- na.omit (outputDataFrame)




#


x <- x[which (x$TYPE == 'FOREST'),]

Si <- x$D50^(-1)
Ti <- x$TOC^(1/2)
Yi <- x$Potassiumgg^(1/2)
Sj <- y$D50[1]^(-1)
Tj <- y$TOC[1]^(1/2)

myfit <- lm (Yi~Si)
myfit$coefficients
Ss <- myfit$coefficients[2]
Ts <- myfit$coefficients[3]

Corrected <- (Yi - (((Si - Sj) * Ss) + ((Ti - Tj) * Ts)))
Corrected^2

#selectedOption <- outputDataFrame[which (as.numeric(as.character(outputDataFrame$AdjR2)) %in% max(as.numeric(as.character(outputDataFrame$AdjR2)))),]



