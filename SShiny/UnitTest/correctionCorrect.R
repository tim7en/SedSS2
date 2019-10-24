#myresult
#datas
#target
corFor <- c('D50', 'TOC')
x <- datas

#correctionIntegrityCheck <- function (x, target, myresult, corFor){
correctionList <- list ()

for (j in seq(1, nrow(target))) {
  myresult$OverCorrection <- FALSE
  for (i in seq(1, nrow(myresult))) {
    datas <- x[which (x[,2] %in% myresult[i,1]),] #subset of the source type
    conc <- as.numeric(unlist(datas[myresult[i, 2]]))
    
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
  #return (correctionList)
#}
#manual check
#FOREST           N       TOC              I(conc^2)~var1       0     0.945       0.466 0.760 0.893           TRUE

# datas <- datas[which(datas$TYPE %in% "FOREST"),]
# conc <- datas$N
# var1 <- datas$TOC
# mymodel <- lm(eval(parse(text = "I(conc^2)~var1")))
# 
# Yi <- mymodel$model[1]
# Ss <- mymodel$model[2]
# SlopeVar1 <- as.numeric(unlist(mymodel$coefficients[2]))
#   
# conc <- target$N[1]
# var1 <- target$TOC[1]
# mymodelT <- lm(eval(parse(text = "I(conc^2)~var1")))
# St <- as.numeric(unlist(mymodelT$model[2]))
# 
# corrected <- Yi - (Ss-St)*SlopeVar1
# sqrt (corrected)

#confirmed overcorrection (sqrt(val<0) is NaN)