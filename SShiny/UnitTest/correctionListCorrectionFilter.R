library(dplyr)

#selectedCorrectionTabs <- function (correctionList, target){
  # correctionList
for (i in seq(1, length(correctionList))) {
  correctionList[[i]] <- correctionList[[i]][which(correctionList[[i]]$OverCorrection %in% FALSE), ]
  f <- correctionList[[i]]
  f <- f %>%
    dplyr::group_by(SourceType, Response) %>%
    dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
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


for (i in seq(1, length(correctionList))) {
  correctionList[[i]] <- correctionList[[i]][which(correctionList[[i]]$OverCorrection %in% FALSE), ]
  f <- correctionList[[i]]
  f <- f %>%
    dplyr::group_by(SourceType, Response) %>%
    dplyr::mutate(rank = rank(desc(PearsonR2), ties.method = "first"))
  correctionList[[i]] <- data.frame(f[which(f$rank == 1), ])
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
      }
      
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
      }
      transformedResultDf[which(transformedResultDf[,2] %in% myresult[it, 1]),myresult[it, 2]] <- corrected
    }
  }
  correctedDataFrames[[j]] <- transformedResultDf
}
  #correctedDataFrames
#}
