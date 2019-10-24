library(testthat)
data <- read.csv("powerMixed.csv")
siteID <- data[,1]
data <- data[, -1] # drop columns with site id's
data[, 1] <- factor(data[, 1], levels = c(as.character(unique(data[, 1]))))

# functions of ladder of powers
tfunc <- function(x) {
  c(x, x^2, x^(1 / 2), x^(1 / 3), x^(-1), x^(-1 / 2), log10(x))
}

# names of transformations in th ladder of powers
tnames <- c("x", "x^2", "x^(1/2)", "x^(1/3)", "x^(-1)", "x^(-1/2)", "log10(x)")

# functions for negatives or for mixed data
tfuncWneg <- function(x) {
  c(x, sign(x) * (abs(x))^(1 / 3), x^(-1))
}
tnamesneg <- c("x", "sign(x)*abs(x)^(1/3)", "x^(-1)")
tfunczero <- function(x) {
  c(x, sign(x) * (abs(x))^(1 / 3))
}
tnameszero <- c("x", "sign(x)*(abs(x)^(1/3)")


userShapiroTest <- function(x) {
  tryCatch({
    shapiro.test(x)$p
  }, error = function(er) {

  })
}
# check for func
funcTransform <- function(x, tabversion, pVal) {
  x <- as.numeric(x)
  if (shapiro.test(x)$p > pVal) {
    valMax <- shapiro.test(x)$p
    valFormula <- "None"
    valArray <- x
  } else {
    if (all(x < 0)) {
      y <- tfuncWneg(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 3)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnamesneg[which(l %in% valMax)]
      valArray <- transformedValues[,which(l %in% valMax)]
      if (valMax < pVal) {
        x <- x * -1
        funcCheck(x, tabversion, pVal)
      }
    } else if (any(x < 0)) {
      y <- tfuncWneg(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 3)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnamesneg[which(l %in% valMax)]
      valArray <- transformedValues[,which(l %in% valMax)]
      if (valMax < pVal) {
        x <- x + 1 ## need to change to add a constant
        funcCheck(x, tabversion, pVal)
      }
    } else if (any(x == 0)) {
      y <- tfunczero(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 2)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnameszero[which(l %in% valMax)]
      valArray <- transformedValues[,which(l %in% valMax)]
      if (valMax < pVal) {
        x <- x + 1 ## need to change to add a constant
        funcCheck(x, tabversion, pVal)
      }
    } else if (all(x > 0)) {
      y <- tfunc(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 7)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnames[which(l %in% valMax)]
      valArray <- transformedValues[,which(l %in% valMax)]
    } else {
      print("Not comfortable array")
    }
  }
  if (tabversion == "formulas") {
    if (valMax < pVal) {
      valFormula <- "None"
    }
    return(valFormula)
  } else if(tabversion == "mat"){
    if (valMax < pVal){
      return (x)
    } else {
      round (valArray,3)
    }
  } else {
    if (valMax < pVal) {
      round (shapiro.test(x)$p)
    } else {
      round (valMax,3)
    }
  }
}
# unique sources
uniSource <- unique(data[, 1])
# Final output table
myDataOutput <- NULL
myFormulaOutput <- NULL
myDataMat <- NULL

for (i in uniSource) {
  subsetBySource <- data[which(data[, 1] %in% i), ]
  shapiroPvals <- apply(subsetBySource[, -1], 2, funcTransform, tabversion = "val", pVal = 0.05) # apply each function and check if all the values are numeric
  shapiroPeq <- apply(subsetBySource[, -1], 2, funcTransform, tabversion = "formulas", pVal = 0.05)
  shapiroMat <- apply(subsetBySource[, -1], 2, funcTransform, tabversion = "mat", pVal = 0.05)
  shapiroMat <- cbind (i, shapiroMat)
  myDataOutput <- rbind(myDataOutput, shapiroPvals)
  myFormulaOutput <- rbind(myFormulaOutput, shapiroPeq)
  myDataMat <- rbind (myDataMat, shapiroMat)
}

# Final output table
myDataOutput <- as.data.frame(myDataOutput)
myDataOutput <- cbind(uniSource, myDataOutput)

# Final output table
myFormulaOutput <- as.data.frame(myFormulaOutput)
myFormulaOutput <- cbind(uniSource, myFormulaOutput)

myDataMat <- cbind (as.character(siteID), myDataMat)
myDataMat <- as.data.frame (myDataMat)
myDataMat[-c(1,2)] <- apply (myDataMat[-c(1,2)], 2, as.numeric)
colnames (myDataMat)[c(1,2)] <- c('sample', 'source')
