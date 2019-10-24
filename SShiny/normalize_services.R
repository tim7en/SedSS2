# Function used over a data frame to compute shapiro wilk test of each column in a data frame
# Column 1 - id of rows
# Column 2 - source type
# Rest of columns are numeric are numeric values
# This is a shortcut that avoids looping over each source outside of current function
rawShapiro <- function(x) {
  sourcefacts <- unique(x[, 2]) # column 2 should be factor (it used to find classes)
  l <- c(as.character(sourcefacts))

  extractpval <- function(y) {   # extract shapiro wilk test p-values
    return(y$p.value)
  }
  dfShapiro <- function(y) {
    source <- x[which(x[, 2] == y), ]
    shapres <- tryCatch({
      apply(source[, 3:dim(source)[2]], 2, shapiro.test)
    }, error = function(e) {
      return(NULL)
    })
    shapiropvals <- unlist(lapply(shapres, extractpval))
  }
  lvals <- lapply(l, dfShapiro)
  names(lvals) <- l
  dfoutput <- t(round(as.data.frame(lvals), 3))
  return(dfoutput)
}


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
      valArray <- transformedValues[, which(l %in% valMax)]
      if (valMax < pVal) {
        x <- x * -1
        funcTransform(x, tabversion, pVal)
      }
    } else if (any(x < 0)) {
      y <- tfuncWneg(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 3)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnamesneg[which(l %in% valMax)]
      valArray <- transformedValues[, which(l %in% valMax)]
      if (valMax < pVal) {
        x <- x + 1 ## need to change to add a constant
        funcTransform(x, tabversion, pVal)
      }
    } else if (any(x == 0)) {
      y <- tfunczero(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 2)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnameszero[which(l %in% valMax)]
      valArray <- transformedValues[, which(l %in% valMax)]
      if (valMax < pVal) {
        x <- x + 1 ## need to change to add a constant
        funcTransform(x, tabversion, pVal)
      }
    } else if (all(x > 0)) {
      y <- tfunc(x)
      transformedValues <- matrix(y, nrow = length(x), ncol = 7)
      l <- unlist(apply(transformedValues, 2, userShapiroTest))
      valMax <- max(l[!is.nan(l)])
      valFormula <- tnames[which(l %in% valMax)]
      valArray <- transformedValues[, which(l %in% valMax)]
    } else {
      print("Not comfortable array")
    }
  }
  if (tabversion == "formulas") {
    if (valMax < pVal) {
      valFormula <- "None"
    }
    return(valFormula)
  } else if (tabversion == "mat") {
    if (valMax < pVal) {
      return(x)
    } else {
      round(valArray, 3)
    }
  } else {
    if (valMax < pVal) {
      round(shapiro.test(x)$p)
    } else {
      round(valMax, 3)
    }
  }
}
# available functions for transformation applied for Shapiro-Wilk
tfunc <- function(x) {
  c(x, x^2, x^(1 / 2), x^(1 / 3), x^(-1), x^(-1 / 2), log10(x))
}
tnames <- c("x", "x^2", "x^(1/2)", "x^(1/3)", "x^(-1)", "x^(-1/2)", "log10(x)")
tfuncWneg <- function(x) {
  c(x, sign(x) * (abs(x))^(1 / 3), x^(-1))
}
tnamesneg <- c("x", "sign(x)*abs(x)^(1/3)", "x^(-1)")
tfunczero <- function(x) {
  c(x, sign(x) * (abs(x))^(1 / 3))
}
tnameszero <- c("x", "sign(x)*(abs(x)^(1/3)")
