## Multivariate normality and box cox transformation
library(MVN)
library(mvnormtest)
library (smwrBase)
# neg_glob <- "x/1000+1,(x-1)*1000"
# x <- read.csv("AllenSource.csv")
# x <- convert(x, neg_glob)

OUTPUT_MVTEST <- function(x, drop = NULL, shapiro_p_val = 0.05) {
  
  #OUTPUT_MVTEST(source_l_glob[[6]], 'D50', 0.01)
  #x <- source_l_glob[[6]]
  if (is.null(drop)){
    leaveout <- 1
  } else {
    leaveout <- which (names(x) %in% drop)
    if (length(leaveout)<1){ leaveout <- 1}
  }
  
  MVTEST <- function(x) {
    mardia_result <- mvn(x[, -c(1, 2, leaveout)], mvnTest = c("mardia"))$`multivariateNormality`
    mardia_skew <- as.character(mardia_result[1, 4])
    mardia_kurtosis <- as.character(mardia_result[2, 4])
    henze_zirkler <- as.character(mvn(x[, -c(1, 2, leaveout)], mvnTest = c("hz"))$`multivariateNormality`[, 4])
    royston <- as.character(mvn(x[, -c(1, 2, leaveout)], mvnTest = c("royston"))$`multivariateNormality`[, 4])
    myx <- as.matrix(x[, -c(1, 2)])

    shapiro_wilk <- "NOT APPLICABLE"
    tryCatch({
      shapiro_wilk <- mshapiro.test(t(as.matrix(myx)))
      if (shapiro_wilk$p.value > shapiro_p_val) { # if multivariate normality, say yes, else no
        shapiro_wilk <- "YES"
      } else {
        shapiro_wilk <- "NO"
      }
    }, error = function(e) {
      shapiro_wilk <- "NOT APPLICABLE"
    })
    return(c(mardia_skew, mardia_kurtosis, henze_zirkler, royston, shapiro_wilk))
  }



  rawDataResult <- tryCatch ({
    #test for multivariate normality raw data
    MVTEST(x)
  }, warning = function (w){
    c('NO', 'NO', 'NO', 'NO', 'NO')
  })

  
  #try box cox tranformations
  bcTransformed <- tryCatch({
    car::powerTransform(x[, -c(1, 2, leaveout)], family = "bcPower")
  }, error = function(e) {
    tryCatch ({
      #turn into positive space
      x <- convert (x, neg_glob)
      bcTransformed <- car::powerTransform(x[, -c(1, 2, leaveout)], family = "yjPower")
      bcTransformed
    }, error = function (e){
      return (NULL)
    })    
  }, warning = function (w){
    
  })
  
  if (is.null(bcTransformed)){
    myvar <- x[,-c(1,2,leaveout)]
  } else {
    # get lambda values for box cox transformation
    mylambda <- bcTransformed$lambda
    my_x <- convert (x, neg_glob)
    myvar <- my_x[, -c(1, 2, leaveout)]
    # transform data with box cox transformation
    for (i in seq(1, ncol(myvar))) {
      myvar[, i] <- smwrBase::boxCox(myvar[, i], as.numeric(mylambda)[i])
    }
  }

  #add columns with names and source types
  if (leaveout != 1){
    myvar <- data.frame(x[, 1], x[, 2], x[ ,c(leaveout)], myvar)
    names(myvar)[c(1, 2, leaveout)] <- names(x)[c(1, 2, leaveout)]
  } else {
    myvar <- data.frame(x[, 1], x[, 2], myvar)
    names(myvar)[c(1, 2)] <- names(x)[c(1, 2)]
  }

  
  
  boxcoxDataResult <- tryCatch ({
    #Test boxcox tranformed data for multivariate normality
    MVTEST(myvar)
  }, warning = function (w){
    c('NO', 'NO', 'NO', 'NO', 'NO')
  })
  

  #return results
  rawResult <- length(which(rawDataResult == "NO"))
  bcResult <- length(which(boxcoxDataResult == "NO"))
  if (rawResult == bcResult) {
    myreturn <- data.frame(myvar)
    names(myreturn)[c(1, 2)] <- names(x)[c(1, 2)]
    return(myreturn)
  } else if (bcResult < rawResult) {
    myreturn <- data.frame(myvar)
    names(myreturn)[c(1, 2)] <- names(x)[c(1, 2)]
    return(myreturn)
  } else {
    return (x)
  }
  
}
