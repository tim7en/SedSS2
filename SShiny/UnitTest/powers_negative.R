library (testthat)
data <- read.csv ('powersNegativ.csv')
data[,1] <- factor (data[,1], levels = c(as.character(unique(data[,1]))))

#functions for negatives or for mixed data
tfuncWneg <- function(x) {
  c(x, sign(x) * (abs(x))^(1 / 3), x^(-1))
}

tfunczero <- function(x) {
  c(x, sign(x) * (abs(x))^(1 / 3))
}

#unique sources
uniSource <- unique (data[,1])

#Final output table
myDataOutput <- NULL
for (i in uniSource){
  subsetBySource <- data[which(data[,1] %in% i),]
  for (j in c(2,3)){
    useAllTransforms <- tfuncWneg (subsetBySource[,j])
    if (is.numeric(useAllTransforms)){
    } else {
      useAllTransforms <-tfunczero(subsetBySource[,j])
      if (is.numeric (useAllTransforms)){
      } else {
        cat(paste0('Unit test POWERS NEGATIVE: FAIL ',j, '\n'), file = "test.log", append = TRUE)
      }
    }
  }
}
cat(paste0('Unit test POWERS NEGATIVE: PASS ','\n'), file = "test.log", append = TRUE)
cat(paste0('###############################','\n'), file = "test.log", append = TRUE)