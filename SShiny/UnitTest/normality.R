library (testthat)
data <- read.csv ('normality.csv')
data[,1] <- factor (data[,1], levels = c(as.character(unique(x))))

#functions of ladder of powers
tfunc <- function(x) {
  c(x, x^2, x^(1 / 2), x^(1 / 3), x^(-1), x^(-1 / 2), log10(x))
}

#names of transformations in th ladder of powers
tnames <- c("x", "x^2", "x^(1/2)", "x^(1/3)", "x^(-1)", "x^(-1/2)", "log10(x)")

#unique sources
uniSource <- unique (data[,1])

#Final output table
myDataOutput <- NULL
for (i in uniSource){
  subsetBySource <- data[which(data[,1] %in% i),]
  useAllTransforms <- tfunc (subsetBySource[,2])
  transformedValues <- matrix(useAllTransforms,nrow = nrow(subsetBySource),ncol = 7)
  myDataOutput <- rbind (myDataOutput, transformedValues)
}
myDataOutput <- as.data.frame (myDataOutput)
names (myDataOutput) <- tnames
myDataOutput <- cbind (data[,1], myDataOutput)
names (myDataOutput)[1] <- 'SourceType'

tryCatch({
  expect_that(dim(myDataOutput), is_equivalent_to(c(65,8)))
  cat('Unit test NORMALITY: PASS ', file = "test.log", append = TRUE)
}, error = function (er){
  log_con <- file("test.log")
  cat('Unit test REFACTOR: FAIL ', file = "test.log", append = TRUE)
  cat(paste0(er$message,'\n'), file = "test.log", append = TRUE)
  cat(paste0('###############################','\n'), file = "test.log", append = TRUE)
})