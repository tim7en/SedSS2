library (plyr)
library (testthat)
data <- read.csv ('myinput.csv')

#input array
x <- data[,1]
x <- plyr::mapvalues(x, from = c('123','FOREST'), to = c('FOREST', 'FOREST'))
x <- factor (x, levels = c(as.character(unique(x))))

tryCatch({
  expect_that(levels(x), is_equivalent_to(c(unique(x))))
  cat(paste0('Unit test REFACTOR: PASS ','\n'), file = "test.log", append = TRUE)
  cat(paste0('###############################','\n'), file = "test.log", append = TRUE)
}, error = function (er){
  log_con <- file("test.log")
  cat('Unit test REFACTOR: FAIL ', file = "test.log", append = TRUE)
  cat(paste0(er$message,'\n'), file = "test.log", append = TRUE)
  cat(paste0('###############################','\n'), file = "test.log", append = TRUE)
})
