library(shinytest)
library(testthat)

context("Test shiny app")

#open shiny app
app <- ShinyDriver$new("D:/SEDSATV2/SShiny/SShiny", loadTimeout = 10000)

test_that("app gets expected output", {
  #set numeric input
  app$setInputs(num = 20)
  #get output
  output <- app$getValue(name = "out")
  #test
  expect_equal(output, "400")  
})

#stop shiny app
app$stop()
