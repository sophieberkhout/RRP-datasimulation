context("minimum and maximum values")
source(file = "../../R/Shiny/mySample.r")
source(file = "../../R/Shiny/createSigma.r")
test_that("minimum is minimum",{
  expect_equal(min(mySample(10, rep(0, 6), -1, 1, createSigma("2x3"))), -1)
})
