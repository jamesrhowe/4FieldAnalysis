library(testthat)
library(shinytest)

test_that("Application works", {
  expect_pass(testApp("4-Quadrant-Analysis", compareImages = FALSE))
})

