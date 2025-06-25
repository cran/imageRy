test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
library(testthat)
library(imageRy)
library(grDevices)

test_that("im.multiframe executes without error", {
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  
  expect_silent(im.multiframe(2, 2))
  
  dev.off()
})
