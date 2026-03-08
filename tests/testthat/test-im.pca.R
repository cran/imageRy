library(testthat)
library(imageRy)
library(terra)

test_that("im.pca correctly performs Principal Component Analysis (PCA)", {
  # Create a raster with three bands (simulating multispectral data)
  r <- rast(nrows = 10, ncols = 10, nlyrs = 3)
  values(r) <- runif(300)  # 10 * 10 * 3
  
  # Perform PCA without plotting
  pca_result <- im.pca(r, n_samples = 100, plot = FALSE)
  
  # Ensure that the result is a `SpatRaster`
  expect_s4_class(pca_result, "SpatRaster")
  
  # Ensure that the number of layers in the result matches the requested components
  expect_equal(nlyr(pca_result), 3)
  
  # Check that the output contains values
  expect_false(all(is.na(values(pca_result))))
})