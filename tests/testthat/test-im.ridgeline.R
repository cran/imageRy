library(testthat)
library(imageRy)
library(terra)
library(ggplot2)

skip_if_not_installed("ggridges")

test_that("im.ridgeline generates a valid ridgeline plot", {
  # Create a raster with three layers
  r <- rast(nrows = 10, ncols = 10, nlyrs = 3)
  values(r) <- runif(ncell(r) * nlyr(r))  # Fill with random values
  
  # Call the function
  ridgeline_plot <- im.ridgeline(r, scale = 2, palette = "viridis")
  
  # Check that it's a ggplot
  expect_s3_class(ridgeline_plot, "ggplot")
  
  # Check for the ggridges geom
  geoms <- lapply(ridgeline_plot$layers, function(l) class(l$geom)[1])
  expect_true("GeomDensityRidgesGradient" %in% geoms)
})
