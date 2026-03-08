#' Automatically Plot a Raster Image as an RGB Composite
#'
#' This function visualizes a multispectral raster image using the first three bands
#' as an RGB composite. It applies a linear contrast stretch to enhance visualization.
#'
#' @param x A `SpatRaster` object representing the input multispectral image.
#' @param title A character string specifying the plot title (default: "Main").
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [im.import()], [im.ggplot()]
#'
#' @examples
#' library(terra)
#'
#' r <- rast(nrows = 10, ncols = 10, nlyrs = 3)
#' values(r) <- runif(ncell(r) * 3)
#'
#' im.plotRGB.auto(r, title = "RGB Visualization")
#' @export
im.plotRGB.auto <- function(x, title = "Main") {
  if (!inherits(x, "SpatRaster")) {
    stop("'x' must be a SpatRaster object.")
  }
  
  if (terra::nlyr(x) < 3) {
    stop("'x' must have at least 3 layers to create an RGB composite.")
  }
  
  terra::plotRGB(x, 1, 2, 3, stretch = "lin", main = title)
  
  invisible(x)
}