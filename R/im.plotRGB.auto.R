#' Automatically Plot a Raster Image as an RGB Composite
#'
#' This function visualizes a multispectral raster image using the first three bands
#' as an RGB composite. It applies a linear contrast stretch to enhance visualization.
#'
#' @param x A `SpatRaster` object representing the input multispectral image.
#' @param title A character string specifying the plot title (default: "Main").
#'
#' @return This function does not return an object. It directly generates a plot.
#'
#' @details
#' - The function assumes that the **first three bands** of the raster correspond to the Red, Green, and Blue channels.
#' - It uses `plotRGB()` with `stretch="lin"` to enhance contrast.
#' - The plot title is customizable via the `title` parameter.
#' - The axis and label colors are set to white for better contrast with dark backgrounds.
#'
#' @seealso [im.import()], [im.ggplot()]
#'
#' @examples
#' library(terra)
#'
#' # Create a 3-band raster with random values
#' r <- rast(nrows = 10, ncols = 10, nlyrs = 3)
#' values(r) <- runif(ncell(r) * 3)
#'
#' # Plot RGB composite
#' im.plotRGB.auto(r, title = "RGB Visualization")
#' @export
im.plotRGB.auto <- function(x, title = 'Main') {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))  # deve essere subito dopo la modifica
  plotRGB(x, 1, 2, 3, stretch = "lin")
  par(col.axis = "white", col.lab = "white", tck = 0)
  title(main = title, cex.main = 1.3, line = 2.5)
}
