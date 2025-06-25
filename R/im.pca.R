#' Perform Principal Component Analysis (PCA) on a Raster Image
#'
#' This function applies Principal Component Analysis (PCA) to a multispectral raster image,
#' extracting all available principal components. It reduces dimensionality while preserving
#' the most important variance in the dataset.
#'
#' @param input_image A `SpatRaster` object representing the input multispectral image.
#' @param n_samples An integer specifying the number of random samples used for PCA computation (default: 100).
#' @param n_components Number of principal components to compute.
#'
#' @return A `SpatRaster` object containing all computed principal components.
#'
#' @details
#' Principal Component Analysis (PCA) is a statistical technique used to transform correlated
#' raster bands into a set of orthogonal components, capturing the most variance in fewer bands.
#'
#' - The function **automatically determines** the number of components based on the number of bands.
#' - A sample of `n_samples` pixels is used to compute the PCA transformation.
#' - The **full image** is then projected onto the principal component space.
#' - The resulting raster contains **all computed principal components**.
#' - The output is visualized using a `viridis` color scale.
#'
#' @seealso [im.import()], [im.ggplot()]
#'
#' @examples
#' library(terra)
#' library(viridis)
#'
#' # Create a 3-band raster
#' r <- rast(nrows = 10, ncols = 10, nlyrs = 3)
#' values(r) <- runif(ncell(r) * 3)
#'
#' # Perform PCA
#' pca_result <- im.pca(r, n_samples = 100)
#'
#' # Plot the first principal component
#' plot(pca_result[[1]])
#'
#' @name im.pca
#' @export
im.pca <- function(input_image, n_samples = 100, n_components = 3) {
  # Check if input is a SpatRaster
  if (!inherits(input_image, "SpatRaster")) {
    stop("input_image should be a SpatRaster object.")
  }
  
  # Sample n pixels from all bands (as a data frame)
  sample <- terra::spatSample(input_image, size = n_samples, as.df = TRUE, na.rm = TRUE)
  
  # Perform PCA on the sampled data
  pca <- stats::prcomp(sample, center = TRUE, scale. = TRUE)
  print(summary(pca))
  
  # Apply the PCA transformation to the full raster
  pci <- terra::predict(input_image, model = pca, index = 1:n_components)
  
  # Define the viridis color palette
  viridis_palette <- grDevices::colorRampPalette(viridis::viridis(7))(255)
  
  # Plot the resulting components
  terra::plot(pci, col = viridis_palette)
  
  # Return the full raster of principal components
  return(pci)
}
