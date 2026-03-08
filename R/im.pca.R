#' Perform Principal Component Analysis (PCA) on a Raster Image
#'
#' This function applies Principal Component Analysis (PCA) to a multispectral raster image,
#' extracting principal components from a sample of pixels and projecting the full raster
#' into the PCA space.
#'
#' @param input_image A `SpatRaster` object representing the input multispectral image.
#' @param n_samples An integer specifying the number of random samples used for PCA computation (default: 100).
#' @param n_components Number of principal components to compute (default: 3).
#' @param plot Logical. If `TRUE`, the resulting principal components are plotted. Default is `TRUE`.
#'
#' @return A `SpatRaster` object containing the computed principal components.
#'
#' @details
#' Principal Component Analysis (PCA) is a statistical technique used to transform correlated
#' raster bands into a set of orthogonal components, capturing the most variance in fewer bands.
#'
#' - A sample of `n_samples` pixels is used to compute the PCA transformation.
#' - The full image is then projected onto the principal component space.
#' - The output raster contains the selected principal components.
#' - If `plot = TRUE`, the output is visualized using a `viridis` color scale.
#'
#' @seealso [im.import()], [im.ggplot()]
#'
#' @examples
#' library(terra)
#'
#' # Create a 3-band raster
#' r <- rast(nrows = 10, ncols = 10, nlyrs = 3)
#' values(r) <- runif(ncell(r) * 3)
#'
#' # Perform PCA without plotting
#' pca_result <- im.pca(r, n_samples = 100, plot = FALSE)
#'
#' # Plot the first principal component
#' plot(pca_result[[1]])
#'
#' @name im.pca
#' @export
im.pca <- function(input_image, n_samples = 100, n_components = 3, plot = TRUE) {
  if (!inherits(input_image, "SpatRaster")) {
    stop("'input_image' must be a SpatRaster object.")
  }
  
  n_bands <- terra::nlyr(input_image)
  
  if (n_bands < 2) {
    stop("'input_image' must have at least 2 layers for PCA.")
  }
  
  if (!is.numeric(n_samples) || length(n_samples) != 1 || is.na(n_samples) || n_samples <= 1) {
    stop("'n_samples' must be a single number greater than 1.")
  }
  
  if (!is.numeric(n_components) || length(n_components) != 1 || is.na(n_components) || n_components < 1) {
    stop("'n_components' must be a single number greater than or equal to 1.")
  }
  
  n_components <- as.integer(n_components)
  n_samples <- as.integer(n_samples)
  
  if (n_components > n_bands) {
    stop("'n_components' cannot be greater than the number of raster layers.")
  }
  
  sample <- terra::spatSample(
    input_image,
    size = n_samples,
    as.df = TRUE,
    na.rm = TRUE
  )
  
  if (nrow(sample) < 2) {
    stop("Not enough valid sampled pixels to perform PCA.")
  }
  
  pca <- stats::prcomp(sample, center = TRUE, scale. = TRUE)
  
  pci <- terra::predict(input_image, model = pca, index = 1:n_components)
  
  if (isTRUE(plot)) {
    viridis_palette <- grDevices::colorRampPalette(viridis::viridis(7))(255)
    terra::plot(pci, col = viridis_palette)
  }
  
  return(pci)
}