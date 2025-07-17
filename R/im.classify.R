#' Classify a Raster Image Using K-Means Clustering
#'
#' This function performs unsupervised classification on a raster image using k-means clustering.
#' It rescales the pixel values to 0–255 to improve visual clustering of scientific TIFFs.
#'
#' @param input_image A `SpatRaster` object representing the input raster image.
#' @param num_clusters An integer specifying the number of clusters (default: 3).
#' @param seed An optional integer seed for reproducibility of k-means clustering results (default: NULL).
#' @param do_plot A logical value indicating whether to display the classified raster (default: TRUE).
#' @param custom_colors A vector of custom colors to be used for classification visualization (default: NULL).
#' If NULL, a predefined set of colors is used.
#' @param num_colors The number of colors to interpolate in the visualization palette (default: 100).
#'
#' @return A `SpatRaster` object with cluster assignments.
#' @export
im.classify <- function(input_image, num_clusters = 3, seed = NULL, do_plot = TRUE, custom_colors = NULL, num_colors = 100) {
  
  
  if (!inherits(input_image, "SpatRaster")) {
    stop("input_image should be a SpatRaster object.")
  }
  
  
  nlyr <- terra::nlyr(input_image)
  if (nlyr > 3) {
    stop("This function is for RGB or grayscale images only. Reduce bands before classification.")
  }
  
  
  base_colors <- c('khaki', 'slateblue', 'olivedrab', 'salmon', 'lightpink', 'darkgrey')
  colors <- if (is.null(custom_colors)) {
    if (num_clusters > length(base_colors)) {
      colorRampPalette(base_colors)(num_clusters)
    } else {
      base_colors[1:num_clusters]
    }
  } else {
    if (num_clusters > length(custom_colors)) {
      colorRampPalette(custom_colors)(num_clusters)
    } else {
      custom_colors[1:num_clusters]
    }
  }
  
  
  image_values <- terra::as.matrix(input_image)
  
  
  for (i in seq_len(ncol(image_values))) {
    min_val <- min(image_values[, i], na.rm = TRUE)
    max_val <- max(image_values[, i], na.rm = TRUE)
    image_values[, i] <- 255 * (image_values[, i] - min_val) / (max_val - min_val)
  }
  
  
  image_values <- na.omit(image_values)
  
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  kmeans_result <- kmeans(image_values, centers = num_clusters)
  
  
  classified_image <- input_image[[1]]
  terra::values(classified_image) <- NA
  terra::values(classified_image)[!is.na(terra::values(input_image[[1]]))] <- kmeans_result$cluster
  
  
  if (do_plot) {
    color_palette <- colorRampPalette(colors)(num_colors)
    terra::plot(classified_image, col = color_palette, axes = FALSE)
  }
  
  return(classified_image)
}
