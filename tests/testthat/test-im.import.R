#' Import a raster image
#'
#' This function imports a raster image from the package's internal image collection,
#' from a user-specified local path, or from a remote Zenodo repository if the image
#' is not found locally.
#'
#' @param im A character string. Either the name of an image included in the package, 
#'           or a full file path to a user-provided raster image.
#'
#' @return A `SpatRaster` object.
#' @export
im.import <- function(im) {
  suppressWarnings({
    
    # 1. If it's a local path
    if (file.exists(im)) {
      message("Loading local file: ", im)
      r <- terra::rast(im)
      plot(r)
      return(r)
    }
    
    # 2. If it's included in the package
    pkg_files <- list.files(system.file("images", package = "imageRy"))
    fname <- pkg_files[grep(im, pkg_files)]
    if (length(fname) > 0) {
      fpath <- system.file("images", fname, package = "imageRy")
      message("Loading image from package: ", fpath)
      r <- terra::rast(fpath)
      plot(r)
      return(r)
    }
    
    # 3. Download from Zenodo
    message("Image not found locally. Attempting download from Zenodo...")
    base_url <- "https://zenodo.org/records/15645465/files"
    url <- paste0(base_url, "/", im, "?download=1")
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(im)))
    
    message("Downloading from: ", url)
    download.file(url, destfile = temp_file, mode = "wb")
    
    r <- terra::rast(temp_file)
    plot(r)
    return(r)
  })
}
