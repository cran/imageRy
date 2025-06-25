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
    # If 'im' is a valid file path, read it directly
    if (file.exists(im)) {
      r <- terra::rast(im)
      return(r)
    }
    
    # Search in package images
    ls <- list.files(system.file("images", package = "imageRy"))
    fname <- ls[grep(im, ls)]
    
    if (length(fname) > 0) {
      fpath <- system.file("images", fname, package = "imageRy")
      r <- terra::rast(fpath)
      return(r)
    }
    
    # Download from Zenodo
    message(" Not found locally. Trying to download from Zenodo...")
    base_url <- "https://zenodo.org/records/15645465/files"
    remote_url <- paste0(base_url, "/", im, "?download=1")
    
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(im)))
    tryCatch({
      utils::download.file(remote_url, destfile = temp_file, mode = "wb", quiet = TRUE)
      r <- terra::rast(temp_file)
      return(r)
    }, error = function(e) {
      stop("Image '", im, "' could not be found locally or downloaded from Zenodo.")
    })
  })
}
