#' Import one or more raster images
#'
#' This function imports raster images from the package's internal collection,
#' from user-specified local paths, or from the remote Zenodo repository.
#'
#' @param im Character vector. Names of images included in the package,
#'           or full file paths to user-provided raster images.
#'
#' @return A `SpatRaster` object (stack of layers).
#' @export
im.import <- function(im) {
  suppressWarnings({
    # Lista finale di path da passare a terra::rast()
    all_paths <- character(0)
    
    for (single_im in im) {
      # 1. File locale completo
      if (file.exists(single_im)) {
        all_paths <- c(all_paths, single_im)
        next
      }
      
      # 2. Cerca nel pacchetto
      ls <- list.files(system.file("images", package = "imageRy"))
      fname <- ls[grep(single_im, ls)]
      
      if (length(fname) > 0) {
        fpath <- system.file("images", fname, package = "imageRy")
        all_paths <- c(all_paths, fpath)
        next
      }
      
      # 3. Download da Zenodo
      message("Not found locally. Trying to download '", single_im, "' from Zenodo...")
      base_url <- "https://zenodo.org/records/15645465/files"
      remote_url <- paste0(base_url, "/", single_im, "?download=1")
      
      temp_file <- tempfile(fileext = paste0(".", tools::file_ext(single_im)))
      tryCatch({
        utils::download.file(remote_url, destfile = temp_file, mode = "wb", quiet = TRUE)
        all_paths <- c(all_paths, temp_file)
      }, error = function(e) {
        stop("Image '", single_im, "' could not be found locally or downloaded.")
      })
    }
    
    # Importa tutti i raster in uno stack
    r <- terra::rast(all_paths)
    return(r)
  })
}
