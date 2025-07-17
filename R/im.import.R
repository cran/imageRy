#' Import one or more raster images
#'
#' This function imports raster images from the remote Zenodo repository
#' using exact or partial matches. It returns a SpatRaster stack of all matching files.
#'
#' @param im Character vector. Full or partial names of images available on Zenodo.
#'
#' @return A `SpatRaster` object (stack of layers).
#' @export
im.import <- function(im) {
  suppressWarnings({
    
    
    remote_files <- c(
      "bletterbach.jpg",
      "dolansprings_oli_2013088_canyon_lrg.jpg",
      "EN_01.png", "EN_02.png", "EN_03.png", "EN_04.png", "EN_05.png", "EN_06.png",
      "EN_07.png", "EN_08.png", "EN_09.png", "EN_10.png", "EN_11.png", "EN_12.png", "EN_13.png",
      "greenland.2000.tif", "greenland.2005.tif", "greenland.2010.tif", "greenland.2015.tif",
      "iss063e039892_lrg.jpg",
      "matogrosso_ast_2006209_lrg.jpg", "matogrosso_l5_1992219_lrg.jpg",
      "NDVI_rainbow.png", "NDVI_rainbow_legend.png",
      "sentinel.dolomites.b2.tif", "sentinel.dolomites.b3.tif",
      "sentinel.dolomites.b4.tif", "sentinel.dolomites.b8.tif",
      "sentinel.png",
      "Sentinel2_NDVI_2020-02-21.tif", "Sentinel2_NDVI_2020-05-21.tif",
      "Sentinel2_NDVI_2020-08-01.tif", "Sentinel2_NDVI_2020-11-27.tif",
      "Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg"
    )
    
    base_url <- "https://zenodo.org/records/15645465/files"
    all_paths <- character(0)
    
    for (single_im in im) {
      has_ext <- grepl("\\.", single_im)
      
      
      matches <- if (has_ext) {
        remote_files[remote_files == single_im]
      } else {
        remote_files[grepl(single_im, remote_files, ignore.case = TRUE)]
      }
      
      if (length(matches) == 0) {
        stop("No matching files found on Zenodo for '", single_im, "'")
      }
      
      message("Downloading ", length(matches), " file(s) from Zenodo for pattern '", single_im, "'...")
      
      for (fname in matches) {
        ext <- tools::file_ext(fname)
        temp_file <- tempfile(fileext = paste0(".", ext))
        remote_url <- paste0(base_url, "/", fname, "?download=1")
        
        tryCatch({
          utils::download.file(remote_url, destfile = temp_file, mode = "wb", quiet = TRUE)
          all_paths <- c(all_paths, temp_file)
        }, error = function(e) {
          warning("Failed to download ", fname)
        })
      }
    }
    
    return(terra::rast(all_paths))
  })
}
