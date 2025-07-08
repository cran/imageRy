#' List available example files
#'
#' This function lists example files included locally in the package and, optionally,
#' remote files hosted on Zenodo.
#'
#' @param include_remote Logical. If TRUE (default), also lists remote files hosted on Zenodo.
#'
#' @return A character vector of file names.
#'
#' @examples
#' im.list()
#' im.list(include_remote = FALSE)
#' @export
im.list <- function(include_remote = TRUE) {
  # File locali
  local_files <- list.files(system.file("extdata", package = "imageRy"))
  
  # File remoti statici
  remote_files <- c(
    "bletterbach.jpg",
    "dolansprings_oli_2013088_canyon_lrg.jpg",
    "EN_01.png",
    "EN_02.png",
    "EN_03.png",
    "EN_04.png",
    "EN_05.png",
    "EN_06.png",
    "EN_07.png",
    "EN_08.png",
    "EN_09.png",
    "EN_10.png",
    "EN_11.png",
    "EN_12.png",
    "EN_13.png",
    "greenland.2000.tif",
    "greenland.2005.tif",
    "greenland.2010.tif",
    "greenland.2015.tif",
    "iss063e039892_lrg.jpg",
    "matogrosso_ast_2006209_lrg.jpg",
    "matogrosso_l5_1992219_lrg.jpg",
    "NDVI_rainbow.png",
    "NDVI_rainbow_legend.png",
    "sentinel.dolomites.b2.tif",
    "sentinel.dolomites.b3.tif",
    "sentinel.dolomites.b4.tif",
    "sentinel.dolomites.b8.tif",
    "sentinel.png",
    "Sentinel2_NDVI_2020-02-21.tif",
    "Sentinel2_NDVI_2020-05-21.tif",
    "Sentinel2_NDVI_2020-08-01.tif",
    "Sentinel2_NDVI_2020-11-27.tif",
    "Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg"
  )
  
  # Unisci risultati
  if (include_remote) {
    result <- c(local_files, remote_files)
  } else {
    result <- local_files
  }
  
  return(result)
}
