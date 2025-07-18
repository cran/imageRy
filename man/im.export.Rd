% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/im.export.R
\name{im.export}
\alias{im.export}
\title{Export a Raster to GeoTIFF, PNG, or JPG}
\usage{
im.export(x, filename, overwrite = TRUE)
}
\arguments{
\item{x}{A `SpatRaster` object representing the raster to be saved.}

\item{filename}{A character string specifying the output file path with `.tif`, `.png`, or `.jpg` extension.}

\item{overwrite}{A logical value indicating whether to overwrite an existing file (default: TRUE).}
}
\value{
No return value. The function writes the raster to disk.
}
\description{
This function saves a `SpatRaster` object to disk in **GeoTIFF**, **PNG**, or **JPG** format.
}
\details{
- **GeoTIFF (`.tif`)**: Uses `terra::writeRaster()`, preserving geospatial information.
- **PNG/JPG (`.png`, `.jpg`, `.jpeg`)**: Converts the raster to an image and saves it with `png()` or `jpeg()`.
- **If the raster has multiple bands**, only the first band is saved in PNG/JPG format.
}
\examples{
library(terra)

# Create a sample raster
r <- rast(nrows = 10, ncols = 10)
values(r) <- runif(ncell(r))

# Export as GeoTIFF to temporary file
tif_path <- tempfile(fileext = ".tif")
im.export(r, tif_path)

# Export as PNG to temporary file
png_path <- tempfile(fileext = ".png")
im.export(r, png_path)
}
\seealso{
[im.import()], [writeRaster()]
}
