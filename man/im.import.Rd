% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/im.import.R
\name{im.import}
\alias{im.import}
\title{Import one or more raster images}
\usage{
im.import(im)
}
\arguments{
\item{im}{Character vector. Full or partial names of images available on Zenodo.}
}
\value{
A `SpatRaster` object (stack of layers).
}
\description{
This function imports raster images from the remote Zenodo repository
using exact or partial matches. It returns a SpatRaster stack of all matching files.
}
