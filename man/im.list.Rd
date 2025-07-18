% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/im.list.R
\name{im.list}
\alias{im.list}
\title{List available example files}
\usage{
im.list(include_remote = TRUE)
}
\arguments{
\item{include_remote}{Logical. If TRUE (default), also lists remote files hosted on Zenodo.}
}
\value{
A character vector of file names.
}
\description{
This function lists example files included locally in the package and, optionally,
remote files hosted on Zenodo.
}
\examples{
im.list()
im.list(include_remote = FALSE)
}
