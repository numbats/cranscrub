% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{pkg_updates}
\alias{pkg_updates}
\title{Get package release and update dates}
\usage{
pkg_updates(pkgs, message = FALSE)
}
\arguments{
\item{pkgs}{A character vector of the name of CRAN packages.}

\item{message}{Should the message for which package update is extracted be shown?
The default is FALSE.}
}
\description{
This harvests the information from the CRAN website.
Alternatively look at \code{pkgsearch::cran_package_history()} which sources
information from R-hub server.
}
