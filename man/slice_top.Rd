% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice.R
\name{slice_top}
\alias{slice_top}
\title{Subset the rows of the data by top package}
\usage{
slice_top(
  .data,
  order_by = "n_unique",
  n,
  prop,
  with_ties = TRUE,
  .fun = sum,
  rank = "package",
  from = Sys.Date() - 365,
  to = Sys.Date()
)
}
\arguments{
\item{.data}{A data frame, consisting of a column \code{date}, that rank a category
based on some metric for specified range of dates}

\item{order_by}{The name of the column to order the ranking by.}

\item{n}{The number of top packages to filter the data by.}

\item{prop}{The proportion of the the top package to filter the data by. Currently not implemented.}

\item{with_ties}{Whether to include ties or not. Currently not implemented.}
}
\description{
This function allow to easily subset the full temporal data by an aggregate statistic
across (subset of) the temporal variable. For example, we have the daily download count for each
package from 2012 to 2020 but we want to subset the data based on the top \code{n} packages, where top
is determined by the total downloads over 2018-2020.
}
\examples{
library(ggplot2)
ctvExperimentalDesign \%>\% 
  slice_top(n = 10) \%>\% 
  ggplot(aes(date, n_unique, group = package)) + 
  geom_line() + 
  facet_grid(package ~ .)
}
