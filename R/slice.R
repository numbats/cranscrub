

#' Subset the rows of the data by top package
#' 
#' This function allow to easily subset the full temporal data by an aggregate statistic 
#' across (subset of) the temporal variable. For example, we have the daily download count for each 
#' package from 2012 to 2020 but we want to subset the data based on the top `n` packages, where top
#' is determined by the total downloads over 2018-2020. 
#' 
#' @param .data A data frame, consisting of a column `date`, that rank a category 
#'   based on some metric for specified range of dates
#' @param order_by The name of the column to order the ranking by.
#' @param n The number of top packages to filter the data by.
#' @param prop The proportion of the the top package to filter the data by. Currently not implemented.
#' @param with_ties Whether to include ties or not. Currently not implemented.
#' @examples 
#' library(ggplot2)
#' ctvExperimentalDesign %>% 
#'   slice_top(n = 10) %>% 
#'   ggplot(aes(date, n_unique, group = package)) + 
#'   geom_line() + 
#'   facet_grid(package ~ .)
#' @export
slice_top <- function(.data, order_by = "n_unique", n, prop, with_ties = TRUE, 
                      .fun = sum, rank = "package", 
                      from = Sys.Date() - 365,
                      to = Sys.Date()) {
  data <- subset(.data, date >= from & date <= to)
  top <- tapply(data[[order_by]], data[[rank]], .fun)
  toppkgs <- names(top[order(-top)])[1:n]
  subset(.data, package %in% toppkgs)
}