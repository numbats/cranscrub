


#' Get the unprocessed RStudio CRAN log 
#' 
#' @param date The date given either as a string format of "YYYY-MM-DD" or 
#'   a date object.
#' @export
read_cran_log <- function(date) {
  url <- "http://cran-logs.rstudio.com"
  date <- as.Date(date)
  year <- function(x) as.POSIXlt(x, tz = tz(x))$year + 1900
  vroom::vroom(paste0(url, "/", year(date), "/", date, ".csv.gz"))
}