
#' Package downloads from the RStudio CRAN mirror with duplicates removed
#' 
#' The arguments behave the same as `cranlogs::cran_downloads`.
#' 
#' @param packages A character vector of CRAN package names 
#' @param when Either `last-day`, `last-week` or `last-month`. If this is 
#' given then `from` and `to` are ignored. NOT implemented yet!
#' @param from Start date in YYYY-MM-DD format. 
#' @param to Start date in YYYY-MM-DD format.
#' @examples 
#' cranscrub(packages = "fable", from = "2020-01-03", to = "2020-01-05")
#' @export
cranscrub <- function(packages = NULL, 
                      when = c("last-day", "last-week", "last-month"),
                      from = "last-day",
                      to = "last-day") {
  DATALOC <- "https://raw.github.com/numbats/cranscrub/main/Data/cranlogs_processed/"
  from <- ifelse(from=="last-day", "2021-09-01", from)
  to <- ifelse(to=="last-day", "2021-09-01", to)
  out <- data.frame(date = character(),
                    package = character(),
                    n_unique = integer(),
                    n_total = integer())
  dates <- seq(as.Date(from), as.Date(to), by = 1)
  # for(adate in dates) changes adate into integer :(
  for(i in seq_along(dates)) {
    TMPDIR <- tempdir()
    fn <- paste0(TMPDIR, "/", as.character(dates[i]))
    download.file(paste0(DATALOC, as.character(dates[i]), ".csv.gz"),
                  fn)
    df <- read.csv(gzfile(fn))
    out <- rbind(out, subset(df, package %in% packages))
  }
  out
}


