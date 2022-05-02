
#' Get package release and update dates
#' 
#' This harvests the information from the CRAN website. 
#' Alternatively look at `pkgsearch::cran_package_history()` which sources 
#' information from R-hub server. 
#' 
#' @param pkgs A character vector of the name of CRAN packages.
#' 
#' @export
pkg_updates <- function(pkgs) {
  pkg_url <- "https://cran.r-project.org/web/packages/{pkg}/index.html"
  pkg_archive <- "https://cran.r-project.org/src/contrib/Archive/{pkg}/"
  res <- lapply(pkgs, function(pkg) {
    web <- rvest::read_html(glue::glue(pkg_url))
    tab <- rvest::html_table(web)
    last_update <- as.Date(subset(tab[[1]], X1=="Published:", select = 2)[[1]])

    archive_dates <- tryCatch({ 
      webarchive <- rvest::read_html(glue::glue(pkg_archive))
      tabarchive <- rvest::html_table(webarchive)
      updates <- as.POSIXct(tabarchive[[1]][["Last modified"]], tz = "UTC", "%Y-%m-%d %H:%M")
      updates <- as.Date(na.omit(updates))
    }, error = function(e) {
      NULL
    })
    if(is.null(archive_dates)) return(data.frame(pkg = pkg, update = last_update))
    data.frame(pkg = pkg, update = c(archive_dates, last_update))
  })
  do.call("rbind", res)
}



#' Get the package database
#' 
#' All CRAN mirrors contain the packages.rds file and this downloads the one 
#' in the RStudio CRAN mirror. 
#' 
#' @inheritParams pkg_updates
#' @export
pkg_db <- function(pkgs) {
  url <- "http://cran.rstudio.com/web/packages/packages.rds"
  db <- as.data.frame(readRDS(url(url)))
  db <- subset(db, Package %in% pkgs)
  db$Description <- gsub("\n", " ", db$Description)
  db$Description <- gsub("(^ +| +$)", "", db$Description)
  db$Title <- gsub("\n", " ", db$Title)
  db
}