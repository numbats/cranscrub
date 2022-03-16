

#' Get the list of packages for a given task 
#' @param topic A topic listed in the CRAN task view. 
#' @param repos A base URL of the repository. By default, `getOption("repos")` is tried
#'  and otherwise `getOption("CRAN")` is used.
#' @export
ctv_pkgs <- function(topic = ctv_topics(), repos = NULL) {
  topic <- match.arg(topic)
  w <- match(topic, ctv_topics())
  query <- ctv::available.views(repos = repos)[w]
  unname(unlist(lapply(query, function(.x) .x$packagelist$name)))
}

#' Get all the list of topics in the CRAN task views 
#' 
#' @source https://cran.r-project.org/web/views/
#' @export
ctv_topics <- function() {
  names(ctv::available.views())
}