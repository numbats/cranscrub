

#' Get the list of packages for a given task 
#' @param topic A topic listed in the CRAN task view. 
#' @param repos A base URL of the repository. By default, `getOption("repos")` is tried
#'  and otherwise `getOption("CRAN")` is used.
#' @export
ctv_pkgs <- function(name = ctv_names(), repos = NULL) {
  topic <- match.arg(topic)
  w <- match(topic, ctv_topics())
  query <- ctv::available.views(repos = repos)[w]
  unname(unlist(lapply(query, function(.x) .x$packagelist$name)))
}

#' Get all the list of topics in the CRAN task views 
#' 
#' @source https://cran.r-project.org/web/views/
#' @export
ctv_names <- function() {
  names(ctv::available.views())
}

#' Get the CRAN task view as a table 
#' 
#' @inheritParams ctv_pkgs
#' @export
ctv_table <- function(repos = NULL) {
  ctv_list <- ctv::available.views(repos = repos)
  res <- lapply(ctv_list, function(x) data.frame(name = x$name, 
                                              topic = x$topic, 
                                              maintainer = x$maintainer, 
                                              email = x$email, 
                                              version = x$version, 
                                              packages = I(list(x$packagelist$name)), 
                                              repository = x$repository))
  do.call(rbind, res)
}