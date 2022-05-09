

#' Get the list of packages for a given task 
#' @param name A topic listed in the CRAN task view. 
#' @param repos A base URL of the repository. By default, `getOption("repos")` is tried
#'  and otherwise `getOption("CRAN")` is used.
#' @export
ctv_pkgs <- function(name = ctv_names(), repos = NULL) {
  name <- match.arg(name)
  w <- match(name, ctv_names())
  query <- ctv::available.views(repos = repos)[w]
  unname(unlist(lapply(query, function(.x) .x$packagelist$name)))
}

#' Get all the list of topics in the CRAN task views 
#' 
#' @param show_topic A logical value to indicate whether to return the topic or name.
#'  Default is FALSE and returns the task view name.
#' 
#' @source https://cran.r-project.org/web/views/
#' @export
ctv_names <- function(show_topic = FALSE) {
  x <- ctv::available.views(repos = "http://cran.rstudio.com/")
  if(show_topic) {
    setNames(vapply(x, function(.x) .x[["topic"]], character(1)), names(x))
  } else {
    names(x)
  }
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