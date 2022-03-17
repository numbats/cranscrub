

#' The CRAN logs for the RStudio mirror for the CRAN task view Experimental Design
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{date}{date}
#'   \item{package}{the package name}
#'   \item{n_unique}{the number of unique downloads}
#'   \item{n_total}{the number of total downloads. This should be the same as `cranlogs::cran_downloads()`.}
#' }
#' @examples 
#' library(ggplot2)
#' ggplot(ctvExperimentalDesign, aes(n_unique, n_total)) + 
#'   geom_point() + 
#'   geom_abline(intercept = 0, slope = 1, color = "red")
"ctvExperimentalDesign"



