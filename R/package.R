
#' Get package release and update dates
#' 
#' This harvests the information from the CRAN website. 
#' Alternatively look at `pkgsearch::cran_package_history()` which sources 
#' information from R-hub server. 
#' 
#' @param pkgs A character vector of the name of CRAN packages.
#' @param message Should the message for which package update is extracted be shown? 
#'   The default is FALSE.
#' 
#' @export
pkg_updates <- function(pkgs, message = FALSE) {
  pkg_url <- "https://cran.r-project.org/web/packages/{pkg}/index.html"
  pkg_archive <- "https://cran.r-project.org/src/contrib/Archive/{pkg}/"
  res <- lapply(pkgs, function(pkg) {
    web <- rvest::read_html(glue::glue(pkg_url))
    tab <- rvest::html_table(web)
    last_update <- as.Date(subset(tab[[1]], X1=="Published:", select = 2)[[1]])
    if(message) message(glue::glue("Getting updates for {pkg}."))
    
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
  if(!missing(pkgs)) {
    db <- subset(db, Package %in% pkgs)
  }
  db$Description <- gsub("\n", " ", db$Description)
  db$Description <- gsub("(^ +| +$)", "", db$Description)
  db$Title <- gsub("\n", " ", db$Title)
  db
}

db_replace <- function(db, var = NULL, replace = NULL) {
  if(!is.null(replace) & !is.null(var)) {
    db[[var]][db[[var]] %in% names(replace)] <- replace[db[[var]][db[[var]] %in% names(replace)]]
  }
  db
}

#' Manually check Author entry from `pkg_db()`
#' 
#' There are so many issues with the author entry in database from `pkg_db()` 
#' that manual checks are unavoidable. This function prints out the original 
#' Author entry and contrast to that of replaced one, `Author@R` and the cleaned up 
#' version.
#' 
#' @param db The data from `pkg_db()` output
#' @param replace A named character vector to replace the Author value with. The ones 
#'   that were manually fixed are contains in `author_fix()`.
#' @param start The starting row index corresponding to `db`. 
#' 
#' 
#' @export
pkg_db_manual_author_check <- function(db, replace = author_fix(), start = 1) {
  if(missing(db)) db <- pkg_db()
  dba <- pkg_db_clean_author(db, replace = replace)
  out <- c()
  for(i in start:nrow(db)) {
    cat("Database entry", i, "-", dba$Package[i], "\n")
    print(db$Author[i])
    cat(paste0("\033[33m", dba$Author[i], "\033[39m\n"))
    cat(paste0("\033[34m", dba$`Authors@R`[i], "\033[39m\n"))
    cat(paste0("\033[32m", dba$AuthorClean[i], "\033[39m\n\n"))
    x <- readline("Press:\n - q to exit,\n - Enter to proceed to next\n - Any other entries to record the entry number\n-------- ")
    if(x!="") out <- c(out, i)
    if(x=="q") break
  }
  out
}

#' Clean the "Depends" column in `pkg_db()`
#' 
#' @inheritParams pkg_db_manual_author_check
#' @export
pkg_db_clean_depends <- function(db) {
  if(missing(db)) db <- pkg_db()
  db$DependsVec <- strsplit(db$Depends, ", ")
  db$DependsVec <- lapply(db$DependsVec, function(x) sapply(regmatches(x, gregexpr("^[A-Za-z0-9]+", x)), identity))
  db
}

#' Clean the "Imports" column in `pkg_db()`
#' 
#' @inheritParams pkg_db_manual_author_check
#' @export
pkg_db_clean_imports <- function(db) {
  if(missing(db)) db <- pkg_db()
  db$ImportsVec <- strsplit(db$Imports, ", ")
  db$ImportsVec <- lapply(db$ImportsVec, function(x) sapply(regmatches(x, gregexpr("^[A-Za-z0-9]+", x)), identity))
  db
}

#' Clean the "Suggests" column in `pkg_db()`
#' 
#' @inheritParams pkg_db_manual_author_check
#' @export
pkg_db_clean_suggests <- function(db) {
  if(missing(db)) db <- pkg_db()
  db$SuggestsVec <- strsplit(db$Suggests, ", ")
  db$SuggestsVec <- lapply(db$SuggestsVec, function(x) sapply(regmatches(x, gregexpr("^[A-Za-z0-9]+", x)), identity))
  db
}

#' Clean the Author names from `pkg_db()`
#' 
#' @inheritParams pkg_db_manual_author_check
#' 
#' @export
pkg_db_clean_author <- function(db, replace = author_fix()) {
  if(missing(db)) db <- pkg_db()
  person <- function(given = "", family = "", ... , email, role, comment) {
    paste(c(given, family), collapse = " ")
  }
  as.person <- function(x) {
    x
  }
  str_squish <- function(x) {
    gsub("(^ +| +$)", "", x)
  }
  
  dba <- db_replace(db, "Author", replace)
  dba$AuthorVec <- gsub("\\]\\n", "\\], ", dba$Author)
  dba$AuthorVec <- gsub("(\\[[^]]+\\]|\\n|\\t)", " ", dba$AuthorVec)
  dba$AuthorVec <- gsub("(with )?contributions (from|by|of) ", "", dba$AuthorVec)
  dba$AuthorVec <- gsub("(R port by |S-plus original by |S original by |Original by |Fortran original by )", "", dba$AuthorVec)
  dba$AuthorVec <- gsub(" {2,}", " ", dba$AuthorVec)
  dba$AuthorVec <- strsplit(dba$AuthorVec, "(, and |, | and | & )")
  dba$AuthorVec <- lapply(dba$AuthorVec, function(x) gsub("(<.+>|\\([^)]+\\)|\\))", "", x))
  dba$AuthorVec <- lapply(dba$AuthorVec, function(x) str_squish(x))
  dba$AuthorVec <- lapply(dba$AuthorVec, function(x) sub("[.,]$", "", x))
  dba$AuthorR <- lapply(dba$`Authors@R`, function(x) {
    res <- eval(parse(text = x))
    res <- gsub(" {2,}", " ", res)
    res <- str_squish(res)
    res <- gsub("\\n", "", res)
    res <- res[res!="" & res!=" "]
    unname(res)
  })
  dba$AuthorClean = lapply(1:nrow(db), function(i) {
    if(!is.na(dba$`Authors@R`[i])) dba$AuthorR[i] else dba$AuthorVec[i]
  })
  dba
}

#' Manual author name fixes for `pkg_db()`
#' 
#' The output is a named character vector where the name is the exact match with the 
#' "Author" column and the value is what it should be replaced as. 
#' 
#' @seealso pkg_db_clean_author, pkg_db_manual_author_check
#' 
#' @export
author_fix <- function() {
    c("Stéphane Dray <stephane.dray@univ-lyon1.fr>, Anne-Béatrice Dufour <anne-beatrice.dufour@univ-lyon1.fr>, and Jean Thioulouse <jean.thioulouse@univ-lyon1.fr>, with contributions from Thibaut Jombart, Sandrine Pavoine, Jean R. Lobry, Sébastien Ollier, Daniel Borcard, Pierre Legendre, Stéphanie Bougeard and Aurélie Siberchicot. Based on earlier work by Daniel Chessel." = 
                   "Stéphane Dray <stephane.dray@univ-lyon1.fr>, Anne-Béatrice Dufour <anne-beatrice.dufour@univ-lyon1.fr>, and Jean Thioulouse <jean.thioulouse@univ-lyon1.fr>, with contributions from Thibaut Jombart, Sandrine Pavoine, Jean R. Lobry, Sébastien Ollier, Daniel Borcard, Pierre Legendre, Stéphanie Bougeard, Aurélie Siberchicot, Daniel Chessel",
                 "Ravi Varadhan (with contributions from Gabor Grothendieck)" = "Ravi Varadhan, Gabor Grothendieck",
                 "Authors@R" = "Cedric Landerer",
                 "Nielson, R. M., H. Sawyer, and T. L. McDonald (WEST, Inc.,\n        www.west-inc.com)" = 
                   "R. M. Nielson, H. Sawyer, and T. L. McDonald",
                 #"Ulrich Bodenhofer, Johannes Palme, Chrats Melkonian, Andreas Kothmeier,\n\tNikola Kostic" = 
                 #  "Ulrich Bodenhofer, Johannes Palme, Chrats Melkonian, Andreas Kothmeier, \n\tNikola Kostic",
                 #"Charles J. Geyer <charlie@stat.umn.edu>." = "Charles J. Geyer <charlie@stat.umn.edu>",
                 "Compiled by Kjetil Halvorsen" = "Kjetil Halvorsen",
                 "\n\tGrace Chiu <bentcable@gmail.com>,\n\tCSIRO,\n\tCSIT Bldg, ANU Campus, Acton, ACT 2601, Australia" = 
                   "Grace Chiu",
                 "Przemyslaw Biecek \\& Ewa Szczurek" = "Przemyslaw Biecek, Ewa Szczurek",
                 "Martell-Juarez, D.A. & Nieto-Barajas, L.E." = "D.A. Martell-Juarez & L.E. Nieto-Barajas",
                 #"Alberto Caimo [aut, cre], \n        Lampros Bouranis [aut],\n        Robert Krause [aut] \n        Nial Friel [ctb]" = 
                 #  "Alberto Caimo [aut, cre], \n        Lampros Bouranis [aut],\n        Robert Krause [aut], \n        Nial Friel [ctb]",
                 "S original by Barry W. Brown, James Lovato and Kathy Russel. \n            R port by Kjetil B Halvorsen <kjetil1001@gmail.com>" = 
                   "Barry W. Brown, James Lovato and Kathy Russel, \n            Kjetil B Halvorsen <kjetil1001@gmail.com>",
                 "Valeria Bejarano Salcedo <vbejaranos@unal.edu.co>,\n        Sergio Alejandro Calderon Villanueva <sacalderonv@unal.edu.co>\n        Andrey Duvan Rincon Torres <adrincont@unal.edu.co>" = 
                   "Valeria Bejarano Salcedo <vbejaranos@unal.edu.co>,\n        Sergio Alejandro Calderon Villanueva <sacalderonv@unal.edu.co>,\n        Andrey Duvan Rincon Torres <adrincont@unal.edu.co>",
                 "Steven L. Scott is the sole author and creator of the BOOM project.\n   Some code in the BOOM libraries has been modified from other open source\n   projects.  These include Cephes (obtained from Netlib, written by Stephen\n   L. Moshier), NEWUOA (M.J.D Powell, obtained from Powell's web site), and a\n   modified version of the R math libraries (R core development team).  Original\n   copyright notices have been maintained in all source files.  In these cases,\n   copyright claimed by Steven L. Scott is limited to modifications made to the\n   original code.  Google claims copyright for code written while Steven\n   L. Scott was employed at Google from 2008 - 2018, but BOOM is not an\n   officially supported Google project." = 
                   "Steven L. Scott",
                 "OpenBUGS was developed by Andrew Thomas, Dave Lunn, David Spiegelhalter and Nicky Best.  R interface developed by Uwe Ligges, Sibylle Sturtz, Andrew Gelman, Gregor Gorjanc and Chris Jackson.  Linux port and most recent developments by Chris Jackson." = 
                   "Andrew Thomas, Dave Lunn, David Spiegelhalter, Nicky Best, Uwe Ligges, Sibylle Sturtz, Andrew Gelman, Gregor Gorjanc and Chris Jackson",
                 "S original, from StatLib, by Rob Tibshirani.  R port by\n        Friedrich Leisch." = "Rob Tibshirani, Friedrich Leisch",
                 "Paola Rebora,Agus Salim, Marie Reilly" = "Paola Rebora, Agus Salim, Marie Reilly",
                 "Ernesto Barrios based on Daniel Meyer's code." = "Ernesto Barrios, Daniel Meyer",
                 "Ported to R by Ted Harding and Fernando Tusell. Original by\n        Joseph L. Schafer <jls@stat.psu.edu>." = 
                   "Ted Harding, Fernando Tusell,        Joseph L. Schafer <jls@stat.psu.edu>.",
                 "David Magis (Belgium), Gilles Raiche (UQAM, Canada), Juan Ramon Barrada (U Zaragoza, Spain)" = 
                   "David Magis, Gilles Raiche, Juan Ramon Barrada",
                 "David Bronaugh <bronaugh@uvic.ca> for the Pacific Climate Impacts\n    Consortium" = "David Bronaugh",
                 "Original Matlab routines by C.J.F. ter Braak and A.P. Schaffers. R port by Gavin L. Simpson.\n  Function simpls based on simpls.fit (package pls) by Ron Wehrens and Bjorn-Helge Mevik." = 
                   "C.J.F. ter Braak, A.P. Schaffers, Gavin L. Simpson, Ron Wehrens, Bjorn-Helge Mevik",
                 "M. Helena Goncalves and M. Salome Cabral,\n  apart from a set of Fortran-77 subroutines written by R. Piessens\n  and E. de Doncker, belonging to the suite \"Quadpack\"." = 
                   "M. Helena Goncalves, M. Salome Cabral, R. Piessens, E. de Doncker",
                 "Stephen Milborrow.  Derived from mda:mars by Trevor Hastie and\n    Rob Tibshirani.  Uses Alan Miller's Fortran utilities\n    with Thomas Lumley's leaps wrapper." = 
                   "Stephen Milborrow, Trevor Hastie,    Rob Tibshirani, Alan Miller, Thomas Lumley",
                 "Paul Bailey [aut, cre],\n        Ahmad Emad [aut],\n        Huade Huo [aut],\n        Michael Lee [aut],\n        Yuqi Liao [aut],\n        Alex Lishinski [aut],\n        Trang Nguyen [aut],\n        Qingshu Xie [aut],\n        Jiao Yu [aut],\n        Ting Zhang [aut],\n        Eric Buehler [aut],\n        person(\"Sun-joo\", \"Lee\", role=\"aut\"),\n        person(\"Emmanuel\", \"Sikali\", role=\"pdr\", email = \"Emmanuel.Sikali@ed.gov\")),\n        Jeppe Bundsgaard [ctb],\n        Ren C'deBaca [ctb],\n        Anders Astrup Christensen [ctb]" = 
                   "Paul Bailey [aut, cre],\n        Ahmad Emad [aut],\n        Huade Huo [aut],\n        Michael Lee [aut],\n        Yuqi Liao [aut],\n        Alex Lishinski [aut],\n        Trang Nguyen [aut],\n        Qingshu Xie [aut],\n        Jiao Yu [aut],\n        Ting Zhang [aut],\n        Eric Buehler [aut],\n        Sun-joo Lee,\n        Emmanuel Sikali,        Jeppe Bundsgaard [ctb],\n        Ren C'deBaca [ctb],\n        Anders Astrup Christensen [ctb]",
                 "Mai Zhou. (Art Owen for el.test(). Yifan Yang for some C code.)" = "Mai Zhou, Art Owen, Yifan Yang",
                 "William H. Barton <williamhbarton1@gmail.com> under the supervision of Dr. Mai Zhou" = "William H. Barton, Mai Zhou",
                 "Alec Stephenson. Function fbvpot by Chris Ferro." = "Alec Stephenson, Chris Ferro",
                 "code by Fionn Murtagh and R development team, modifications and\n        packaging by Peter Langfelder" = 
                   "Fionn Murtagh, R Core Team, Peter Langfelder",
                 "Chenguang Wang [aut, cre],\n    Andrew Leroux [aut, cre],\n    Elizabeth Colantuoni [aut],\n    Daniel O Scharfstein [aut],\n    Trustees of Columbia University [cph] (tools/make_cpp.R, R/stanmodels.R)" = 
                   "Chenguang Wang [aut, cre],\n    Andrew Leroux [aut, cre],\n    Elizabeth Colantuoni [aut],\n    Daniel O Scharfstein [aut],\n    Trustees of Columbia University [cph]",
                 "Original S functions written by Janet E. Heffernan with R port and R \n  documentation provided by Alec G. Stephenson." = 
                   "Janet E. Heffernan, Alec G. Stephenson",
                 "Dave Zes, Jimmy Lewis, Dana Landis @ Korn/Ferry International" = "Dave Zes, Jimmy Lewis, Dana Landis", 
                 "David Makowski (INRA) [aut],\n  Francois Piraux (Arvalis) [aut],\n  Francois Brun (ACTA) [aut, cre]" = 
                   "David Makowski [aut],\n  Francois Piraux [aut],\n  Francois Brun [aut, cre]",
                 "Original by Klein and Moeschberger, modifications by Jun Yan\n        <jun.yan@uconn.edu>" = "Jun Yan",
                 "Justin Lokhorst, Bill Venables and Berwin Turlach;\n  port to R, tests etc: Martin Maechler <maechler@stat.math.ethz.ch>" = 
                   "Justin Lokhorst, Bill Venables and Berwin Turlach, Martin Maechler",
                 "Charlie Casper, Thomas Cook and Oscar A. Perez.  Based on FORTRAN\n        program ld98." = 
                   "Charlie Casper, Thomas Cook and Oscar A. Perez",
                 "Thomas Lumley based on Fortran code by Alan Miller" = "Thomas Lumley, Alan Miller",
                 "Original by Joseph L. Schafer" = "Joseph L. Schafer",
                 "Thomas Ruf, partially based on C original by Press et al. (Numerical Recipes) and the Python module Astropy." = "Thomas Ruf",
                 "Stephen Tueller. Funded by the National Institute on Drug Abuse (NIDA) Award number 1R03DA030850, the National Institute on Alcohol Abuse and Alcoholism (NIAAA) Award Number R03 AA019775, and the National Institute of Justice Award Number 2011-RY-BX-0003." = 
                   "Stephen Tueller",
                 "S scripts originally by Jan Beran <jan.beran@uni-konstanz.de>;\n        Datasets via Brandon Whitcher <brandon@stat.washington.edu>.\n\tToplevel R functions and much more by Martin Maechler." = "Jan Beran, Brandon Whitcher, Martin Maechler",
                 "Learned Pattern Similarity (LPS) for time series by Mustafa Gokce Baydogan" = "Mustafa Gokce Baydogan",
                 "Original S code by Richard A. Becker and Allan R. Wilks.\n\tR version by Ray Brownrigg <Ray.Brownrigg@ecs.vuw.ac.nz>." = "Richard A. Becker, Allan R. Wilks, Ray Brownrigg",
                 "Doug McIlroy.  Packaged for R by Ray Brownrigg and Thomas P\n        Minka, transition to Plan 9 codebase by Roger Bivand." = 
                   "Doug McIlroy, Ray Brownrigg and Thomas P Minka, Roger Bivand.",
                 "Original S code by Richard A. Becker and Allan R. Wilks.\n\tR version by Ray Brownrigg.\n        Enhancements by Thomas P Minka and Alex Deckmyn." = 
                   "Richard A. Becker, Allan R. Wilks, Ray Brownrigg, Thomas P Minka and Alex Deckmyn",
                 "S original by Alessandra R. Brazzale <alessandra.brazzale@unipd.it>.\n  R port by Alessandra R. Brazzale <alessandra.brazzale@unipd.it>, following \n  earlier work by Douglas Bates." = 
                   "Alessandra R. Brazzale <alessandra.brazzale@unipd.it>, Douglas Bates",
                 "Jeff Laake <jefflaake@gmail.com>, Devin Johnson\n    <devin.johnson@noaa.gov>, Paul Conn <paul.conn@noaa.gov>, example for simHMM\n    from Jay Rotella" = 
                   "Jeff Laake <jefflaake@gmail.com>, Devin Johnson\n    <devin.johnson@noaa.gov>, Paul Conn <paul.conn@noaa.gov>, Jay Rotella",
                 "S original by Trevor Hastie & Robert Tibshirani. Original R port by Friedrich Leisch, Kurt Hornik and Brian D. Ripley. Balasubramanian Narasimhan has contributed to the upgrading of the code." = 
                   "Trevor Hastie & Robert Tibshirani, Friedrich Leisch, Kurt Hornik and Brian D. Ripley, Balasubramanian Narasimhan",
                 "Original by Joseph L. Schafer <jls@stat.psu.edu>. " = "Joseph L. Schafer",
                 "Francesco Bartolucci, Silvia Bacci - University of Perugia (IT)" = "Francesco Bartolucci, Silvia Bacci",
                 "Kenneth Knoblauch and Laurence T. Maloney, \n  based in part on C code written by Laurence T. Maloney \n  and J. N. Yang" = 
                   "Kenneth Knoblauch and Laurence T. Maloney, Laurence T. Maloney \n  and J. N. Yang",
                 "Adelchi Azzalini [aut, cre],\n   Alan Genz [aut] (most Fortran code),\n   Alan Miller [ctb] (Fortran routine PHI),\n   Michael J. Wichura  [ctb] (Fortran routine PHINV),\n   G. W. Hill [ctb] (Fortran routine STDINV),\n   Yihong Ge [ctb] (Fortran routines BNVU and MVBVU)." = 
                   "Adelchi Azzalini [aut, cre],\n   Alan Genz [aut], Alan Miller [ctb],  Michael J. Wichura  [ctb],   G. W. Hill [ctb],\n   Yihong Ge [ctb]",
                 "Francesco Bartolucci, Silvia Bacci, Michela Gnaldi - University of Perugia (IT)" = "Francesco Bartolucci, Silvia Bacci, Michela Gnaldi",
                 "S original by Alessandra R. Brazzale <alessandra.brazzale@unipd.it>\n  and Ruggero Bellio <ruggero.bellio@uniud.it>.  R port by Alessandra R. \n  Brazzale <alessandra.brazzale@unipd.it>, following earlier work by \n  Douglas Bates." = 
                   "S original by Alessandra R. Brazzale <alessandra.brazzale@unipd.it>\n  and Ruggero Bellio <ruggero.bellio@uniud.it>.  R port by Alessandra R. \n  Brazzale <alessandra.brazzale@unipd.it>, Douglas Bates.",
                 "Ported to R by Alvaro A. Novo <alvaro@novo-online.net>.\n    Original by Joseph L. Schafer <jls@stat.psu.edu>. " = "Alvaro A. Novo <alvaro@novo-online.net>, Joseph L. Schafer <jls@stat.psu.edu>",
                 "Ian Fellows, using the JMapViewer library by Jan Peter Stotz" = "Ian Fellows, Jan Peter Stotz",
                 "Jon Clayden, based on Onigmo by K. Kosako and K. Takata" = "Jon Clayden, K. Kosako and K. Takata",
                 "Adelchi Azzalini, Giovanna Menardi for the current version;\n   Adelchi Azzalini, Giovanna Menardi and Tiziana Rosolin up to version 0.1-13." = 
                   "Adelchi Azzalini, Giovanna Menardi, Adelchi Azzalini, Giovanna Menardi and Tiziana Rosolin",
                 "originally written as R2WinBUGS by Andrew Gelman <gelman@stat.columbia.edu>;\n changes and packaged by Sibylle Sturtz <sturtz@statistik.tu-dortmund.de>\n and Uwe Ligges <ligges@statistik.tu-dortmund.de>.\n With considerable contributions by Gregor Gorjanc <gregor.gorjanc@bfro.uni-lj.si>\n and Jouni Kerman <kerman@stat.columbia.edu>.\n Adapted to R2OpenBUGS from R2WinBUGS by Neal Thomas." = 
                   "Andrew Gelman, Sibylle Sturtz, Uwe Ligges, Gregor Gorjanc, Jouni Kerman, Neal Thomas",
                 "originally written by Andrew Gelman <gelman@stat.columbia.edu>;\n changes and packaged by Sibylle Sturtz <sturtz@statistik.tu-dortmund.de>\n and Uwe Ligges <ligges@statistik.tu-dortmund.de>.\n With considerable contributions by Gregor Gorjanc <gregor.gorjanc@bfro.uni-lj.si>\n and Jouni Kerman <kerman@stat.columbia.edu>.\n Ported to S-PLUS by Insightful Corp." = 
                   "Andrew Gelman, Sibylle Sturtz, Uwe Ligges, Gregor Gorjanc, Jouni Kerman, Insightful Corp",
                 "R port by Yohan Chalabi, Christophe Dutang, Petr Savicky and Diethelm Wuertz with some underlying C codes of (i) the SFMT algorithm from M. Matsumoto and M. Saito, (ii) the Knuth-TAOCP RNG from D. Knuth. " = 
                   "Yohan Chalabi, Christophe Dutang, Petr Savicky, Diethelm Wuertz, M. Matsumoto, M. Saito, D. Knuth",
                 "Dirk Eddelbuettel; the authors and contributors of QuantLib" = "Dirk Eddelbuettel, QuantLib",
                 "Dirk Eddelbuettel, Robert G Brown, David Bauer\n plus contributors to DieHarder" = "Dirk Eddelbuettel, Robert G Brown, David Bauer, DieHarder",
                 "G. Grothendieck. Author of h2 is Thomas Mueller." = "G. Grothendieck, Thomas Mueller.",
                 "Patrick J. Heagerty <heagerty@u.washington.edu>, packaging by\n        Paramita Saha-Chaudhuri <paramita.sahachaudhuri.work@gmail.com>" = 
                   "Patrick J. Heagerty, Paramita Saha-Chaudhuri",
                 "Michael U. Kemp <mukemp+RNCEP@gmail.com> with contributions from E. Emiel van Loon, Judy Shamoun-Baranes, and Willem Bouten" = 
                   "Michael U. Kemp, E. Emiel van Loon, Judy Shamoun-Baranes, and Willem Bouten",
                 "E. Andres Houseman, Sc.D. and Devin C. Koestler, Ph.D." = "E. Andres Houseman, and Devin C. Koestler.",
                 "Dirk Eddelbuettel with contributions by Bill Evans, Mike Birdgeneau, \n Henrik Bengtsson, Seth Wenchel, Colin Gillespie and Chan-Yub Park" = 
                   "Dirk Eddelbuettel, Bill Evans, Mike Birdgeneau, \n Henrik Bengtsson, Seth Wenchel, Colin Gillespie and Chan-Yub Park",
                 "Madeleine Thompson, except dchud.f and dchdd.f, which were written by G. W. Stewart." = 
                   "Madeleine Thompson, G. W. Stewart.",
                 "Noory Y. Kim. Advisors: Shrikant I. Bangdiwala, Gerald Gartlehner. " = "Noory Y. Kim, Shrikant I. Bangdiwala, Gerald Gartlehner. ",
                 "Seong-Yun Hong <syhong@khu.ac.kr> and \n  David O'Sullivan <david.osullivan@vuw.ac.nz>, with code contributions by \n  Benjamin Jarvis and Changlock Choi" = 
                   "Seong-Yun Hong <syhong@khu.ac.kr> and \n  David O'Sullivan <david.osullivan@vuw.ac.nz>, Benjamin Jarvis and Changlock Choi",
                 "Ariane Straub, under the supervision of Torsten Hothorn" = "Ariane Straub, Torsten Hothorn",
                 "S original by James J. Majure <majure@iastate.edu> Iowa State University, R port + extensions by Albrecht Gebhardt <albrecht.gebhardt@aau.at>" = 
                   "S original by James J. Majure, Albrecht Gebhardt <albrecht.gebhardt@aau.at>",
                 "John P. Nolan, with parts adapted from Fortran and matlab code by Alan Genz" = "John P. Nolan, Alan Genz",
                 "Original by F.L. Ramsey and D.W. Schafer;\n    modifications by Daniel W. Schafer, Jeannie Sifneos and Berwin\n    A. Turlach; vignettes contributed by Nicholas Horton, Kate Aloisio\n    and Ruobing Zhang, with corrections by Randall Pruim" =
                   "F.L. Ramsey, D.W. Schafer, Daniel W. Schafer, Jeannie Sifneos, Berwin\n    A. Turlach, Nicholas Horton, Kate Aloisio, Ruobing Zhang, Randall Pruim",
                 "Original by F.L. Ramsey and D.W. Schafer; \n    modifications by Daniel W. Schafer, Jeannie Sifneos and Berwin\n    A. Turlach; vignettes contributed by Nicholas Horton, Linda Loi,\n    Kate Aloisio and Ruobing Zhang, with corrections by Randall Pruim" = 
                   "F.L. Ramsey, D.W. Schafer, Daniel W. Schafer, Jeannie Sifneos, Berwin\n    A. Turlach, Nicholas Horton, Linda Loi,\n    Kate Aloisio, Ruobing Zhang, Randall Pruim",
                 "Michal Juraska <mjuraska@u.washington.edu>, with contributions\n        from Peter B. Gilbert <pgilbert@scharp.org>, Xiaomin Lu\n        <xlu2@phhp.ufl.edu>, Min Zhang <mzhangst@umich.edu>, Marie\n        Davidian <davidian@stat.ncsu.edu>, and Anastasios A. Tsiatis\n        <tsiatis@stat.ncsu.edu>" = 
                   "Michal Juraska <mjuraska@u.washington.edu>, Peter B. Gilbert <pgilbert@scharp.org>, Xiaomin Lu\n        <xlu2@phhp.ufl.edu>, Min Zhang <mzhangst@umich.edu>, Marie\n        Davidian <davidian@stat.ncsu.edu>, and Anastasios A. Tsiatis\n        <tsiatis@stat.ncsu.edu>",
                 "Marek Gagolewski [aut, cre, cph] (<https://orcid.org/0000-0003-0637-6028>),\n    Bartek Tartanus [ctb], and others (stringi source code);\n    Unicode, Inc. and others (ICU4C source code, Unicode Character Database)" = 
                   "Marek Gagolewski [aut, cre, cph] (<https://orcid.org/0000-0003-0637-6028>),\n    Bartek Tartanus [ctb], and others",
                 "Liang Li <LLi15@mdanderson.org>, Cai Wu <cwu13@mdanderson.org>\n    Department of Biostatistics, The University of Texas MD Anderson Cancer\n    Center" = 
                   "Liang Li <LLi15@mdanderson.org>, Cai Wu <cwu13@mdanderson.org>",
                 "Ed Zhang ; Vicky Qian Wu ; Shein-Chung Chow ; Harry G.Zhang\n        (Quality check) <ed.zhang.jr@gmail.com>" = 
                   "Ed Zhang, Vicky Qian Wu, Shein-Chung Chow, Harry G.Zhang", 
                 "Josh Pasek [aut, cre], with some assistance from Alex Tahk and some code modified from R-core; Additional contributions by Gene Culter and Marcus Schwemmle." = 
                   "Josh Pasek [aut, cre], Alex Tahk, Gene Culter, Marcus Schwemmle.", 
                 "Rezgar Arabzadeh; Parisa Aberi; Kaveh Panaghi; Shahab Araghinejad; Majid Montaseri" = 
                   "Rezgar Arabzadeh, Parisa Aberi, Kaveh Panaghi, Shahab Araghinejad, Majid Montaseri"
                 #"Belinda Hernandez [aut, cre]\n    Adrian E. Raftery [aut]\n    Stephen R Pennington [aut]\n    Andrew C. Parnell [aut]\n    Eoghan O'Neill [ctb]" = 
                 #  "Belinda Hernandez [aut, cre],\n    Adrian E. Raftery [aut],\n    Stephen R Pennington [aut],\n    Andrew C. Parnell [aut],\n    Eoghan O'Neill [ctb]"
    )
  }