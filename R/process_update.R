
# This is the script for updating the cranlog file to 1st of March
# Process all the cranlog data on the server 

start <- as.Date('2021-08-28')
today <- as.Date('2022-03-01')
all_days <- seq(start, today, by = 'day')
year <- as.POSIXlt(all_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')
writeLines(urls, "pkg_cranlogs.txt")


destfile <- paste0(all_days, '.csv.gz')

n <- length(destfile)

for (i in c(1:n)){
  download.file(url = urls[i],destfile = destfile[i])
}

paths <- list.files("cranlog_new", pattern = "\\.csv\\.gz$", full.names = TRUE)

done_paths <- list.files("cranlogs_processed/", pattern = "\\.csv\\.gz$")
paths <- paths[!(basename(paths) %in% done_paths)]

library(tidyverse)
library(progressr)

process_data <- function(paths) {
  p <- progressor(along = paths)
  for(file in paths) {
    read_csv(file,
             col_types = cols(
               date = col_date(format = ""),
               time = col_time(format = ""),
               size = col_double(),
               r_version = col_character(),
               r_arch = col_character(),
               r_os = col_character(),
               package = col_character(),
               version = col_character(),
               country = col_character(),
               ip_id = col_double()
             )) %>% 
      count(date, r_version, r_arch, r_os, package, version, country, ip_id) %>% 
      group_by(date, package) %>% 
      summarise(n_unique = n(), n_total = sum(n), .groups = "drop") %>% 
      write_csv(file.path(dirname(file), basename(file)))
    gc()
    p(xfun::sans_ext(basename(file)))
  }
  invisible(NULL)
}

handlers("progress")
with_progress(process_data(paths))


cran_new <- list.files(path="cranlogs_processed/processed", pattern = "\\.csv\\.gz$",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

saveRDS(cran_new, file = "processed_new.rds")

saveRDS(cran_new, file = "processed_new.rds")

