## code to prepare `cranlogs_expdesign` dataset goes here
library(dplyr)
path <- here::here("data-raw", "cranlogs_processed/")
fns <- dir(path)
dl <- lapply(fns, function(afile) {
    read.csv(gzfile(file.path(path, afile))) %>% 
    dplyr::filter(package %in% ctv_pkgs("ExperimentalDesign"))
  })


ctvExperimentalDesign <- dplyr::bind_rows(!!!dl) %>% 
  mutate(date = as.Date(date)) %>% 
  select(-file_date)
usethis::use_data(ctvExperimentalDesign, overwrite = TRUE)
