## code to prepare `cranlogdata` dataset goes here

path <- here::here("data-raw", "cranlogs_processed/")
fns <- dir(path)
dl <- lapply(fns[grepl("^20(20)", fns)], function(afile) read.csv(gzfile(file.path(path, afile))))

# Only 5 mb of data for CRAN 
# Current one goes to 117 MB. Need to reduce. \
# 2020 alone is 25 MB
 
cranlogdata <- dplyr::bind_rows(!!!dl) 
usethis::use_data(cranlogdata, overwrite = TRUE)
