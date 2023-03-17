trios_march <- read_trios("inst/extdata/trios_march/", depths = 0:9)

usethis::use_data(trios_march, overwrite = TRUE)
