## code to prepare `irs_profiles` dataset goes here
irs_profiles <- read.csv("data-raw/irs_profiles.csv", header=T)
usethis::use_data(irs_profiles, overwrite = TRUE)
