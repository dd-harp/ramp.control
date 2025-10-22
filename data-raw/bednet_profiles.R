## code to prepare `bednet_profiles` dataset goes here
bednet_profiles <- read.csv("data-raw/bednet_profiles.csv", header=T)
usethis::use_data(bednet_profiles, overwrite=TRUE)
