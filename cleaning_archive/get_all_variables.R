library(tidyverse)
library(haven)
library(tools)
library(purrr)
library(lubridate)

path <- "~/R PRACTICE/SAS2024CP25"

files <- list.files(
  path,
  pattern = "\\.sas7bdat$",
  full.names = T
)

system.time(
  all_variables <- lapply(files, read_sas, n_max = 200)
)

names(all_variables) <- file_path_sans_ext(basename(files))

save.image(file = "all_variables.Rdata")
