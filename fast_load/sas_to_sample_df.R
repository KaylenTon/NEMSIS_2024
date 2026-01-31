library(arrow)
library(dplyr)
library(glue)

load_and_sample_Sas <- function(output_dir_name, col_to_sample = PcrKey, sample_size = 1000){
  # PARAMETERS:
  # output_dir_name = name of directory where parquet files are being stored
  # col_to_sample = which column to sample from
  # sample_size = how large your sample is
  
  ds <- open_dataset(glue("fast_load/{output_dir_name}"), format = "parquet")
  
  all_ids <- ds %>%
    select(col_to_sample) %>%
    collect()
  
  sample_size <- 1000
  sampled_keys <- sample(all_ids$col_to_sample, size = sample_size)
  
  random_sample <- ds %>%
    filter(col_to_sample %in% sampled_keys) %>%
    collect()
  
  unlink("fast_load/{output_dir_name}", recursive = TRUE)
  
  return(random_sample)
}


sampled_df <- load_and_sample_Sas("computed_elements")