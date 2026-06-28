library(arrow)
library(haven)
library(duckdb)
library(dplyr)
library(parquetize)
library(DBI)
library(tools)

set.seed(67)

time_path <- ("C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrtime.sas7bdat")

system.time(
  keys <- read_sas(time_path, col_select = "PcrKey")
  )

 system.time(
   sample_keys_one_percent <- keys %>% 
  slice_sample(prop = .01)
 )
 

 select_paths <- c("C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/computedelements.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/pub_pcrevents.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/pcrpatientracegroup.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrturnarounddelay.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrtime.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrscenedelay.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrresponsedelay.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrdestinationteam.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrprimaryimpression.sas7bdat",
                   "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/esituation_11ref.sas7bdat")
 
dir.create("parquet_data", showWarnings = FALSE)

for (p in select_paths){
  cat("Processing: ", basename(p), "\n")
  df <- read_sas(p)
  out_path <- file.path("parquet_data",paste0(file_path_sans_ext(basename(p)), ".parquet"))
  write_parquet(df, out_path)
  rm(df)
  gc()
} 

 

#set an in memory DB
con <- dbConnect(duckdb(), dbdir = ":memory:")

#query files without needing to load into R
dbGetQuery(conn,
           SELECT COUNT(*)
           FROM 'factpcrtime.parquet')

sas_file <- "C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/computedelements.sas7bdat"
df <- read_sas(sas_file)
