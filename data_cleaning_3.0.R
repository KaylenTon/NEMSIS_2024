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
 
 #save sample_keys as a R data object
 save(sample_keys_one_percent, file = "sample_keys_one_percent.RData")
 

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
 
select_variables <- list(
     # computedelements 1
     c("PcrKey",
       "USCensusRegion",
       "USCensusDivision",
       "NasemsoRegion",
       "Urbanicity",
       "ageinyear",
       "EMSTransportTimeMin",
       "EMSTotalCallTimeMin"),
     # pub_pcrevents 2
     c("PcrKey",
       "eDispatch_01",
       "eDispatch_02",
       "eDisposition_19",
       "eDisposition_16",
       "eDisposition_21",
       "eDisposition_22",
       "eDisposition_32",
       "eDisposition_27",
       "eDisposition_28",
       "eDisposition_29",
       "eDisposition_30",
       "eResponse_05",
       "eResponse_07",
       "eResponse_23",
       "ePatient_15",
       "ePatient_16",
       "eDisposition_17",
       "eSituation_13",
       "eDisposition_19"),
     # pcrpatientracegroup 3
     c("PcrKey",
       "ePatient_14"),
     # factpcrturnarounddelay 4
     c("PcrKey",
       "eResponse_12"),
     # factpcrtime 5
     c("PcrKey",
       "eTimes_01",
       "eTimes_03",
       "eTimes_05",
       "eTimes_06",
       "eTimes_07",
       "eTimes_09",
       "eTimes_11",
       "eTimes_12",
       "eTimes_13"),
     # factpcrscenedelay 6
     c("PcrKey",
       "eResponse_10"),
     # factpcrresponsedelay 7
     c("PcrKey",
       "eResponse_09"),
     # factpcrdestinationteam 8
     c("PcrKey",
       "eDisposition_25",
       "eDisposition_24"),
     # factpcrprimaryimpression 9
     c("PcrKey",
       "eSituation_11"),
     # esituation_11ref 10
     c("eSituation_11",
       "DiagnosisCodeDescr")
   )

dir.create("C:/Users/xmega/University of South Florida/Loni Hagen - data/parquet", showWarnings = FALSE)

for (p in select_paths){
  cat("Processing: ", basename(p), "\n")
  df <- read_sas(p)
  out_path <- file.path("C:/Users/xmega/University of South Florida/Loni Hagen - data/parquet",paste0(file_path_sans_ext(basename(p)), ".parquet"))
  write_parquet(df, out_path)
  rm(df)
  gc()
}


#read all parquet files using arrow
ds <- open_dataset("C:/Users/xmega/University of South Florida/Loni Hagen - data/parquet")

#get parquet directory
parquet_dir <- "C:/Users/xmega/University of South Florida/Loni Hagen - data/parquet"

#store all parquet files from the parquet directory into files variable
files <- list.files(
  parquet_dir,
  pattern ="\\.parquet$",
  full.names = TRUE
)

#create an empty datasets list
datasets <- list()

#for each file in files, get the name of the file and open it. Save it with its original name in the datasets list
for (f in files){
  name <- tools::file_path_sans_ext(basename(f))
  
  datasets[[name]] <- open_dataset(f)
}

###############################
#### CONNECTING TO DUCK DB ####
###############################

con <- dbConnect(duckdb(), dbdir = ":memory:")

#register all datasets into duckdb
for (name in names(datasets)){
  duckdb_register_arrow(
    con,
    name,
    datasets[[name]]
  )
}

#sample a reproducible query of 1%
sample_keys <- dbGetQuery(con, "
                          SELECT PcrKey
                          FROM factpcrtime
                          ORDER BY RANDOM()
                          LIMIT (
                            SELECT CAST(COUNT(*) * 0.01 AS INTEGER)
                            FROM factpcrtime
                          )")

#register sample to db
dbWriteTable(con, "sample_keys", sample_keys, overwrite = TRUE)

tables <- names(datasets)

#remove eSituation_11 from tables; sampling it will not work - it is a description table, not a PCR table
tables <- tables[-2]
tables

#create the sampled tables
for (tbl in tables) {
  
  dbExecute(con, paste0("
                        CREATE OR REPLACE TABLE ", tbl, "_sample AS
                        SELECT t.*
                        FROM ", tbl," t
                        INNER JOIN sample_keys s
                          ON t.PcrKey = s.PcrKey"))
}

#show current tables in db
dbGetQuery(con, "SHOW TABLES")

#create a record to show total rows vs unique keys of each 1% sampled table
dup_summary <- lapply(tables, function(tbl) {
  dbGetQuery(
    con,
    paste0(
      "SELECT '", tbl, "' AS table_name,
              COUNT(*) AS rows,
              COUNT(DISTINCT PcrKey) AS unique_keys
       FROM ", tbl
    )
  )
}) |> dplyr::bind_rows()

#print duplicate record summary
print(dup_summary)

#save names of tables with duplicates
dup_tables <- dup_summary$table_name[dup_summary$rows > dup_summary$unique_keys]

#redefine tables variable; get all current tables from the current DB connection
tables <- dbListTables(con)

#save to schemas object: for each table, get column name and information
schemas <- lapply(tables, function(tbl) {
  x <- dbGetQuery(
    con,
    paste0("DESCRIBE ", tbl)
  )
  
  x$table_name <- tbl
  x
})

schemas <- do.call(rbind, schemas)

schemas

### DEDUPLICATE ###
#tables that must be deduplicated: factpcrdestinationteam, factpcrresponsedelay, factpcrscenedelay, factpcrturnarounddelay, pcrpatientracegroup
dup_tables

dup_table_schemas <- lapply(dup_tables, function(tbl) {
  x <- dbGetQuery(
    con, paste0("DESCRIBE ", tbl)
  )
  
  x$table_name <- tbl
  x
})

dup_table_schemas <- do.call(rbind, dup_table_schemas)

dup_table_schemas

dup_report <- list()

for (tbl in dup_tables){
  
  #get the columns from each table with duplicates from dup_tables
  cols <- dbGetQuery(
    con,
    paste0("DESCRIBE ", tbl)
  )$column_name
  
  #remove PcrKey from each cols list, we only want to focus on the trouble columns
  cols <- setdiff(cols, "PcrKey")
  
  column_results <- lapply(cols, function(col){
    
    query <- paste0("
      SELECT
        '", tbl, "' AS table_name,
        '", col, "' AS column_name,
        COUNT(*) AS keys_with_multiple_values
      FROM (
        SELECT PcrKey
        FROM ", tbl, "
        GROUP BY PcrKey
        HAVING COUNT(DISTINCT ", col, ") > 1
      )")
    
    dbGetQuery(con, query)

  })

  dup_report[[tbl]] <- do.call(rbind, column_results)
}


variation_report <- do.call(rbind, dup_report)

variation_report <- subset(
  variation_report,
  keys_with_multiple_values > 0)
  
variation_report


# Fill in applicable null codes with NA
to_NA <- c("7701003", "7701001", "7701005", "Not Recorded", "Not Applicable", "")

#rename columns

#deduplicate tables with PcrKey duplicates

#join tables together on PcrKey using an "_"

#save as an R object
