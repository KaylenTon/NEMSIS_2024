library(arrow)
library(haven)
library(duckdb)
library(dplyr)
library(parquetize)
library(DBI)
library(tools)
library(lubridate)

set.seed(67)

# time_path <- ("C:/Users/xmega/University of South Florida/Loni Hagen - data/SAS2024CP25/factpcrtime.sas7bdat")
# 
# system.time(
#   keys <- read_sas(time_path, col_select = "PcrKey")
#   )
# 
#  system.time(
#    sample_keys_one_percent <- keys %>% 
#   slice_sample(prop = .01)
#  )
#  
#  #save sample_keys as a R data object
#  save(sample_keys_one_percent, file = "sample_keys_one_percent.RData")
 

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

#create a parquet folder to store parquet files
dir.create("C:/Users/xmega/University of South Florida/Loni Hagen - data/parquet", showWarnings = FALSE)

#for each SAS file, convert to SAS and store in the parquet folder
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

sample_keys <- datasets$factpcrtime %>%
  select(PcrKey) %>%
  distinct() %>%
  collect() %>%
  slice_sample(prop = 0.01)

sample_keys_arrow <- arrow_table(sample_keys)

sampled_datasets <- lapply(datasets, function(ds){
  cols <- names(ds)
  
  #skip lookup tables without PcrKey
  if (!"PcrKey" %in% cols){
    return(ds)
  }
  
  ds %>%
    semi_join(sample_keys_arrow,
              by="PcrKey")
})

#convert arrow datasets to R DataFrames
sampled_dfs <- lapply(sampled_datasets,
                      collect)

#add diagnosis description
sampled_dfs$factpcrprimaryimpression <-
  sampled_dfs$factpcrprimaryimpression %>%
  left_join(
    sampled_dfs$esituation_11ref,
    by = "eSituation_11"
  )


### REMOVE NAs ###

to_NA <- c(
  "7701003",
  "7701001",
  "7701005",
  "Not Recorded",
  "Not Applicable",
  ""
)

sampled_dfs <- lapply(sampled_dfs, function(df){
  
  df %>%
    mutate(across(everything(),~ replace(.x, as.character(.x) %in% to_NA, NA)))
  
})

### RENAME AND CAST TO VARIABLE TYPES ###

column_map <- c(eDisposition_25 = "datetime_of_destination_prearrival_alert_or_activation",
                eDisposition_24 = "destination_team_prearrival_alert_or_activation")

#for each df, print all columns and unique values for each column

### FACTPCRDDESTINATIONTEAM ###
for (col in names(sampled_dfs$factpcrdestinationteam)){
  
  cat("\n====================\n")
  cat("COLUMN:", col, "\n")
  cat("====================\n")
  
  print(unique(sampled_dfs$factpcrdestinationteam[[col]]))
  
}

#create code lookups
eDisposition_24_lookup <- c(
  "4224001" = "No",
  "4224003" = "Yes-Adult Trauma",
  "4224005" = "Yes-Cardiac Arrest",
  "4224007" = "Yes-Obstetrics",
  "4224009" = "Yes-Other",
  "4224011" = "Yes-Pediatric Trauma",
  "4224013" = "Yes-STEMI",
  "4224015" = "Yes-Stroke",
  "4224017" = "Yes-Trauma (General)",
  "4224019" = "Yes-Sepsis"
)

sampled_dfs$factpcrdestinationteam <-
  sampled_dfs$factpcrdestinationteam %>%
  mutate(
    eDisposition_24 =
      eDisposition_24_lookup[
        as.character(eDisposition_24)
      ]
  )

test_df <- sampled_dfs$factpcrdestinationteam
#remove NA and translate code values to english


# rename all table columns to english

sampled_dfs <- lapply(sampled_dfs, function(df){
  
  names(df) <- ifelse(
    names(df) %in% names(column_map),
    column_map[names(df)],
    names(df)
  )
  
  df
  
})


# ###############################
# #### CONNECTING TO DUCK DB ####
# ###############################
# 
# con <- dbConnect(duckdb(), dbdir = ":memory:")
# 
# #register all datasets into duckdb
# for(tbl in setdiff(names(sampled_datasets), "eSituation_11ref")) {
#   
#   try(
#     duckdb_register_arrow(
#       con,
#       paste0(tbl, "_sample"),
#       sampled_datasets[[tbl]]
#     ),
#     silent = TRUE
#   )
#   
# }
# 
# # #sample a reproducible query of 1%
# # sample_keys <- dbGetQuery(con, "
# #                           SELECT PcrKey
# #                           FROM factpcrtime
# #                           ORDER BY RANDOM()
# #                           LIMIT (
# #                             SELECT CAST(COUNT(*) * 0.01 AS INTEGER)
# #                             FROM factpcrtime
# #                           )")
# # 
# # #register sample to db
# # dbWriteTable(con, "sample_keys", sample_keys, overwrite = TRUE)
# # 
# # tables <- names(datasets)
# # 
# # #remove eSituation_11 from tables; sampling it will not work - it is a description table, not a PCR table
# # tables <- tables[-2]
# # tables
# # 
# # #create the sampled tables
# # for (tbl in tables) {
# #   
# #   dbExecute(con, paste0("
# #                         CREATE OR REPLACE TABLE ", tbl, "_sample AS
# #                         SELECT *
# #                         FROM ", tbl,"
# #                         SEMI JOIN sample_keys
# #                           USING(PcrKey)"))
# # }
# 
# 
# tables <- names(sampled_datasets)
# pcr_tables <- tables[-2]
# 
# #create a record to show total rows vs unique keys of each 1% sampled table
# dup_summary <- lapply(pcr_tables, function(tbl) {
#   dbGetQuery(
#     con,
#     paste0(
#       "SELECT '", tbl, "_sample' AS table_name,
#               COUNT(*) AS rows,
#               COUNT(DISTINCT PcrKey) AS unique_keys
#        FROM ", tbl, "_sample"
#     )
#   )
# }) |> dplyr::bind_rows()
# 
# #print duplicate record summary
# print(dup_summary)
# 
# ### RESULTS ###
# # table_name   rows unique_keys
# # 1         computedelements_sample 602986      602986
# # 2   factpcrdestinationteam_sample 603307      602986
# # 3 factpcrprimaryimpression_sample 588968      588968
# # 4     factpcrresponsedelay_sample 606041      602986
# # 5        factpcrscenedelay_sample 607282      602986
# # 6              factpcrtime_sample 602986      602986
# # 7   factpcrturnarounddelay_sample 613752      602986
# # 8      pcrpatientracegroup_sample 609777      602608
# # 9            pub_pcrevents_sample 602986      602986
# 
# #save names of tables with duplicates
# dup_tables <- dup_summary$table_name[dup_summary$rows > dup_summary$unique_keys]
# 
# #save to schemas object: for each table, get column name and information
# schemas <- lapply(pcr_tables, function(tbl) {
#   x <- dbGetQuery(
#     con,
#     paste0("DESCRIBE ", tbl,"_sample")
#   )
#   
#   x$table_name <- tbl
#   x
# })
# 
# schemas <- do.call(rbind, schemas)
# 
# schemas
# 
# ##################
# ### REMOVE NAs ###
# ##################    
# 
# na_values <- c(
#   "7701003",
#   "7701001",
#   "7701005",
#   "Not Recorded",
#   "Not Applicable",
#   ""
# )
# 
# na_sql <- paste(sprintf("'%s'", na_values), collapse = ",")
# 
# sample_tables <- paste0(names(sampled_datasets), "_sample")
# 
# for(tbl in sample_tables){
#   
#   cols <- dbGetQuery(
#     con,
#     paste0("DESCRIBE ", tbl)
#   )$column_name
#   
#   select_sql <- sapply(cols, function(col){
#     
#     if(col == "PcrKey"){
#       return("PcrKey")
#     }
#     
#     paste0(
#       "CASE
#          WHEN CAST(", col, " AS VARCHAR) IN (", na_sql, ")
#          THEN NULL
#          ELSE ", col, "
#        END AS ", col
#     )
#     
#   })
#   
#   query <- paste0(
#     "CREATE OR REPLACE TABLE ",
#     sub('_sample$', '_na', tbl),
#     " AS
#      SELECT ",
#     paste(select_sql, collapse = ",\n"),
#     "
#      FROM ", tbl
#   )
#   
#   dbExecute(con, query)
# }
# 
# ###################
# ### DEDUPLICATE ###
# ###################
# 
# #tables that must be deduplicated: factpcrdestinationteam, factpcrresponsedelay, factpcrscenedelay, factpcrturnarounddelay, pcrpatientracegroup
# dup_tables
# 
# dup_table_schemas <- lapply(dup_tables, function(tbl) {
#   x <- dbGetQuery(
#     con, paste0("DESCRIBE ", tbl)
#   )
#   
#   x$table_name <- tbl
#   x
# })
# 
# dup_table_schemas <- do.call(rbind, dup_table_schemas)
# 
# dup_table_schemas
# 
# dup_report <- list()
# 
# for (tbl in dup_tables){
#   
#   #get the columns from each table with duplicates from dup_tables
#   cols <- dbGetQuery(
#     con,
#     paste0("DESCRIBE ", tbl)
#   )$column_name
#   
#   #remove PcrKey from each cols list, we only want to focus on the trouble columns
#   cols <- setdiff(cols, "PcrKey")
#   
#   column_results <- lapply(cols, function(col){
#     
#     query <- paste0("
#       SELECT
#         '", tbl, "' AS table_name,
#         '", col, "' AS column_name,
#         COUNT(*) AS keys_with_multiple_values
#       FROM (
#         SELECT PcrKey
#         FROM ", tbl, "
#         GROUP BY PcrKey
#         HAVING COUNT(DISTINCT ", col, ") > 1
#       )")
#     
#     dbGetQuery(con, query)
# 
#   })
# 
#   dup_report[[tbl]] <- do.call(rbind, column_results)
# }
# 
# 
# variation_report <- do.call(rbind, dup_report)
# 
# variation_report <- subset(
#   variation_report,
#   keys_with_multiple_values > 0)
#   
# variation_report
# 
# ## RESULTS OF VARIATION REPORT ##
# 
# #                                                    table_name            column_name keys_with_multiple_values
# # factpcrdestinationteam_sample.1 factpcrdestinationteam_sample        eDisposition_25                       201
# # factpcrdestinationteam_sample.2 factpcrdestinationteam_sample        eDisposition_24                       344
# # factpcrresponsedelay_sample       factpcrresponsedelay_sample           eResponse_09                      2640
# # factpcrscenedelay_sample             factpcrscenedelay_sample           eResponse_10                      3511
# # factpcrturnarounddelay_sample   factpcrturnarounddelay_sample           eResponse_12                      6950
# # pcrpatientracegroup_sample.1       pcrpatientracegroup_sample PcrPatientRaceGroupKey                      4549
# # pcrpatientracegroup_sample.2       pcrpatientracegroup_sample            ePatient_14                      4549
# 
# ### DUPLICATE COLUMNS - DO NOT RENAME YET ###
# # datetime_of_destination_prearrival_alert_or_activation = eDisposition_25
# 
# # destination_team_prearrival_alert_or_activation = eDisposition_24
# # type_of_response_delay = eResponse_09
# # type_of_scene_delay = eResponse_10
# # type_of_turn_around_delay= eResponse_12
# # patient_race = ePatient_14
# ####################################################
# 
# #ePatient_14
# dbExecute(con, "CREATE OR REPLACE TABLE pcrpatientracegroup_clean AS
#           SELECT
#             PcrKey,
#             string_agg(DISTINCT CAST(ePatient_14 AS VARCHAR), '_') AS ePatient_14
#           FROM pcrpatientracegroup_sample
#           GROUP BY PcrKey;")
# 
# #eResponse_09
# dbExecute(con, "CREATE OR REPLACE TABLE factpcrresponsedelay_clean AS
#           SELECT
#             PcrKey,
#             string_agg(DISTINCT CAST(eResponse_09 AS VARCHAR), '_') AS eResponse_09
#           FROM factpcrresponsedelay_sample
#           GROUP BY PcrKey;")
# 
# #eResponse_10
# dbExecute(con, "CREATE OR REPLACE TABLE factpcrscenedelay_clean AS
#           SELECT
#             PcrKey,
#             string_agg(DISTINCT CAST(eResponse_10 AS VARCHAR), '_') AS eResponse_10
#           FROM factpcrscenedelay_sample
#           GROUP BY PcrKey;")
# 
# #eResponse_12
# dbExecute(con, "CREATE OR REPLACE TABLE factpcrturnarounddelay_clean AS
#           SELECT
#             PcrKey,
#             string_agg(DISTINCT CAST(eResponse_12 AS VARCHAR), '_') AS eResponse_12
#           FROM factpcrturnarounddelay_sample
#           GROUP BY PcrKey;")
# 
# #time column: keep the latest timestamp and create a new column calculating the difference between the latest and earliest timestamp
# 
# #eDisposition_24 and eDisposition_25
# dbExecute(con,"
# CREATE OR REPLACE TABLE factpcrdestinationteam_clean AS
# SELECT
#     PcrKey,
# 
#     string_agg(
#         DISTINCT CAST(eDisposition_24 AS VARCHAR),
#         '_'
#     ) AS eDisposition_24,
# 
#     MAX(
#         strptime(
#             eDisposition_25,
#             '%d%b%Y:%H:%M:%S'
#         )
#     ) AS latest_eDisposition_25,
# 
#     MIN(
#         strptime(
#             eDisposition_25,
#             '%d%b%Y:%H:%M:%S'
#         )
#     ) AS earliest_eDisposition_25,
# 
#     datediff(
#         'minute',
#         MIN(
#             strptime(
#                 eDisposition_25,
#                 '%d%b%Y:%H:%M:%S'
#             )
#         ),
#         MAX(
#             strptime(
#                 eDisposition_25,
#                 '%d%b%Y:%H:%M:%S'
#             )
#         )
#     ) AS eDisposition_25_duration
# 
# FROM factpcrdestinationteam_sample
# 
# WHERE eDisposition_25 NOT IN (
#     'Not Recorded',
#     'Not Applicable',
#     ''
# )
# 
# GROUP BY PcrKey
# ")
# 
# ### DEDUPLICATION END ###
# 
# #########################################
# ### INSPECTION OF DEDUPLICATED TABLES ###
# #########################################
# 
# #Inspect cleaned deduplicated tables
# dbListTables(con)
# tables <- names(sampled_datasets)
# dedup_tables <- dbListTables(con)
# 
# #inspect if the clean tables contain any more duplicated PcrKeys
# dedup_report <- list()
# 
# for (tbl in dedup_tables){
#   
#   #get the columns from each table
#   cols <- dbGetQuery(
#     con,
#     paste0("DESCRIBE ", tbl)
#   )$column_name
#   
#   #remove PcrKey from each cols list
#   cols <- setdiff(cols, "PcrKey")
#   
#   column_results <- lapply(cols, function(col){
#     
#     query <- paste0("
#       SELECT
#         '", tbl, "' AS table_name,
#         '", col, "' AS column_name,
#         COUNT(*) AS keys_with_multiple_values
#       FROM (
#         SELECT PcrKey
#         FROM ", tbl, "
#         GROUP BY PcrKey
#         HAVING COUNT(DISTINCT ", col, ") > 1
#       )")
#     
#     dbGetQuery(con, query)
#     
#   })
#   
#   dedup_report[[tbl]] <- do.call(rbind, column_results)
# }
# 
# 
# variation_dedup_report <- do.call(rbind, dedup_report)
# 
# variation_dedup_report <- subset(
#   variation_dedup_report,
#   keys_with_multiple_values > 0)
# 
# variation_dedup_report
# 
# #inspect rows containing "_"
# str_dedup_tables <- dedup_tables[-c("factpcrdestinationteam_clean")]
# 
# for (tbl in str_dedup_tables){
#   
#   #get the columns from each table
#   cols <- dbGetQuery(
#     con,
#     paste0("DESCRIBE ", tbl)
#   )$column_name
#   
#   #remove PcrKey from each cols list
#   cols <- setdiff(cols, "PcrKey")
#   
#   column_results <- lapply(cols, function(col){
#     
#     query <- paste0("
#       SELECT * FROM ", tbl, "
#                     WHERE ", col, "
#                     LIKE '%_%")
#     
#     dbGetQuery(con, query)
#     
#   })
# }
# 
# #nothing returned = deduplication succeeded
# 
# #inspect factpcrdestinationteam separately
# dbGetQuery(con, "
# SELECT *
# FROM factpcrdestinationteam_clean
# WHERE eDisposition_25_duration IS NOT NULL
# ")
# 
# ############################
# ### JOIN TABLES TOGETHER ###
# ############################
# 
# 
# ################################
# ### RENAME COLUMNS AND CODES ###
# ################################
# 
# 
# #save as an R object
