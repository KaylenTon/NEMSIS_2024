library(tidyverse)
library(haven)
library(tools)
library(purrr)
library(lubridate)

# Random sample 1% --------------------------------------------------------

set.seed(67)

time_path <- ("C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrtime.sas7bdat")

system.time(
  keys <- read_sas(time_path, col_select = "PcrKey")
)

system.time(
  sample_keys_one_percent <- keys %>% 
    slice_sample(prop = .01)
)

# head(sample_keys_one_percent)

select_paths <- c("C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/computedelements.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/pub_pcrevents.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/pcrpatientracegroup.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrturnarounddelay.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrtime.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrscenedelay.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrresponsedelay.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrdestinationteam.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/esituation_11ref.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrprimaryimpression.sas7bdat")

select_variables <- list(
  # computedelements
  c("PcrKey",
    "USCensusRegion",
    "USCensusDivision",
    "NasemsoRegion",
    "Urbanicity",
    "ageinyear",
    "EMSTransportTimeMin",
    "EMSTotalCallTimeMin"),
  # pub_pcrevents
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
  # pcrpatientracegroup
  c("PcrKey", 
    "ePatient_14"),
  # factpcrturnarounddelay
  c("PcrKey", 
    "eResponse_12"),
  # factpcrtime
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
  # factpcrscenedelay
  c("PcrKey", 
    "eResponse_10"),
  # factpcrresponsedelay
  c("PcrKey", 
    "eResponse_09"),
  # factpcrdestinationteam
  c("PcrKey", 
    "eDisposition_25", 
    "eDisposition_24"),
  # factpcrprimaryimpression
  c("PcrKey",
    "eSituation_11"),
  # esituation_11ref
  c("eSituation_11",
    "DiagnosisCodeDescr")
)

sas_data_list <- list()

for (i in seq_along(select_paths)) {
  
  interation_time <- system.time({
    
    temporary <- read_sas(select_paths[i], col_select = select_variables[[i]])
    
    DATA <- temporary %>% 
      semi_join(sample_keys_one_percent, by = "PcrKey")
    
    sas_data_list[[i]] <- DATA
    
    rm(temporary)
    gc()
    
  })
  
  print(paste("Iteration", i, "complete"))
  print(interation_time)
  
}

names(sas_data_list) <- file_path_sans_ext(basename(select_paths))