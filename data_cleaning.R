library(tidyverse)
library(haven)
library(tools)

path <- "~/R PRACTICE/NEMSIS_2024/SAS2024CP25"

files <- list.files(
  path,
  pattern = "\\.sas7bdat$",
  full.names = T
)

data <- lapply(files, read_sas, n_max = 1000)

names(data) <- file_path_sans_ext(basename(files))

library(purrr)
use_tables <- c("pub_pcrevents",
                "pcrpatientracegroup",
                "factpcrturnarounddelay",
                "factpcrtime",
                "factpcrscenedelay",
                "factpcrresponsedelay",
                "factpcrdestinationteam")

use_data <- data[use_tables] %>% 
  reduce(left_join, by = "PcrKey")


# CLEANING ----------------------------------------------------------------

select_data <- use_data %>% 
  select(PcrKey, contains(c("Dispatch", "Disposition", "Response", "Times", "Patient_", "Crew"))) %>% 
  rename(
    dispatch_reason = eDispatch_01,
    EMD_performed = eDispatch_02,
    acuity_upon_EMS_release_of_patients = eDisposition_19,
    EMS_transport_method = eDisposition_16,
    type_of_destination = eDisposition_21,
    hospital_in_patient_destination = eDisposition_22,
    level_of_care_provided_per_protocol = eDisposition_32,
    unit_disposition = eDisposition_27,
    patient_evaluation_care = eDisposition_28,
    crew_disposition = eDisposition_29,
    transport_disposition = eDisposition_30,
    transport_mode_from_scene = eDisposition_17,
    datetime_of_destination_prearrival_alert_or_activation = eDisposition_25,
    desination_team_prearrival_alert_or_activation = eDisposition_24,
    type_of_service_requested = eResponse_05,
    unit_transport_and_equipment_capability = eResponse_07, # different type of ems vehicles may affect time to scene and time to destination
    response_mode_to_scene = eResponse_23,
    type_of_turn_around_delay= eResponse_12,
    type_of_scene_delay = eResponse_10,
    type_of_response_delay = eResponse_09,
    PSAP_call_datetime = eTimes_01,
    unit_notified_by_dispatch_datetime = eTimes_03,
    unit_en_route_datetime = eTimes_05,
    unit_arrived_on_scene_datetime = eTimes_06,
    arrived_at_patient_datetime = eTimes_07,
    unit_left_scene_datetime = eTimes_09,
    patient_arrived_at_destination_datetime = eTimes_11,
    destination_patient_transfer_of_care_datetime = eTimes_12,
    unit_back_in_service_datetime = eTimes_13,
    patient_age = ePatient_15,
    patient_age_units = ePatient_16,
    patient_race = ePatient_14
  )
