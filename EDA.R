library(tidyverse)
library(ggplot2)
library(hms)

# RQ: Is there a significant difference between the younger and older age groups regarding:
#       1. dispatch_reason
#       2. time_resolve_issue

# Getting the data --------------------------------------------------------

from_event <- event_df %>% 
  select(PcrKey, dispatch_reason, EMD_performed, level_of_care_provided_per_protocol, transport_mode_from_scene, response_mode_to_scene, type_of_service_requested, unit_transport_and_equipment_capability)

from_time <- time_df %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime, time_resolve_issue)

from_patient <- patient_df %>% 
  select(-patient_race)

focus_data <- from_event %>% 
  left_join(from_time, by = "PcrKey") %>% 
  left_join(from_patient, by = "PcrKey") %>% 
  filter(
    type_of_service_requested == "Emergency Response (Primary Response Area)",
    response_mode_to_scene == "Emergent (Immediate Response)"
  ) %>% 
  select(-c(type_of_service_requested, response_mode_to_scene))


# Count time series + dispatch reason + age groups ------------------------


