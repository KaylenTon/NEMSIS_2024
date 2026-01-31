library(tidyverse)
library(haven)
library(tools)
library(purrr)
library(lubridate)

# path <- "~/R PRACTICE/SAS2024CP25"
# 
# files <- list.files(
#   path,
#   pattern = "\\.sas7bdat$",
#   full.names = T
# )
# 
# system.time(
#   data <- lapply(files, read_sas, n_max = 5)
# )
# 
# names(data) <- file_path_sans_ext(basename(files))
# 
# use_tables <- c("pub_pcrevents",
#                 "pcrpatientracegroup",
#                 "factpcrturnarounddelay",
#                 "factpcrtime",
#                 "factpcrscenedelay",
#                 "factpcrresponsedelay",
#                 "factpcrdestinationteam")
# 
# unused_data <- data[use_tables] %>%
#   reduce(left_join, by = "PcrKey") %>%
#   distinct(PcrKey, .keep_all = TRUE)


# Random sample 1% --------------------------------------------------------

time_path <- ("C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrtime.sas7bdat")
system.time(
  keys <- read_sas(time_path, col_select = "PcrKey")
  )

set.seed(73)

system.time(
  sample_keys_one_percent <- keys %>% 
    slice_sample(prop = .01)
  )

head(sample_keys_one_percent)

select_paths <- c("C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/computedelements.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/pub_pcrevents.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/pcrpatientracegroup.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrturnarounddelay.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrtime.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrscenedelay.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrresponsedelay.sas7bdat",
                  "C:/Users/Kaylen/OneDrive - University of South Florida/Documents/R PRACTICE/SAS2024CP25/factpcrdestinationteam.sas7bdat")

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
    "eDisposition_17"),
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
    "eDisposition_24")
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




# Start here to retrieve dfs after .RData ---------------------------------

use_data <- reduce(sas_data_list, left_join, by = "PcrKey") %>% 
  distinct(PcrKey, .keep_all = TRUE)

# Selecting/reordering variables ------------------------------------------

select_data <- use_data %>% 
  select(PcrKey:EMSTotalCallTimeMin, contains(c("Dispatch", "Disposition", "Response", "Times", "Patient_", "Crew")))

# Removing NAs ------------------------------------------------------------

to_NA <- c("7701003", "7701001", "7701005", "Not Recorded", "Not Applicable")

clean_NA <- select_data %>% 
  mutate(across(everything(), ~ if_else(.x %in% to_NA, NA, .x)))


# Event table -------------------------------------------------------------

event_df <- clean_NA %>% 
  select(PcrKey:Urbanicity, eDispatch_01:eResponse_09) %>% 
  rename(
    dispatch_reason = eDispatch_01,
    EMD_performed = eDispatch_02,
    acuity_upon_EMS_release_of_patient = eDisposition_19,
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
    unit_transport_and_equipment_capability = eResponse_07,
    response_mode_to_scene = eResponse_23,
    type_of_turn_around_delay= eResponse_12,
    type_of_scene_delay = eResponse_10,
    type_of_response_delay = eResponse_09,
  ) %>% 
  mutate(
    dispatch_reason = recode(
      dispatch_reason,
      "2301001" = "Abdominal Pain/Problems",
      "2301003" = "Allergic Reaction/Stings",
      "2301005" = "Animal Bite",
      "2301007" = "Assault",
      "2301009" = "Automated Crash Notification",
      "2301011" = "Back Pain (Non-Traumatic)",
      "2301013" = "Breathing Problem",
      "2301015" = "Burns/Explosion",
      "2301017" ="Carbon Monoxide/Hazmat/Inhalation/CBRN",
      "2301019" = "Cardiac Arrest/Death",
      "2301021" = "Chest Pain (Non-Traumatic)",
      "2301023" = "Choking",
      "2301025" = "Convulsions/Seizure",
      "2301027" = "Diabetic Problem",
      "2301029" = "Electrocution/Lightning",
      "2301031" = "Eye Problem/Injury",
      "2301033" = "Falls",
      "2301035" = "Fire",
      "2301037" = "Headache",
      "2301039" = "Healthcare Professional/Admission",
      "2301041" = "Heart Problems/AICD",
      "2301043" = "Heat/Cold Exposure",
      "2301045" = "Hemorrhage/Laceration",
      "2301047" = "Industrial Accident/Inaccessible Incident/Other Entrapments (Non-Vehicle)",
      "2301049" = "Medical Alarm",
      "2301051" = "No Other Appropriate Choice",
      "2301053" = "Overdose/Poisoning/Ingestion",
      "2301055" = "Pandemic/Epidemic/Outbreak",
      "2301057" = "Pregnancy/Childbirth/Miscarriage",
      "2301059" = "Psychiatric Problem/Abnormal Behavior/Suicide Attempt",
      "2301061" = "Sick Person",
      "2301063" = "Stab/Gunshot Wound/Penetrating Trauma",
      "2301065" = "Standby",
      "2301067" = "Stroke/CVA",
      "2301069" = "Traffic/Transportation Incident",
      "2301071" = "Transfer/Interfacility/Palliative Care",
      "2301073" = "Traumatic Injury",
      "2301075" = "Well Person Check",
      "2301077" = "Unconscious/Fainting/Near-Fainting",
      "2301079" = "Unknown Problem/Person Down",
      "2301081" = "Drowning/Diving/SCUBA Accident",
      "2301083" = "Airmedical Transport",
      "2301085" = "Altered Mental Status",
      "2301087" = "Intercept",
      "2301089" = "Nausea",
      "2301091" = "Vomiting",
      "2301093" = "Hanging/Strangulation/Asphyxiation",
      "2301095" = "Intoxicated Subject",
      "2301097" = "EMS Requested by Law Enforcement",
      "2301099" = "Active Shooter"
      ),
    EMD_performed = recode(
      EMD_performed,
      "2302001" = "No",
      "2302003" = "Yes, With Pre-Arrival Instructions",
      "2302005" = "Yes, Without Pre-Arrival Instructions",
      "2302007" = "Yes, Unknown if Pre-Arrival Instructions Given"
      ),
    acuity_upon_EMS_release_of_patient = recode(
      acuity_upon_EMS_release_of_patient,
      "4219001" = "Critical (Red)",
      "4219003" = "Emergent (Yellow)",
      "4219005" = "Lower Acuity (Green)",
      "4219007" = "Dead without Resuscitation Efforts (Black)",
      "4219009" = "Dead with Resuscitation Efforts (Black)",
      "4219011" = "Non-Acute/Routine"
      ),
    EMS_transport_method = recode(
      EMS_transport_method,
      "4216001" = "Air Medical-Fixed Wing",
      "4216003" = "Air Medical-Rotor Craft",
      "4216005" = "Ground-Ambulance",
      "4216007" = "Ground-ATV or Rescue Vehicle",
      "4216009" = "Ground-Bariatric",
      "4216011" = "Ground-Other Not Listed",
      "4216013" = "Ground-Mass Casualty Bus/Vehicle",
      "4216015" = "Ground-Wheelchair Van",
      "4216017" = "Water-Boat"
    ),
    type_of_destination = recode(
      type_of_destination,
      "4221001" = "Home",
      "4221003" = "Hospital-Emergency Department",
      "4221005" = "Hospital-Non-Emergency Department Bed",
      "4221007" = "Clinic",
      "4221009" = "Morgue/Mortuary",
      "4221013" = "Other",
      "4221015" = "Other EMS Responder (air)",
      "4221017" = "Other EMS Responder (ground)",
      "4221019" = "Police/Jail",
      "4221021" = "Urgent Care",
      "4221023" = "Freestanding Emergency Department",
      "4221025" = "Dialysis Center",
      "4221027" = "Diagnostic Services",
      "4221029" = "Assisted Living Facility",
      "4221031" = "Mental Health Facility",
      "4221033" = "Nursing Home",
      "4221035" = "Other Recurring Care Center",
      "4221037" = "Physical Rehabilitation Facility",
      "4221039" = "Drug and/or Alcohol Rehabilitation Facility",
      "4221041" = "Skilled Nursing Facility"
    ),
    hospital_in_patient_destination = recode(
      hospital_in_patient_destination,
      "4222001" = "Hospital-Burn",
      "4222003" = "Hospital-Cath Lab",
      "4222005" = "Hospital-CCU",
      "4222007" = "Hospital-Endoscopy",
      "4222009" = "Hospital-Hospice",
      "4222011" = "Hospital-Hyperbaric Oxygen Treatment",
      "4222013" = "Hospital-ICU",
      "4222015" = "Hospital-Labor and Delivery",
      "4222017" = "Hospital-Med/Surg",
      "4222019" = "Hospital-Mental Health",
      "4222021" = "Hospital-MICU",
      "4222023" = "Hospital-NICU",
      "4222025" = "Hospital-Nursery",
      "4222027" = "Hospital-Peds (General)",
      "4222029" = "Hospital-Peds ICU",
      "4222031" = "Hospital-OR",
      "4222033" = "Hospital-Orthopedic",
      "4222035" = "Hospital-Other",
      "4222037" = "Hospital-Out-Patient Bed",
      "4222039" = "Hospital-Radiology Services - MRI",
      "4222041" = "Hospital-Radiology Services - CT/PET",
      "4222043" = "Hospital-Radiology Services - X-Ray",
      "4222045" = "Hospital-Radiation",
      "4222047" = "Hospital-Rehab",
      "4222049" = "Hospital-SICU",
      "4222051" = "Hospital-Oncology",
      "4222053" = "Hospital-Outpatient Surgery"
    ),
    level_of_care_provided_per_protocol = recode(
      level_of_care_provided_per_protocol,
      "4232001" = "BLS - All Levels",
      "4232003" = "ALS - AEMT/Intermediate",
      "4232005" = "ALS - Paramedic",
      "4232007" = "EMS and Other Health-Care Staff",
      "4232009" = "Critical Care",
      "4232011" = "Integrated Health Care",
      "4232013" = "No Care Provided"
    ),
    unit_disposition = recode(
      unit_disposition,
      "4227001" = "Patient Contact Made",
      "4227003" = "Cancelled on Scene",
      "4227005" = "Cancelled Prior to Arrival at Scene",
      "4227007" = "No Patient Contact",
      "4227009" = "No Patient Found",
      "4227011" = "Non-Patient Incident (Not Otherwise Listed)"
    ),
    patient_evaluation_care = recode(
      patient_evaluation_care,
      "4228001" = "Patient Evaluated and Care Provided",
      "4228003" = "Patient Evaluated and Refused Care",
      "4228005" = "Patient Evaluated, No Care Required",
      "4228007" = "Patient Refused Evaluation/Care",
      "4228009" = "Patient Support Services Provided"
    ),
    crew_disposition = recode(
      crew_disposition,
      "4229001" = "Initiated and Continued Primary Care",
      "4229003" = "Initiated Primary Care and Transferred to Another EMS Crew",
      "4229005" = "Provided Care Supporting Primary EMS Crew",
      "4229007" = "Assumed Primary Care from Another EMS Crew",
      "4229009" = "Incident Support Services Provided (Including Standby)",
      "4229011" = "Back in Service, No Care/Support Services Required",
      "4229013" = "Back in Service, Care/Support Services Refused"
    ),
    transport_disposition = recode(
      transport_disposition,
      "4230001" = "Transport by This EMS Unit (This Crew Only)",
      "4230003" = "Transport by This EMS Unit, with a Member of Another Crew",
      "4230005" = "Transport by Another EMS Unit/Agency",
      "4230007" = "Transport by Another EMS Unit/Agency, with a Member of This Crew",
      "4230009" = "Patient Refused Transport",
      "4230011" = "Non-Patient Transport (Not Otherwise Listed)",
      "4230013" = "No Transport"
    ),
    transport_mode_from_scene = recode(
      transport_mode_from_scene,
      "4217001" = "Emergent (Immediate Response)",
      "4217003" = "Emergent Downgraded to Non-Emergent",
      "4217005" = "Non-Emergent",
      "4217007" = "Non-Emergent Upgraded to Emergent"
    ),
    datetime_of_destination_prearrival_alert_or_activation = as.POSIXct(fast_strptime(datetime_of_destination_prearrival_alert_or_activation, format = "%d%b%Y:%H:%M:%S")),
    desination_team_prearrival_alert_or_activation = recode(
      desination_team_prearrival_alert_or_activation,
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
    ),
    type_of_service_requested = recode(
      type_of_service_requested,
      "2205001" = "Emergency Response (Primary Response Area)",
      "2205003" = "Emergency Response (Intercept)",
      "2205009" = "Emergency Response (Mutual Aid)",
      "2205005" = "Hospital-to-Hospital Transfer",
      "2205015" = "Hospital to Non-Hospital Facility Transfer",
      "2205017" = "Non-Hospital Facility to Non-Hospital Facility Transfer",
      "2205019" = "Non-Hospital Facility to Hospital Transfer",
      "2205007" = "Other Routine Medical Transport",
      "2205011" = "Public Assistance",
      "2205013" = "Standby",
      "2205021" = "Support Services",
      "2205023" = "Non-Patient Care Rescue/Extrication",
      "2205025" = "Crew Transport Only",
      "2205027" = "Transport of Organs or Body Parts",
      "2205029" = "Mortuary Services",
      "2205031" = "Mobile Integrated Health Care Encounter",
      "2205033" = "Evaluation for Special Referral/Intake Programs",
      "2205035" = "Administrative Operations"
    ),
    unit_transport_and_equipment_capability = recode(
      unit_transport_and_equipment_capability,
      "2207011" = "Air Transport-Helicopter",
      "2207013" = "Air Transport-Fixed Wing",
      "2207015" = "Ground Transport (ALS Equipped)",
      "2207017" = "Ground Transport (BLS Equipped)",
      "2207019" = "Ground Transport (Critical Care Equipped)",
      "2207021" = "Non-Transport-Medical Treatment (ALS Equipped)",
      "2207023" = "Non-Transport-Medical Treatment (BLS Equipped)",
      "2207025" = "Wheel Chair Van/Ambulette",
      "2207027" = "Non-Transport-No Medical Equipment"
    ),
    response_mode_to_scene = recode(
      response_mode_to_scene,
      "2223001" = "Emergent (Immediate Response)",
      "2223003" = "Emergent Downgraded to Non-Emergent",
      "2223005" = "Non-Emergent",
      "2223007" = "Non-Emergent Upgraded to Emergent"
    ),
    type_of_turn_around_delay = recode(
      type_of_turn_around_delay,
      "2212001" = "Clean-up",
      "2212003" = "Decontamination",
      "2212005" = "Distance",
      "2212007" = "Documentation",
      "2212009" = "ED Overcrowding / Transfer of Care",
      "2212011" = "Equipment Failure",
      "2212013" = "Equipment/Supply Replenishment",
      "2212015" = "None/No Delay",
      "2212017" = "Other",
      "2212019" = "Rendezvous Transport Unavailable",
      "2212021" = "Route Obstruction (e.g., Train)",
      "2212023" = "Staff Delay",
      "2212025" = "Traffic",
      "2212027" = "Vehicle Crash of this Unit",
      "2212029" = "Vehicle Failure of this Unit",
      "2212031" = "Weather",
      "2212033" = "EMS Crew Accompanies Patient for Facility Procedure"
    ),
    type_of_scene_delay = recode(
      type_of_scene_delay,
      "2210001" = "Awaiting Air Unit",
      "2210003" = "Awaiting Ground Unit",
      "2210005" = "Crowd",
      "2210007" = "Directions/Unable to Locate",
      "2210009" = "Distance",
      "2210011" = "Extrication",
      "2210013" = "HazMat",
      "2210015" = "Language Barrier",
      "2210017" = "None/No Delay",
      "2210019" = "Other",
      "2210021" = "Patient Access",
      "2210023" = "Safety-Crew/Staging",
      "2210025" = "Safety-Patient",
      "2210027" = "Staff Delay",
      "2210029" = "Traffic",
      "2210031" = "Triage/Multiple Patients",
      "2210033" = "Vehicle Crash Involving this Unit",
      "2210035" = "Vehicle Failure of this Unit",
      "2210037" = "Weather",
      "2210039" = "Mechanical Issue-Unit, Equipment, etc."
    ),
    type_of_response_delay = recode(
      type_of_response_delay,
      "2209001" = "Crowd",
      "2209003" = "Directions/Unable to Locate",
      "2209005" = "Distance",
      "2209007" = "Diversion (Different Incident)",
      "2209009" = "HazMat",
      "2209011" = "None/No Delay",
      "2209013" = "Other",
      "2209015" = "Rendezvous Transport Unavailable",
      "2209017" = "Route Obstruction (e.g., Train)",
      "2209019" = "Scene Safety (Not Secure for EMS)",
      "2209021" = "Staff Delay",
      "2209023" = "Traffic",
      "2209025" = "Vehicle Crash Involving this Unit",
      "2209027" = "Vehicle Failure of this Unit",
      "2209029" = "Weather",
      "2209031" = "Mechanical Issue-Unit, Equipment, etc.",
      "2209033" = "Flight Planning",
      "2209035" = "Out of Service Area Response"
    )
  )


# Time table --------------------------------------------------------------

time_df <- clean_NA %>% 
  select(PcrKey, EMSTransportTimeMin, EMSTotalCallTimeMin, eTimes_01:eTimes_13) %>% 
  rename(
    PSAP_call_datetime = eTimes_01,
    unit_notified_by_dispatch_datetime = eTimes_03,
    unit_en_route_datetime = eTimes_05,
    unit_arrived_on_scene_datetime = eTimes_06,
    arrived_at_patient_datetime = eTimes_07,
    unit_left_scene_datetime = eTimes_09,
    patient_arrived_at_destination_datetime = eTimes_11,
    destination_patient_transfer_of_care_datetime = eTimes_12,
    unit_back_in_service_datetime = eTimes_13
  ) %>% 
  mutate(
    across(PSAP_call_datetime:unit_back_in_service_datetime, ~ as.POSIXct(fast_strptime(.x, format = "%d%b%Y:%H:%M:%S"))
  )) %>% 
# Creating time_resolve_issue variable [upate: maybe we should delete this made variable]
  mutate(
    time_resolve_issue = as.numeric(difftime(unit_back_in_service_datetime, unit_notified_by_dispatch_datetime, units = "mins")),
    time_resolve_issue = round(time_resolve_issue, 2)
  )

# # Duration option
# time_df_2 <- time_df %>% 
#   mutate(
#     time_resolve_issue = difftime(unit_back_in_service_datetime, unit_notified_by_dispatch_datetime, units = "mins"),
#     time_resolve_issue = round(time_resolve_issue, 2)
#   )


# Patient table -----------------------------------------------------------

patient_df <- clean_NA %>% 
  select(PcrKey, ageinyear, ePatient_15:ePatient_14) %>% 
  rename(
    patient_age = ePatient_15,
    patient_age_units = ePatient_16,
    patient_race = ePatient_14
  ) %>% 
  mutate(
    patient_age_units = recode(
      patient_age_units,
      "2516001" = "Days",
      "2516003" = "Hours",
      "2516005" = "Minutes",
      "2516007" = "Months",
      "2516009" = "Years"),
    patient_race = recode(
      patient_race,
      "2514001" = "American Indian or Alaska Native",
      "2514003" = "Asian",
      "2514005" = "Black or African American",
      "2514007" = "Hispanic or Latino",
      "2514009" = "Native Hawaiian or Other Pacific Islander",
      "2514011" = "White",
      "2514013" = "Middle Eastern or North African")
  ) %>% 
# Standardizing age units all to years. If anyone is below 12 months of age, whether in months, days, hours, or minutes, they're 0 years old. If a baby is in between 12 and 24 months, they're one. I also grouped them into age groups: 64 and below are "Younger" and 65 and above are "Senior"
  mutate(
    patient_age = case_when(
      patient_age_units == "Years"  ~ patient_age,
      patient_age_units == "Months" ~ patient_age %/% 12,
      patient_age_units == "Days" ~ patient_age %/% 365,
      patient_age_units == "Hours" ~ patient_age %/% 8760,
      patient_age_units == "Minutes" ~ patient_age %/% 525600,
      is.na(patient_age_units) ~ patient_age
    )
  ) %>% 
  select(-patient_age_units) %>% 
  mutate(
    age_group = as.factor(ifelse(patient_age >= 65, "Senior", "Younger")
  )) %>% 
  rename(
    patient_age_years = patient_age
  ) %>% 
  mutate(
    age_decade_group = as.factor(case_when(
      patient_age_years >= 0 & patient_age_years < 5 ~ "0-4",
      patient_age_years >= 5 & patient_age_years < 10 ~ "5-9",
      patient_age_years >= 10 & patient_age_years < 15 ~ "10-14",
      patient_age_years >= 15 & patient_age_years < 20 ~ "15-19",
      patient_age_years >= 20 & patient_age_years < 25 ~ "20-24",
      patient_age_years >= 25 & patient_age_years < 30 ~ "25-29",
      patient_age_years >= 30 & patient_age_years < 35 ~ "30-34",
      patient_age_years >= 35 & patient_age_years < 40 ~ "35-39",
      patient_age_years >= 40 & patient_age_years < 45 ~ "40-44",
      patient_age_years >= 45 & patient_age_years < 50 ~ "45-49",
      patient_age_years >= 50 & patient_age_years < 55 ~ "50-54",
      patient_age_years >= 55 & patient_age_years < 60 ~ "55-59",
      patient_age_years >= 60 & patient_age_years < 65 ~ "60-64",
      patient_age_years >= 65 & patient_age_years < 70 ~ "65-69",
      patient_age_years >= 70 & patient_age_years < 75 ~ "70-74",
      patient_age_years >= 75 & patient_age_years < 80 ~ "75-79",
      patient_age_years >= 80 & patient_age_years < 85 ~ "80-84",
      patient_age_years >= 85 ~ "85+"
    ))
  ) %>% 
  mutate(
    patient_age_years = case_when(
      is.na(patient_age_years) ~ ageinyear,
      patient_age_years > ageinyear ~ patient_age_years, # So that 12 month babies are 1 years old.
      TRUE ~ ageinyear
    )
  ) %>% 
  select(-ageinyear)

  levels(patient_df$age_group) <- c("Younger", "Senior")
  levels(patient_df$age_decade_group) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

# save.image(file = "cleaningDataFileObjects.RData")

rm(clean_NA, sample_keys_one_percent, sas_data_list, select_data, use_data)

