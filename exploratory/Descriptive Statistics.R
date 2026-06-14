library(tidyverse)
library(ggplot2)

# Event variables ----------------------------------------------------------------

z <- location_df %>% 
  count(USCensusRegion) %>% 
  mutate(
    proportion = n / sum(n),
    table = "location_df",
    variable = "USCensusRegion"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = USCensusRegion)

y <- location_df %>% 
  count(USCensusDivision) %>% 
  mutate(
    proportion = n / sum(n),
    table = "location_df",
    variable = "USCensusDivision"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = USCensusDivision)

x <- location_df %>% 
  count(NasemsoRegion) %>% 
  mutate(
    proportion = n / sum(n),
    table = "location_df",
    variable = "NasemsoRegion"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = NasemsoRegion)

m <- location_df %>% 
  count(Urbanicity) %>% 
  mutate(
    proportion = n / sum(n),
    table = "location_df",
    variable = "Urbanicity"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = Urbanicity)

a <- event_df %>%
  count(dispatch_reason) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "dispatch_reason"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = dispatch_reason) %>% 
  print(n = Inf)

b <- event_df %>%
  count(EMD_performed) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "EMD_performed"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = EMD_performed)

c <- event_df %>%
  count(acuity_upon_EMS_release_of_patient) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "acuity_upon_EMS_release_of_patient"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = acuity_upon_EMS_release_of_patient)

d <- event_df %>%
  count(EMS_transport_method) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "EMS_transport_method"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = EMS_transport_method)

e <- event_df %>%
  count(type_of_destination) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "type_of_destination"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = type_of_destination)

f <- event_df %>%
  count(hospital_in_patient_destination) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "hospital_in_patient_destination"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = hospital_in_patient_destination)

g <- event_df %>%
  count(level_of_care_provided_per_protocol) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "level_of_care_provided_per_protocol"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = level_of_care_provided_per_protocol)

h <- event_df %>%
  count(unit_disposition) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "unit_disposition"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = unit_disposition)

i <- event_df %>%
  count(patient_evaluation_care) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "patient_evaluation_care"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = patient_evaluation_care)

j <- event_df %>%
  count(crew_disposition) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "crew_disposition"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = crew_disposition)

k <- event_df %>%
  count(transport_disposition) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "transport_disposition"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = transport_disposition)

l <- event_df %>%
  count(transport_mode_from_scene) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "transport_mode_from_scene"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = transport_mode_from_scene)

n <- event_df %>%
  count(desination_team_prearrival_alert_or_activation) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "desination_team_prearrival_alert_or_activation"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = desination_team_prearrival_alert_or_activation)

o <- event_df %>%
  count(type_of_service_requested) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "type_of_service_requested"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = type_of_service_requested)


p <- event_df %>%
  count(unit_transport_and_equipment_capability) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "unit_transport_and_equipment_capability"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = unit_transport_and_equipment_capability)

q <- event_df %>%
  count(response_mode_to_scene) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "response_mode_to_scene"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = response_mode_to_scene)

r <- event_df %>%
  count(type_of_turn_around_delay) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "type_of_turn_around_delay"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = type_of_turn_around_delay)

s <- event_df %>%
  count(type_of_scene_delay) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "type_of_scene_delay"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = type_of_scene_delay)

t <- event_df %>%
  count(type_of_response_delay) %>%
  mutate(
    proportion = n / sum(n),
    table = "event_df",
    variable = "type_of_response_delay"
  ) %>%
  arrange(desc(n)) %>% 
  rename(value = type_of_response_delay)


# Time variables ----------------------------------------------------------

summary(time_df$time_resolve_issue)


# Patient variables -------------------------------------------------------
  
  # patient_age_years
  ggplot(patient_df, aes(x = patient_age_years)) + 
    geom_histogram(binwidth = 5, fill = "maroon", color = "white") + 
    scale_x_continuous(n.breaks = 10) +
    labs(
      title = "Patient Ages",
      x = "Years",
      y = "Count"
    ) +
    theme_light()
  
  summary(patient_df$patient_age_years)
  
  # patient_race
  u <- patient_df %>%
    count(patient_race) %>%
    mutate(
      proportion = n / sum(n),
      table = "patient_df",
      variable = "patient_race"
    ) %>%
    arrange(desc(n)) %>% 
    rename(value = patient_race)
  
  # age_group
  v <- patient_df %>%
    count(age_group) %>%
    mutate(
      proportion = n / sum(n),
      table = "patient_df",
      variable = "age_group"
    ) %>%
    arrange(desc(n)) %>% 
    rename(value = age_group)
  
  # age_interval_group
  w <- patient_df %>%
    count(age_interval_group) %>%
    mutate(
      proportion = n / sum(n),
      table = "patient_df",
      variable = "age_interval_group"
    ) %>%
    arrange(desc(n)) %>% 
    rename(value = age_interval_group)


# combine tables ----------------------------------------------------------

frequency_tables <- bind_rows(
  z, y, x, m, a, b, c, d, e, f, g, h, i, j, k, l, n, o, p, q, r, s, t, u, v, w
) %>% 
  select(table, variable, value, n, proportion)

write.csv(frequency_tables, "frequency_tables.csv", row.names = F)
