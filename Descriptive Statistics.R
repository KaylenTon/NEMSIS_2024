library(tidyverse)
library(ggplot2)

# Event variables ----------------------------------------------------------------

  # dispatch_reason
  dispatch_reason_table <- event_df %>%
    count(dispatch_reason) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))

  ggplot(dispatch_reason_table, aes(x = reorder(dispatch_reason, n), y = n, fill = dispatch_reason)) + 
    geom_col(show.legend = F) + 
    coord_flip() +
    labs(
      title = "Dispatch Reason",
      x = NULL,
      y = "Count"
    ) +
    geom_text(
      aes(label = n),
      hjust = -.35,
      size = 4,
      color = "black"
    )
  
  # EMD_performed
  event_df %>%
    count(EMD_performed) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # acuity_upon_EMS_release_of_patient
  event_df %>%
    count(acuity_upon_EMS_release_of_patient) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # EMS_transport_method
  event_df %>%
    count(EMS_transport_method) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # type_of_destination
  event_df %>%
    count(type_of_destination) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # hospital_in_patient_destination
  event_df %>%
    count(hospital_in_patient_destination) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # level_of_care_provided_per_protocol
  event_df %>%
    count(level_of_care_provided_per_protocol) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # unit_disposition
  event_df %>%
    count(unit_disposition) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # patient_evaluation_care
  event_df %>%
    count(patient_evaluation_care) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # crew_disposition
  event_df %>%
    count(crew_disposition) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # transport_disposition
  event_df %>%
    count(transport_disposition) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # transport_mode_from_scene
  event_df %>%
    count(transport_mode_from_scene) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # # datetime_of_destination_prearrival_alert_or_activation
  # event_df %>%
  #   count(datetime_of_destination_prearrival_alert_or_activation) %>%
  #   mutate(proportion = n / sum(n)) %>% 
  #   arrange(desc(proportion)) %>%
  #   print(n = nrow(.))
  
  # # desination_team_prearrival_alert_or_activation
  # event_df %>%
  #   count(desination_team_prearrival_alert_or_activation) %>%
  #   mutate(proportion = n / sum(n)) %>% 
  #   arrange(desc(proportion)) %>%
  #   print(n = nrow(.))
  
  # type_of_service_requested
  event_df %>%
    count(type_of_service_requested) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # unit_transport_and_equipment_capability
  event_df %>%
    count(unit_transport_and_equipment_capability) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # response_mode_to_scene
  event_df %>%
    count(response_mode_to_scene) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # type_of_turn_around_delay
  event_df %>%
    count(type_of_turn_around_delay) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # type_of_scene_delay
  event_df %>%
    count(type_of_scene_delay) %>%
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))
  
  # # type_of_response_delay
  # event_df %>%
  #   count(type_of_response_delay) %>%
  #   mutate(proportion = n / sum(n)) %>%
  #   arrange(desc(proportion)) %>%
  #   print(n = nrow(.))

# Patient variables -------------------------------------------------------
  
  # patient_age_years
  ggplot(patient_df, aes(x = patient_age_years)) + 
    geom_histogram(binwidth = 5, fill = "maroon", color = "white") + 
    scale_x_continuous(n.breaks = 10) +
    labs(
      title = "Patient Ages",
      x = "Years",
      y = NULL
    ) +
    theme_light()
  
  summary(patient_df$patient_age_years)
  
  # patient_race
  patient_df %>% 
      count(patient_race) %>% 
      mutate(proportion = n / sum(n)) %>% 
      arrange(desc(proportion)) %>% 
      print(n = nrow(.))
  
  # age_group
  patient_df %>% 
      count(age_group) %>% 
      mutate(proportion = n / sum(n)) %>% 
      arrange(desc(proportion)) %>% 
      print(n = nrow(.))
  
  # age_decade_group
  patient_df %>% 
    count(age_decade_group) %>% 
    mutate(proportion = n / sum(n)) %>% 
    arrange(desc(proportion)) %>% 
    print(n = nrow(.))  