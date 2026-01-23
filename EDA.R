library(tidyverse)
library(ggplot2)
library(hms)
library(RColorBrewer)

display.brewer.all()

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

# After filtering, there are 656 observations. 


# count per dispatch reason faceted by age group --------------------------

top_10_dispatch_reasons <- c("Sick Person",
                             "Transfer/Interfacility/Palliative Care",
                             "Breathing Problem",
                             "Falls",
                             "No Other Appropriate Choice",
                             "Chest Pain (Non-Traumatic)",
                             "Traffic/Transportation Incident",
                             "Unconscious/Fainting/Near-Fainting",
                             "Unknown Problem/Person Down",
                             "Psychiatric Problem/Abnormal Behavior/Suicide Attempt")

reason_per_age_group_and_hour <- focus_data %>% 
  select(PcrKey, dispatch_reason, unit_notified_by_dispatch_datetime, age_group) %>% 
  filter(dispatch_reason %in% top_10_dispatch_reasons) %>% 
  mutate(unit_notified_time = as_hms(round_date(unit_notified_by_dispatch_datetime, "hour"))) %>% 
  group_by(unit_notified_time, dispatch_reason, age_group) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  arrange(desc(count))
  #   %>%
  # mutate(
  #   hour = hour(unit_notified_time),
  #   interval_per_8 = hour %/% 8,
  #   interval_per_8 = case_when(
  #     interval_per_8 == 0 ~ "00:00:00 - 07:59:00", #00:00:00 - 07:59:00
  #     interval_per_8 == 1 ~ "08:00:00 - 15:59:00", #08:00:00 - 15:59:00
  #     interval_per_8 == 2 ~ "16:00:00 - 23:59:00", #16:00:00 - 23:59:00
  #   )
  # )

ggplot(na.omit(reason_per_age_group_and_hour), aes(x = unit_notified_time, y = count, .group = dispatch_reason, fill = dispatch_reason)) + 
  geom_vline(xintercept = c(28800, 28800 * 2), linetype = "dashed", color = "black", linewidth = 1) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,15), n.breaks = 5) +
  facet_wrap(~age_group) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(
    title = "Top 10 Dispacth Reasons At Each Hour",
    subtitle = "418 observations",
    x = "Time",
    y = "Count",
    fill = "Dispacth Reason"
  ) +
  geom_text(
    aes(label = count),
    position = position_stack(vjust = .5),
    color = "white"
  )



# time resolved per dispatch reason faceted by age group ------------------

  # Top 10 dispatch reasons

  time_per_age_group_and_10_reasons <- focus_data %>% 
    select(PcrKey, dispatch_reason, age_group, time_resolve_issue) %>% 
    filter(dispatch_reason %in% top_10_dispatch_reasons) %>% 
    group_by(dispatch_reason, age_group) %>% 
    summarize(avg_time = mean(time_resolve_issue), .groups = "drop") %>% 
    mutate(avg_time = round(avg_time)) %>% 
    arrange(desc(avg_time))
  
  ggplot(na.omit(time_per_age_group_and_10_reasons), aes(x = dispatch_reason, y = avg_time, .group = dispatch_reason, fill = dispatch_reason)) + 
    geom_col(show.legend = F) + 
    scale_fill_brewer(palette = "Paired") +
    coord_flip() +
    facet_wrap(~age_group) +
    theme_light() +
    theme(legend.position = "bottom") +
    labs(
      title = "Average Time Per Per Top 10 Dispatch Reason",
      subtitle = "418 observations",
      x = NULL,
      y = "Minutes"
    ) +
    geom_text(
      aes(label = avg_time),
      hjust = 1.5,
      color = "white"
    )

  # ALL dispatch reasons

  time_per_age_group_and_all_reasons <- focus_data %>% 
    select(PcrKey, dispatch_reason, age_group, time_resolve_issue) %>% 
    #filter(dispatch_reason %in% top_10_dispatch_reasons) %>% 
    group_by(dispatch_reason, age_group) %>% 
    summarize(avg_time = mean(time_resolve_issue), .groups = "drop") %>% 
    mutate(avg_time = round(avg_time)) %>% 
    arrange(desc(avg_time))
  
  ggplot(na.omit(time_per_age_group_and_all_reasons), aes(x = dispatch_reason, y = avg_time, .group = dispatch_reason, fill = dispatch_reason)) + 
    geom_col(show.legend = F) + 
    #scale_fill_brewer(palette = "Paired") +
    coord_flip() +
    facet_wrap(~age_group) +
    theme_light() +
    theme(legend.position = "bottom") +
    labs(
      title = "Average Time Per Dispatch Reason",
      subtitle = "564 observations",
      x = NULL,
      y = "Minutes"
    ) +
    geom_text(
      aes(label = avg_time),
      hjust = 1.5,
      color = "white"
    )

# avg_time_by_reason_age <- focus_data %>%
#   filter(age_group %in% c("Senior", "Younger")) %>%
#   group_by(dispatch_reason, age_group) %>%
#   summarize(
#     avg_time_resolve = mean(time_resolve_issue),
#     .groups = "drop"
#   )
# 
# focus_data %>%
#   filter(
#     age_group == "Younger",
#     dispatch_reason == "Altered Mental Status"
#   ) %>%
#   summarize(avg_time_resolve = mean(time_resolve_issue, na.rm = TRUE)) %>%
#   pull()
