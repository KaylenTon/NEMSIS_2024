library(tidyverse)
library(ggplot2)
library(hms)
library(RColorBrewer)
library(scales)

display.brewer.all()

# Source - https://stackoverflow.com/a/57157075
# Posted by Tung
# Retrieved 2026-02-06, License - CC BY-SA 4.0

palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


# RQ: Is there a significant difference between the younger and older age groups regarding:
#       1. dispatch_reason
#       2. EMSTotalCallTimeMin

# Getting the data --------------------------------------------------------

from_event <- event_df %>% 
  select(PcrKey, dispatch_reason, EMD_performed, level_of_care_provided_per_protocol, transport_mode_from_scene, response_mode_to_scene, type_of_service_requested, unit_transport_and_equipment_capability)

from_time <- time_df %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime, EMSTotalCallTimeMin)

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
                             "Chest Pain (Non-Traumatic)",
                             "Traffic/Transportation Incident",
                             "Unconscious/Fainting/Near-Fainting",
                             "Unknown Problem/Person Down",
                             "Psychiatric Problem/Abnormal Behavior/Suicide Attempt",
                             "Convulsions/Seizure")

reason_per_age_group_and_hour <- focus_data %>% 
  select(PcrKey, dispatch_reason, unit_notified_by_dispatch_datetime, age_group) %>% 
  filter(dispatch_reason %in% top_10_dispatch_reasons) %>% 
  mutate(unit_notified_time = as_hms(round_date(unit_notified_by_dispatch_datetime, "hour"))) %>% 
  group_by(unit_notified_time, dispatch_reason, age_group) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  group_by(unit_notified_time, age_group) %>% 
  mutate(prop = count / sum(count)) %>% 
  ungroup()
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

# # Stacked bar chart - postion = "stack"
# ggplot(na.omit(reason_per_age_group_and_hour), 
#        aes(x = unit_notified_time, 
#            y = count, 
#            .group = dispatch_reason, 
#            fill = dispatch_reason)) + 
#   geom_vline(xintercept = c(28800, 28800 * 2), linetype = "dashed", color = "black", linewidth = 1) +
#   geom_col(position = "stack") +
#   scale_fill_manual(values = palette) +
#   scale_y_continuous(labels = percent) +
#   facet_wrap(~age_group) +
#   theme_light() +
#   theme(legend.position = "bottom",
#         strip.background = element_rect(
#           fill = "black"
#         )) +
#   labs(
#     title = "Top 10 Dispacth Reasons At Each Hour",
#     x = "Time",
#     y = "Count",
#     fill = NULL
#   ) +
#   geom_text(
#     aes(label = count),
#     position = position_stack(vjust = .5),
#     color = "white"
#   )

# Proportional - position = "fill"
ggplot(reason_per_age_group_and_hour, 
       aes(x = unit_notified_time, 
           y = prop, 
           fill = dispatch_reason)) + 
  geom_vline(xintercept = c(28800, 28800 * 2),
             linetype = "dashed",
             color = "black",
             linewidth = 1) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = palette) +
  facet_wrap(~age_group, ncol = 2) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(
    fill = "black"),
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    strip.text = element_text(size = 50, face = "bold", color = "white"),
    legend.text  = element_text(size = 11),
    legend.title = element_text(size = 12)
  ) +
  labs(
    title = "Proportion of Top 10 Dispatch Reasons by Hour",
    x = "Time",
    y = "Proportion",
    fill = NULL
  ) +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white"
  )


# time resolved per dispatch reason faceted by age group ------------------

  # Time series of avg_time per dispatch reason
  
  time_per_age_group_and_10_reasons_through_time <- focus_data %>% 
    select(PcrKey, dispatch_reason, age_group, unit_notified_by_dispatch_datetime, EMSTotalCallTimeMin) %>% 
    filter(dispatch_reason %in% top_10_dispatch_reasons) %>% 
    mutate(unit_notified_time = as_hms(round_date(unit_notified_by_dispatch_datetime, "hour"))) %>% 
    group_by(unit_notified_time, dispatch_reason, age_group) %>% 
    summarize(avg_time = mean(EMSTotalCallTimeMin, na.rm = TRUE), .groups = "drop") %>% 
    mutate(avg_time = round(avg_time)) %>% 
    arrange(desc(avg_time))
  
  ggplot(na.omit(time_per_age_group_and_10_reasons_through_time), 
         aes(x = unit_notified_time, 
             y = avg_time, 
             .group = dispatch_reason, 
             color = dispatch_reason)) + 
    geom_line(linewidth = 1.25, alpha = .5) +
    geom_point(size = 2) +
    facet_wrap(~age_group) +
    scale_color_manual(values = palette) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(
        fill = "black"),
      plot.title = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12),
      strip.text = element_text(size = 50, face = "bold", color = "white"),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12)
    ) +
    geom_vline(xintercept = c(28800, 28800 * 2), linetype = "dashed") +
    labs(
      title = "The Average Time to Conclude PCR per Top 10 Dispatch Reason Throughout the Day",
      x = "Time",
      y = "Minutes",
      color = NULL
    ) +
    geom_text(
      aes(label = avg_time),
      vjust = -1,
      size = 4
    )
  
    # Includes an extreme outlier. PCR # 310894504 (senior and sick @ 21:00:00) lasted 25 hours.
  
  # avg_time per dispatch reason BOX AND WHISKER PLOTS
  
  time_per_dispatch_boxplot <- focus_data %>% 
    select(PcrKey, dispatch_reason, age_group, EMSTotalCallTimeMin) %>% 
    filter(dispatch_reason %in% top_10_dispatch_reasons) %>%
    group_by(dispatch_reason)
  
  time_per_dispatch_boxplot  %>% 
    summarise(
      n = n(),
      Mean = mean(EMSTotalCallTimeMin, na.rm = TRUE),
      Median = median(EMSTotalCallTimeMin, na.rm = TRUE),
      Sd = sd(EMSTotalCallTimeMin, na.rm = TRUE),
      Q1 = quantile(EMSTotalCallTimeMin, 0.25, na.rm = TRUE),
      Q3 = quantile(EMSTotalCallTimeMin, 0.75, na.rm = TRUE),
      Min = min(EMSTotalCallTimeMin, na.rm = TRUE),
      Max = max(EMSTotalCallTimeMin, na.rm = TRUE)
    ) %>%
    arrange(desc(Median)) %>% 
    print(n = Inf)
  
  ggplot(na.omit(time_per_dispatch_boxplot), aes(x = dispatch_reason, y = EMSTotalCallTimeMin, fill = age_group)) +
    geom_violin() +
    scale_fill_manual(values = palette) +
    coord_cartesian(ylim = c(0, 300)) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 250), n.breaks = 15) +
    labs(
      title = "Box & Whisker Plot : Time Per Dispatch Reason",
      subtitle = "Some events were took 'negative minutes' -> NA & removed \nMinute values go up to 1433",
      x = NULL,
      y = "Minutes",
      fill = "Age group"
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(
        fill = "black"),
      plot.title = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12),
      strip.text = element_text(size = 50, face = "bold", color = "white"),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12)
    )

# statistical tests -------------------------------------------------------

t.test(data = na.omit(focus_data), EMSTotalCallTimeMin ~ age_group) 
  # Need to decide what to do with outliers then decide additional arguments of t-tests.
  
senior <- focus_data %>% 
  filter(age_group == "Senior")

summary(senior$EMSTotalCallTimeMin)
ggplot(senior, aes(x = EMSTotalCallTimeMin)) + geom_histogram()

younger <- focus_data %>% 
  filter(age_group == "Younger")

summary(younger$EMSTotalCallTimeMin)
ggplot(younger, aes(x = EMSTotalCallTimeMin)) + geom_histogram()

# do anova for other 2+ categorical groupings
