library(tidyverse)
library(ggplot2)
library(lubridate)
library(hms)
library(scales)

# Source - https://stackoverflow.com/a/57157075
# Posted by Tung
# Retrieved 2026-02-06, License - CC BY-SA 4.0

palette_12 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

custom_theme <- theme(
  text = element_text(family = "Sans"),
  #rect = element_blank(),
  plot.title = element_text(size = 30, face = "bold"),
  # axis
  axis.title.x = element_text(size = 24, face = "bold", margin = margin(t = 30, b = 15)),
  axis.title.y = element_text(size = 24, face = "bold"),
  axis.text.x  = element_text(size = 20),
  axis.text.y  = element_text(size = 20),
  axis.ticks.length = unit(3, "mm"),
  axis.ticks = element_line(linewidth = 1),
  # legend
  legend.position = "bottom",
  legend.text  = element_text(size = 20),
  legend.title = element_text(size = 22),
  legend.key.width  = unit(10, "mm"),
  legend.key.height = unit(10, "mm"),
  strip.background = element_rect(fill = "black"),
  strip.text = element_text(size = 24, face = "bold", color = "white"),
  # margin: (top, right, bottom, left, unit)
  plot.margin = margin(20, 30, 15, 10, "mm")
)

# interest variables ------------------------------------------------------

from_location <- location_df

from_event <- event_df %>% 
  select(PcrKey, dispatch_reason, EMD_performed, level_of_care_provided_per_protocol, transport_mode_from_scene, response_mode_to_scene, type_of_service_requested, unit_transport_and_equipment_capability)

from_time <- time_df %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime, EMSTotalCallTimeMin)

from_patient <- patient_df %>% 
  select(-patient_race)

focus_data <- location_df %>% 
  left_join(from_event, by = "PcrKey") %>% 
  left_join(from_time, by = "PcrKey") %>% 
  left_join(from_patient, by = "PcrKey") 
  # %>%
  # filter(
  #   type_of_service_requested == "Emergency Response (Primary Response Area)",
  #   response_mode_to_scene == "Emergent (Immediate Response)"
  # ) %>%
  # select(-c(type_of_service_requested, response_mode_to_scene))

# total calls per hour ----------------------------------------------------

calls_per_hour <- focus_data %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime) %>% 
  mutate(unit_notified_hour = round_date(unit_notified_by_dispatch_datetime, "hour")) %>% 
  mutate(unit_notified_hour = as_hms(unit_notified_hour)) %>% 
  group_by(unit_notified_hour) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(
    hour = hour(unit_notified_hour),
    interval_per_8 = hour %/% 8,
    interval_per_8 = case_when(
      interval_per_8 == 0 ~ "00:00:00 - 07:59:00", #00:00:00 - 07:59:00
      interval_per_8 == 1 ~ "08:00:00 - 15:59:00", #08:00:00 - 15:59:00
      interval_per_8 == 2 ~ "16:00:00 - 23:59:00", #16:00:00 - 23:59:00
    )
  )

ggplot(calls_per_hour, aes(x = unit_notified_hour, y = n, color = interval_per_8)) + 
  geom_line(linewidth = 1, linetype = "dashed") + 
  geom_point(size = 2) +
  # scale_y_continuous(limits = c(0,125)) +
  labs(
    title = "Calls per interval using:  unit_notified_by_dispatch_datetime",
    subtitle = "Count Time Series",
    x = "Time", 
    y = "Call Count",
    color = "Time interval"
  ) +
  theme_light() +
  geom_text(
    aes(label = n),
    vjust = -1,
    size = 4,
    color = "black"
  ) +
  custom_theme

# year count time series --------------------------------------------------

year_events <- focus_data %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime) %>% 
  mutate(
    unit_notified_by_dispatch_datetime = date(unit_notified_by_dispatch_datetime)
  ) %>% 
  group_by(unit_notified_by_dispatch_datetime) %>% 
  count()

ggplot(year_events, aes(x = unit_notified_by_dispatch_datetime, y = n)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Count of EMS Dispatches Throughout 2024",
    x = NULL,
    y = "Count"
  ) +
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
  )


# Fill bar chart - top 10 dispatch reasons by hour ------------------------

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
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = palette_12) +
  facet_wrap(~age_group, ncol = 2) +
  theme_classic() +
  custom_theme +
  labs(
    title = "Proportion of Top 10 Dispatch Reasons by Hour",
    x = "Time",
    y = NULL,
    fill = NULL
  ) +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white"
  )
