library(tidyverse)
library(ggplot2)
library(hms)

# unique minute
calls_per_interval_unique_minute <- time_df %>%
  select(PcrKey, unit_notified_by_dispatch_datetime) %>%
  mutate(unit_notified_time = as_hms(unit_notified_by_dispatch_datetime)) %>%
  group_by(unit_notified_time) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(
    hour = hour(unit_notified_time),
    interval_per_8 = hour %/% 8,
    interval_per_8 = case_when(
      interval_per_8 == 0 ~ "A", #00:00:00 - 07:59:00
      interval_per_8 == 1 ~ "B", #08:00:00 - 15:59:00
      interval_per_8 == 2 ~ "C", #16:00:00 - 23:59:00
    )
  )

# calls_per_interval_unique_minute %>%
#   group_by(interval_per_8) %>%
#   summarise(sum = sum(n))
# 
# calls_per_interval_unique_minute %>%
#   filter(interval_per_8 == "A") %>%
#   group_by(interval_per_8) %>%
#   summarise(total_calls = sum(n))

ggplot(calls_per_interval_unique_minute, aes(x = unit_notified_time, y = n, color = interval_per_8)) + geom_line() + scale_y_continuous(limits = c(0, 5))

# unique hour
calls_per_interval_unique_hour <- time_df %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime) %>% 
  mutate(unit_notified_hour = round_date(unit_notified_by_dispatch_datetime, "hour")) %>% 
  mutate(unit_notified_time = as_hms(unit_notified_hour)) %>% 
  group_by(unit_notified_time) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(
    hour = hour(unit_notified_time),
    interval_per_8 = hour %/% 8,
    interval_per_8 = case_when(
      interval_per_8 == 0 ~ "00:00:00 - 07:59:00", #00:00:00 - 07:59:00
      interval_per_8 == 1 ~ "08:00:00 - 15:59:00", #08:00:00 - 15:59:00
      interval_per_8 == 2 ~ "16:00:00 - 23:59:00", #16:00:00 - 23:59:00
    )
  )

ggplot(calls_per_interval_unique_hour, aes(x = unit_notified_time, y = n, color = interval_per_8)) + 
  geom_line(size = .5, linetype = "dashed") + 
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,125)) +
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
  )

# calls_per_interval_unique_hour %>%
#   group_by(interval_per_8) %>%
#   summarise(sum = sum(n))




# Visualizing Calls Per Interval with Age Groups --------------------------

# Faceting by age_group [NA IS OMITED]
calls_per_interval_variables <- time_df %>% 
  left_join(patient_df, by = "PcrKey") %>% 
  select(PcrKey, unit_notified_by_dispatch_datetime, age_group) %>% 
  mutate(unit_notified_hour = round_date(unit_notified_by_dispatch_datetime, "hour")) %>% 
  mutate(unit_notified_time = as_hms(unit_notified_hour)) %>% 
  mutate(
    hour = hour(unit_notified_time),
    interval_per_8 = hour %/% 8,
    interval_per_8 = case_when(
      interval_per_8 == 0 ~ "00:00:00 - 07:59:00", #00:00:00 - 07:59:00
      interval_per_8 == 1 ~ "08:00:00 - 15:59:00", #08:00:00 - 15:59:00
      interval_per_8 == 2 ~ "16:00:00 - 23:59:00", #16:00:00 - 23:59:00
    )
  ) %>% 
  group_by(unit_notified_time, age_group) %>% 
  mutate(count = n())

ggplot(na.omit(calls_per_interval_variables), aes(x = unit_notified_time, y = count, color = interval_per_8)) + 
  geom_line(size = .5, linetype = "dashed") + 
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,125)) +
  facet_wrap(~as.factor(age_group)) + # Should stack charts in rows instead of having columns tbh.
  labs(
    title = "Calls per interval using:  unit_notified_by_dispatch_datetime",
    subtitle = "Count Time Series",
    x = "Time", 
    y = "Call Count",
    color = "Time interval"
  ) +
  theme_bw() +
  geom_text(
    aes(label = count),
    vjust = -1,
    size = 4,
    color = "black"
  )

# Multiple line graph using .group = age_group [NA IS OMITED]
ggplot(na.omit(calls_per_interval_variables), aes(x = unit_notified_time, y = count, .group = age_group, color = age_group)) + 
  geom_line(size = 1, linetype = "dashed") + 
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,70)) +
  labs(
    title = "Calls per interval using:  unit_notified_by_dispatch_datetime",
    subtitle = "Count Time Series",
    x = "Time", 
    y = "Call Count",
    color = "Age Groups"
  ) +
  theme_bw() +
  geom_text(
    aes(label = count, 
        color = age_group),
    vjust = -1,
    size = 5
  ) +
  geom_vline(xintercept = c(28800, 28800 * 2), linetype = "dashed")
