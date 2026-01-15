library(tidyverse)
library(ggplot2)

merged_interest_variables <- event_df[c(1:3, 8)] %>% 
  left_join(time_df[c(1, 11)], by = "PcrKey") %>% 
  left_join(patient_df, by = "PcrKey")

average_time_per_dispatch_reason <- merged_interest_variables %>% 
  group_by(dispatch_reason) %>% 
  summarise(avg_time_resolve = mean(time_resolve_issue)) %>% 
  arrange(desc(avg_time_resolve))

ggplot(average_time_per_dispatch_reason, aes(x = reorder(dispatch_reason, avg_time_resolve), y = avg_time_resolve)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(n.breaks = 15) +
  labs(
    title = "Average Time per Dispatch Reason",
    x = "Reason",
    y = "Minutes"
  )

average_time_per_level_of_care <- merged_interest_variables %>% 
  group_by(level_of_care_provided_per_protocol) %>% 
  summarise(avg_time_resolve = mean(time_resolve_issue)) %>% 
  arrange(desc(avg_time_resolve))

ggplot(na.omit(average_time_per_level_of_care), aes(x = reorder(level_of_care_provided_per_protocol, avg_time_resolve), y = avg_time_resolve)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(n.breaks = 15) +
  labs(
    title = "Average Time per Level of Care",
    x = "Level of Care",
    y = "Minutes"
  )

# EMD_by_age_group <- merged_interest_variables %>% 
#   group_by(EMD_performed, age_group)