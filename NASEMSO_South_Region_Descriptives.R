library(dplyr)
library(ggplot2)
library(forcats)
library(scales)


data <- read.csv("C:/Users/ntole/Downloads/final_clean_NA.csv")
names(data)
View(data)

south_data <- data[data$NasemsoRegion == "South", ]

south_age <- south_data %>%
  filter(!is.na(age_group))

table(south_age$age_group, useNA = "ifany")

# get top 10 dispatch reasons overall
top_dispatch <- south_age %>%
  count(dispatch_reason, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(dispatch_reason)

# top 10 dispatch reasons percent table
dispatch_plot_data <- south_age %>%
  filter(dispatch_reason %in% top_dispatch) %>%
  count(age_group, dispatch_reason) %>%
  group_by(age_group) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

# Visualize dispatch reason by age group
dispatch_plot <- ggplot(dispatch_plot_data,
                        aes(x = fct_reorder(dispatch_reason, percent),
                            y = percent,
                            fill = age_group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top Dispatch Reasons by Age Group, NASEMSO South",
    x = "Dispatch Reason",
    y = "Percent within Age Group",
    fill = "Age Group"
  ) +
  theme_minimal()

dispatch_plot

# Initial acuity percent table
initial_acuity_plot_data <- south_age %>%
  filter(!is.na(initial_patient_acuity)) %>%
  count(age_group, initial_patient_acuity) %>%
  group_by(age_group) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

# Visualize initial acuity by age group
initial_acuity_plot <- ggplot(initial_acuity_plot_data,
                              aes(x = initial_patient_acuity,
                                  y = percent,
                                  fill = age_group)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Initial Patient Acuity by Age Group, NASEMSO South",
    x = "Initial Patient Acuity",
    y = "Percent within Age Group",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

initial_acuity_plot

# final acuity percent table
final_acuity_plot_data <- south_age %>%
  filter(!is.na(acuity_upon_EMS_release_of_patient)) %>%
  count(age_group, acuity_upon_EMS_release_of_patient) %>%
  group_by(age_group) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

# visualize final acuity by age group
final_acuity_plot <- ggplot(final_acuity_plot_data,
                            aes(x = acuity_upon_EMS_release_of_patient,
                                y = percent,
                                fill = age_group)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Acuity Upon EMS Release by Age Group, NASEMSO South",
    x = "Acuity Upon EMS Release",
    y = "Percent within Age Group",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

final_acuity_plot

# level of care percent table
care_plot_data <- south_age %>%
  filter(!is.na(level_of_care_provided_per_protocol)) %>%
  count(age_group, level_of_care_provided_per_protocol) %>%
  group_by(age_group) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

# Visualize level of care by age group
care_plot <- ggplot(care_plot_data,
                    aes(x = level_of_care_provided_per_protocol,
                        y = percent,
                        fill = age_group)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Level of Care Provided by Age Group, NASEMSO South",
    x = "Level of Care Provided per Protocol",
    y = "Percent within Age Group",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

care_plot