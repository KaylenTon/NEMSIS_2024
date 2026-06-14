####ANOVA######
#install.packages("tidyverse") # nolint
#install.packages("ggplot2") # nolint

library(tidyverse)
library(ggplot2)
library(dplyr)

#import dataset
load("cleaningDataFileObjects.RData")
ls()
str(sas_data_list)
names(sas_data_list$computedelements)
View(sas_data_list$computedelements)
summary(sas_data_list$computedelements$ageinyear)

#create an age_interval dataset to work with
ems <- sas_data_list$computedelements

ems$age_interval_group <- cut(
  ems$ageinyear,
  breaks = c(0, 20, 40, 60, 80, 100, 120),
  labels = c("0-20", "21-40", "41-60", "61-80", "81-100", "101-120")
  )
head(ems)
#remove NA values
ems_clean <- subset(ems,
                    !is.na(age_interval_group) &
                    !is.na(EMSTotalCallTimeMin))

#Take a look at the distribution of means
ems_clean %>%
    group_by(age_interval_group) %>%
    summarize(mean_call_time = mean(EMSTotalCallTimeMin, na.rm = TRUE)) %>%
    arrange(mean_call_time)

# Research question: Does the mean EMS total call time
#                    differ between age groups?
#
# Hypothesis testing: H0: The mean EMS total call time is equal
#                         across age groups
#                     H1: The mean EMS total call time is not equal
#                         across age groups
# Observation: Differences in mean EMS total call time are observed across
#              age groups in the sample data. The question is whether these
#              differences are statistically significant at the
#              0.05 significance level.

# Create ANOVA model
aov_model <- aov(EMSTotalCallTimeMin ~ age_interval_group, data = ems_clean)
summary(aov_model)

#Is this significantly being driven by a particular EMS call time? 
TukeyHSD(aov_model) %>%
  plot()
tukey_results <- TukeyHSD(aov_model)
print(tukey_results)

#plotting the data: boxplot with means overlaid
ggplot(ems_clean, aes(x = age_interval_group, 
                      y = EMSTotalCallTimeMin, 
                      fill = age_interval_group)) +
  geom_boxplot(outlier.alpha = 0.2) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 3,
               color = "#4195f4") +
  coord_cartesian(ylim = c(0, 200)) +
  labs(title = "EMS Total Call Time by Age Interval Group",
       x = "Age Interval Group",
       y = "EMS Total Call Time (Minutes)") +
  theme_minimal()

#plotting the data: smooth plot with means and confidence intervals
ggplot(ems_clean, aes(x = ageinyear,
                      y = EMSTotalCallTimeMin, 
                      fill = age_interval_group)) +
  geom_smooth(se = TRUE, color = "#2C7FB8", fill = "#A6CEE3") +
  labs(title = "Trend of EMS Total Call Time by Age",
       x = "Age (Years)",
       y = "EMS Total Call Time (Minutes)") +
  theme_minimal()


# ------Summary of Results------
#ANOVA test was statistically significant, (F-value=617.9, p < 0.001). 
#Tukey post-hoc tests identified age groups (61-80, 81-100) have significantly 
#higher call times compared to younger groups. 
#The age group (101-120) shows large shading signalling inconsistency, likely due
#to small sample size. 
#Overall, EMS call time increases with age, peaking around 70 years of age, then
#declining slightly. 

