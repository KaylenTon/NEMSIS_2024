####ANOVA######
#install.packages("tidyverse") # nolint
#install.packages("ggplot2") # nolint
#install.packages("patchwork") # nolint
#install.packages("forecats") # nolint

library(tidyverse)
library(ggplot2)
library(patchwork)
library(forcats)
library(dplyr)

#import dataset
load("cleaningDataFileObjects.RData")
ls()
str(sas_data_list)
names(sas_data_list$computedelements)
View(sas_data_list$computedelements)

#create an age_interval dataset to work with
ems <- sas_data_list$computedelements

ems$age_interval_group <- cut(
  ems$ageinyear,
  breaks = c(-Inf, 30, 60, Inf),
  labels = c("Young", "Middle", "Old")
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
#                    differ between Younger, Middle, and
#                    Older age groups?
#
# Hypothesis testing: H0: The mean EMS total call time is equal
#                         across Young, Middle, and Old groups
#                     H1: The mean EMS total call time is not equal
#                         across Young, Middle, and Old groups
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

#End results interpretation: H0 is rejected
#ANOVA Numerical Data:
#F-value: 1020
#p-value: <2e-16
#Mean Sq between groups: 4332,
#meaning typical within-group variance
#is large enough for there to be stil
#have a lot of variability within the groups

#Tukey HSD post-hoc test results:
#Comparison: Middle vs Young
#Mean Difference: 1.32 minutes
#p adj: 8.8e-06
#Comparison: Old vs Young
#Mean Difference: 9.09 minutes
#p adj: 0.0
#Comparison: Old vs Middle
#Mean Difference: 7.77 minutes
#p adj: 0.0


#The one- way ANOVA revealed a statistically significant difference 
#in mean EMS total call time across age groups, F(2, 519,283) = 1020, p < .001.
#Post hoc Tukey comparisons indicated that the Old age group had significantly
#higher mean call times than both the Young and Middle groups, with differences
#of approximately 9 and 8 minutes, respectively. Although the Middle group also
#differed significantly from the Young group, the magnitude of this difference
#was small (approximately 1.3 min), suggesting limited practical significance.

#although this test explains through ANOVA that older age groups have higher
#EMS total call times, there needs to be further investigation into whether age
#is the true driving factor EMS total call time difference between age groups.
#The variance of 44332 minutes between groups is large enough not to ignore,
#and such needs to be tested:
#how much of the variation in EMS total call time is explained by age group?
#This will answer if age is the underlying factor driving the differences in
#EMS total call time between age groups, or if there are other factors at
#play that are not being accounted for in this analysis.
