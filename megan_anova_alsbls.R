library(ggplot2)
library(car)
library(dplyr)
library(report)
library(multcomp)
table(final_clean_NA$level_of_care_provided_per_protocol)


#t-test for significant difference between level_of_care_provided using age as the continuous variable
#type of service: eDisposition.32

#variable type
#Quantitative variable: patient_age
#Qualitative variable (groups):
# ALS - AEMT/Intermediate
# ALS - Paramedic 
# BLS - All Levels

sub_df <- subset(final_clean_NA, select = c(level_of_care_provided_per_protocol, patient_age))
groups <- c("ALS - AEMT/Intermediate","ALS - Paramedic","BLS - All Levels")
sub_df_a <- sub_df[sub_df$level_of_care_provided_per_protocol %in% groups,]
sub_df_a$level_of_care_provided_per_protocol <- as.factor(sub_df_a$level_of_care_provided_per_protocol)
str(sub_df_a)

ggplot(sub_df_a) +
  aes(x = level_of_care_provided_per_protocol,
      y = patient_age,
      color = level_of_care_provided_per_protocol) +
  geom_boxplot() +
  theme(legend.position = "none")


#independence: we used random sampling to collect the observations, and none are duplicates. Thus, we assume independence.

#normality
ggplot(sub_df_a) +
  aes(x=patient_age, fill = level_of_care_provided_per_protocol) +
  geom_histogram(color = "blue", position = "identity", alpha = 0.6)

#q-q plot
set.seed(0)
qqnorm(sub_df_a$patient_age, main="Patient Age")

#checking for NA
sum(is.na(sub_df_a$level_of_care_provided_per_protocol))
sum(is.na(sub_df_a$patient_age))

#summary statistics
group_by(sub_df_a, level_of_care_provided_per_protocol) %>%
  summarise(
    mean = mean(patient_age, na.rm = TRUE),
    sd = sd(patient_age, na.rm = TRUE)
  )

#ANOVA tests
oneway.test(patient_age ~ level_of_care_provided_per_protocol,
            data = sub_df_a,
            var.equal = TRUE)

res_aov <- aov(patient_age ~ level_of_care_provided_per_protocol,
               data = sub_df_a)

summary(res_aov)

#results
report(res_aov)

##TEST ALS combined VS BLS
post_test <- glht(res_aov,
                  linfct = mcp(level_of_care_provided_per_protocol = "Tukey"))
summary(post_test)

#since ALS - Paramedic and ALS - AEMT/Intermediate don't significantly differ in terms of age, we can try grouping them together

#Part 2: T-test
sub_df_t <- sub_df_a
sub_df_t$level_of_care_provided_per_protocol <- gsub(".*ALS.*","ALS",sub_df_t$level_of_care_provided_per_protocol)
sub_df_t$level_of_care_provided_per_protocol <- as.factor(sub_df_t$level_of_care_provided_per_protocol)
str(sub_df_t)

ggplot(sub_df_t, aes(x = level_of_care_provided_per_protocol, y = patient_age, fill = level_of_care_provided_per_protocol)) +
  geom_boxplot()

set.seed(0)
t_result <- t.test(patient_age ~ level_of_care_provided_per_protocol,
       data = sub_df_t,
       var.equal = FALSE)
t_result

report(t_result)



