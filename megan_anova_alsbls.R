library(ggplot2)
library(car)
library(dplyr)
library(tidyr)
library(report)
library(multcomp)
library(MASS)
library(mice)
library(mitml)
table(final_clean_NA$level_of_care_provided_per_protocol)

#anova for significant difference between level_of_care_provided using age as the continuous variable
#type of service: eDisposition.32
#about: the general level of care provided to this patient as defined per provider level in local EMS protocols or clinical guidelines

# NULL VALUES - this was generated after collapsing together the duplicated pcrkeys
# 4277 
# ALS - AEMT/Intermediate 
# 13172 
# ALS - Paramedic 
# 193090 
# BLS - All Levels 
# 155712 
# Critical Care 
# 6731 
# EMS and Other Health-Care Staff 
# 745 
# Integrated Health Care 
# 867 
# No Care Provided 
# 11511

#group null values to NA values
final_clean_NA <- final_clean_NA %>%
  mutate(level_of_care_provided_per_protocol = na_if(level_of_care_provided_per_protocol, ""))

table(final_clean_NA$level_of_care_provided_per_protocol)

#check for total NA
table(final_clean_NA$level_of_care_provided_per_protocol, useNA = "ifany")

#variable type
#Quantitative variable: patient_age
#Qualitative variable (groups): level of care provided


sub_df <- subset(final_clean_NA, select = c(level_of_care_provided_per_protocol, patient_age))
#groups <- c("ALS - AEMT/Intermediate","ALS - Paramedic","BLS - All Levels")
#sub_df_a <- sub_df[sub_df$level_of_care_provided_per_protocol %in% groups,]
str(sub_df)


#range of patient age
hist(sub_df$patient_age)
summary(sub_df$patient_age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   41.00   63.00   58.41   77.00  120.00 

#The mean is less than the median, meaning the data is slightly skewed to the left
# is this an issue? Should data be normalized before performing an ANOVA test?

#each group of life support follows similar distributions in terms of age

#imputing missing data
sum(is.na(sub_df$level_of_care_provided_per_protocol))/nrow(sub_df) #26% of data is missing
sum(is.na(sub_df$patient_age))/nrow(sub_df)

#replace NA values with "Missing"
sub_df <- sub_df %>%
  mutate(level_of_care_provided_per_protocol = ifelse(is.na(level_of_care_provided_per_protocol),
                                                      "Unknown",
                                                      level_of_care_provided_per_protocol))

table(sub_df$level_of_care_provided_per_protocol)
sub_df$level_of_care_provided_per_protocol <- as.factor(sub_df$level_of_care_provided_per_protocol)
str(sub_df)

# boxplot
ggplot(sub_df) +
  aes(x = level_of_care_provided_per_protocol,
      y = patient_age,
      color = level_of_care_provided_per_protocol) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Boxplot of Patient Age by Level of Care Provided",
       y = "Patient Age",
       x = "Level of Care Provided Per Protocol") +
  theme_minimal()

#range of patient age by type of life support
ggplot(sub_df) +
  aes(x=patient_age, fill = level_of_care_provided_per_protocol) +
  geom_histogram(color = "blue", position = "identity", alpha = 0.6) + 
  labs(title = "Histogram of Patient Age by Level of Care Provided",
       x = "Patient Age",
       y = "Count",
       fill = "Level of Care Provided Per Protocol") +
  theme_minimal()

#summary statistics
group_by(sub_df, level_of_care_provided_per_protocol) %>%
  summarise(
    mean = mean(patient_age, na.rm = TRUE),
    sd = sd(patient_age, na.rm = TRUE),
    median = median(patient_age, na.rm = TRUE)
  )

# because the patient_age data is slightly skewed, we use the levene test to make sure the variability within each group are consistent with each other
leveneTest(patient_age ~ level_of_care_provided_per_protocol, data = sub_df)
#H0: population variances are equal
#H1: at least two population variances differ
# p-value is less than 0.001 (***), so we reject the null hypothesis

fligner.test(patient_age ~ level_of_care_provided_per_protocol, data = sub_df)

#independence: we used random sampling to collect the observations, and none are duplicates. Thus, we assume independence.

#q-q plot
set.seed(0)
qqnorm(sub_df$patient_age, main="Patient Age")
#helps us test if the data came from a normal theoretical distribution
# plots two quantiles against each other (the real distribution vs normal distribution)

# not exactly a straight 45-degree line, its a little skewed

# since the distribution is not normal, we use a Kruskal-Wallis test (non-parametric) which compares medians rather than means
kruskal.test(patient_age ~ level_of_care_provided_per_protocol, data = sub_df)

#ANOVA welch tests
res_welch <- oneway.test(patient_age ~ level_of_care_provided_per_protocol,
            data = sub_df,
            var.equal = TRUE)

res_aov <- aov(patient_age ~ level_of_care_provided_per_protocol,
               data = sub_df)

res_welch
summary(res_aov)


#results
report(res_aov)


## end anova test ##


##difference signifiance between variables
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

#chi square test for comparing homogeneity between ALS/BLS and patient race
subc_df <- subset(clean_NA, select = c(level_of_care_provided_per_protocol, patient_race))
groups <- c("ALS - AEMT/Intermediate","ALS - Paramedic","BLS - All Levels")
subc_df <- subc_df[subc_df$level_of_care_provided_per_protocol %in% groups,]
subc_df$level_of_care_provided_per_protocol <- as.factor(subc_df$level_of_care_provided_per_protocol)
str(subc_df)

subc_df_counts <- subc_df |>
  count(level_of_care_provided_per_protocol, patient_race)

ggplot(subc_df_counts, aes(x=level_of_care_provided_per_protocol,y=n, fill=patient_race)) +
  geom_bar(position="dodge", stat="identity")

contingency_table <- table(subc_df$level_of_care_provided_per_protocol, subc_df$patient_race)

print(contingency_table)

chi_sq_test <- chisq.test(contingency_table)

print(chi_sq_test)

#removing NA values
subc_df_na <- na.omit(subc_df)

subc_df_counts_na <- subc_df_na |>
  count(level_of_care_provided_per_protocol, patient_race)

ggplot(subc_df_counts_na, aes(x=level_of_care_provided_per_protocol,y=n, fill=patient_race)) +
  geom_bar(position="dodge", stat="identity")

contingency_table <- table(subc_df_na$level_of_care_provided_per_protocol, subc_df_na$patient_race)

print(contingency_table)

chi_sq_test <- chisq.test(contingency_table)

print(chi_sq_test)

#pearson residuals
print(round(chi_sq_test$residuals, 2))

#visualize heatmap for chi-square test contributions
library(pheatmap)

contributions <- (chi_sq_test$observed - chi_sq_test$expected)^2 / chi_sq_test$expected

#percentage contributions
total_chi_square <- chi_sq_test$statistic
percentage_contributions <- 100 * contributions / total_chi_square

print(round(percentage_contributions, 2))

pheatmap(percentage_contributions,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Percentage Contribution to Chi-Square Statistic")
