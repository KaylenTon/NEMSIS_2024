library(ggplot2)
library(car)
library(dplyr)
library(tidyr)
library(report)
library(multcomp)
library(MASS)
table(final_clean_NA$level_of_care_provided_per_protocol)


#anova for significant difference between level_of_care_provided using age as the continuous variable
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

#range of patient age
hist(sub_df_a$patient_age)
summary(sub_df_a$patient_age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   41.00   63.00   58.41   77.00  120.00 

#The mean is less than the median, meaning the data is slightly skewed to the left
# is this an issue? Should data be normalized before performing an ANOVA test?

#range of patient age by type of life support
ggplot(sub_df_a) +
  aes(x=patient_age, fill = level_of_care_provided_per_protocol) +
  geom_histogram(color = "blue", position = "identity", alpha = 0.6) + 
  labs(title = "Histogram of Patient Age by Level of Care Provided",
       x = "Patient Age",
       y = "Count",
       fill = "Level of Care Provided Per Protocol") +
  theme_minimal()

#each group of life support follows similar distributions in terms of age

# boxplot
ggplot(sub_df_a) +
  aes(x = level_of_care_provided_per_protocol,
      y = patient_age,
      color = level_of_care_provided_per_protocol) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Boxplot of Patient Age by Level of Care Provided",
       y = "Patient Age",
       x = "Level of Care Provided Per Protocol") +
  theme_minimal()

#summary statistics
group_by(sub_df_a, level_of_care_provided_per_protocol) %>%
  summarise(
    mean = mean(patient_age, na.rm = TRUE),
    sd = sd(patient_age, na.rm = TRUE)
  )

#BLS has slightly older individuals, in terms of media, than ALS group. ALS - Paramedic has more variance

#independence: we used random sampling to collect the observations, and none are duplicates. Thus, we assume independence.

#q-q plot
set.seed(0)
qqnorm(sub_df_a$patient_age, main="Patient Age")

# not exactly a straight 45-degree line, its a little skewed
# but the spread of ages between groups don't differ so much (a lack of heteroscedasticity), so we can assume anova will work fine
# according to the Central Limit Theorem, our sample size should be big enough so that it's resistant to the affects of slightly skewed data

# since the distribution is slightly not normal, we use a Kruskal-Wallis test for significant differences
kruskal.test(patient_age ~ level_of_care_provided_per_protocol, data = sub_df_a)

#checking for NA
sum(is.na(sub_df_a$level_of_care_provided_per_protocol))
sum(is.na(sub_df_a$patient_age))


#ANOVA tests
oneway.test(patient_age ~ level_of_care_provided_per_protocol,
            data = sub_df_a,
            var.equal = TRUE)

res_aov <- aov(patient_age ~ level_of_care_provided_per_protocol,
               data = sub_df_a)

summary(res_aov)

#results
report(res_aov)

#now lets try testing after normalizing patient_age
box_cox_fit <-

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
