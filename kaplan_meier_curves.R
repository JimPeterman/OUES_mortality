
library(readxl)
library(dplyr)
library(survival)

# Creating Kaplan Meier curves to illustrate survival data from study.
# Graph the 1st and 3rd OUES tertiles.

data <- read_xlsx(here::here("../CLEANED_OUES_dataset_4_27_2022.xlsx"))


# Create groups for OUES and fitness
data <- data %>%
  mutate(OUES_tertile = ntile(OUES, 3)) %>%
  mutate(OUES_tertile = if_else(OUES_tertile == 1, "Good", if_else(OUES_tertile == 2, "Ok", "Bad"))) 

# data <- data %>%
#   mutate(FRIEND_cat = if_else(FRIEND_pct < 33.3, "Bad", ifelse(FRIEND_pct > 66.6, "Good", "Ok")))

df <- filter(data, OUES_tertile == "Good" | OUES_tertile == "Bad")


##########################################################################
# Create OUES figure with both sexes.

# Create the plot.
all_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, data=df), 
  # palette = c("red3", "darkgreen"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Overall Survival Probability",
  title = "Kaplan-Meier Curve for OUES",
  legend = "right",
  legend.title = "OUES Tertiles",
  legend.labs = c("High OUES (Good)", "Low OUES (Bad)"))


# Create the plot for just MALES
male_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, 
                data=filter(df, sex == "Male")), 
  palette = c("salmon", "skyblue4"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Overall Survival Probability",
  title = "Kaplan-Meier Curve for OUES in Males",
  legend = "right",
  legend.title = "OUES Tertiles",
  legend.labs = c("High OUES (Good)", "Low OUES (Bad)"))


# Create the plot for just FEMALES
female_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, 
                data=filter(df, sex == "Female")), 
  palette = c("paleturquoise3", "darkslateblue"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Overall Survival Probability",
  title = "Kaplan-Meier Curve for OUES in Females",
  legend = "right",
  legend.title = "OUES Tertiles",
  legend.labs = c("High OUES (Good)", "Low OUES (Bad)"))






