
library(readxl)
library(dplyr)
library(survival)

# Creating Kaplan Meier curves to illustrate survival data from study.

data <- read_xlsx(here::here("../CLEANED_OUES_dataset_5_2_2022.xlsx"))


##########################################################################
# Create OUES figure with both sexes (ALL OUES comparisons).
##########################################################################

##########################################################################
# OUES Plots.

# df_all <- data
# df_all$OUES_tertile <- ordered(df_all$OUES_tertile, levels = c("Bad", "Ok", "Good"))
# # Create the plot for ALL
# all_plot <- survminer::ggsurvplot(
#   fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, 
#                 data=df_all), 
#   palette = c("palegreen3", "dodgerblue3", "indianred"),
#   pval = T,
#   conf.int = T,
#   xlab = "Years", 
#   ylab = "Overall Survival Probability",
#   title = "Kaplan-Meier Curve for OUES Tertiles",
#   legend = "right",
#   legend.title = "OUES Tertiles",
#   legend.labs = c("Top (Good)", "Middle", "Bottom (Bad)"))


df_male <- filter(data, sex == "Male")
df_male$OUES_tertile <- ordered(df_male$OUES_tertile, levels = c("Bad", "Ok", "Good"))
# Create the plot for just MALES
male_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, 
                data=df_male), 
  palette = c("palegreen3", "dodgerblue3", "indianred"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Survival Probability",
  title = "A",
  legend = "right",
  legend.title = "OUES Tertiles",
  legend.labs = c("Top (Good)", "Middle", "Bottom (Bad)"))


df_female <- filter(data, sex == "Female")
df_female$OUES_tertile <- ordered(df_female$OUES_tertile, levels = c("Bad", "Ok", "Good"))
# Create the plot for just FEMALES
female_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, 
                data=df_female), 
  palette = c("palegreen3", "dodgerblue3", "indianred"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Survival Probability",
  title = "C",
  legend = "right",
  legend.title = "OUES Tertiles",
  legend.labs = c("Top (Good)", "Middle", "Bottom (Bad)"))


##########################################################################
# Normalized OUES Plots.

df_male$OUES_norm_tertile <- ordered(df_male$OUES_norm_tertile, levels = c("Bad", "Ok", "Good"))
# Create the plot for just MALES
male_norm_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_norm_tertile, 
                data=df_male), 
  palette = c("palegreen3", "dodgerblue3", "indianred"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Survival Probability",
  title = "B",
  legend = "right",
  legend.title = "Normalized OUES\nTertiles",
  legend.labs = c("Top (Good)", "Middle", "Bottom (Bad)"))


df_female$OUES_norm_tertile <- ordered(df_female$OUES_norm_tertile, levels = c("Bad", "Ok", "Good"))
# Create the plot for just FEMALES
female_norm_plot <- survminer::ggsurvplot(
  fit = survfit(Surv(follow_up_yrs, mortality_status) ~ OUES_norm_tertile, 
                data=df_female), 
  palette = c("palegreen3", "dodgerblue3", "indianred"),
  pval = T,
  conf.int = T,
  xlab = "Years", 
  ylab = "Survival Probability",
  title = "D",
  legend = "right",
  legend.title = "Normalized OUES\nTertiles",
  legend.labs = c("Top (Good)", "Middle", "Bottom (Bad)"))


##########################################################################
# Combine the male and female plots (both OUES and normalized OUES) into one figure.
plot_list <- c(male_plot[1], male_norm_plot[1], female_plot[1], female_norm_plot[1])
gridExtra::grid.arrange(grobs=plot_list, nrow=2)


##########################################################################
# Create OUES figure with both sexes (only Top and Bottom OUES comparisons).
##########################################################################

df <- filter(data, OUES_tertile == "Good" | OUES_tertile == "Bad")

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
  legend.labs = c("Top (Good)", "Bottom (Bad)"))


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
  legend.labs = c("Top (Good)", "Bottom (Bad)"))


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
  legend.labs = c("Top (Good)", "Bottom (Bad)"))



