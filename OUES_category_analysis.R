
library(readxl)
library(dplyr)
library(writexl)
library(survival)
library(survminer)
library(stringr)

data <- read_xlsx(here::here("../data/CLEANED_OUES_dataset_5_2_2022.xlsx"))

#######################################################################
# Cox proportional hazards and Concordance tests.
#######################################################################

# group_type <- c("all", "Male", "Female")
# death_var <- c("all", "Cancer", "CVD")
temp_df <- filter(data, sex == "Male")


# Model 1 - Univariate Model.
res_cox_uni <- coxph(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile, data = temp_df)
summary(res_cox_uni)

# Model 2 - Control for: age, test year.
res_cox_multi_2 <- coxph(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile + age + record_year, data = temp_df)
summary(res_cox_multi_2)

# Model 3 - Control for: age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year.
res_cox_multi_3 <- coxph(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile + age + record_year +
                           obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker, data = temp_df)
summary(res_cox_multi_3)

# Model 4 - Control for: age, test year, obesity, hypertension, dyslipidemia, diabetes, PA, smoking, record year, VO2max.
res_cox_multi_4 <- coxph(Surv(follow_up_yrs, mortality_status) ~ OUES_tertile + age + record_year +
                           obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker +
                           VO2_rel, data = temp_df)
summary(res_cox_multi_4)
