
###########################################################################################
###########################################################################################
# Create a dataset for analyzing the utility of OUES in predicting mortality risk.
# Uses data from the BALL ST cohort of apparently healthy individuals.
# (Filter down to the definition of apparently healthy we use).
###########################################################################################
###########################################################################################

library(readxl)
library(dplyr)
library(writexl)
library(FRIENDanalysis)

data <- read_excel(here::here("../BALLST_healthy_cohort_dataset.xlsx"))


###########################################################################################
# Dropping tests.
###########################################################################################

# Keep only those with sex known.
data <- data[(data$sex == "Male" | data$sex == "Female"),]

# Drop individuals aged over 85 or under 20.
data <- data[!(data$age>85 | data$age<20),]

# Drop those taking beta blocker medications (keeps them if value is missing).
data <- data[!(data$med_beta %in% 1),]

# Include only those with an RER>=1.0.
data <- data %>% filter(max_rer >= 1.0)

# Keep only treadmill tests.
data <- filter(data, test_mode=="TM")

################################################
# Need to filter down to years of interest for analysis.
# First, get the year of death and the year of the test. 
#### If death_year doesn't show up as date, try rearranging it in Excel file so cells at top of file are filled.
data <- mutate(data, death_year = as.numeric(format(as.Date(data$death_date), "%Y")))
data <- mutate(data, record_year = as.numeric(format(as.Date(data$record_date),"%Y")))

# Need to drop/change variables for those who died >=2020 as this will mess up the stats.
# NDI went through 2019 so don't include those who died since then.
data$death_date[data$death_year >= 2020] <- NA
data$death_cause_generic[data$death_year >= 2020] <- NA
data$mortality_status[data$death_year >= 2020] <- 0
data$mortality_grouping[data$death_year >= 2020] <- NA
data$death_year[data$death_year >= 2020] <- NA

# Create a "Follow-Up" variable with the time between the record and death or record and NDI (12/31/2019).
ndi <- strptime("2019-12-31", format = "%Y-%m-%d")
data <- mutate(data, follow_up_days = ifelse(!(is.na(data$death_date)), difftime(data$death_date, data$record_date, units = "days"),
                                             difftime(ndi, data$record_date, units = "days")))
data <- mutate(data, follow_up_yrs = round(data$follow_up_days/365, 2))
data$follow_up_days <- NULL

# Include only tests performed before the year 2019 
# (NDI searched through 2019 and want >=1 year follow-up).
data <- data %>% filter(record_year<2019)

# Drops those if they passed away within 1 year of exercise test.
# (again, want >=1 year follow-up to account for possible underlying disease).
data <- mutate(data, date_diff = difftime(data$death_date, data$record_date, units = "days"))
data$date_diff[is.na(data$date_diff)] <- 1000
data <- filter(data, data$date_diff>365)
data$date_diff <- NULL

################################################

# Drops individuals who don't have these key variables 
# (want everyone to have all covariates for cox regressions).
var_int <- c("VO2_rel", "sex", "age", "mortality_status", 
             "obesity", "hypertension", "dyslipidemia", "diabetes", "inactivity", 
             "smoker", "record_year", "OUES")
data <- data[complete.cases(data[,var_int]),]

###########################################################################################
# Adding variables.
###########################################################################################

# Create a normalized OUES variable (normalized to body surface area).
# First calculate BSA (from Haycock et al.).
data <- mutate(data, bsa = 
                 round((0.024265 * (height_SI ^ 0.3964) * (weight_SI ^ 0.5378)) , 2))
data <- mutate(data, OUES_norm = round((OUES/bsa), 2))

# Determine FRIEND percentile (using 2021 ref standards). 
data <- mutate(data, FRIEND_pct = FRIENDpercentile(VO2 = VO2_rel, age = age,
                                                     sex = sex, ex_mode = test_mode,
                                                     ref_edition = 2))
data <- mutate(data, FRIEND_grp = ifelse(data$FRIEND_pct <= 33, "low fit", 
                                         ifelse(data$FRIEND_pct >= 67, "high fit", "avg fit")))

###########################################################################################
# Final filtering and dataset creation.
###########################################################################################

# Drops those with OUES values outside of the "normal" range.
data <- data[(data$OUES <= 6.0 & data$OUES >= 1.0),]

# Drops those missing an OUES normalized to body surface area 
# (n=1 but matters for concurrence tests).
data <- filter(data, !(is.na(OUES_norm)))

# Select only the earliest test from the remaining complete dataset.
data <- data %>%
  group_by(ID) %>% 
  arrange(record_date) %>% 
  slice(1L) %>%
  ungroup(ID)


###########################################################################################
# Save file.
###########################################################################################

write_xlsx(data, here::here("../CLEANED_OUES_dataset_4_27_2022_.xlsx"))

