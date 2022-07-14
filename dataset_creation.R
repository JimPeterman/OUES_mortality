
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

# Drop individuals aged over 85 or under 18
data <- data[!(data$age>85 | data$age<18),]

# Drop those taking beta blocker medications (keeps them if value is missing).
data <- data[!(data$med_beta %in% 1),]

# Drop those with short test times (need ≥2 minutes for 50% of test time). 
data <- filter(data, test_time >=5)

# Include only those with an RER>=1.0.
data <- data %>% filter(max_rer >= 1.0)

# Keep only treadmill tests.
data <- filter(data, test_mode=="TM")

# Drops those with OUES values outside of the "normal" range.
data <- data[(data$OUES <= 6.0 & data$OUES >= 1.0),]

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

# Create tertile groups for OUES.
data <- data %>%
  mutate(OUES_tertile = ntile(OUES, 3)) %>%
  mutate(OUES_tertile = if_else(OUES_tertile == 1, "Good", if_else(OUES_tertile == 2, "Ok", "Bad"))) 

data <- data %>%
  mutate(OUES_norm_tertile = ntile(OUES_norm, 3)) %>%
  mutate(OUES_norm_tertile = if_else(OUES_norm_tertile == 1, "Good", 
                                     if_else(OUES_norm_tertile == 2, "Ok", "Bad"))) 

data <- data %>%
  mutate(OUES_50_tertile = ntile(OUES_50, 3)) %>%
  mutate(OUES_50_tertile = if_else(OUES_50_tertile == 1, "Good", 
                                     if_else(OUES_50_tertile == 2, "Ok", "Bad"))) 

data <- data %>%
  mutate(OUES_75_tertile = ntile(OUES_75, 3)) %>%
  mutate(OUES_75_tertile = if_else(OUES_75_tertile == 1, "Good", 
                                     if_else(OUES_75_tertile == 2, "Ok", "Bad"))) 


#####################################################################
# Add the submaximal OUES data from the "minute" dataset.
#####################################################################

# For the MINUTE dataset:
# Searched for something (*) in rel VO2 column of minute columns in FileMaker.
# Data downloaded using the "Export Minutes" button (10/4/2021).
data_min_all <- read_excel(here::here("../FileMaker Minutes Download_10_13_2021.xlsx"),
                           na = "?")

# Rename the two key column labels used to sort.
data_min_all <- data_min_all %>% 
  rename("ID"="Person ID", 
         "testNumber"="Test Number")

# Create vectors of column labels.
col_vo2 <- character(length = 30)
col_ve <- character(length = 30)
col_ve_calc <- character(length = 30)
for(i in 1:30){
  col_vo2[i] <- paste("VO2", i, sep="")
  col_ve[i] <- paste("VE BTPS", i, sep="")
  col_ve_calc[i] <- paste("VE BTPS_calc", i, sep="")
}
col_int <- c(col_vo2, col_ve, col_ve_calc)

data_min <- select(data_min_all, ID, testNumber, all_of(col_int))


# Drop missing ID or test number (needed for matching).
data_min <- filter(data_min, !(is.na(ID)), !(is.na(testNumber)))

# Convert columns to numeric.
data_min <- data_min %>% 
  mutate_at(.vars = col_int,
            .funs = list(~replace(.,grepl("n/a", ., ignore.case = T),NA)))
data_min <- data_min %>% 
  mutate_at(.vars = col_int,
            .funs = list(~replace(.,grepl("n", ., ignore.case = T),NA)))
# Should all be fixed now in FileMaker.
data_min$`VE BTPS1`[data_min$`VE BTPS1` == "22..17"] <- "22.17"
data_min$`VE BTPS3`[data_min$`VE BTPS3` == "\\"] <- NA
data_min$`VE BTPS4`[data_min$`VE BTPS4` == "b/a"] <- NA
data_min$`VE BTPS5`[data_min$`VE BTPS5` == "44q"] <- "44"
data_min$`VE BTPS6`[data_min$`VE BTPS6` == "39..52"] <- "39.52"
data_min$`VE BTPS7`[data_min$`VE BTPS7` == "58..88"] <- "58.88"
data_min$`VE BTPS11`[data_min$`VE BTPS11` == "93..5"] <- "93.5"

data_min[] <- lapply(data_min, as.numeric) 

#####################################################################
# Add in the calculated OUES (using data from 50% and 75% of total test time).

# Add columns to write to in the MAIN dataset.
data <- mutate(data, OUES_50 = NA)
data <- mutate(data, OUES_75 = NA)
# Did "100%" to compare FM calculation but only downloaded 30min so that may not include all data points.
# After running the analysis, looks real similar between 100% calc and FileMaker calc.
data <- mutate(data, OUES_100 = NA)

# Write in the OUES values from the minute date. 
for(i in 1:nrow(data_min)){
  temp_id <- paste(data_min[i, "ID"])
  temp_test_num <- paste(data_min[i, "testNumber"])
  
  # Determine if the ID and test are in the main dataset.
  # And if the test is in main dataset, filter to just that.
  if ((as.numeric(temp_id) %in% data$ID)){
    temp_df <- filter(data, ID == as.numeric(temp_id))
    if (as.numeric(temp_test_num) %in% temp_df$test_number){
      temp_df <- filter(temp_df, test_number == as.numeric(temp_test_num))
      # Get total test time from main dataset.
      test_time_temp <- temp_df$test_time
      
      # Skip the test if there's no test time.
      if (!is.na(test_time_temp)){
        
        # Need a temp df from minute dataset.
        temp_min_df <- filter(data_min, ID == as.numeric(temp_id),
                              testNumber == as.numeric(temp_test_num))
        
        ##############                      
        # Now calculate OUES_50 (using 50% of total test time minute data).
        ##############                      
        # Will have to convert some values (like VO2rel to VO2abs).
        wt <- temp_df$weight_SI
        
        # Create the vectors of VO2/VE for finding the slope (OUES) of 50% test time.
        ve_vec_50 <- as.numeric(character(ceiling(test_time_temp/2)))
        vo2_vec_50 <- as.numeric(character(ceiling(test_time_temp/2)))
        for(j in 1:ceiling(test_time_temp/2)){
          # Use VE BTPS unless it's missing, then use the calculated version.
          if(is.na(temp_min_df[[col_ve[j]]])){
            ve_vec_50[j] <- (log10(temp_min_df[[col_ve_calc[j]]]))
          } else {
            ve_vec_50[j] <- (log10(temp_min_df[[col_ve[j]]]))
          }
          vo2_vec_50[j] <- as.numeric(temp_min_df[[col_vo2[j]]] * wt /1000)
        }
        # Calculate the OUES (slope) if both vectors are not only NA.
        if(all(is.na(ve_vec_50)) == FALSE & all(is.na(vo2_vec_50)) == FALSE){
          # lm is called as lm(y~x).
          slope = lm(vo2_vec_50 ~ ve_vec_50)$coefficients[2]
          data$OUES_50[(data$ID == as.numeric(temp_id)) &
                         (data$test_number == as.numeric(temp_test_num))] <- round(slope, 2)
        }
        
        
        ##############                      
        # Now calculate OUES_75 (using 75% of total test time minute data).
        ############## 
        ve_vec_75 <- as.numeric(character(ceiling(test_time_temp * 0.75)))
        vo2_vec_75 <- as.numeric(character(ceiling(test_time_temp * 0.75)))
        for(j in 1:ceiling(test_time_temp * 0.75)){
          # Use VE BTPS unless it's missing, then use the calculated version.
          if(is.na(temp_min_df[[col_ve[j]]])){
            ve_vec_75[j] <- (log10(temp_min_df[[col_ve_calc[j]]]))
          } else {
            ve_vec_75[j] <- (log10(temp_min_df[[col_ve[j]]]))
          }
          vo2_vec_75[j] <- as.numeric(temp_min_df[[col_vo2[j]]] * wt /1000)
        }
        # Calculate the OUES (slope) if both vectors are not only NA.
        if(all(is.na(ve_vec_75)) == FALSE & all(is.na(vo2_vec_75)) == FALSE){
          # lm is called as lm(y~x).
          slope = lm(vo2_vec_75 ~ ve_vec_75)$coefficients[2]
          data$OUES_75[(data$ID == as.numeric(temp_id)) &
                         (data$test_number == as.numeric(temp_test_num))] <- round(slope, 2)
        }
        
        ##############                      
        # Now calculate OUES_100 (using 100% of total test time minute data).
        # This is to check the OUES calculations in FileMaker.
        ############## 
        if(test_time_temp>30){
          test_time_temp <- 30
        }
        ve_vec_100 <- as.numeric(character(floor(test_time_temp)))
        vo2_vec_100 <- as.numeric(character(floor(test_time_temp)))
        for(j in 1:floor(test_time_temp)){
          # Use VE BTPS unless it's missing, then use the calculated version.
          if(is.na(temp_min_df[[col_ve[j]]])){
            ve_vec_100[j] <- (log10(temp_min_df[[col_ve_calc[j]]]))
          } else {
            ve_vec_100[j] <- (log10(temp_min_df[[col_ve[j]]]))
          }
          vo2_vec_100[j] <- as.numeric(temp_min_df[[col_vo2[j]]] * wt /1000)
        }
        # Calculate the OUES (slope) if both vectors are not only NA.
        if(all(is.na(ve_vec_100)) == FALSE & all(is.na(vo2_vec_100)) == FALSE){
          # lm is called as lm(y~x).
          slope = lm(vo2_vec_100 ~ ve_vec_100)$coefficients[2]
          data$OUES_100[(data$ID == as.numeric(temp_id)) &
                          (data$test_number == as.numeric(temp_test_num))] <- round(slope, 2)
        }
      }
    }
  }
}

###########################################################################################
# Final filtering and dataset creation.
###########################################################################################

# Drops values outside of "normal" range for OUES_50 and OUES_75
# (not dropping these tests though!)
# In other words, the submax analyses will be a subset from the main dataset.
data <- mutate(data, OUES_50 = ifelse(OUES_50 > 10.0, NA,
                                      ifelse(OUES_50 < 1.0, NA, OUES_50)))
data <- mutate(data, OUES_75 = ifelse(OUES_75 > 10.0, NA,
                                      ifelse(OUES_75 < 1.0, NA, OUES_75)))
data <- filter(data, !(is.na(OUES_75)))

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

write_xlsx(data, here::here("../CLEANED_OUES_dataset_5_27_2022.xlsx"))

