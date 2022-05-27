

df1 <- read_xlsx(here::here("../CLEANED_OUES_dataset_5_3_2022_1.xlsx"))
df2 <- read_xlsx(here::here("../CLEANED_OUES_dataset_5_3_2022_2.xlsx"))



df1 <- select(df1, ID, test_number, record_year)
df2 <- select(df2, ID, test_number, record_year)



df_id <- anti_join(df1, df2, by=c("ID"))
df_yr <- anti_join(df1, df2, by=c("ID", "record_year"))

df <- anti_join(df_yr, df_id, by="ID")

