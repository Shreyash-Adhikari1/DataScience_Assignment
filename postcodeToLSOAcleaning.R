library(tidyverse)

CleanedHousePrices = read.csv("C:/Users/Acer/Documents/ST5014CEM_DataScienceForDevelopers_PrajwolKhadka_230225/Cleaned Data/Cleaned_Town_Population.csv")

LSOA = read_csv("C:/Users/Acer/Documents/ST5014CEM_DataScienceForDevelopers_PrajwolKhadka_230225/Obtained Data/PostcodeToLSOA/Postcode to LSOA.csv", show_col_types = FALSE)


pattern = ' .*$'
LSOA_Cleaned = LSOA %>%
  select(lsoa11cd, pcds) %>%
  mutate(shortPostcode = gsub(pattern, "", pcds)) %>%
  right_join(Cleaned_HP, by = "shortPostcode") %>%
  group_by(lsoa11cd) %>%
  select(lsoa11cd, shortPostcode, Town, District, County,
         Population2020, Population2021, Population2022, Population2023, Population2024) %>%
  distinct()


colnames(LSOA_Cleaned)[1] = "LSOA_code"

View(LSOA_Cleaned)

write.csv(LSOA_Cleaned, "C:/Users/Acer/Documents/ST5014CEM_DataScienceForDevelopers_PrajwolKhadka_230225/Cleaned Data/Cleaned_LSOA.csv", row.names = FALSE)
