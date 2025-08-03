library(tidyverse)

# Read cleaned town population data from CSV
CleanedHousePrices = read.csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Town_Population.csv")

# Read raw postcode to LSOA mapping data
LSOA = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\PostcodeToLSOA\\Postcode to LSOA.csv", show_col_types = FALSE)

# Define regex pattern to extract the area part of the postcode (before the first space)
pattern = ' .*$'

# Clean and join LSOA data with town population data
LSOA_Cleaned = LSOA %>%
  # Select only LSOA code and postcode columns
  select(lsoa11cd, pcds) %>%
  # Extract short postcode (area code) by removing everything after first space
  mutate(shortPostcode = gsub(pattern, "", pcds)) %>%
  # Join with cleaned house prices data on shortPostcode to get town, district, county, and population info
  right_join(CleanedHousePrices, by = "shortPostcode") %>%
  # Group by LSOA code for potential aggregation (not performed here, but groups rows)
  group_by(lsoa11cd) %>%
  # Select relevant columns to keep
  select(lsoa11cd, shortPostcode, Town, District, County,
         Population2020, Population2021, Population2022, Population2023, Population2024) %>%
  # Remove duplicate rows
  distinct()

# Rename the first column to "LSOA_code" for clarity
colnames(LSOA_Cleaned)[1] = "LSOA_code"

# View the cleaned and joined LSOA data (for inspection)
View(LSOA_Cleaned)

# Check column names (optional)
colnames(LSOA_Cleaned)

# Save the cleaned LSOA data with population and location info to CSV file without row numbers
write.csv(LSOA_Cleaned, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv", row.names = FALSE)
