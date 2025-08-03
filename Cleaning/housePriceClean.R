library(tidyverse)
library(dplyr)

# Read raw house price CSV files for years 2021 to 2024 from the specified directory
housePrice2021 = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2021.csv", show_col_types = FALSE)
housePrice2022 = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2022.csv", show_col_types = FALSE)
housePrice2023 = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2023.csv", show_col_types = FALSE)
housePrice2024 = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2024.csv", show_col_types = FALSE)

# Define consistent column names to assign to each year’s dataset for easier handling
colnames_set = c("TransactionID", "Price", "DateOfTransfer", "Postcode", "PropertyType",
                 "OldOrNew", "Duration", "PAON", "SAON", "Street", "Locality",
                 "Town", "District", "County", "PPDCategoryType", "RecordStatus")

# Apply the defined column names to all house price datasets
colnames(housePrice2021) = colnames_set
colnames(housePrice2022) = colnames_set
colnames(housePrice2023) = colnames_set
colnames(housePrice2024) = colnames_set

# Combine all yearly datasets into one, remove duplicates and rows with missing values, convert to tibble
HousePrices = bind_rows(housePrice2021, housePrice2022, housePrice2023, housePrice2024) %>%
  distinct() %>%       # Remove duplicate rows if any
  na.omit() %>%        # Remove rows with any missing values
  as_tibble()

View(HousePrices)

# Filter the combined dataset to include only properties in South Yorkshire and West Yorkshire
# Extract a shortened postcode (area part before space), extract year from transfer date,
# convert Price to numeric, and select relevant columns for further analysis
CleanedHousePrices = HousePrices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%  # Filter by specified counties
  mutate(
    shortPostcode = gsub(" .*$", "", Postcode),          # Extract area code from postcode (before first space)
    Year = format(as.Date(DateOfTransfer), "%Y"),        # Extract year from date of transfer
    Price = as.numeric(Price)                             # Convert price column to numeric type
  ) %>%
  select(Postcode, shortPostcode, Year, PAON, Price, Town, District, County) %>%  # Keep only necessary columns
  distinct() %>%           # Remove any duplicate rows after filtering and selecting columns
  na.omit()                # Remove any remaining rows with missing values

View(CleanedHousePrices)

# Save the cleaned and filtered house price data to a new CSV file for future use
write_csv(CleanedHousePrices, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")
