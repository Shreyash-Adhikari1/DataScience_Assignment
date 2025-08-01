library(tidyverse)
library(dplyr)

# Reading the unorganized/unclean data [csv files] from where the obtained data-sets are stored

housePrice2021=read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2021.csv", show_col_types = FALSE)
housePrice2022=read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2022.csv", show_col_types = FALSE)
housePrice2023=read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2023.csv", show_col_types = FALSE)
housePrice2024=read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\HousePrices\\house-price-2024.csv", show_col_types = FALSE)


# Assigning column names to the CSV files to make them organized  

colnames_set=c("TransactionID", "Price", "DateOfTransfer", "Postcode", "PropertyType",
               "OldOrNew", "Duration", "PAON", "SAON", "Street", "Locality",
               "Town", "District", "County", "PPDCategoryType", "RecordStatus")

colnames(housePrice2021)=colnames_set
colnames(housePrice2022)=colnames_set
colnames(housePrice2023)=colnames_set
colnames(housePrice2024)=colnames_set



# Combining house prices of all given years into a single data-set

HousePrices=bind_rows(housePrice2021, housePrice2022, housePrice2023, housePrice2024) %>%
  distinct() %>%      
  na.omit() %>%
  as_tibble()
View(HousePrices)


# Filter South and West Yorkshire only from the obtained data-set

CleanedHousePrices=HousePrices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(
    shortPostcode = gsub(" .*$", "", Postcode),          
    Year = format(as.Date(DateOfTransfer), "%Y"), 
    Price = as.numeric(Price)
  ) %>%
  select(Postcode, shortPostcode, Year, PAON, Price, Town, District, County) %>%  # Select the columns that we want for further processing
  distinct() %>%
  na.omit()
View(CleanedHousePrices)


# Saving the cleaned filtered data-set into csv file i.e. "CleanedHousePrices.csv"

write_csv(CleanedHousePrices, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")
