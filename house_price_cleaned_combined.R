library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(readr)


hh2021 = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\house-price-2021.csv",show_col_types = FALSE)
hh2022= read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\house-price-2022.csv",show_col_types = FALSE)
hh2023= read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\house-price-2023.csv",show_col_types = FALSE)
hh2024= read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\house-price-2024.csv",show_col_types=FALSE)

colnames(hh2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2" )
colnames(hh2022) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2")
colnames(hh2023) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")
colnames(hh2024) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County" , "Type1", "Type2")

HousePrices = rbind(hh2021,hh2022,hh2023,hh2024) %>% 
  na.omit() %>% 
  distinct() %>% 
  as_tibble()
View(HousePrices)

write.csv(HousePrices, file = "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\combined_house_prices.csv", row.names = FALSE)



FilteredHousePrices = filter(HousePrices,County == 'WEST YORKSHIRE' | County == 'SOUTH YORKSHIRE' )

# Replace "YORK" with "YORKSHIRE" in the COUNTY column
FilteredHousePrices$County[FilteredHousePrices$County == "YORK"] <- "YORKSHIRE"


View(FilteredHousePrices)

pattern = ' .*$'

FilteredHousePrices = FilteredHousePrices %>% 
  mutate(shortPostcode=gsub(pattern,"",PostCode)) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  select(PostCode,shortPostcode,Year,PAON,Price) %>% 
  na.omit() %>% 
  distinct() %>% 
  as_tibble()
View(FilteredHousePrices)


# exporting filteredhouseprices data set to  csv
write.csv(FilteredHousePrices, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\filtered_house_prices_2021-2024.csv", row.names = TRUE)
