library(tidyverse)
library(dplyr)
library(stringi)
library(scales)


# BROADBAND DATA CLEANING
Broadband = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\broadband-speed\\201805_fixed_pc_performance_r03.csv", show_col_types = FALSE)
View(Broadband)

pattern = ' .*$'
BroadbandData = Broadband %>%
  mutate(shortPostcode=gsub(pattern,"",postcode_space)) %>% 
  mutate(ID = row_number()) %>% 
  select(`ID`, `postcode area`, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Minimum upload speed (Mbit/s)`) %>% 
  na.omit()
colnames(BroadbandData) = c( "ID","postcode area", "shortPostcode", "Avgdownload",
                             "Average upload speed (Mbit/s)", "Mindownload",
                             "Minimum upload speed (Mbit/s)")
write.csv(BroadbandData, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv",row.names = FALSE)

View(BroadbandData)
