library(tidyverse)

UncleanBroadbandData = read_csv("C://Users//ADMIN//Desktop//Data Science Assignment//Obtained Data//broadband-speed//201805_fixed_pc_performance_r03.csv",show_col_types = FALSE)
View(UncleanBroadbandData)

# Create shortPostcode from postcode_space and select key columns
CleanBroadbandData <- UncleanBroadbandData %>%
  mutate(
    shortPostcode = str_trim(substr(postcode_space, 1, 4)),
    ID = row_number()
  ) %>%
  select(
    ID,
    postcode_area = `postcode area`,
    shortPostcode,
    avgDownload = `Average download speed (Mbit/s)`,
    avgUpload = `Average upload speed (Mbit/s)`,
    minDownload = `Minimum download speed (Mbit/s)`,
    minUpload = `Minimum upload speed (Mbit/s)`
  ) %>%
  na.omit() %>%
  distinct()

# Writing the cleaned CSV file as cleaned_broadband_data.csv
write_csv(CleanBroadbandData, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv")

# Viewing the data after it is cleaned
View(CleanBroadbandData)
