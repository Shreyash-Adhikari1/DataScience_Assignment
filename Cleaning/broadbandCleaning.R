library(tidyverse)

Broadband = read_csv( "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\broadband-speed\\201805_fixed_pc_performance_r03.csv",show_col_types = FALSE)

HousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")

colnames(HousePrices)
postcode_map = HousePrices %>%
  mutate(shortPostcode = substr(Postcode, 1, 4)) %>%
  count(shortPostcode, Town, District, County, name = "n") %>%
  group_by(shortPostcode) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(shortPostcode, Town, District, County)

BroadbandData = Broadband %>%
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
  distinct() %>%
  left_join(postcode_map, by = "shortPostcode") %>%
  filter(!is.na(District), !is.na(County))

write_csv(BroadbandData, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Broadband_Data.csv"
)
View(BroadbandData)
colnames(BroadbandData)


BroadbandData
