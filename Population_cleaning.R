library(tidyverse)

# Load full combined house price dataset (already merged)
HousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\combined_house_prices.csv")
colnames(HousePrices)

# Load population data and calculate projections
PopulationData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\Population2011.csv")
View(PopulationData)

PopulationData = PopulationData %>%  
  mutate(shortPostcode = str_trim(substring(Postcode, 1, 4))) %>% 
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population, na.rm = TRUE)) %>% 
  mutate(Population2012 = 1.00695353132322269 * Population2011) %>%
  mutate(Population2013 = 1.00669740535540783 * Population2012) %>%
  mutate(Population2014 = 1.00736463978721671 * Population2013) %>%
  mutate(Population2015 = 1.00792367505802859 * Population2014) %>%
  mutate(Population2016 = 1.00757874492811929 * Population2015) %>%
  mutate(Population2017 = 1.00679374473924223 * Population2016) %>%
  mutate(Population2018 = 1.00605929132212552 * Population2017) %>%
  mutate(Population2019 = 1.00561255390388033 * Population2018) %>%
  mutate(Population2020 = 1.00561255390388033 * Population2019) %>%
  mutate(Population2021 = 1.005425 * Population2020) %>%
  mutate(Population2022 = 1.004920 * Population2021) %>%
  mutate(Population2023 = 1.004510 * Population2022) %>%
  mutate(Population2024 = 1.004220 * Population2023) %>% 
  select(shortPostcode, Population2020, Population2021, Population2022, Population2023, Population2024)

# Filter towns and join population data
Towns = HousePrices %>%
  filter(County == "SOUTH YORKSHIRE" | County == "WEST YORKSHIRE") %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1, 4))) %>% 
  left_join(PopulationData, by = "shortPostcode") %>% 
  select(shortPostcode, Town, District, County, 
         Population2020, Population2021, 
         Population2022, Population2023, Population2024) %>% 
  group_by(shortPostcode) %>% 
  filter(row_number() == 1) %>% 
  arrange(County)
View(Towns)

# Save to CSV
write_csv(Towns,"C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Town_Population.csv")
