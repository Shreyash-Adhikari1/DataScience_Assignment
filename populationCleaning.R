library(tidyverse)


CleanedHousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")


PopulationData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\Population\\Population2011.csv", show_col_types = FALSE)

PopulationDataClean = PopulationData %>%
  mutate(shortPostcode = str_trim(substr(Postcode, 1, 4))) %>%
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Population2012 = 1.00695353132322269 * Population2011,
    Population2013 = 1.00669740535540783 * Population2012,
    Population2014 = 1.00736463978721671 * Population2013,
    Population2015 = 1.00792367505802859 * Population2014,
    Population2016 = 1.00757874492811929 * Population2015,
    Population2017 = 1.00679374473924223 * Population2016,
    Population2018 = 1.00605929132212552 * Population2017,
    Population2019 = 1.00561255390388033 * Population2018,
    Population2020 = 1.00561255390388033 * Population2019,
    Population2021 = 1.005425 * Population2020,
    Population2022 = 1.004920 * Population2021,
    Population2023 = 1.004510 * Population2022,
    Population2024 = 1.004220 * Population2023
  ) %>%
  select(shortPostcode, Population2020:Population2024)


Towns = CleanedHousePrices %>%
  left_join(PopulationDataClean, by = "shortPostcode") %>%
  select(shortPostcode, Town, District, County, Population2020, Population2021, Population2022, Population2023, Population2024) %>%
  group_by(shortPostcode) %>%
  filter(row_number() == 1) %>%
  arrange(County) %>%
  ungroup()


write_csv(Towns, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Town_Population.csv")
