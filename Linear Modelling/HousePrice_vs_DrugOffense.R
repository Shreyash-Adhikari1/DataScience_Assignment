library(tidyverse)
library(ggpubr)


house_prices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv", show_col_types = FALSE)
crime_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv", show_col_types = FALSE)
lsoa_lookup = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv", show_col_types = FALSE)


crime_postcode = crime_data %>%
  left_join(lsoa_lookup %>% select(LSOA_code, shortPostcode), by = "LSOA_code") %>%
  filter(!is.na(shortPostcode))


drug_crimes_postcode = crime_postcode %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  count(shortPostcode, name = "DrugOffenseCount")


crime_with_rate = drug_crimes_postcode %>%
  left_join(lsoa_lookup %>%
              select(shortPostcode, Population2023, County) %>%
              distinct(), by = "shortPostcode") %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 10000) %>%
  drop_na()


house_filtered = house_prices %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(shortPostcode, County, Price) %>%
  group_by(shortPostcode, County) %>%
  summarise(AvgHousePrice = mean(Price, na.rm = TRUE), .groups = "drop")


merged_data = inner_join(house_filtered, crime_with_rate, by = c("shortPostcode", "County"))

ggplot(merged_data, aes(x = DrugOffenseRate, y = AvgHousePrice, color = County)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Average House Price vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average House Price",
    color = "County"
  ) +
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()


lm_model = lm(AvgHousePrice ~ DrugOffenseRate * County, data = merged_data)
summary(lm_model)

overall_cor = cor(merged_data$AvgHousePrice, merged_data$DrugOffenseRate, use = "complete.obs")
cat("Overall Correlation Coefficient:", round(overall_cor, 3), "\n")

county_correlations = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(AvgHousePrice, DrugOffenseRate))
print(county_correlations)
