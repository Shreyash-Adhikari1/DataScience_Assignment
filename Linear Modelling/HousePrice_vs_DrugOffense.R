library(tidyverse)
library(ggpubr)

# Load house price data
house_prices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv", show_col_types = FALSE)

# Load crime data
crime_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv", show_col_types = FALSE)

# Load LSOA lookup data to map crime data to postcodes
lsoa_lookup = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv", show_col_types = FALSE)

# Join crime data with postcode info, filtering out rows with missing postcode
crime_postcode = crime_data %>%
  left_join(lsoa_lookup %>% select(LSOA_code, shortPostcode), by = "LSOA_code") %>%
  filter(!is.na(shortPostcode))

# Filter drug crimes for the year 2023, and count number of drug offenses per postcode
drug_crimes_postcode = crime_postcode %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  count(shortPostcode, name = "DrugOffenseCount")

# Join drug offense counts with population and county info, calculate offense rate per 10,000 people
crime_with_rate = drug_crimes_postcode %>%
  left_join(lsoa_lookup %>%
              select(shortPostcode, Population2023, County) %>%
              distinct(), by = "shortPostcode") %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 10000) %>%
  drop_na() # remove rows with missing values

# Filter house price data for 2023 and relevant counties, then calculate average house price by postcode and county
house_filtered = house_prices %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(shortPostcode, County, Price) %>%
  group_by(shortPostcode, County) %>%
  summarise(AvgHousePrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Merge house prices with drug offense rates by postcode and county
merged_data = inner_join(house_filtered, crime_with_rate, by = c("shortPostcode", "County"))

# Plot scatter plot: Drug Offense Rate (x) vs Average House Price (y), colored by county, with regression lines
ggplot(merged_data, aes(x = DrugOffenseRate, y = AvgHousePrice, color = County)) +
  geom_point(size = 3, alpha = 0.7) +             # Points with some transparency
  geom_smooth(method = "lm", se = TRUE) +         # Linear regression line with confidence interval
  labs(
    title = "Average House Price vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average House Price",
    color = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +    # Format y axis with commas for thousands
  theme_minimal()                                 # Clean minimal theme

# Fit linear model with interaction term to test effect of drug offense rate on house price, differing by county
lm_model = lm(AvgHousePrice ~ DrugOffenseRate * County, data = merged_data)

# Print summary of linear model to console
summary(lm_model)

# Calculate and print overall Pearson correlation coefficient between house price and drug offense rate
overall_cor = cor(merged_data$AvgHousePrice, merged_data$DrugOffenseRate, use = "complete.obs")
cat("Correlation Between Variables (Overall):", round(overall_cor, 3), "\n")

# Calculate and print correlation coefficient grouped by County
county_correlations = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(AvgHousePrice, DrugOffenseRate))
print(county_correlations)

