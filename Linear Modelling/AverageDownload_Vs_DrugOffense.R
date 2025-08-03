library(tidyverse)
library(ggpubr)

# 1. Load datasets
BroadbandData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv", show_col_types = FALSE)
crime_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv", show_col_types = FALSE)
lsoa_lookup = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv", show_col_types = FALSE)

# 2. Map each crime record to its postcode using LSOA lookup
crime_postcode = crime_data %>%
  left_join(lsoa_lookup %>% select(LSOA_code, shortPostcode), by = "LSOA_code") %>%
  filter(!is.na(shortPostcode))  # Remove records without mapped postcode

# 3. Filter for 2023 drug-related crimes and count occurrences per postcode
drug_crimes_postcode = crime_postcode %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  count(shortPostcode, name = "DrugOffenseCount")

# 4. Join with population info and compute drug offense rate per 10,000 people
crime_with_rate = drug_crimes_postcode %>%
  left_join(
    lsoa_lookup %>%
      select(shortPostcode, Population2023, County) %>%
      distinct(),
    by = "shortPostcode"
  ) %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 10000) %>%
  drop_na()  # Ensure no NA in essential columns

# 5. Filter broadband data for South and West Yorkshire, and average by postcode
broadband_filtered = BroadbandData %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# 6. Merge broadband data with crime rate data on shortPostcode and County
merged_data = inner_join(broadband_filtered, crime_with_rate, by = c("shortPostcode", "County"))

# 7. Scatter plot: Drug Offense Rate vs. Average Download Speed
ggplot(merged_data, aes(x = DrugOffenseRate, y = avgDownload, color = County)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +  # Add linear regression line with CI
  labs(
    title = "Download Speed vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average Download Speed (Mbps)",
    color = "County"
  ) +
  theme_minimal()

# 8. Fit linear model with interaction between County and Drug Offense Rate
lm_model = lm(avgDownload ~ DrugOffenseRate * County, data = merged_data)
summary(lm_model)

# 9. Compute overall correlation coefficient (Download Speed vs Drug Offense Rate)
overall_cor = cor(merged_data$avgDownload, merged_data$DrugOffenseRate, use = "complete.obs")
cat("Overall Correlation Coefficient:", round(overall_cor, 3), "\n")

# 10. Compute separate correlation coefficients for each county
county_correlations = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avgDownload, DrugOffenseRate))
print(county_correlations)
