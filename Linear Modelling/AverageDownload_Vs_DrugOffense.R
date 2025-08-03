library(tidyverse)
library(ggpubr)

# Load datasets
BroadbandData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv", show_col_types = FALSE)
crime_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv", show_col_types = FALSE)
lsoa_lookup = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv", show_col_types = FALSE)

# Link each crime record to its corresponding postcode using the LSOA lookup
crime_postcode = crime_data %>%
  left_join(lsoa_lookup %>% select(LSOA_code, shortPostcode), by = "LSOA_code") %>%
  filter(!is.na(shortPostcode))  # Exclude records without postcode info

# Filter drug-related crimes for the year 2023 and count incidents per postcode
drug_crimes_postcode = crime_postcode %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  count(shortPostcode, name = "DrugOffenseCount")

# Merge in population data and calculate drug offense rate per 10,000 people
crime_with_rate = drug_crimes_postcode %>%
  left_join(
    lsoa_lookup %>%
      select(shortPostcode, Population2023, County) %>%
      distinct(),
    by = "shortPostcode"
  ) %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 10000) %>%
  drop_na()

# Compute average broadband download speed per postcode, filtering for the two counties
broadband_filtered = BroadbandData %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# Join broadband and crime data by postcode and county
merged_data = inner_join(broadband_filtered, crime_with_rate, by = c("shortPostcode", "County"))

# Plot the relationship between drug offense rate and download speed
ggplot(merged_data, aes(x = DrugOffenseRate, y = avgDownload, color = County)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Download Speed vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average Download Speed (Mbps)",
    color = "County"
  ) +
  theme_minimal()

# Fit a linear model with interaction between county and drug offense rate
lm_model = lm(avgDownload ~ DrugOffenseRate * County, data = merged_data)
summary(lm_model)

# Calculate the overall correlation between download speed and drug offense rate
overall_cor = cor(merged_data$avgDownload, merged_data$DrugOffenseRate, use = "complete.obs")
cat("Correlation Between Variables (Overall)t:", round(overall_cor, 3), "\n")

# Compute correlation coefficients separately for each county
county_correlations = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avgDownload, DrugOffenseRate))
print(county_correlations)
