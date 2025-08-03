library(tidyverse)
library(broom)
library(ggpubr)

# Read in cleaned datasets for school performance, crime records, and LSOA mappings
attainment_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv", show_col_types = FALSE)
crime_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv", show_col_types = FALSE)
Lsoa = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv", show_col_types = FALSE)

# Map each crime record to a short postcode using LSOA
crime_postcode = crime_data %>%
  left_join(Lsoa %>% select(LSOA_code, shortPostcode), by = "LSOA_code") %>%
  filter(!is.na(shortPostcode))

# Filter for drug-related offenses in 2023 and count the number of such crimes per short postcode
drug_crimes_postcode = crime_postcode %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  count(shortPostcode, name = "DrugOffenseCount")

# Join crime counts with population data to compute drug offense rate per 10,000 people
crime_with_rate = drug_crimes_postcode %>%
  left_join(Lsoa %>%
              select(shortPostcode, Population2023, County) %>%
              distinct(), by = "shortPostcode") %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 10000) %>%
  drop_na()

# Filter attainment scores from the 2021–2022 academic year and retain relevant columns
attainment_filtered = attainment_data %>%
  filter(Year == "2021-2022", County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(shortPostcode, County, ATT8SCR) %>%
  distinct()

# Merge drug offense rate with school attainment score using postcode as the join key
merged_data = inner_join(attainment_filtered, crime_with_rate, by = "shortPostcode") %>%
  rename(County = County.x) %>%
  filter(!is.na(ATT8SCR), !is.na(DrugOffenseRate))

# Scatter plot of Attainment 8 Scores vs Drug Offense Rate, colored by county
ggplot(merged_data, aes(x = ATT8SCR, y = DrugOffenseRate, color = County)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Attainment 8 Score",
    color = "County"
  ) +
  theme_minimal()

# Linear regression model including interaction between drug offense rate and county
lm_model_by_county = lm(ATT8SCR ~ DrugOffenseRate * County, data = merged_data)
summary(lm_model_by_county)

# Calculate and display overall correlation coefficient
overall_cor = cor(merged_data$ATT8SCR, merged_data$DrugOffenseRate, use = "complete.obs")
cat("Overall Correlation Coefficient:", round(overall_cor, 3), "\n")

# Compute correlation coefficients separately for each county
county_correlations = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(ATT8SCR, DrugOffenseRate))
print(county_correlations)
