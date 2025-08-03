# Load libraries for data manipulation and plotting
library(tidyverse)
library(scales)
library(forcats)
library(ggplot2)

# Load datasets from local paths
clean_house_prices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")
cleaned_broadband = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv")
cleaned_crime = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv")
attainment_data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv")
cleaned_LSOA = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/Cleaned_LSOA.csv")

# Enrich crime data with location information from LSOA lookup
crime_enriched = cleaned_crime %>%
  left_join(
    cleaned_LSOA %>% select(LSOA_code, Town, District, County) %>% distinct(),
    by = "LSOA_code"
  )

# Calculate average house prices for 2023 by location
house_prices_summary = clean_house_prices %>%
  filter(Year == 2023) %>%
  group_by(County, District, Town) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Compute average broadband speed per town
broadband_summary = cleaned_broadband %>%
  group_by(County, District, Town) %>%
  summarise(AvgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# Count drug-related crimes per town
crime_summary = crime_enriched %>%
  filter(CrimeType == "Drugs") %>%
  group_by(County, District, Town) %>%
  summarise(DrugCount = n(), .groups = "drop")

# Calculate average Attainment 8 scores for the most recent year available
attainment_summary = attainment_data %>%
  filter(Year == "2021-2022") %>%
  group_by(County, District, Town) %>%
  summarise(AvgAttainment = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Combine all metrics into a single dataset for scoring
merged_all = house_prices_summary %>%
  inner_join(broadband_summary, by = c("County", "District", "Town")) %>%
  inner_join(crime_summary, by = c("County", "District", "Town")) %>%
  inner_join(attainment_summary, by = c("County", "District", "Town"))

# Normalize metrics and compute a weighted score reflecting priority of factors
norm_data = merged_all %>%
  mutate(
    norm_price = rescale(-AvgPrice),
    norm_crime = rescale(-DrugCount),
    norm_attainment = rescale(AvgAttainment),
    norm_download = rescale(AvgDownload)
  ) %>%
  mutate(
    QualityScore = 0.4 * norm_price +
      0.3 * norm_crime +
      0.2 * norm_attainment +
      0.1 * norm_download
  ) %>%
  arrange(desc(QualityScore))

# Save the full ranked list of towns
write_csv(norm_data, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Scripts\\RecommendationSystem\\Ranked_Towns.csv")

# Prepare top 10 towns based on quality score for visualization and reporting
top_10_towns = norm_data %>%
  slice(1:10) %>%
  mutate(TownLabel = paste0(Town, " (", District, ")")) %>%
  mutate(TownLabel = fct_reorder(TownLabel, QualityScore))

# Show the top 10 towns in the console
cat("Top 10 Towns to Live In (Overall Ranking):\n")
print(top_10_towns %>% select(County, District, Town, QualityScore))

# Export the top 10 towns as a separate file
write_csv(top_10_towns, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Scripts\\RecommendationSystem\\Top_10_Towns.csv")

# Create a horizontal bar plot of the top 10 towns by score
ggplot(top_10_towns, aes(x = TownLabel, y = QualityScore, fill = County)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(
    title = "Top 10 Towns to Live In (Based on Affordability, Crime, Education, and Broadband)",
    x = "Town (District)",
    y = "Overall Quality Score"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
