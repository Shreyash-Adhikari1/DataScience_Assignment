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


# Count total crimes per town
crime_summary = crime_enriched %>%
  group_by(County, District, Town) %>%
  summarise(TotalCrimeCount = n(), .groups = "drop")

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
    norm_crime = rescale(-TotalCrimeCount),
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


# Top 10 towns with best internet speed
cat("\nTop 10 Towns with Fastest Average Internet (Download Speed):\n")
merged_all %>%
  arrange(desc(AvgDownload)) %>%
  slice(1:10) %>%
  select(County, District, Town, AvgDownload) %>%
  print()

#Top 10 most affordable towns (lowest house price)
cat("\nTop 10 Most Affordable Towns (Lowest Average House Prices):\n")
merged_all %>%
  arrange(AvgPrice) %>%
  slice(1:10) %>%
  select(County, District, Town, AvgPrice) %>%
  print()

#Top 10 most Safest towns
cat("\nTop 10 Safest Towns (Lowest Total Crime Count):\n")
merged_all %>%
  arrange(TotalCrimeCount) %>%
  slice(1:10) %>%
  select(County, District, Town, TotalCrimeCount) %>%
  print()


# Top 10 towns with highest average attainment score
cat("\nTop 10 Towns with Highest Average Attainment 8 Score:\n")
merged_all %>%
  arrange(desc(AvgAttainment)) %>%
  slice(1:10) %>%
  select(County, District, Town, AvgAttainment) %>%
  print()

# Show the top 10 towns in the console
cat("Top 10 Towns to Live In (Overall Ranking):\n")
print(top_10_towns %>% select(County, District, Town, QualityScore))

# Export the top 10 towns as a separate file
write_csv(top_10_towns, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Scripts\\RecommendationSystem\\Top_10_Towns.csv")

#Assign Ratings To the Towns
top_10_towns = top_10_towns %>%
  mutate(Rating = round(QualityScore * 5, 1))  # Rating out of 5

# Create a horizontal bar plot of the top 10 towns by score

ggplot(top_10_towns, aes(x = TownLabel, y = Rating, fill = County)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(
    title = "Top 10 Towns to Live In",
    subtitle = "Based on Affordability, Crime, Education, and Broadband",
    x = "Town (District)",
    y = "Rating [ 0-5 ]"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 5, 0.5))

