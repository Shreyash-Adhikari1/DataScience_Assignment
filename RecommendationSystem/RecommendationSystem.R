# 📦 Load libraries
library(tidyverse)
library(scales)
library(forcats)
library(ggplot2)

# 📂 Load Data
clean_house_prices <- read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")
cleaned_broadband <- read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv")
cleaned_crime <- read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv")
attainment_data <- read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv")
cleaned_LSOA <- read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/Cleaned_LSOA.csv")

# 🧩 STEP 1: Add location info to Crime Data
crime_enriched <- cleaned_crime %>%
  left_join(
    cleaned_LSOA %>% select(LSOA_code, Town, District, County) %>% distinct(),
    by = "LSOA_code"
  )

# 📊 STEP 2: Aggregate

# 2.1 House Prices (2023)
house_prices_summary <- clean_house_prices %>%
  filter(Year == 2023) %>%
  group_by(County, District, Town) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

# 2.2 Broadband
broadband_summary <- cleaned_broadband %>%
  group_by(County, District, Town) %>%
  summarise(AvgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# 2.3 Crime — Drug-related only
crime_summary <- crime_enriched %>%
  filter(CrimeType == "Drugs") %>%
  group_by(County, District, Town) %>%
  summarise(DrugCount = n(), .groups = "drop")

# 2.4 Attainment (2021–2022)
attainment_summary <- attainment_data %>%
  filter(Year == "2021-2022") %>%
  group_by(County, District, Town) %>%
  summarise(AvgAttainment = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# 🔗 STEP 3: Merge all data
merged_all <- house_prices_summary %>%
  inner_join(broadband_summary, by = c("County", "District", "Town")) %>%
  inner_join(crime_summary, by = c("County", "District", "Town")) %>%
  inner_join(attainment_summary, by = c("County", "District", "Town"))

# ⚖️ STEP 4: Normalize + Priority-based Scoring
norm_data <- merged_all %>%
  mutate(
    norm_price = rescale(-AvgPrice),          # Lower = better
    norm_crime = rescale(-DrugCount),         # Lower = better
    norm_attainment = rescale(AvgAttainment), # Higher = better
    norm_download = rescale(AvgDownload)      # Higher = better
  ) %>%
  mutate(
    QualityScore = 0.4 * norm_price +
      0.3 * norm_crime +
      0.2 * norm_attainment +
      0.1 * norm_download
  ) %>%
  arrange(desc(QualityScore))

# 💾 STEP 5: Save full ranked town list
write_csv(norm_data, "C:/Users/ADMIN/Desktop/Data Science Assignment/Recommendation System/Ranked_Towns.csv")

# 🔟 STEP 6: Top 10 Towns
top_10_towns <- norm_data %>%
  slice(1:10) %>%
  mutate(TownLabel = paste0(Town, " (", District, ")")) %>%
  mutate(TownLabel = fct_reorder(TownLabel, QualityScore))

cat("🏆 Top 10 Towns to Live In (Overall Ranking):\n")
print(top_10_towns %>% select(County, District, Town, QualityScore))

# 💾 Save top 10
write_csv(top_10_towns, "C:/Users/ADMIN/Desktop/Data Science Assignment/Recommendation System/Top_10_Towns.csv")

# 📊 STEP 7: Bar Plot
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
