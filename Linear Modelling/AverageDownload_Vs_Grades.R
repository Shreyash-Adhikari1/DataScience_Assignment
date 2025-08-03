# Load libraries
library(tidyverse)
library(ggpubr)

# Read datasets
BroadbandData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv", show_col_types = FALSE)
SchoolPerf = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv", show_col_types = FALSE)

# Find the most recent academic year available in school performance data
latest_year = SchoolPerf %>%
  filter(!is.na(Year)) %>%
  distinct(Year) %>%
  arrange(desc(Year)) %>%
  slice(1) %>%
  pull(Year)

cat("Latest Year found:", latest_year, "\n")

# Filter school performance data to only include records from the latest year
RecentAttainmentData = SchoolPerf %>% filter(Year == latest_year)

# Aggregate school performance scores to postcode level to avoid many-to-many joins
RecentAttainmentData = RecentAttainmentData %>%
  group_by(shortPostcode, County) %>%
  summarise(ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Merge broadband data with school performance data using shortPostcode and County
merged_data = inner_join(BroadbandData, RecentAttainmentData, by = c("shortPostcode", "County"))

cat("Merged data dimensions:", dim(merged_data), "\n")

# Create scatterplot showing Attainment 8 Score vs Average Download Speed, with regression lines
Net_Vs_Score = ggplot(merged_data, aes(x = ATT8SCR, y = avgDownload, color = County)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = paste("Average Download Speed vs Attainment 8 Score (", latest_year, ")", sep = ""),
    x = "Attainment 8 Score",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

print(Net_Vs_Score)

# Fit linear model including interaction between Attainment 8 Score and County
model = lm(avgDownload ~ ATT8SCR * County, data = merged_data)
cat("Linear model summary:\n")
print(summary(model))

# Compute overall and per-county correlation between broadband speed and Attainment 8 Score
overall_cor = cor(merged_data$avgDownload, merged_data$ATT8SCR, use = "complete.obs")
cat("Correlation Between Variables (Overall):", round(overall_cor, 3), "\n")

cor_by_county = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avgDownload, ATT8SCR, use = "complete.obs")) %>%
  arrange(desc(Correlation))

cat("Correlation by County:\n")
print(cor_by_county)
