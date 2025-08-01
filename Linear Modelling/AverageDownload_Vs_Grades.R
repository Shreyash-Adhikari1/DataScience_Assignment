# Load libraries
library(tidyverse)
library(ggpubr)

# Read datasets
BroadbandData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv", show_col_types = FALSE)
SchoolPerf = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv", show_col_types = FALSE)

# Step 1: Find latest year
latest_year = SchoolPerf %>%
  filter(!is.na(Year)) %>%
  distinct(Year) %>%
  arrange(desc(Year)) %>%
  slice(1) %>%
  pull(Year)

cat("Latest Year found:", latest_year, "\n")

# Step 2: Filter school data for latest year
ks4_latest = SchoolPerf %>% filter(Year == latest_year)

# Step 3: Aggregate school data to ensure unique keys (avoid many-to-many join)
ks4_latest = ks4_latest %>%
  group_by(shortPostcode, County) %>%
  summarise(ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Step 4: Join datasets on shortPostcode and County
merged_data = inner_join(BroadbandData, ks4_latest, by = c("shortPostcode", "County"))

cat("Merged data dimensions:", dim(merged_data), "\n")

# Step 5: Plot Average Download Speed vs Attainment 8 Score with regression lines by County
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

# Step 6: Linear model with interaction between ATT8SCR and County
model = lm(avgDownload ~ ATT8SCR * County, data = merged_data)
cat("Linear model summary:\n")
print(summary(model))

# Step 7: Calculate correlation coefficients
overall_cor = cor(merged_data$avgDownload, merged_data$ATT8SCR, use = "complete.obs")
cat("Overall Correlation Coefficient:", round(overall_cor, 3), "\n")

cor_by_county = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avgDownload, ATT8SCR, use = "complete.obs")) %>%
  arrange(desc(Correlation))

cat("Correlation by County:\n")
print(cor_by_county)
