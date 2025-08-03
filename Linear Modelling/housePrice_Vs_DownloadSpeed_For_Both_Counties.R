library(tidyverse)

# Define counties to analyze
chosen_counties = c("SOUTH YORKSHIRE", "WEST YORKSHIRE")

# Read datasets
HousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv", show_col_types = FALSE)
BroadbandData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv", show_col_types = FALSE)

# Aggregate average house prices by postcode and county
HouseSummary = HousePrices %>%
  filter(County %in% chosen_counties) %>%
  group_by(shortPostcode, County) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Aggregate average download speeds by postcode and county
BroadbandSummary = BroadbandData %>%
  filter(County %in% chosen_counties) %>%
  group_by(shortPostcode, County) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# Merge datasets on postcode and county
merged_data_cleaned = inner_join(HouseSummary, BroadbandSummary, by = c("shortPostcode", "County"))

# Scatter plot of house price vs download speed with separate regression lines per county
ggplot(merged_data_cleaned, aes(x = avgDownload, y = Price, color = County)) +
  geom_point(alpha = 0.5) +
  scale_y_log10() +  # Log scale on price to handle skewness
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "House Price vs Download Speed by County",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (£)"
  ) +
  theme_minimal(base_size = 14)

# Print linear model summaries and correlation coefficients for each county
for (county in chosen_counties) {
  cat("\n--- Linear Model Summary for", county, "---\n")
  
  df_sub = merged_data_cleaned %>% filter(County == county)
  
  # Fit linear model predicting Price from avgDownload
  model = lm(Price ~ avgDownload, data = df_sub)
  print(summary(model))
  
  # Calculate correlation coefficient between Price and avgDownload
  cor_val = cor(df_sub$Price, df_sub$avgDownload, use = "complete.obs")
  cat("Correlation coefficient:", cor_val, "\n")
}
