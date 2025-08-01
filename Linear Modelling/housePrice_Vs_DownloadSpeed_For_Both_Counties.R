library(tidyverse)

#House Price vs Download Speed for both Counties in single diagram (include linear model summary report and correlation) 

chosen_counties = c("SOUTH YORKSHIRE", "WEST YORKSHIRE")

HousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv", show_col_types = FALSE)
BroadbandData = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv", show_col_types = FALSE)

HouseSummary = HousePrices %>%
  filter(County %in% chosen_counties) %>%
  group_by(shortPostcode, County) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop")

BroadbandSummary = BroadbandData %>%
  filter(County %in% chosen_counties) %>%
  group_by(shortPostcode, County) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

merged_data_cleaned = inner_join(HouseSummary, BroadbandSummary, by = c("shortPostcode", "County"))

# Basic scatter plot with linear regression lines per County
ggplot(merged_data_cleaned, aes(x = avgDownload, y = Price, color = County)) +
  geom_point(alpha = 0.5) +
  scale_y_log10()+
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "House Price vs Download Speed by County",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (£)"
  ) +
  theme_minimal(base_size = 14)


for (county in chosen_counties) {
  cat("\n--- Linear Model Summary for", county, "---\n")
  df_sub = merged_data_cleaned %>% filter(County == county)
  model = lm(Price ~ avgDownload, data = df_sub)
  print(summary(model))
  cor_val = cor(df_sub$Price, df_sub$avgDownload)
  cat("Correlation coefficient:", cor_val, "\n")
}
