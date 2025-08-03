library(tidyverse)

# Load cleaned datasets
house_prices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv", show_col_types = FALSE)
att8 = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv", show_col_types = FALSE)

# Extract the numeric year from the academic year string in ATT8 data (e.g., "2021-2022" → 2021)
att8 = att8 %>%
  mutate(Year = as.numeric(str_sub(Year, 1, 4)))

# Aggregate house prices by shortPostcode, year, and county (mean price)
house_prices_agg = house_prices %>%
  group_by(shortPostcode, Year, County) %>%
  summarise(Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Aggregate school performance scores (mean ATT8 score per postcode and year)
att8_agg = att8 %>%
  group_by(shortPostcode, Year) %>%
  summarise(ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Merge the two datasets on postcode and year, keep only the two target counties
house_pricesXatt8 = house_prices_agg %>%
  inner_join(att8_agg, by = c("shortPostcode", "Year")) %>%
  rename(County = County) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Remove top 5% of house price values to reduce outlier effects in scatter plot
house_pricesXatt8_no_outliers = house_pricesXatt8 %>%
  filter(Price < quantile(Price, 0.95, na.rm = TRUE))

# Scatter plot: ATT8 score vs average house price (log-scaled), with regression line
ggplot(house_pricesXatt8_no_outliers, aes(x = ATT8SCR, y = Price, color = County)) +
  geom_point(alpha = 0.7) +
  scale_y_log10() +  # Log scale helps visualize wide price ranges
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Attainment 8 Score vs House Price (Both Counties, No Outliers)",
    x = "Average Attainment 8 Score",
    y = "Average House Price"
  ) +
  theme_minimal()

# Linear model: log-transformed house price ~ ATT8 score
model_linear = lm(log10(Price) ~ ATT8SCR, data = house_pricesXatt8_no_outliers)
summary(model_linear)

# Overall correlation between ATT8 score and house price
cor_overall = cor(house_pricesXatt8_no_outliers$ATT8SCR, house_pricesXatt8_no_outliers$Price, use = "complete.obs")
print(paste("Overall Correlation:", cor_overall))

# Correlation split by county
cor_south = cor(filter(house_pricesXatt8_no_outliers, County == "SOUTH YORKSHIRE")$ATT8SCR, 
                filter(house_pricesXatt8_no_outliers, County == "SOUTH YORKSHIRE")$Price, 
                use = "complete.obs")
cor_west = cor(filter(house_pricesXatt8_no_outliers, County == "WEST YORKSHIRE")$ATT8SCR, 
               filter(house_pricesXatt8_no_outliers, County == "WEST YORKSHIRE")$Price, 
               use = "complete.obs")
print(paste("South Yorkshire Correlation:", cor_south))
print(paste("West Yorkshire Correlation:", cor_west))
