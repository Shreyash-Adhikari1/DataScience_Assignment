library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)

##--------------------------------- House Prices Visualizations [ START ] ------------------------------##

# Reading the cleaned house price dataset
HousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")

#----------------------------Line Graphs for Average house prices from (2021-2024)----------------------------#

# Calculate average house price grouped by Year, District, and County
Average_House_Price_Data = HousePrices %>%
  group_by(Year, District, County) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Plot line graph showing trend of house prices over years by district
ggplot(Average_House_Price_Data, aes(x = Year, y = AveragePrice, color = District, group = District)) +
  geom_line(size = 1) +
  labs(
    title = "Trends in Average House Prices by District (2021–2024)",
    subtitle = "Comparison of District-Level Prices Across South and West Yorkshire",
    x = "Year",
    y = "Average House Price (£)"
  )+
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-----------------------Bar chart Average house prices of 2023 for both counties-------------------------------#

# Filter data for the year 2023 and compute average price per district
HousePrices %>%
  filter(Year == 2023) %>%
  group_by(County, District) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  
  # Reorder districts based on average price
  mutate(District = fct_reorder(District, AvgPrice)) %>%
  
  # Create bar chart comparing districts across both counties
  ggplot(aes(x = District, y = AvgPrice, fill = County)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(
    title = "Average House Prices by District and County in 2023",
    x = "District",
    y = "Average House Price (£)"
  )+
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#00BFFF", "WEST YORKSHIRE" = "#FF6F61")) +
  theme_minimal()


#-----------------------Box-plot for average house prices for both counties-------------------------#

# Calculate average house prices per district by year and county

avg_house_prices <- HousePrices %>%
  group_by(Year, District, County) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")


# Boxplot for South Yorkshire: Distribution of average house prices by district

average_Price_SY <- avg_house_prices %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District, y = AveragePrice, fill = District)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(
    title = "Distribution of Average House Prices by District — South Yorkshire",
    x = "District",
    y = "Average House Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



# Boxplot for West Yorkshire: Distribution of average house prices by district


average_Price_WY <- avg_house_prices %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District, y = AveragePrice, fill = District)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(
    title = "Distribution of Average House Prices by District — West Yorkshire",
    x = "District",
    y = "Average House Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Display plots separately
print(average_Price_SY)
print(average_Price_WY)


##--------------------------------- House Prices Visualizations [ END ] ------------------------------##
