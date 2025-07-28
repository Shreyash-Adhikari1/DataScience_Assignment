library(tidyverse)
library(scales)

##--------------------------------- House Prices Visualizations [ START ] ------------------------------##

#Reading the Cleaned House Prices csv

HousePrices = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv")

#----------------------------Line Graphs for Average house prices from (2021-2024)----------------------------#

# Calculate average price per District per Year per County
Average_House_Price_Data = HousePrices %>%
  group_by(Year, District, County) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Plot: line graph by District (or County if preferred)
ggplot(Average_House_Price_Data, aes(x = Year, y = AveragePrice, color = District, group = District)) +
  geom_line(size = 1) +
  labs(
    title = "Average House Prices in South and West Yorkshire (2021–2024)",
    subtitle = "By Districts and Year",
    x = "Year",
    y = "Average House Price (£)"
  )+
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-----------------------Bar chart Average house prices of 2023 for both counties-------------------------------#

HousePrices %>%
  filter(Year == 2023) %>%
  group_by(County, District) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  
  mutate(District = fct_reorder(District, AvgPrice)) %>%
  ggplot(aes(x = District, y = AvgPrice, fill = County)) +
  geom_col(position = position_dodge(width = 0.8)) + 
  coord_flip() + 
  labs(title = "Average House Prices in 2023 by District and County",
       x = "District", y = "Average Price (£)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#00BFFF", "WEST YORKSHIRE" = "#FF6F61")) +
  theme_minimal()

#-----------------------Boxplot for average house prices for both counties-------------------------#

# Filter for 2023 and the two counties
HousePrices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(County, District) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = County, y = AvgPrice, fill = County)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  geom_jitter(width = 0.2, color = "black", size = 2) +
  labs(
    title = "Boxplot of Average District House Prices (2021–2024)",
    x = "County",
    y = "Average Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#00BFFF", "WEST YORKSHIRE" = "#FF6F61"))



##--------------------------------- House Prices Visualizations [ END ] ------------------------------##




##---------------------------------- Broadband Data Visualization [ START ] --------------------------##

# Reading the Cleaned Broadband data
Broadband_Data= read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Broadband_Data.csv")


#----------------------------------  Box Plot for Average Download Speed by District ------------------#

Broadband_Data_filtered <- Broadband_Data %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

ggplot(Broadband_Data_filtered, aes(x = reorder(District, avgDownload, median), y = avgDownload, fill = County)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  facet_wrap(~County, scales = "free_x") +
  labs(
    title = "Boxplots of Average Download Speeds by District",
    subtitle = "Separated by County",
    x = "District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#00BFFF", "WEST YORKSHIRE" = "#FF6F61"))


#---------------------------------  Bar Charts for Town vs Average Download Speed ---------------------#

# Summarize avgDownload per Town and County

Broadband_Summary = Broadband_Data %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(County, Town) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# Create filtered data-sets
SouthYorkshire = Broadband_Summary %>% filter(County == "SOUTH YORKSHIRE")
WestYorkshire  = Broadband_Summary %>% filter(County == "WEST YORKSHIRE")


#------------------------- SOUTH YORKSHIRE bar chart ------------------------#

ggplot(SouthYorkshire, aes(x = reorder(Town, avgDownload), y = avgDownload)) +
  geom_col(fill = "#00BFFF", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average Download Speed by Town (South Yorkshire)",
    x = "Town",
    y = "Average Download Speed (Mbps)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 10))

#------------------------- WEST YORKSHIRE bar chart ------------------------#

ggplot(WestYorkshire, aes(x = reorder(Town, avgDownload), y = avgDownload)) +
  geom_col(fill = "#FF6F61", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average Download Speed by Town (West Yorkshire)",
    x = "Town",
    y = "Average Download Speed (Mbps)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 10))


