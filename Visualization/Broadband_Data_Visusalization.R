library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)

##---------------------------------- Broadband Data Visualization [ START ] --------------------------##

# Reading the cleaned broadband dataset
Broadband_Data = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Broadband_Data.csv")

#----------------------------------  Box Plot for Average Download Speed by District ------------------#

# Filter the dataset for only South and West Yorkshire counties
Broadband_Data_filtered = Broadband_Data %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Create a boxplot to compare average download speeds across districts, faceted by county
ggplot(Broadband_Data_filtered, aes(x = reorder(District, avgDownload, median), y = avgDownload, fill = County)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +  # Boxplot with red outliers
  facet_wrap(~County, scales = "free_x") +            # Create separate plots for each county
  labs(
    title = "Average Download Speeds Across Districts by County",
    subtitle = "Comparison of Median Download Speeds by District in South Yorkshire and West Yorkshire",
    x = "District (ordered by median download speed)",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#00BFFF", "WEST YORKSHIRE" = "#FF6F61"))

#---------------------------------  Bar Charts for Town vs Average Download Speed ---------------------#

# Group and summarize the average download speed per town and county
Broadband_Summary = Broadband_Data %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(County, Town) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# Split the summary into separate datasets for each county
SouthYorkshire = Broadband_Summary %>% filter(County == "SOUTH YORKSHIRE")
WestYorkshire  = Broadband_Summary %>% filter(County == "WEST YORKSHIRE")

#------------------------- SOUTH YORKSHIRE bar chart ------------------------#

# Create a vertical bar chart for South Yorkshire towns
ggplot(SouthYorkshire, aes(x = reorder(Town, avgDownload), y = avgDownload)) +
  geom_col(fill = "#00BFFF", alpha = 0.8) +
  labs(
    title = "Average Download Speeds by Town — South Yorkshire",
    x = "Town (ordered by download speed)",
    y = "Average Download Speed (Mbps)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 10))

#------------------------- WEST YORKSHIRE bar chart ------------------------#

# Create a horizontal bar chart for West Yorkshire towns
ggplot(WestYorkshire, aes(x = reorder(Town, avgDownload), y = avgDownload)) +
  geom_col(fill = "#FF6F61", alpha = 0.8) +
  coord_flip() +  # Flip coordinates to make bars horizontal
  labs(
    title = "Average Download Speeds by Town — West Yorkshire",
    x = "Town (ordered by download speed)",
    y = "Average Download Speed (Mbps)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 10))

##---------------------------------- Broadband Data Visualization [ END ] --------------------------##
