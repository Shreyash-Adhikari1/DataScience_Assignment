library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)

##---------------------------------- Broadband Data Visualization [ START ] --------------------------##

# Reading the Cleaned Broadband data
Broadband_Data= read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Broadband_Data.csv")


#----------------------------------  Box Plot for Average Download Speed by District ------------------#

Broadband_Data_filtered = Broadband_Data %>%
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
  coord_flip()+
  labs(
    title = "Average Download Speed by Town (West Yorkshire)",
    x = "Town",
    y = "Average Download Speed (Mbps)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 10))

##---------------------------------- Broadband Data Visualization [ END ] --------------------------##