library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)

##----------------------------------- School Performance Data Visualization [ Start ] ---------------------------##

# Reading the cleaned school performance dataset
SchoolPerformances = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/Cleaned_School_Performance.csv")


#-------------------------- Boxplot for Distribution of Attainment 8 Scores by District [South Yorkshire 2022]---------------------------#

# Filter data for South Yorkshire schools for academic year 2021–2022
school_data_2022_SY = SchoolPerformances %>%
  filter(County == "SOUTH YORKSHIRE", Year == "2021-2022") %>%
  filter(!is.na(ATT8SCR))

# Create boxplot to visualize distribution of Attainment 8 scores across districts
ggplot(school_data_2022_SY, aes(x = District, y = ATT8SCR, fill = District)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Variation in Attainment 8 Scores by District (South Yorkshire, 2021–2022)",
    x = "District",
    y = "Attainment 8 Score"
  )+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")


#-------------------------- Boxplot for Distribution of Attainment 8 Scores by District [West Yorkshire 2022]---------------------------#

# Filter data for West Yorkshire schools for academic year 2021–2022
school_data_2022_WY = SchoolPerformances %>%
  filter(County == "WEST YORKSHIRE", Year == "2021-2022") %>%
  filter(!is.na(ATT8SCR))

# Create boxplot to show how attainment scores vary between districts
ggplot(school_data_2022_WY, aes(x = District, y = ATT8SCR, fill = District)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Variation in Attainment 8 Scores by District (West Yorkshire, 2021–2022)",
    x = "District",
    y = "Attainment 8 Score"
  )+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")


#--------------------------------------- Line Graph for Attainment 8 Score Trend Across Years by District -------------------##

# Prepare data for trend analysis: filter valid scores and convert columns
attainment_trend = SchoolPerformances %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  filter(!ATT8SCR %in% c("SUPP", "NE"), !is.na(ATT8SCR)) %>%
  mutate(
    ATT8SCR = as.numeric(ATT8SCR),
    Year = factor(Year, levels = c("2021-2022", "2022-2023", "2023-2024")),
    Label = paste(District, "(", County, ")")
  )

# Generate a color for each district–county combination
custom_colors = scales::hue_pal()(length(unique(attainment_trend$Label)))

# Create line graph showing how average Attainment 8 scores have changed over time
ggplot(attainment_trend, aes(x = Year, y = ATT8SCR, group = Label, color = Label)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  facet_wrap(~ County, scales = "free_x") +
  labs(
    title = "Attainment 8 Score Trends by District (2021–2024)",
    subtitle = "Average Performance Across South and West Yorkshire Districts",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    color = "District (County)"
  )+
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = 10),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

##----------------------------------- School Performance Data Visualization [ END ] ---------------------------##
