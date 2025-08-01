library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)

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

#-----------------------Box-plot for average house prices for both counties-------------------------#

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



##----------------------------------- Crime Data Visualization [ Start ] ---------------------------##


#Reading the necessary data-sets
crime_data = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/CleanedCrimeData.csv")
LSOA_Pop = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/Cleaned_LSOA.csv")



#Box plot for Drug offense rate in the district of both counties (two separate diagrams). Take variables (drug offense rate vs District) 

drug_crime_data = crime_data %>%
  filter(CrimeType == "Drugs") %>%
  rename(LSOA_code = LSOA_code) %>% 
  group_by(LSOA_code) %>%
  summarise(DrugOffenseCount = n(), .groups = "drop")


drug_crime_rates = drug_crime_data %>%
  left_join(LSOA_Pop, by = "LSOA_code") %>%
  filter(!is.na(Population2024), Population2024 > 0) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 1000)

south_yorkshire = drug_crime_rates %>% filter(County == "SOUTH YORKSHIRE")
west_yorkshire = drug_crime_rates %>% filter(County == "WEST YORKSHIRE")


SY_Drug_Offense = ggplot(south_yorkshire, aes(x = District, y = DrugOffenseRate)) +
  geom_boxplot(fill = "#1b7837", alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Drug Offense Rate per LSOA in South Yorkshire (per 1,000 people)",
    x = "District", y = "Drug Offense Rate (per 1,000)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

WY_Drug_Offense = ggplot(west_yorkshire, aes(x = District, y = DrugOffenseRate)) +
  geom_boxplot(fill = "#762a83", alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Drug Offense Rate per LSOA in West Yorkshire (per 1,000 people)",
    x = "District", y = "Drug Offense Rate (per 1,000)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(SY_Drug_Offense)
print(WY_Drug_Offense)


#------------------------------ Radar Chart for vehicle crime rate in South Yorkshire-2024-06 -----------------------------#

# Parameters 
chosen_county = "SOUTH YORKSHIRE"
chosen_year = "2024"
chosen_month = "06"


vehicle_crimes = crime_data %>%
  filter(CrimeType %in% c("Vehicle crime")) %>%
  filter(str_starts(Month, paste0(chosen_year, "-", chosen_month))) %>%
  left_join(LSOA_Pop, by = c("LSOA_code" = "LSOA_code")) %>%
  filter(County == chosen_county) %>%
  group_by(District) %>%
  summarise(
    VehicleCrimeCount = n(),
    Population = mean(Population2023, na.rm = TRUE),
    VehicleCrimeRate = (VehicleCrimeCount / Population) * 1000,
    .groups = "drop"
  )
print(paste("Number of districts:", n_distinct(vehicle_crimes$District)))


max_rate = max(vehicle_crimes$VehicleCrimeRate, na.rm = TRUE)
min_rate = 0

radar_data = vehicle_crimes %>%
  select(District, VehicleCrimeRate) %>%
  column_to_rownames("District") %>%
  t() %>%
  as.data.frame()


radar_data = rbind(
  max = rep(max_rate, ncol(radar_data)),
  min = rep(min_rate, ncol(radar_data)),
  radar_data
)


colors_border = scales::hue_pal()(ncol(radar_data))

fmsb::radarchart(
  radar_data,
  pcol = colors_border,
  pfcol = scales::alpha(colors_border, 0.3),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(min_rate, max_rate, length.out = 5),
  calcex = 0.7,
  vlcex = 0.8,
  title = paste("Vehicle Crime Rate in", chosen_county, "-",chosen_month,"-", chosen_year)
)


#------------------------ Pie Chart for Robbery Rate ---------------------------------#

# Parameters
chosen_county = "WEST YORKSHIRE"
chosen_year = "2023"
chosen_month = "08"  


robbery_data = crime_data %>%
  filter(CrimeType == "Robbery") %>%
  filter(str_starts(Month, paste0(chosen_year, "-", chosen_month))) %>%
  left_join(LSOA_Pop, by = c("LSOA_code" = "LSOA_code")) %>%
  filter(County == chosen_county) %>%
  group_by(District) %>%
  summarise(
    RobberyCount = n(),
    Population = mean(Population2023, na.rm = TRUE),
    RobberyRate = (RobberyCount / Population) * 1000,
    .groups = "drop"
  ) %>%
  filter(RobberyRate > 0)


robbery_data = robbery_data %>%
  mutate(Percent = RobberyRate / sum(RobberyRate) * 100,
         Label = paste0(District, "\n", round(Percent, 1), "%"))

ggplot(robbery_data, aes(x = "", y = RobberyRate, fill = District)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = paste("Robbery Rate Proportions by District in", chosen_county, "-", month.name[as.integer(chosen_month)], chosen_year),
    fill = "District"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#-----------------------------------------  Drug Offense Rates (per 10,000 people)  Both Counties ----------------------------#

drug_offense_yearly = crime_data %>%
  filter(CrimeType == "Drugs") %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(LSOA_code, Year) %>%
  summarise(DrugOffenseCount = n(), .groups = "drop")


drug_rates = drug_offense_yearly %>%
  left_join(LSOA_Pop, by = c("LSOA_code" = "LSOA_code")) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  filter(!is.na(Population2023), Population2023 > 0) %>%
  group_by(County, Year) %>%
  summarise(
    TotalDrugOffenseCount = sum(DrugOffenseCount),
    TotalPopulation = sum(Population2023),
    .groups = "drop"
  ) %>%
  mutate(
    DrugOffenseRate_per_10000 = (TotalDrugOffenseCount / TotalPopulation) * 10000
  ) %>%
  arrange(Year)


ggplot(drug_rates, aes(x = Year, y = DrugOffenseRate_per_10000, color = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Drug Offense Rates per 10,000 People",
    x = "Year",
    y = "Drug Offense Rate (per 10,000)",
    color = "County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

##----------------------------------- Crime Data Visualization [ END ] ---------------------------##




##----------------------------------- School Performance Data Visualization [ Start ] ---------------------------##

# Reading The School Performances Data
SchoolPerformances = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/Cleaned_School_Performance.csv")


#-------------------------- Boxplot for Distribution of Attainment 8 Scores by District [South Yorkshire 2022]---------------------------#

# Filter for South Yorkshire and Year 2022
school_data_2022_SY = SchoolPerformances %>%
  filter(County == "SOUTH YORKSHIRE", Year == "2021-2022") %>%
  filter(!is.na(ATT8SCR))


# Create the box-plot
ggplot(school_data_2022_SY, aes(x = District, y = ATT8SCR, fill = District)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Distribution of Attainment 8 Scores by District (South Yorkshire, 2022)",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")



#-------------------------- Boxplot for Distribution of Attainment 8 Scores by District [West Yorkshire 2022]---------------------------#

#filter for West Yorkshire 2022
school_data_2022_WY = SchoolPerformances %>%
  filter(County == "WEST YORKSHIRE", Year == "2021-2022") %>%
  filter(!is.na(ATT8SCR))


# Create the box-plot
ggplot(school_data_2022_WY, aes(x = District, y = ATT8SCR, fill = District)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Distribution of Attainment 8 Scores by District (West Yorkshire, 2022)",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")


#--------------------------------------- Line Graph for Attainment 8 Score Trend Across Years by District -------------------##
# Prepare and clean data
attainment_trend = SchoolPerformances %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  filter(!ATT8SCR %in% c("SUPP", "NE"), !is.na(ATT8SCR)) %>%
  mutate(
    ATT8SCR = as.numeric(ATT8SCR),
    Year = factor(Year, levels = c("2021-2022", "2022-2023", "2023-2024")),
    Label = paste(District, "(", County, ")")
  )

custom_colors = scales::hue_pal()(length(unique(attainment_trend$Label)))

# Create the plot
ggplot(attainment_trend, aes(x = Year, y = ATT8SCR, group = Label, color = Label)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  facet_wrap(~ County, scales = "free_x") +
  labs(
    title = "Attainment 8 Score Trend Across Years by District",
    subtitle = "Comparison of average scores in South and West Yorkshire",
    x = "Academic Year",
    y = "Average Attainment 8 Score",
    color = "District (County)"
  ) +
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




