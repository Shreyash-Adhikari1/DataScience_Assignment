library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)

##----------------------------------- Crime Data Visualization [ Start ] ---------------------------##

# Reading the cleaned crime and LSOA population datasets
crime_data = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/CleanedCrimeData.csv")
LSOA_Pop = read_csv("C:/Users/ADMIN/Desktop/Data Science Assignment/Cleaned Data/Cleaned_LSOA.csv")

# Box plot for drug offense rate across districts in both counties

# Filter for drug-related crimes and count occurrences by LSOA
drug_crime_data = crime_data %>%
  filter(CrimeType == "Drugs") %>%
  rename(LSOA_code = LSOA_code) %>%
  group_by(LSOA_code) %>%
  summarise(DrugOffenseCount = n(), .groups = "drop")

# Join with population data and calculate offense rate per 1,000 people
drug_crime_rates = drug_crime_data %>%
  left_join(LSOA_Pop, by = "LSOA_code") %>%
  filter(!is.na(Population2024), Population2024 > 0) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 1000)

# Split into separate datasets for plotting
south_yorkshire = drug_crime_rates %>% filter(County == "SOUTH YORKSHIRE")
west_yorkshire = drug_crime_rates %>% filter(County == "WEST YORKSHIRE")

# Boxplot for South Yorkshire
ggplot(south_yorkshire, aes(x = District, y = DrugOffenseRate)) +
  geom_boxplot(fill = "#1b7837", alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Drug Offense Rates by District in South Yorkshire (per 1,000 Residents)",
    x = "District", y = "Drug Offense Rate (per 1,000)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for West Yorkshire
ggplot(west_yorkshire, aes(x = District, y = DrugOffenseRate)) +
  geom_boxplot(fill = "#762a83", alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Drug Offense Rates by District in West Yorkshire (per 1,000 Residents)",
    x = "District", y = "Drug Offense Rate (per 1,000)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------------------------------ Radar Chart for vehicle crime rate in South Yorkshire-2024-06 -----------------------------#

# Define the county, year, and month to filter
county_taken_SY = "SOUTH YORKSHIRE"
year_taken_SY = "2024"
month_taken_SY = "06"

# Filter vehicle crime for selected month and county, and calculate rate per district
vehicle_crimes = crime_data %>%
  filter(CrimeType %in% c("Vehicle crime")) %>%
  filter(str_starts(Month, paste0(year_taken_SY, "-", month_taken_SY))) %>%
  left_join(LSOA_Pop, by = c("LSOA_code" = "LSOA_code")) %>%
  filter(County == county_taken_SY) %>%
  group_by(District) %>%
  summarise(
    VehicleCrimeCount = n(),
    Population = mean(Population2023, na.rm = TRUE),
    VehicleCrimeRate = (VehicleCrimeCount / Population) * 1000,
    .groups = "drop"
  )

# Print the number of districts used in the radar chart
print(paste("Number of districts:", n_distinct(vehicle_crimes$District)))

# Prepare radar chart data with max and min limits
max_rate = max(vehicle_crimes$VehicleCrimeRate, na.rm = TRUE)
min_rate = 0

radar_data = vehicle_crimes %>%
  select(District, VehicleCrimeRate) %>%
  column_to_rownames("District") %>%
  t() %>%
  as.data.frame()

# Add max and min rows to set scale in radar chart
radar_data = rbind(
  max = rep(max_rate, ncol(radar_data)),
  min = rep(min_rate, ncol(radar_data)),
  radar_data
)

# Generate color palette for radar chart
colors_border = scales::hue_pal()(ncol(radar_data))

# Plot radar chart
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
  title = paste("Vehicle Crime Rates by District — South Yorkshire (June 2024)")
)

#------------------------ Pie Chart for Robbery Rate ---------------------------------#

# Define parameters for month, year, and county
county_taken_WY= "WEST YORKSHIRE"
year_taken_WY = "2023"
month_taken_WY = "08"

# Filter and calculate robbery rate by district for selected date
robbery_data = crime_data %>%
  filter(CrimeType == "Robbery") %>%
  filter(str_starts(Month, paste0(year_taken_WY, "-", month_taken_WY))) %>%
  left_join(LSOA_Pop, by = c("LSOA_code" = "LSOA_code")) %>%
  filter(County == county_taken_WY) %>%
  group_by(District) %>%
  summarise(
    RobberyCount = n(),
    Population = mean(Population2023, na.rm = TRUE),
    RobberyRate = (RobberyCount / Population) * 1000,
    .groups = "drop"
  ) %>%
  filter(RobberyRate > 0)

# Add percentage and label formatting for the pie chart
robbery_data = robbery_data %>%
  mutate(Percent = RobberyRate / sum(RobberyRate) * 100,
         Label = paste0(District, "\n", round(Percent, 1), "%"))

# Plot pie chart
ggplot(robbery_data, aes(x = "", y = RobberyRate, fill = District)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = paste("Distribution of Robbery Rates by District — West Yorkshire (August 2023)"),
    fill = "District"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#-----------------------------------------  Drug Offense Rates (per 10,000 people)  Both Counties ----------------------------#

# Calculate annual drug offense rates per county
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

# Plot line chart to show drug offense trends over time
ggplot(drug_rates, aes(x = Year, y = DrugOffenseRate_per_10000, color = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Annual Drug Offense Rate Trends per 10,000 Residents",
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
