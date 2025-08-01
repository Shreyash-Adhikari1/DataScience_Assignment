library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(fmsb)


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