library(tidyverse)

# Define base directory path where school performance data folders are located
base_path = "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\SchoolPerformances"

# List of year folders to process
year_folders = c("2021-2022", "2022-2023", "2023-2024")

# Function to read and clean the KS4 final data CSV for a given year folder
read_clean_ks4final = function(year_folder) {
  # Build full folder path for the specific year
  folder_path = file.path(base_path, year_folder)
  
  # Find the CSV file matching the pattern (case-insensitive) within the folder
  csv_file = list.files(folder_path, pattern = "(?i)england_ks4final.*\\.csv$", full.names = TRUE)
  
  # Read the CSV file quietly (no column type messages)
  df = read_csv(csv_file, show_col_types = FALSE)
  
  # Specify columns to keep, intersect with actual columns in the file to avoid errors
  cols_to_keep = c("LEA", "URN", "SCHNAME", "TOWN", "PCODE", "ATT8SCR")
  cols_to_keep = intersect(cols_to_keep, colnames(df))
  
  # Clean and filter the dataset
  df_clean = df %>%
    select(all_of(cols_to_keep)) %>%     # Keep only relevant columns
    na.omit() %>%                        # Remove rows with missing values in selected columns
    mutate(
      # Extract and standardize short postcode (uppercase, trimmed, first alphanumeric group in PCODE)
      shortPostcode = str_to_upper(str_trim(str_extract(PCODE, "^[A-Z0-9]+"))),
      # Convert ATT8SCR to numeric, suppress warnings if coercion issues occur
      ATT8SCR = suppressWarnings(as.numeric(ATT8SCR)),
      # Add Year column to indicate data year from folder name
      Year = year_folder
    ) %>%
    # Keep only rows where ATT8SCR is not NA and at least 10 (filtering for valid attainment scores)
    filter(!is.na(ATT8SCR), ATT8SCR >= 10)
  
  return(df_clean)
}

# Apply the cleaning function to all specified year folders and combine into one dataframe
ks4_filtered = map_dfr(year_folders, read_clean_ks4final)

# Load cleaned town and population data for Yorkshire counties
towns_yorkshire = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Town_Population.csv") %>%
  mutate(
    shortPostcode = str_to_upper(str_trim(shortPostcode)),  # Standardize shortPostcode
    County = str_to_upper(str_trim(County))                 # Standardize County names
  ) %>%
  # Filter to keep only South Yorkshire and West Yorkshire entries
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Join school performance data with Yorkshire town info on short postcode,
# then filter again for the two Yorkshire counties and select relevant columns
combined_ks4final = ks4_filtered %>%
  left_join(towns_yorkshire, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(LEA, URN, SCHNAME, PCODE, ATT8SCR, shortPostcode, Year, Town, District, County)

# Save the cleaned and combined school performance data to CSV
write_csv(combined_ks4final, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv")

# View the final combined dataframe for inspection
View(combined_ks4final)
