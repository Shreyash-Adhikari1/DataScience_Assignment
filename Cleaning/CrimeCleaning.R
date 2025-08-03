library(tidyverse)

# Define the base directory path where raw crime CSV files are stored
base_path = "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\Crime_Dataset"

# List all CSV files in the directory and its subdirectories
csv_files = list.files(path = base_path, pattern = "*.csv", recursive = TRUE, full.names = TRUE)

# Define a function to clean an individual crime CSV file
clean_crime_file = function(file_path) {
  # Read the CSV file without printing column type messages
  df = read_csv(file_path, show_col_types = FALSE)
  
  # Check if the required columns exist in the dataframe
  if (all(c("Crime ID", "Month", "LSOA code", "Crime type", "LSOA name") %in% names(df))) {
    df %>%
      # Filter out rows with missing critical information
      filter(
        !is.na(`Crime ID`),
        !is.na(`LSOA code`),
        !is.na(`Crime type`),
        !is.na(Month)
      ) %>%
      # Extract year from Month, and rename columns for clarity
      mutate(
        Year = substr(Month, 1, 4),
        LSOA_code = `LSOA code`,
        CrimeType = `Crime type`
      ) %>%
      # Select only relevant columns for further processing
      select(
        Month,
        Year,
        LSOA_code,
        `LSOA name`,
        CrimeType
      )
  } else {
    # Return an empty tibble if the required columns are not present in the file
    tibble()
  }
}

# Apply the cleaning function to all CSV files and combine results into one dataframe
CleanedCrimeData = map_dfr(csv_files, clean_crime_file) %>%
  distinct()    # Remove duplicate rows if any

# If the combined data contains a Year column, arrange by Year and LSOA_code for organized viewing
if ("Year" %in% colnames(CleanedCrimeData)) {
  CleanedCrimeData = CleanedCrimeData %>%
    arrange(Year, LSOA_code)
}

# Save the cleaned and combined crime dataset to a CSV file for later use
write_csv(CleanedCrimeData, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv")

View(CleanedCrimeData)

# Display column names of the cleaned crime data (optional)
colnames(CleanedCrimeData)
