library(tidyverse)

# Base path where the year folders are stored
base_path <- "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\SchoolPerformances"

# List of year folders
year_folders <- c("2021-2022", "2022-2023", "2023-2024")

# Function to read and clean one england_ks4final CSV from a given year folder
read_clean_ks4final <- function(year_folder) {
  folder_path <- file.path(base_path, year_folder)
  
  # Find the CSV filename containing 'england_ks4final' (case-insensitive)
  csv_file <- list.files(folder_path, pattern = "(?i)england_ks4final.*\\.csv$", full.names = TRUE)
  
  if (length(csv_file) == 0) {
    message("No england_ks4final file found in ", year_folder)
    return(tibble())
  }
  
  df <- read_csv(csv_file, show_col_types = FALSE)
  
  # Columns to keep; add more if needed
  cols_to_keep <- c("LEA", "URN", "SCHNAME", "TOWN", "PCODE", "ATT8SCR", "TOTATT8")
  cols_to_keep <- intersect(cols_to_keep, colnames(df))
  
  df_clean <- df %>%
    select(all_of(cols_to_keep)) %>%
    na.omit() %>%
    mutate(
      shortPostcode = str_trim(substr(PCODE, 1, 4)),
      Year = year_folder
    )
  
  return(df_clean)
}

# Read and clean all three years and combine
combined_ks4final <- map_dfr(year_folders, read_clean_ks4final)

# View the combined cleaned dataset 
View(combined_ks4final)

# Save the combined cleaned data
write_csv(combined_ks4final, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performances_2021-2024.csv")
