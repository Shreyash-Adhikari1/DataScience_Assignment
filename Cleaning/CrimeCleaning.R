library(tidyverse)


base_path = "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\Crime_Dataset"


csv_files = list.files(path = base_path, pattern = "*.csv", recursive = TRUE, full.names = TRUE)


clean_crime_file = function(file_path) {
  df = read_csv(file_path, show_col_types = FALSE)
  
  if (all(c("Crime ID", "Month", "LSOA code", "Crime type", "LSOA name") %in% names(df))) {
    df %>%
      filter(
        !is.na(`Crime ID`),
        !is.na(`LSOA code`),
        !is.na(`Crime type`),
        !is.na(Month)
      ) %>%
      mutate(
        Year = substr(Month, 1, 4),
        LSOA_code = `LSOA code`,
        CrimeType = `Crime type`
      ) %>%
      select(
        Month,
        Year,
        LSOA_code,
        `LSOA name`,
        CrimeType
      )
  } else {
    tibble()
  }
}

# Apply cleaning to all of the obtained csv files and combine
CleanedCrimeData = map_dfr(csv_files, clean_crime_file) %>%
  distinct()


if ("Year" %in% colnames(CleanedCrimeData)) {
  CleanedCrimeData = CleanedCrimeData %>%
    arrange(Year, LSOA_code)
}


write_csv(CleanedCrimeData, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedCrimeData.csv")


View(CleanedCrimeData)
colnames(CleanedCrimeData)

