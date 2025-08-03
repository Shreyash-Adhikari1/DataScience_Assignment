library(tidyverse)

# Read the raw broadband speed data CSV from the specified directory
Broadband = read_csv(
  "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\broadband-speed\\201805_fixed_pc_performance_r03.csv",
  show_col_types = FALSE
)
View(Broadband)

# Read the previously cleaned house prices data for postcode mapping reference
HousePrices = read_csv(
  "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\CleanedHousePrices.csv"
)

# Inspect column names of the HousePrices data (optional)
colnames(HousePrices)

# Create a mapping table from short postcode to Town, District, and County
postcode_map <- HousePrices %>%
  mutate(
    # Create a standardized uppercase short postcode by extracting the part before the space and trimming whitespace
    shortPostcode = str_to_upper(str_trim(gsub(" .*$", "", Postcode)))
  ) %>%
  # Count occurrences of each shortPostcode along with Town, District, County
  count(shortPostcode, Town, District, County, name = "n") %>%
  group_by(shortPostcode) %>%
  # For each shortPostcode, keep only the row with the highest count (to get the most representative Town, District, County)
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Keep only relevant columns for the mapping
  select(shortPostcode, Town, District, County)

# Clean and organize the broadband data
BroadbandData = Broadband %>%
  mutate(
    # Extract and standardize the short postcode from 'postcode_space' column (uppercase and trimmed)
    shortPostcode = str_to_upper(str_trim(gsub(" .*$", "", postcode_space))),
    # Add a unique ID for each row for reference
    ID = row_number()
  ) %>%
  # Select only relevant columns, renaming for clarity
  select(
    ID,
    postcode_area = `postcode area`,
    shortPostcode,
    avgDownload = `Average download speed (Mbit/s)`,
    avgUpload = `Average upload speed (Mbit/s)`,
    minDownload = `Minimum download speed (Mbit/s)`,
    minUpload = `Minimum upload speed (Mbit/s)`
  ) %>%
  na.omit() %>%      # Remove any rows with missing values
  distinct() %>%     # Remove duplicate rows if any
  # Join broadband data with postcode_map to add Town, District, and County information based on shortPostcode
  left_join(postcode_map, by = "shortPostcode") %>%
  # Filter out any rows where District or County information could not be matched (to keep only valid entries)
  filter(!is.na(District), !is.na(County))

# Save the cleaned broadband dataset with location information for further analysis
write_csv(BroadbandData, "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\cleaned_broadband_data.csv")

View(BroadbandData)

# Check the column names of the cleaned broadband data (optional)
colnames(BroadbandData)
