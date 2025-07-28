library(tidyverse)


base_path = "C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\SchoolPerformances"
year_folders = c("2021-2022", "2022-2023", "2023-2024")


yorkshire_prefixes = c("BD", "DN", "HD", "HG", "HU", "HX", "LS", "S", "WF", "YO")


towns = read_csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Town_Population.csv") %>%
  mutate(
    shortPostcode = str_to_upper(str_trim(shortPostcode)),
    County = str_to_upper(str_trim(County))
  )

towns_yorkshire = towns %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))


read_clean_ks4final = function(year_folder) {
  folder_path = file.path(base_path, year_folder)
  
  csv_file = list.files(folder_path, pattern = "(?i)england_ks4final.*\\.csv$", full.names = TRUE)
  if (length(csv_file) == 0) return(tibble())
  
  df = read_csv(csv_file, show_col_types = FALSE)
  
  cols_to_keep = c("LEA", "URN", "SCHNAME", "TOWN", "PCODE", "ATT8SCR")
  cols_to_keep = intersect(cols_to_keep, colnames(df))
  
  df_clean = df %>%
    select(all_of(cols_to_keep)) %>%
    na.omit() %>%
    mutate(
      
      shortPostcode = str_to_upper(str_trim(str_extract(PCODE, "^[A-Z0-9]+"))),
      Year = year_folder
    ) %>%
    
    filter(str_sub(shortPostcode, 1, 2) %in% yorkshire_prefixes)
  
  return(df_clean)
}


ks4_filtered = map_dfr(year_folders, read_clean_ks4final)


combined_ks4final = ks4_filtered %>%
  left_join(towns_yorkshire, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>% 
  select(
    LEA, URN, SCHNAME,PCODE, ATT8SCR, shortPostcode, Year,
    Town, District, County
  )

if (nrow(combined_ks4final) > 0) {
  View(combined_ks4final)
} else {
  message("No data found for South or West Yorkshire after filtering and join.")
}

colnames(combined_ks4final)

write_csv(combined_ks4final,"C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_School_Performance.csv"
)
