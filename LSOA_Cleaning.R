library(data.table)
library(tidyverse)
library(dplyr)


Cleaned_HP = read.csv("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_Town_Population.csv" )

LSOA = fread("C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Obtained Data\\Postcode to LSOA.csv")
pattern = ' .*$'
LSOA_Cleaned = LSOA %>%
  select(lsoa11cd,pcds) %>% 
  mutate(shortPostcode=gsub(pattern,"",pcds)) %>% 
  right_join(Cleaned_HP,by="shortPostcode")  %>% 
  group_by(lsoa11cd) %>% 
  select(lsoa11cd,shortPostcode,Town,District,County) 


LSOA_Cleaned


colnames(LSOA_Cleaned)[1] <- "LSOA code"
view(LSOA_Cleaned)
write.csv(LSOA_Cleaned,"C:\\Users\\ADMIN\\Desktop\\Data Science Assignment\\Cleaned Data\\Cleaned_LSOA.csv",row.names = FALSE,col.names = FALSE)
