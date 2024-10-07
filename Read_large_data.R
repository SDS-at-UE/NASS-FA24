library(tidyverse)
library(data.table)

#file_connection <- gzfile("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\Data\\qs.crops_20240914.txt.gz", "rt")  # "rt" is for reading text
#file_connection <- gzfile("C:\\Users\\ok16\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\Data\\qs.crops_20240914.txt.gz", "rt")  # "rt" is for reading text
#file_content <- readLines(file_connection)
#close(file_connection)  # Close the connection after reading

load("D:\\Crop_NASS\\cop_data.rda")

selected_crop_data <- crop_data %>% 
  filter(COUNTRY_NAME == "UNITED STATES",
         STATE_NAME %in% c("INDIANA", "MISSOURI", "ILLINOIS", "KENTUCKY", "MICHIGAN", "OHIO"),
         YEAR %in% c(2000:2025))

#save(selected_crop_data, file = "selected_crop_data.rda")

unique(selected_crop_data$COMMODITY_DESC)
unique(selected_crop_data$GROUP_DESC)
unique(selected_crop_data[selected_crop_data$GROUP_DESC == "FIELD CROPS",]$COMMODITY_DESC)


survey_corn_selected <- selected_crop_data %>%
  filter(SOURCE_DESC == 'SURVEY')

census_corn_selected <- selected_crop_data %>%
  filter(SOURCE_DESC == 'CENSUS')

survey_corn_data <- survey_corn_selected %>% 
  filter(GROUP_DESC == "FIELD CROPS",
         COMMODITY_DESC == 'CORN')

census_corn_data <- census_corn_selected %>% 
  filter(GROUP_DESC == "FIELD CROPS",
         COMMODITY_DESC == 'CORN')

state_survey_corn_data <- survey_corn_data %>% 
  





