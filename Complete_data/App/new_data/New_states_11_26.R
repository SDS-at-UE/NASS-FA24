library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

setwd("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\Data")
#setwd("C:\\Users\\ok16\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\Data")

# Function to remove columns with a sum of zero, considering only numeric columns
remove_zero_sum_columns <- function(df) {
  # Identify numeric columns
  numeric_cols <- sapply(df, is.numeric)
  
  # Calculate column sums only for numeric columns
  col_sums <- colSums(df[, numeric_cols], na.rm = TRUE)
  
  # Keep numeric columns where the sum is not zero
  df_numeric_non_zero <- df[, numeric_cols][, col_sums != 0]
  
  # Combine non-numeric columns with filtered numeric columns
  df_filtered <- cbind(df[, !numeric_cols], df_numeric_non_zero)
  
  return(df_filtered)
}


##########################
Data <- read.delim("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\Data\\qs.crops_20241024.txt", sep = "\t")

corn_states <- toupper(c("Illinois", "Indiana", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "Ohio", "South Dakota", "Wisconsin", "Michigan", "Kentucky"))
# Download the counties with geometries for the selected states
corn_county_geometry <- counties(state = corn_states, cb = TRUE) %>% 
  dplyr::select(NAMELSAD, STATE_NAME, geometry)
corn_state_geometry <- states(cb = TRUE) %>%
  dplyr::select(NAME, geometry) %>% 
  filter(toupper(NAME) %in% corn_states)


cotton_states <- toupper(c("Arkansas", "California", "Georgia", "Louisiana", "Mississippi", "North Carolina", "Texas"))
cotton_county_geometry <- counties(state = cotton_states, cb = TRUE) %>% 
  dplyr::select(NAMELSAD, STATE_NAME, geometry)
cotton_state_geometry <- states(cb = TRUE) %>%
  dplyr::select(NAME, geometry) %>% 
  filter(toupper(NAME) %in% cotton_states)



soybeans_states <- toupper(c("Arkansas", "Illinois", "Indiana", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Michigan", "Kentucky"))
soybeans_county_geometry <- counties(state = soybeans_states, cb = TRUE) %>% 
  dplyr::select(NAMELSAD, STATE_NAME, geometry)
soybeans_state_geometry <- states(cb = TRUE) %>%
  dplyr::select(NAME, geometry) %>% 
  filter(toupper(NAME) %in% soybeans_states)



wheat_states <- toupper(c("Colorado", "Illinois", "Kansas", "Missouri", "Montana", "Nebraska", "Ohio", "Oklahoma", "Texas", "Washington","Michigan", "Kentucky", "Indiana"))
wheat_county_geometry <- counties(state = wheat_states, cb = TRUE) %>% 
  dplyr::select(NAMELSAD, STATE_NAME, geometry)
wheat_state_geometry <- states(cb = TRUE) %>%
  dplyr::select(NAME, geometry) %>% 
  filter(toupper(NAME) %in% wheat_states)


potatoes_states <- toupper(c("Idaho", "Maine", "Minnesota", "North Dakota", "Oregon", "Washington", "Wisconsin", "Illinois", "Missouri","Michigan", "Kentucky", "Indiana", "Ohio"))
potatoes_county_geometry <- counties(state = potatoes_states, cb = TRUE) %>% 
  dplyr::select(NAMELSAD, STATE_NAME, geometry)
potatoes_state_geometry <- states(cb = TRUE) %>%
  dplyr::select(NAME, geometry) %>% 
  filter(toupper(NAME) %in% potatoes_states)


all_states <- toupper(unique(c(corn_states, cotton_states, soybeans_states, wheat_states, potatoes_states)))

corn_data <- Data %>% 
  filter(STATE_NAME %in% corn_states,
         COMMODITY_DESC == "CORN")

cotton_data <- Data %>% 
  filter(STATE_NAME %in% cotton_states,
         COMMODITY_DESC == "COTTON")

soybeans_data <- Data %>% 
  filter(STATE_NAME %in% soybeans_states,
         COMMODITY_DESC == "SOYBEANS")

wheat_data <- Data %>% 
  filter(STATE_NAME %in% wheat_states,
         COMMODITY_DESC == "WHEAT")

potatoes_data <- Data %>% 
  filter(STATE_NAME %in% potatoes_states,
         COMMODITY_DESC == "POTATOES")

save(corn_data, file = "corn_data.rda")
save(cotton_data, file = "cotton_data.rda")
save(soybeans_data, file = "soybeans_data.rda")
save(wheat_data, file = "wheat_data.rda")
save(potatoes_data, file = "potatoes_data.rda")
#################################################################################


corn_census <- corn_data %>% 
  filter(SOURCE_DESC == "CENSUS")

corn_survey <- corn_data %>% 
  filter(SOURCE_DESC == "SURVEY")


cotton_census <- cotton_data %>% 
  filter(SOURCE_DESC == "CENSUS")

cotton_survey <- cotton_data %>% 
  filter(SOURCE_DESC == "SURVEY")


soybeans_census <- soybeans_data %>% 
  filter(SOURCE_DESC == "CENSUS")

soybeans_survey <- soybeans_data %>% 
  filter(SOURCE_DESC == "SURVEY")


wheat_census <- wheat_data %>% 
  filter(SOURCE_DESC == "CENSUS")

wheat_survey <- wheat_data %>% 
  filter(SOURCE_DESC == "SURVEY")


potatoes_census <- potatoes_data %>% 
  filter(SOURCE_DESC == "CENSUS")

potatoes_survey <- potatoes_data %>% 
  filter(SOURCE_DESC == "SURVEY")


save(corn_census, file = "corn_census.rda")
save(cotton_census, file = "cotton_census.rda")
save(soybeans_census, file = "soybeans_census.rda")
save(wheat_census, file = "wheat_census.rda")
save(potatoes_census, file = "potatoes_census.rda")

save(corn_survey, file = "corn_survey.rda")
save(cotton_survey, file = "cotton_survey.rda")
save(soybeans_survey, file = "soybeans_survey.rda")
save(wheat_survey, file = "wheat_survey.rda")
save(potatoes_survey, file = "potatoes_survey.rda")

#########################################################################

## corn data
# census:
load("corn_census.rda")

corn_census[corn_census == "(D)"] <- 0
corn_census$VALUE <- as.numeric(gsub(",","",corn_census$VALUE))

corn_census <- data.frame(corn_census, county_state = paste0(str_to_title(corn_census$COUNTY_NAME, locale = "en"), " County, ", 
                                                         str_to_title(corn_census$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "COUNTY",
         FREQ_DESC == "ANNUAL",
         UTIL_PRACTICE_DESC == 'GRAIN',
         REFERENCE_PERIOD_DESC == "YEAR",
         YEAR >= 2000) %>% 
  dplyr::select(county_state, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, county_state) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))

corn_census$county_state <- corn_census$county_state %>%
  str_replace_all("\\bSt\\b", "St.") %>%
  str_replace_all("\\bMc([a-z])", function(x) {
    paste0("Mc", toupper(substr(x, 3, 3)), substr(x, 4, nchar(x)))
  }) %>%
  str_replace_all("\\bSte\\b", "Ste.") %>%
  str_replace_all("\\bO Brien\\b", "O'Brien") %>%
  str_replace_all("(?i)\\bdu Page\\b", "DuPage") %>% 
  str_replace_all("\\bLa Porte\\b", "LaPorte") %>%
  str_replace_all("\\bLa Salle\\b", "LaSalle") %>%
  str_replace_all("\\bFond Du Lac\\b", "Fond du Lac") %>%
  str_replace_all("\\bLake Of The Woods\\b", "Lake of the Woods") %>%
  str_replace_all("\\bLac Qui Parle\\b", "Lac qui Parle") %>%
  str_replace_all("\\bLagrange\\b", "LaGrange") %>%
  str_replace_all("\\bOf\\b", "of") %>%
  str_replace_all("\\bDu\\b", "du") %>%
  str_replace_all("\\bduPage\\b", "DuPage")  %>%
  str_replace_all("La Porte", "LaPorte") %>%         
  str_replace_all("De Kalb", "DeKalb") %>% 
  str_replace_all("\\.\\.", ".")

complete_data <- corn_census %>%
  ungroup() %>%
  complete(county_state, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)

corn_census_final <- remove_zero_sum_columns(wide_data)

corn_census_final$`YIELD_BU / ACRE` = corn_census_final$PRODUCTION_BU/corn_census_final$`AREA HARVESTED_ACRES`

D <- as.data.frame(lapply(corn_census_final, function(x) ifelse(is.nan(x) | is.infinite(x), 0, x)))
names(D) <- names(corn_census_final)
corn_census_final <- D

# corn_census_final <- corn_census_final %>%
#   left_join(corn_county_geometry[, c("county_state", "geometry")], by = "county_state")
###################################################
# survey:
options(scipen = 999)
load("corn_survey.rda")

corn_survey[corn_survey == "(D)"] <- 0
corn_survey$VALUE <- as.numeric(gsub(",","",corn_survey$VALUE))

corn_survey2 <- data.frame(corn_survey, county_state = paste0(str_to_title(corn_survey$COUNTY_NAME, locale = "en"), " County, ", 
                                                             str_to_title(corn_survey$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "STATE",
         FREQ_DESC == "ANNUAL",
         UTIL_PRACTICE_DESC == 'GRAIN',
         REFERENCE_PERIOD_DESC == "YEAR",
         YEAR >= 2000,
         !STATISTICCAT_DESC %in% c("PLANT POPULATION", "AREA PLANTED"),
         !UNIT_DESC %in% c("PCT MILK", "PLANTS / ACRE"),
         PRODN_PRACTICE_DESC == "ALL PRODUCTION PRACTICES") %>% 
  dplyr::select(STATE_NAME, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, STATE_NAME) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))


complete_data <- corn_survey2 %>%
  ungroup() %>%
  complete(STATE_NAME, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)


corn_survey_final <- remove_zero_sum_columns(wide_data)

# corn_survey_final <- wide_data %>%
#   left_join(corn_state_geometry, by = "STATE_NAME")



names(corn_state_geometry)[1] <- c("STATE_NAME")

corn_county_geometry <- corn_county_geometry %>%
  mutate(county_state = paste(NAMELSAD, STATE_NAME, sep = ", ")) %>% 
  select(county_state, geometry)

save(corn_survey_final, file = "Final_data\\new_data\\corn_survey_final.rda")
save(corn_census_final, file = "Final_data\\new_data\\corn_census_final.rda")
save(corn_state_geometry, file = "Final_data\\new_data\\corn_state_geometry.rda")
save(corn_county_geometry, file = "Final_data\\new_data\\corn_county_geometry.rda")


#######################################################################
load("corn_survey.rda")

corn_survey[corn_survey == "(D)"] <- 0
corn_survey$VALUE <- as.numeric(gsub(",","",corn_survey$VALUE))

corn_survey <- corn_survey %>%
  mutate(VALUE = if_else(
    STATISTICCAT_DESC %in% c("PROGRESS", "PROGRESS, PREVIOUS YEAR", "PROGRESS, 5 YEAR AVG") &
      UNIT_DESC == "PCT HARVESTED" &
      UTIL_PRACTICE_DESC == 'SILAGE',
    0,  # Set Value to 0 if conditions are met
    VALUE  # Keep the original Value otherwise
  ))

inx <- grep("WEEK", corn_survey$REFERENCE_PERIOD_DESC, value = FALSE)

corn_survey_week <- corn_survey[inx,] %>% 
  arrange(WEEK_ENDING) %>% 
  filter(YEAR>=2000,
         FREQ_DESC == "WEEKLY",
         #UTIL_PRACTICE_DESC == 'GRAIN',
         STATISTICCAT_DESC %in% c("PROGRESS",
                                  "PROGRESS, PREVIOUS YEAR",
                                  "PROGRESS, 5 YEAR AVG",
                                  "CONDITION",
                                  "CONDITION, PREVIOUS YEAR",
                                  "CONDITION, 5 YEAR AVG"
                                  #"MOISTURE",
                                  #"MOISTURE, PREVIOUS YEAR",
                                  #"MOISTURE, 5 YEAR AVG",
                                  #"HEIGHT, AVG",
                                  #"HEIGHT, AVG, PREVIOUS YEAR",
                                  #"HEIGHT, AVG, 5 YEAR AVG"
                                  )) %>% 
  dplyr::select(WEEK_ENDING,
                REFERENCE_PERIOD_DESC,
                STATE_NAME,
                STATISTICCAT_DESC, 
                UNIT_DESC, 
                VALUE) %>% 
  group_by(WEEK_ENDING,
           REFERENCE_PERIOD_DESC,
           STATE_NAME,
           STATISTICCAT_DESC, 
           UNIT_DESC) %>% 
  summarise(sum_value = sum(VALUE))
  #summarise(sum_value = sum(as.numeric(gsub(",","",VALUE))))

# complete_data_week <- corn_survey_week %>%
#   ungroup() %>%
#   complete(STATE_NAME,REFERENCE_PERIOD_DESC,  WEEK_ENDING, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data_week <- corn_survey_week %>% #complete_data_week %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(WEEK_ENDING)

#corn_survey_week_final <- remove_zero_sum_columns(wide_data_week)

corn_survey_week_final <- wide_data_week %>% 
  mutate(good_excellent = `CONDITION_PCT EXCELLENT` + `CONDITION_PCT GOOD`,
         Year = year(WEEK_ENDING),
         Week_Num = as.numeric(gsub("WEEK #", "", REFERENCE_PERIOD_DESC))) %>%  # Extract numeric week number)
  arrange(STATE_NAME, Week_Num)

save(corn_survey_week_final, file = "Final_data\\new_data\\corn_survey_week_final.rda")



################

#Plot 1
df1 <- corn_survey_week_final %>% 
  ungroup() %>% 
  dplyr::select(REFERENCE_PERIOD_DESC, STATE_NAME, Year, Week_Num, good_excellent) %>% 
  filter(STATE_NAME == "INDIANA", 
         Year %in% 2020:2024) %>% 
  arrange(STATE_NAME, Week_Num)
  
df1$Year <- as.factor(df1$Year) 

# Get the last point with non-NA values for each year to add the label at the end of the line
df_last <- df1 %>%
  group_by(Year) %>%
  filter(!is.na(good_excellent)) %>%  # Filter out NA values
  filter(Week_Num == max(Week_Num))   # Select the last non-NA week for each year

# plot
ggplot(data = df1, 
       aes(x = Week_Num, y = good_excellent, color = Year, group = Year)) +
  geom_line() +  # Add lines for each year
  geom_point() +  # Add points for better visualization, but it can be removed
  # Add year label at the end of each line for every year
  geom_text(data = df_last, aes(label = Year), 
            hjust = -0.3,  # Adjust the horizontal position
            vjust = 0.5,   # Adjust the vertical position
            size = 4) +    # Size of the text
  labs(
    title = "Good Excellent Values by Week for Indiana",
    x = "Week Number",
    y = "Good Excellent",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Remove legend since we’re adding labels
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1)))  # Add extra space on x-axis for labels
########################################
#Plot 2

# Create cumulative columns for each condition
df2 <- corn_survey_week_final %>% 
  ungroup() %>% 
  dplyr::select(
    REFERENCE_PERIOD_DESC, 
    STATE_NAME, 
    Year, 
    Week_Num, 
    `CONDITION_PCT EXCELLENT`,
    `CONDITION_PCT GOOD`,
    `CONDITION_PCT FAIR`,                   
    `CONDITION_PCT POOR`,                    
    `CONDITION_PCT VERY POOR`
  ) %>% 
  filter(STATE_NAME == "INDIANA", 
         Year == 2020) %>% 
  arrange(STATE_NAME, Week_Num)

# Calculate cumulative values
df2 <- df2 %>%
  mutate(
    `CUMULATIVE VERY POOR` = `CONDITION_PCT VERY POOR`,
    `CUMULATIVE POOR` = `CUMULATIVE VERY POOR` + `CONDITION_PCT POOR`,
    `CUMULATIVE FAIR` = `CUMULATIVE POOR` + `CONDITION_PCT FAIR`,
    `CUMULATIVE GOOD` = `CUMULATIVE FAIR` + `CONDITION_PCT GOOD`,
    `CUMULATIVE EXCELLENT` = `CUMULATIVE GOOD` + `CONDITION_PCT EXCELLENT`
  )


ggplot(data = df2) +
  geom_ribbon(aes(x = Week_Num, ymin = 0, ymax = `CUMULATIVE VERY POOR`, fill = "VERY POOR"), alpha = 0.4) +
  geom_ribbon(aes(x = Week_Num, ymin = `CUMULATIVE VERY POOR`, ymax = `CUMULATIVE POOR`, fill = "POOR"), alpha = 0.4) +
  geom_ribbon(aes(x = Week_Num, ymin = `CUMULATIVE POOR`, ymax = `CUMULATIVE FAIR`, fill = "FAIR"), alpha = 0.4) +
  geom_ribbon(aes(x = Week_Num, ymin = `CUMULATIVE FAIR`, ymax = `CUMULATIVE GOOD`, fill = "GOOD"), alpha = 0.4) +
  geom_ribbon(aes(x = Week_Num, ymin = `CUMULATIVE GOOD`, ymax = `CUMULATIVE EXCELLENT`, fill = "EXCELLENT"), alpha = 0.4) +
  scale_fill_manual(
    values = c("EXCELLENT" = "green", "GOOD" = "blue", "FAIR" = "yellow", "POOR" = "orange", "VERY POOR" = "red"),
    breaks = c("EXCELLENT", "GOOD", "FAIR", "POOR", "VERY POOR")  # Correct order for the legend
  ) +
  labs(
    x = "Week Number",
    y = "Cumulative Percentage",
    fill = "Condition",
    title = "Cumulative Corn Condition by Week in Indiana (2020)"
  ) +
  theme_minimal()

#################################################
#plot3

t2 = grep("PROGRESS", names(corn_survey_week_final))
t1 = which(names(corn_survey_week_final) %in% c("STATE_NAME",
                                                "REFERENCE_PERIOD_DESC",
                                                "Year", 
                                                "Week_Num"))

df3 <- corn_survey_week_final[, c(t1,t2)]
df3 <- df3 %>% 
  ungroup() %>% 
  filter(STATE_NAME == "INDIANA", 
         Year == 2020) %>% 
  arrange(STATE_NAME, Week_Num)


df3_long <- df3 %>%
  pivot_longer(
    cols = starts_with("PROGRESS"),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  # Extract the type (current year, previous year, 5-year average) from the column names
  mutate(
    Type = case_when(
      grepl("5 YEAR AVG", Category) ~ "5 Year Avg",
      grepl("PREVIOUS YEAR", Category) ~ "Previous Year",
      TRUE ~ "Current Year"
    ),
    # Extract the progress category (e.g., PLANTED, SILKING, etc.)
    Category = gsub("PROGRESS.*?_PCT ", "", Category)
  )

# # Filter to keep only relevant categories if needed
# df3_long <- df3_long %>%
#   filter(Category %in% c("PLANTED", "MILK", "DENTED", "MATURE"))  # Add any categories as needed

# Plot using ggplot2 with colored labels
ggplot(data = df3_long, aes(x = Week_Num, y = Value, color = Category, linetype = Type)) +
  geom_line(size = 1) +
  geom_text(
    data = df3_long %>%
      filter(Week_Num == max(Week_Num)),  # Position the labels at the end of the lines
    aes(label = Category),
    hjust = -0.1,  # Adjust horizontal position to place labels just outside the plot
    size = 3.5,
    show.legend = FALSE  # Hide legend for the labels
  ) +
  labs(
    title = "Progress of Corn by Category Over Weeks",
    x = "Week Number",
    y = "Percentage",
    color = "Category",
    linetype = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("PLANTED" = "green", "MILK" = "purple", "DENTED" = "orange", "MATURE" = "brown",
                                "EMERGED" = "red" ,  "SILKING" = "gold" ,  "DOUGH" = 'black', "HARVESTED" = "pink")) +
  scale_linetype_manual(values = c("Current Year" = "solid", "Previous Year" = "dashed", "5 Year Avg" = "dotted")) +
  theme(legend.position = "bottom")


####################################################################
#########################################################################

## soybeans data
# census:
load("soybeans_census.rda")

soybeans_census[soybeans_census == "(D)"] <- 0
soybeans_census$VALUE <- as.numeric(gsub(",","",soybeans_census$VALUE))


soybeans_census <- data.frame(soybeans_census, county_state = paste0(str_to_title(soybeans_census$COUNTY_NAME, locale = "en"), " County, ", 
                                                             str_to_title(soybeans_census$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "COUNTY",
         #FREQ_DESC == "ANNUAL",
         #UTIL_PRACTICE_DESC == 'GRAIN',
         #REFERENCE_PERIOD_DESC == "YEAR",
         YEAR >= 2000) %>% 
  dplyr::select(county_state, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, county_state) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))

soybeans_census$county_state <- soybeans_census$county_state %>%
  str_replace_all("\\bSt\\b", "St.") %>%
  str_replace_all("\\bMc([a-z])", function(x) {
    paste0("Mc", toupper(substr(x, 3, 3)), substr(x, 4, nchar(x)))
  }) %>%
  str_replace_all("\\bSte\\b", "Ste.") %>%
  str_replace_all("\\bO Brien\\b", "O'Brien") %>%
  str_replace_all("(?i)\\bdu Page\\b", "DuPage") %>% 
  str_replace_all("\\bLa Porte\\b", "LaPorte") %>%
  str_replace_all("\\bLa Salle\\b", "LaSalle") %>%
  str_replace_all("\\bFond Du Lac\\b", "Fond du Lac") %>%
  str_replace_all("\\bLake Of The Woods\\b", "Lake of the Woods") %>%
  str_replace_all("\\bLac Qui Parle\\b", "Lac qui Parle") %>%
  str_replace_all("\\bLagrange\\b", "LaGrange") %>%
  str_replace_all("\\bOf\\b", "of") %>%
  str_replace_all("\\bDu\\b", "du") %>%
  str_replace_all("\\bduPage\\b", "DuPage")  %>%
  str_replace_all("La Porte", "LaPorte") %>%         
  str_replace_all("De Kalb", "DeKalb")   %>% 
  str_replace_all("\\.\\.", ".") %>% 
  str_replace_all("\\bLa Moure\\b", "LaMoure") %>% 
  str_replace_all("\\bSaint Francis\\b", "St. Francis")



complete_data <- soybeans_census %>%
  ungroup() %>%
  complete(county_state, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)

soybeans_census_final <- remove_zero_sum_columns(wide_data)

soybeans_census_final$`YIELD_BU / ACRE` = soybeans_census_final$PRODUCTION_BU/soybeans_census_final$`AREA HARVESTED_ACRES`

D <- as.data.frame(lapply(soybeans_census_final, function(x) ifelse(is.nan(x) | is.infinite(x), 0, x)))
names(D) <- names(soybeans_census_final)
soybeans_census_final <- D

# soybeans_census_final <- wide_data %>%
#   left_join(soybeans_county_geometry[, c("county_state", "geometry")], by = "county_state")
###################################################
# survey:
load("soybeans_survey.rda")

soybeans_survey[soybeans_survey == "(D)"] <- 0
soybeans_survey$VALUE <- as.numeric(gsub(",","",soybeans_survey$VALUE))

soybeans_survey2 <- data.frame(soybeans_survey, county_state = paste0(str_to_title(soybeans_survey$COUNTY_NAME, locale = "en"), " County, ", 
                                                                      str_to_title(soybeans_survey$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "STATE",
               FREQ_DESC == "ANNUAL",
               UTIL_PRACTICE_DESC == "ALL UTILIZATION PRACTICES", # other values: "OFF FARM","ON FARM" ,"AT HARVEST"
               REFERENCE_PERIOD_DESC == "YEAR",
               YEAR >= 2000,
               #!STATISTICCAT_DESC %in% c("PLANT POPULATION", "AREA PLANTED"),
               #!UNIT_DESC %in% c("PCT MILK", "PLANTS / ACRE"),
               PRODN_PRACTICE_DESC == "ALL PRODUCTION PRACTICES") %>% # "NON-IRRIGATED" , "IRRIGATED" ,"FOLLOWING ANOTHER CROP (DOUBLE CROPPED)", "NOT FOLLOWING ANOTHER CROP", "BIOTECH, HERBICIDE RESISTANT" 
  dplyr::select(STATE_NAME, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, STATE_NAME) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))

complete_data <- soybeans_survey2 %>%
  ungroup() %>%
  complete(STATE_NAME, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)


soybeans_survey_final <- remove_zero_sum_columns(wide_data)


# soybeans_survey_final <- wide_data %>%
#   left_join(soybeans_state_geometry, by = "STATE_NAME")

names(soybeans_state_geometry)[1] <- c("STATE_NAME")

soybeans_county_geometry <- soybeans_county_geometry %>%
  mutate(county_state = paste(NAMELSAD, STATE_NAME, sep = ", ")) %>% 
  select(county_state, geometry)


save(soybeans_survey_final, file = "Final_data\\new_data\\soybeans_survey_final.rda")
save(soybeans_census_final, file = "Final_data\\new_data\\soybeans_census_final.rda")
save(soybeans_state_geometry, file = "Final_data\\new_data\\soybeans_state_geometry.rda")
save(soybeans_county_geometry, file = "Final_data\\new_data\\soybeans_county_geometry.rda")


#####################################################
load("soybeans_survey.rda")
inx <- grep("WEEK", soybeans_survey$REFERENCE_PERIOD_DESC, value = FALSE)

unique(soybeans_survey$STATISTICCAT_DESC)

soybeans_survey_week <- soybeans_survey[inx,] %>% 
  arrange(WEEK_ENDING) %>% 
  filter(YEAR>=2000,
         STATISTICCAT_DESC %in% c("PROGRESS",
                                  "PROGRESS, PREVIOUS YEAR",
                                  "PROGRESS, 5 YEAR AVG",
                                  "CONDITION",
                                  "CONDITION, PREVIOUS YEAR",
                                  "CONDITION, 5 YEAR AVG"
                                  #"MOISTURE",
                                  #"MOISTURE, PREVIOUS YEAR",
                                  #"MOISTURE, 5 YEAR AVG",
                                  #"HEIGHT, AVG",
                                  #"HEIGHT, AVG, PREVIOUS YEAR",
                                  #"HEIGHT, AVG, 5 YEAR AVG"
                                  )) %>% 
  dplyr::select(WEEK_ENDING,
                REFERENCE_PERIOD_DESC,
                STATE_NAME,
                STATISTICCAT_DESC, 
                UNIT_DESC, 
                VALUE) %>% 
  group_by(WEEK_ENDING,
           REFERENCE_PERIOD_DESC,
           STATE_NAME,
           STATISTICCAT_DESC, 
           UNIT_DESC) %>% 
  summarise(sum_value = sum(as.numeric(gsub(",","",VALUE))))

# complete_data_week <- soybeans_survey_week %>%
#   ungroup() %>%
#   complete(STATE_NAME,REFERENCE_PERIOD_DESC,  WEEK_ENDING, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data_week <- soybeans_survey_week %>% #complete_data_week %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(WEEK_ENDING)

#soybeans_survey_week_final <- remove_zero_sum_columns(wide_data_week)

soybeans_survey_week_final <- wide_data_week %>% 
  mutate(good_excellent = `CONDITION_PCT EXCELLENT` + `CONDITION_PCT GOOD`,
         Year = year(WEEK_ENDING),
         Week_Num = as.numeric(gsub("WEEK #", "", REFERENCE_PERIOD_DESC))) %>%  # Extract numeric week number)
  arrange(STATE_NAME, Week_Num)

save(soybeans_survey_week_final, file = "Final_data\\new_data\\soybeans_survey_week_final.rda")


################

#Plot 1
df1 <- soybeans_survey_week_final %>% 
  ungroup() %>% 
  dplyr::select(REFERENCE_PERIOD_DESC, STATE_NAME, Year, Week_Num, good_excellent) %>% 
  filter(STATE_NAME == "INDIANA", 
         Year %in% 2020:2024) %>% 
  arrange(STATE_NAME, Week_Num)

df1$Year <- as.factor(df1$Year) 

# Get the last point with non-NA values for each year to add the label at the end of the line
df_last <- df1 %>%
  group_by(Year) %>%
  filter(!is.na(good_excellent)) %>%  # Filter out NA values
  filter(Week_Num == max(Week_Num))   # Select the last non-NA week for each year

# plot
ggplot(data = df1, 
       aes(x = Week_Num, y = good_excellent, color = Year, group = Year)) +
  geom_line() +  # Add lines for each year
  geom_point() +  # Add points for better visualization, but it can be removed
  # Add year label at the end of each line for every year
  geom_text(data = df_last, aes(label = Year), 
            hjust = -0.3,  # Adjust the horizontal position
            vjust = 0.5,   # Adjust the vertical position
            size = 4) +    # Size of the text
  labs(
    title = "Good Excellent Values by Week for State",
    x = "Week Number",
    y = "Good Excellent",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Remove legend since we’re adding labels
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1)))  # Add extra space on x-axis for labels

#######################################################
#######################################################
#######################################################

## potatoes data
# census:
load("potatoes_census.rda")

potatoes_census[potatoes_census == "(D)"] <- 0
potatoes_census$VALUE <- as.numeric(gsub(",","",potatoes_census$VALUE))

potatoes_census <- data.frame(potatoes_census, county_state = paste0(str_to_title(potatoes_census$COUNTY_NAME, locale = "en"), " County, ", 
                                                                     str_to_title(potatoes_census$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "COUNTY",
         YEAR >= 2000) %>% 
  dplyr::select(county_state, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, county_state) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))

potatoes_census$county_state <- potatoes_census$county_state %>%
  str_replace_all("\\bSt\\b", "St.") %>%
  str_replace_all("\\bMc([a-z])", function(x) {
    paste0("Mc", toupper(substr(x, 3, 3)), substr(x, 4, nchar(x)))
  }) %>%
  str_replace_all("\\bSte\\b", "Ste.") %>%
  str_replace_all("\\bO Brien\\b", "O'Brien") %>%
  str_replace_all("(?i)\\bdu Page\\b", "DuPage") %>% 
  str_replace_all("\\bLa Porte\\b", "LaPorte") %>%
  str_replace_all("\\bLa Salle\\b", "LaSalle") %>%
  str_replace_all("\\bFond Du Lac\\b", "Fond du Lac") %>%
  str_replace_all("\\bLake Of The Woods\\b", "Lake of the Woods") %>%
  str_replace_all("\\bLac Qui Parle\\b", "Lac qui Parle") %>%
  str_replace_all("\\bLagrange\\b", "LaGrange") %>%
  str_replace_all("\\bOf\\b", "of") %>%
  str_replace_all("\\bDu\\b", "du") %>%
  str_replace_all("\\bduPage\\b", "DuPage")  %>%
  str_replace_all("La Porte", "LaPorte") %>%         
  str_replace_all("De Kalb", "DeKalb")   %>% 
  str_replace_all("\\.\\.", ".") %>% 
  str_replace_all("\\bLa Moure\\b", "LaMoure") %>% 
  str_replace_all("\\bSaint Francis\\b", "St. Francis")

complete_data <- potatoes_census %>%
  ungroup() %>%
  complete(county_state, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)

potatoes_census_final <- remove_zero_sum_columns(wide_data)

potatoes_census_final$`YIELD_BU / ACRE` = (potatoes_census_final$PRODUCTION_CWT/potatoes_census_final$`AREA HARVESTED_ACRES`) * 1.667

D <- as.data.frame(lapply(potatoes_census_final, function(x) ifelse(is.nan(x) | is.infinite(x), 0, x)))
names(D) <- names(potatoes_census_final)
potatoes_census_final <- D

# potatoes_census_final <- wide_data %>%
#   left_join(potatoes_county_geometry[, c("county_state", "geometry")], by = "county_state")
###################################################

# survey:
load("potatoes_survey.rda")

potatoes_survey[potatoes_survey == "(D)"] <- 0
potatoes_survey$VALUE <- as.numeric(gsub(",","",potatoes_survey$VALUE))

potatoes_survey2 <- data.frame(potatoes_survey, county_state = paste0(str_to_title(potatoes_survey$COUNTY_NAME, locale = "en"), " County, ", 
                                                                      str_to_title(potatoes_survey$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "STATE",
         FREQ_DESC == "ANNUAL",
         UTIL_PRACTICE_DESC == "ALL UTILIZATION PRACTICES", # there are 7 more possible values ("FRESH MARKET", "PROCESSING" , "SEED" ,"SEED, CERTIFIED" , "HOME USE" ,"SEED, ENTERED FOR CERTIFICATION", "PROCESSING, CHIPS & SHOESTRINGS")
         REFERENCE_PERIOD_DESC == "YEAR",
         YEAR >= 2000,
         #!STATISTICCAT_DESC %in% c("PLANT POPULATION", "AREA PLANTED"),
         #!UNIT_DESC %in% c("PCT MILK", "PLANTS / ACRE"),
         PRODN_PRACTICE_DESC == "ALL PRODUCTION PRACTICES") %>% #"IRRIGATED", "NON-IRRIGATED"           
  dplyr::select(STATE_NAME, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, STATE_NAME) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))

complete_data <- potatoes_survey2 %>%
  ungroup() %>%
  complete(STATE_NAME, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)

potatoes_survey_final <- remove_zero_sum_columns(wide_data)


# potatoes_survey_final <- wide_data %>%
#   left_join(potatoes_state_geometry, by = "STATE_NAME")

names(potatoes_state_geometry)[1] <- c("STATE_NAME")

potatoes_county_geometry <- potatoes_county_geometry %>%
  mutate(county_state = paste(NAMELSAD, STATE_NAME, sep = ", ")) %>% 
  select(county_state, geometry)

save(potatoes_survey_final, file = "Final_data\\new_data\\potatoes_survey_final.rda")
save(potatoes_census_final, file = "Final_data\\new_data\\potatoes_census_final.rda")
save(potatoes_state_geometry, file = "Final_data\\new_data\\potatoes_state_geometry.rda")
save(potatoes_county_geometry, file = "Final_data\\new_data\\potatoes_county_geometry.rda")


#######################################################################
load("potatoes_survey.rda")
inx <- grep("WEEK", potatoes_survey$REFERENCE_PERIOD_DESC, value = FALSE)

potatoes_survey_week <- potatoes_survey[inx,] %>% 
  arrange(WEEK_ENDING) %>% 
  filter(YEAR>=2000,
         STATISTICCAT_DESC %in% c("PROGRESS",
                                  "PROGRESS, PREVIOUS YEAR",
                                  "PROGRESS, 5 YEAR AVG",
                                  "CONDITION",
                                  "CONDITION, PREVIOUS YEAR",
                                  "CONDITION, 5 YEAR AVG"
         )) %>% 
  dplyr::select(WEEK_ENDING,
                REFERENCE_PERIOD_DESC,
                STATE_NAME,
                STATISTICCAT_DESC, 
                UNIT_DESC, 
                VALUE) %>% 
  group_by(WEEK_ENDING,
           REFERENCE_PERIOD_DESC,
           STATE_NAME,
           STATISTICCAT_DESC, 
           UNIT_DESC) %>% 
  summarise(sum_value = sum(as.numeric(gsub(",","",VALUE))))

# complete_data_week <- potatoes_survey_week %>%
#   ungroup() %>%
#   complete(STATE_NAME,REFERENCE_PERIOD_DESC,  WEEK_ENDING, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data_week <- potatoes_survey_week %>% #complete_data_week %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(WEEK_ENDING)

#potatoes_survey_week_final <- remove_zero_sum_columns(wide_data_week)

potatoes_survey_week_final <- wide_data_week %>% 
  mutate(good_excellent = `CONDITION_PCT EXCELLENT` + `CONDITION_PCT GOOD`,
         Year = year(WEEK_ENDING),
         Week_Num = as.numeric(gsub("WEEK #", "", REFERENCE_PERIOD_DESC))) %>%  # Extract numeric week number)
  arrange(STATE_NAME, Week_Num)

save(potatoes_survey_week_final, file = "Final_data\\new_data\\potatoes_survey_week_final.rda")


################
unique(potatoes_survey_week_final$STATE_NAME)
#Plot 1
df1 <- potatoes_survey_week_final %>% 
  ungroup() %>% 
  dplyr::select(REFERENCE_PERIOD_DESC, STATE_NAME, Year, Week_Num, good_excellent) %>% 
  filter(STATE_NAME == "WISCONSIN", 
         Year %in% 2020:2024) %>% 
  arrange(STATE_NAME, Week_Num)

df1$Year <- as.factor(df1$Year) 
# Get the last point with non-NA values for each year to add the label at the end of the line
df_last <- df1 %>%
  group_by(Year) %>%
  filter(!is.na(good_excellent)) %>%  # Filter out NA values
  filter(Week_Num == max(Week_Num))   # Select the last non-NA week for each year
max(df1$Week_Num)
df1$Week_Num

# plot
ggplot(data = df1, 
       aes(x = Week_Num, y = good_excellent, color = Year, group = Year)) +
  geom_line() +  # Add lines for each year
  geom_point() +  # Add points for better visualization, but it can be removed
  # Add year label at the end of each line for every year
  geom_text(data = df_last, aes(label = Year), 
            hjust = -0.3,  # Adjust the horizontal position
            vjust = 0.5,   # Adjust the vertical position
            size = 4) +    # Size of the text
  labs(
    title = "Good Excellent Values by Week for State",
    x = "Week Number",
    y = "Good Excellent",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Remove legend since we’re adding labels
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1)))  # Add extra space on x-axis for labels
########################################
############################################################
################################################################


## wheat data
# census:
load("wheat_census.rda")

wheat_census[wheat_census == "(D)"] <- 0
wheat_census$VALUE <- as.numeric(gsub(",","",wheat_census$VALUE))

wheat_census <- data.frame(wheat_census, county_state = paste0(str_to_title(wheat_census$COUNTY_NAME, locale = "en"), " County, ", 
                                                               str_to_title(wheat_census$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "COUNTY",
         YEAR >= 2000) %>% 
  dplyr::select(county_state, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, county_state) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))

wheat_census$county_state <- wheat_census$county_state %>%
  str_replace_all("\\bSt\\b", "St.") %>%
  str_replace_all("\\bMc([a-z])", function(x) {
    paste0("Mc", toupper(substr(x, 3, 3)), substr(x, 4, nchar(x)))
  }) %>%
  str_replace_all("\\bSte\\b", "Ste.") %>%
  str_replace_all("\\bO Brien\\b", "O'Brien") %>%
  str_replace_all("(?i)\\bdu Page\\b", "DuPage") %>% 
  str_replace_all("\\bLa Porte\\b", "LaPorte") %>%
  str_replace_all("\\bLa Salle\\b", "LaSalle") %>%
  str_replace_all("\\bFond Du Lac\\b", "Fond du Lac") %>%
  str_replace_all("\\bLake Of The Woods\\b", "Lake of the Woods") %>%
  str_replace_all("\\bLac Qui Parle\\b", "Lac qui Parle") %>%
  str_replace_all("\\bLagrange\\b", "LaGrange") %>%
  str_replace_all("\\bOf\\b", "of") %>%
  str_replace_all("\\bDu\\b", "du") %>%
  str_replace_all("\\bduPage\\b", "DuPage")  %>%
  str_replace_all("La Porte", "LaPorte") %>%         
  str_replace_all("De Kalb", "DeKalb")   %>% 
  str_replace_all("\\.\\.", ".") %>% 
  str_replace_all("\\bLa Moure\\b", "LaMoure") %>% 
  str_replace_all("\\bSaint Francis\\b", "St. Francis")%>% 
  str_replace_all("\\bDe Witt\\b", "DeWitt")%>% 
  str_replace_all("\\bLeflore\\b", "Le Flore")%>% 
  str_replace_all("\\bLaSalle\\b", "La Salle")%>% 
  str_replace_all("\\bLewis And Clark\\b", "Lewis and Clark")%>% 
  str_replace_all("\\bDeWitt County, Illinois\\b", "De Witt County, Illinois")%>% 
  str_replace_all("\\bLa Salle County, Illinois\\b", "LaSalle County, Illinois")


complete_data <- wheat_census %>%
  ungroup() %>%
  complete(county_state, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)

wheat_census_final <- remove_zero_sum_columns(wide_data)

wheat_census_final$`YIELD_BU / ACRE` = wheat_census_final$PRODUCTION_BU/wheat_census_final$`AREA HARVESTED_ACRES`

D <- as.data.frame(lapply(wheat_census_final, function(x) ifelse(is.nan(x) | is.infinite(x), 0, x)))
names(D) <- names(wheat_census_final)
wheat_census_final <- D

# wheat_census_final <- wide_data %>%
#   left_join(wheat_county_geometry[, c("county_state", "geometry")], by = "county_state")
###################################################
# survey:
load("wheat_survey.rda")

wheat_survey[wheat_survey == "(D)"] <- 0
wheat_survey$VALUE <- as.numeric(gsub(",","",wheat_survey$VALUE))

wheat_survey2 <- data.frame(wheat_survey, county_state = paste0(str_to_title(wheat_survey$COUNTY_NAME, locale = "en"), " County, ", 
                                                                str_to_title(wheat_survey$STATE_NAME, locale = "en"))) %>% 
  filter(AGG_LEVEL_DESC == "STATE",
         FREQ_DESC == "ANNUAL",
         UTIL_PRACTICE_DESC == 'ALL UTILIZATION PRACTICES',
         REFERENCE_PERIOD_DESC == "YEAR",
         YEAR >= 2000,
         #STATISTICCAT_DESC != "PRODUCTION",
         UNIT_DESC != "BU / PLANTED ACRE",
         PRODN_PRACTICE_DESC == "ALL PRODUCTION PRACTICES") %>% 
  dplyr::select(STATE_NAME, YEAR, STATISTICCAT_DESC,UNIT_DESC, VALUE) %>% 
  group_by(YEAR, STATISTICCAT_DESC,UNIT_DESC, STATE_NAME) %>% 
  summarise(sum_value = sum(VALUE, na.rm = TRUE))


complete_data <- wheat_survey2 %>%
  ungroup() %>%
  complete(STATE_NAME, YEAR, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data <- complete_data %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(YEAR)


wheat_survey_final <- remove_zero_sum_columns(wide_data)

# wheat_survey_final <- wide_data %>%
#   left_join(wheat_state_geometry, by = "STATE_NAME")


names(wheat_state_geometry)[1] <- c("STATE_NAME")

wheat_county_geometry <- wheat_county_geometry %>%
  mutate(county_state = paste(NAMELSAD, STATE_NAME, sep = ", "))%>% 
  select(county_state, geometry)

save(wheat_survey_final, file = "Final_data\\new_data\\wheat_survey_final.rda")
save(wheat_census_final, file = "Final_data\\new_data\\wheat_census_final.rda")
save(wheat_state_geometry, file = "Final_data\\new_data\\wheat_state_geometry.rda")
save(wheat_county_geometry, file = "Final_data\\new_data\\wheat_county_geometry.rda")


#######################################################################
load("wheat_survey.rda")
inx <- grep("WEEK", wheat_survey$REFERENCE_PERIOD_DESC, value = FALSE)

unique(wheat_survey$STATISTICCAT_DESC)

wheat_survey_week <- wheat_survey[inx,] %>% 
  arrange(WEEK_ENDING) %>% 
  filter(YEAR>=2000,
         STATISTICCAT_DESC %in% c("PROGRESS",
                                  "PROGRESS, PREVIOUS YEAR",
                                  "PROGRESS, 5 YEAR AVG",
                                  "CONDITION",
                                  "CONDITION, PREVIOUS YEAR",
                                  "CONDITION, 5 YEAR AVG"
                                  #"DAMAGE", 
                                  #"DAMAGE, PREVIOUS YEAR", 
                                  #"DAMAGE, 5 YEAR AVG",
                                  #"HEIGHT, AVG",
                                  #"HEIGHT, AVG, PREVIOUS YEAR",
                                  #"HEIGHT, AVG, 5 YEAR AVG"
                                  )) %>% 
  dplyr::select(WEEK_ENDING,
                REFERENCE_PERIOD_DESC,
                STATE_NAME,
                STATISTICCAT_DESC, 
                UNIT_DESC, 
                VALUE) %>% 
  group_by(WEEK_ENDING,
           REFERENCE_PERIOD_DESC,
           STATE_NAME,
           STATISTICCAT_DESC, 
           UNIT_DESC) %>% 
  summarise(sum_value = sum(as.numeric(gsub(",","",VALUE))))

# complete_data_week <- wheat_survey_week %>%
#   ungroup() %>%
#   complete(STATE_NAME,REFERENCE_PERIOD_DESC,  WEEK_ENDING, STATISTICCAT_DESC, UNIT_DESC, fill = list(sum_value = 0))


wide_data_week <- wheat_survey_week %>% #complete_data_week %>%
  pivot_wider(
    names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
    values_from = sum_value
  ) %>% 
  arrange(WEEK_ENDING)

#wheat_survey_week_final <- remove_zero_sum_columns(wide_data_week)

wheat_survey_week_final <- wide_data_week %>% 
  mutate(good_excellent = `CONDITION_PCT EXCELLENT` + `CONDITION_PCT GOOD`,
         Year = year(WEEK_ENDING),
         Week_Num = as.numeric(gsub("WEEK #", "", REFERENCE_PERIOD_DESC))) %>%  # Extract numeric week number)
  arrange(STATE_NAME, Week_Num)

save(wheat_survey_week_final, file = "Final_data\\new_data\\wheat_survey_week_final.rda")


################

#Plot 1
df1 <- wheat_survey_week_final %>% 
  ungroup() %>% 
  dplyr::select(REFERENCE_PERIOD_DESC, STATE_NAME, Year, Week_Num, good_excellent) %>% 
  filter(STATE_NAME == "INDIANA", 
         Year %in% 2020:2024) %>% 
  arrange(STATE_NAME, Week_Num)

df1$Year <- as.factor(df1$Year) 

# Get the last point with non-NA values for each year to add the label at the end of the line
df_last <- df1 %>%
  group_by(Year) %>%
  filter(!is.na(good_excellent)) %>%  # Filter out NA values
  filter(Week_Num == max(Week_Num))   # Select the last non-NA week for each year

# plot
ggplot(data = df1, 
       aes(x = Week_Num, y = good_excellent, color = Year, group = Year)) +
  geom_line() +  # Add lines for each year
  geom_point() +  # Add points for better visualization, but it can be removed
  # Add year label at the end of each line for every year
  geom_text(data = df_last, aes(label = Year), 
            hjust = -0.3,  # Adjust the horizontal position
            vjust = 0.5,   # Adjust the vertical position
            size = 4) +    # Size of the text
  labs(
    title = "Good Excellent Values by Week for State",
    x = "Week Number",
    y = "Good Excellent",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Remove legend since we’re adding labels
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1)))  # Add extra space on x-axis for labels
########################################











