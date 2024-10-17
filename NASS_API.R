
#install.packages("rnassqs")

library(rnassqs)
library(tidyverse)

# You must set your api key before requesting data
nassqs_auth(key = "E815374B-D025-33BA-AF2A-990B8657C49B")

# Parameters to query on and data call
# look at the link: https://cran.r-project.org/web/packages/rnassqs/vignettes/rnassqs.html

library(rnassqs)


# ADJUST NEXT FEW LINES FOR EACH SECTOR AND YEAR
df = data.frame(matrix(nrow = 0, ncol = length(colnames(data)))) 
colnames(df) = colnames(data)
state_crops = data.frame()
state_crops

for(yearnumber in 2002:2022){
for(state in c("IN", "OH", "IL", "MI", "KY", "MO")){
# Set up the parameters
params <- list(
  sector_desc = "CROPS",
  year = yearnumber,
  state_alpha = state, 
  agg_level_desc = "STATE")
  data <- nassqs(params)
  df = rbind(df, data)
}

state_crops = rbind(state_crops, df)
}

write.csv(state_crops, "Complete_data/state_crops.csv", row.names = FALSE)
save(state_crops, file = "Complete_data/App/state_crops.rda")
df

# Write them from csv to RData for storage purposes
data <- read.csv("DATA_FILES/2022_county_environmental.csv")
save(data, file = "DATA_FILES/2022_county_environmental.RData")


# TO RDATA

# ADJUST FILES FOR EACH SECTOR AND YEAR - COMBINING INTO SINGLE FILE
files <- c("DATA_FILES/environmental/2002_county_environmental.RData", "DATA_FILES/environmental/2007_county_environmental.RData", "DATA_FILES/environmental/2012_county_environmental.RData", "DATA_FILES/environmental/2017_county_environmental.RData", "DATA_FILES/environmental/2022_county_environmental.RData")

# Load necessary library
library(dplyr)


# Initialize an empty list to store data frames
data_list <- list()

# Function to load and extract data frame from an RData file
load_dataframe <- function(file) {
  # Create a temporary environment
  temp_env <- new.env()
  
  # Load the RData file into the temporary environment
  load(file, envir = temp_env)
  
  # Extract the dataframe (assuming only one dataframe per file)
  data_frames <- ls(envir = temp_env)
  if (length(data_frames) == 1) {
    return(temp_env[[data_frames[1]]])
  } else {
    stop("Expected exactly one dataframe per RData file")
  }
}

# Iterate through each file, load and store the data frames
for (file in files) {
  df <- load_dataframe(file)
  data_list[[file]] <- df
}

# Combine all data frames into one
combined_data <- bind_rows(data_list)

save(combined_data, file = "DATA_FILES/county_environmental_combined.RData")

