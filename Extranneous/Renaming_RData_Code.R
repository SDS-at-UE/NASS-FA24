library(tidyverse)
library(readr)

data <- read.csv(/Users/bige/Desktop/"census2002.csv")

ddata <-

crop_data
setwd("C:/")
save(crop_date, file = "")


data <- read.csv("2002_county_crops.csv")
save(data, file = "2002_county_crops.RData")


data1 = get(load("DATA_FILES/crops/2017_county_crops.RData"))
view(data1)
yes = data.frame(data1)
yes

library(c("httr", "readr", "dplyr"))
library(httr)
library(readr)
library(dplyr)


# Set working directory (if needed)
setwd("DATA_FILES/crops")

# Load the .RData file
load("2017_county_crops.RData")

# Check loaded objects
ls()

# Assuming it contains a data frame named 'df'
head(data)
view(data)
View(data)

countycrop2017 <- data
ls()

load("2022_county_crops.RData")
ls()
View(data)
countycrop2022 <- data

load("2012_county_crops.RData")
ls()
View(data)
countycrop2012 <- data

load("2007_county_crops.RData")
ls()
View(data)
countycrop2007 <- data
View(countycrop2007)
View(data)

load("2002_county_crops.RData")
ls()
View(data)
countycrop2002 <- data
View(countycrop2002)

load("1977_county_crops.RData")
ls()
View(data)
countycrop1977 <- data

load("crops_2018_2023.RData")
ls()
View(data)
census_survey_2018_2023 <- data
View(census_survey_2018_2023)
