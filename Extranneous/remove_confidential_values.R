library(tidyverse)
crops = get(load("DATA_FILES/county_crop_combined.RData"))
enviro = get(load("DATA_FILES/county_environmental_combined.RData"))
econom = get(load("DATA_FILES/county_economics_combined.RData"))
demogr = get(load("DATA_FILES/county_demographics_combined.RData"))
animal = get(load("DATA_FILES/county_animal_products_combined.RData"))

combined = rbind(crops, enviro, econom, demogr, animal)


combined$CV = as.numeric(replace(combined$CV, combined$CV %in% c("(D)", "(H)", "(L)"), NA))
combined$CV = as.numeric(replace(combined$Value, combined$Value %in% c("(D)", "(H)", "(L)"), NA))

save(combined, file = "DATA_FILES/ALL_COUNTY_DATA.RData")
print("saved")

head(combined)
table(combined$county_code)

get(load("DATA_FILES/ALL_COUNTY_DATA.RData"))

table(combined$sector_desc)


