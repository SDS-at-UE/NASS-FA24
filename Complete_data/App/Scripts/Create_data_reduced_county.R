library(tidyverse)
library(data.table)
library(sf)
library(dplyr)

setwd("/Users/andrewthompson/Desktop/NASS-FA24")


# CENSUS DATA ONLY

library(tidyverse)
load("DATA_FILES/combined files/ALL_COUNTY_DATA.RData")
colnames(combined)
head(unique(combined["domaincat_desc"]))

smaller_combined = combined %>% 
  mutate(county_state = paste0(county_name, " COUNTY, ", state_name)) %>% 
  dplyr::select(source_desc, year, agg_level_desc, sector_desc, statisticcat_desc, Value, group_desc, unit_desc, commodity_desc, short_desc, class_desc, domain_desc, county_state)

names(smaller_combined)[names(smaller_combined) == 'year'] <- 'YEAR'

unique(smaller_combined$commodity_desc)

corn_county_harvest_census_acres <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


corn_county_harvest_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

corn_county_sales_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_sales_census_dollar <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

corn_county_production_census_bu <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_production_census_ton <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_production_census_lb <- smaller_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


###############################################
# SOYBEANS
soybeans_county_harvest_census_acres <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


soybeans_county_harvest_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

soybeans_county_sales_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_county_sales_census_dollar <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

soybeans_county_production_census_bu <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_county_production_census_ton <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_county_production_census_lb <- smaller_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(soybeans_county_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

##########################################################################################
# WHEAT

wheat_county_harvest_census_acres <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


wheat_county_harvest_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

wheat_county_sales_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

wheat_county_sales_census_dollar <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

wheat_county_production_census_bu <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

wheat_county_production_census_ton <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

wheat_county_production_census_lb <- smaller_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(wheat_county_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


##########################################################################################
# POTATOES

potatoes_county_harvest_census_acres <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


potatoes_county_harvest_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

potatoes_county_sales_census_operation <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

potatoes_county_sales_census_dollar <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

potatoes_county_production_census_bu <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

potatoes_county_production_census_ton <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

potatoes_county_production_census_lb <- smaller_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'COUNTY',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(county_state, YEAR, Value) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(potatoes_county_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


# COMBINING

load("Complete_data/App/Data/County_geometry.rda")

county_geometry_main <- County_geometry %>% 
  select(county_state, geometry) %>% 
  distinct()
county_geometry_main$county_state <- toupper(county_geometry_main$county_state)


YEAR <- c()
for(i in unique(corn_county_harvest_census_acres$YEAR)){
  YEAR = c(YEAR, rep(i, dim(county_geometry_main)[1]))
}

county_geo_census <- data.frame(county_state = toupper(rep(county_geometry_main$county_state,5)),
                                     YEAR = YEAR,
                                     geometry = rep(county_geometry_main$geometry,5))

county_census <- reduce(list(county_geo_census,
                                  corn_county_harvest_census_acres,
                                  corn_county_harvest_census_operation,
                                  corn_county_sales_census_operation,
                                  corn_county_sales_census_dollar,
                                  corn_county_production_census_bu,
                                  corn_county_production_census_ton,
                                  corn_county_production_census_lb,
                                  

                                  soybeans_county_harvest_census_acres,
                                  soybeans_county_harvest_census_operation,
                                  soybeans_county_sales_census_operation,
                                  soybeans_county_sales_census_dollar,
                                  soybeans_county_production_census_bu,
                                  soybeans_county_production_census_ton,
                                  soybeans_county_production_census_lb,
                                  
                                  
                                  wheat_county_harvest_census_acres,
                                  wheat_county_harvest_census_operation,
                                  wheat_county_sales_census_operation,
                                  wheat_county_sales_census_dollar,
                                  wheat_county_production_census_bu,
                                  wheat_county_production_census_ton,
                                  wheat_county_production_census_lb,
                                  
                                  
                                  potatoes_county_harvest_census_acres,
                                  potatoes_county_harvest_census_operation,
                                  potatoes_county_sales_census_operation,
                                  potatoes_county_sales_census_dollar,
                                  potatoes_county_production_census_bu,
                                  potatoes_county_production_census_ton,
                                  potatoes_county_production_census_lb), 
                             dplyr::left_join, by = c("county_state", "YEAR"))


length(corn_county_harvest_census_acres$county_state)
length(unique(corn_county_sales_census_operation$county_state))
corn_county_sales_census_operation



length(county_geo_corn_census$county_state)
length(unique(county_geo_corn_census$county_state))
str(county_geo_census)




nrow(corn_county_harvest_census_acres)
colnames(corn_county_census)

setwd("/Users/andrewthompson/Desktop/NASS-FA24/Complete_data/App/Data")

save(county_census, file = "smaller_county_census.rda")
county_census
setwd("/Users/andrewthompson/Desktop/NASS-FA24")
county_census %>% filter(!is.na(wheat_county_sales_census_dollar))

########################################################################################################################
########################################################################################################################
########################################################################################################################

# STATE FROM CENSUS DATA ONLY
load("Complete_data/App/Data/state_crops.rda")

colnames(state_crops)
head(unique(state_crops["domaincat_desc"]))


state_combined = state_crops %>% 
  dplyr::select(source_desc, year, agg_level_desc, sector_desc, statisticcat_desc, Value, group_desc, unit_desc, commodity_desc, short_desc, class_desc, domain_desc, state_name)
head(unique(state_combined$agg_level_desc))
names(state_combined)[names(state_combined) == 'year'] <- 'YEAR'
unique(state_combined$commodity_desc)

corn_state_harvest_census_acres <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  dplyr::select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)
corn_state_harvest_census_acres


corn_state_harvest_census_operation <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

corn_state_sales_census_operation <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

corn_state_sales_census_dollar <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

corn_state_production_census_bu <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

corn_state_production_census_ton <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

corn_state_production_census_lb <- state_combined %>% 
  filter(commodity_desc == "CORN",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(corn_state_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

###########################################################################
# soybeans

soybeans_state_harvest_census_acres <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  dplyr::select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_state_harvest_census_operation <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

soybeans_state_sales_census_operation <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_state_sales_census_dollar <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

soybeans_state_production_census_bu <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_state_production_census_ton <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

soybeans_state_production_census_lb <- state_combined %>% 
  filter(commodity_desc == "SOYBEANS",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(soybeans_state_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#####################################
# Potatoes

potatoes_state_harvest_census_acres <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  dplyr::select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)
potatoes_state_harvest_census_acres


potatoes_state_harvest_census_operation <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

potatoes_state_sales_census_operation <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

potatoes_state_sales_census_dollar <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

potatoes_state_production_census_bu <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

potatoes_state_production_census_ton <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

potatoes_state_production_census_lb <- state_combined %>% 
  filter(commodity_desc == "POTATOES",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(potatoes_state_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#############################################################################
# WHEAT

wheat_state_harvest_census_acres <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "ACRES") %>% 
  dplyr::select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_harvest_census_acres = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)
wheat_state_harvest_census_acres


wheat_state_harvest_census_operation <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "AREA HARVESTED",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_harvest_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

#######
#SALES#
#######

wheat_state_sales_census_operation <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "OPERATIONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_sales_census_operation = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

wheat_state_sales_census_dollar <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "SALES",
         source_desc == 'CENSUS',
         unit_desc == "$") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_sales_census_dollar = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)


############
#Production#
############

wheat_state_production_census_bu <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "BU") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_production_census_bu = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

wheat_state_production_census_ton <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "TONS") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_production_census_ton = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)

wheat_state_production_census_lb <- state_combined %>% 
  filter(commodity_desc == "WHEAT",
         agg_level_desc == 'STATE',
         statisticcat_desc == "PRODUCTION",
         source_desc == 'CENSUS',
         unit_desc == "LB") %>% 
  select(state_name, YEAR, Value) %>% 
  group_by(state_name, YEAR) %>% 
  summarise(wheat_state_production_census_lb = sum(Value, na.rm=T)) %>% 
  arrange(YEAR)








# COMBINING

load("Complete_data/App/Data/states_geometry.rda")
colnames(states_geometry) = c("state_name", "geometry")
state_geometry_main <- states_geometry %>% 
  select(state_name, geometry) %>% 
  distinct()
state_geometry_main$state_name <- toupper(state_geometry_main$state_name)


year <- c()
for(i in unique(corn_state_harvest_census_acres$YEAR)){
  year = c(year, rep(i, dim(state_geometry_main)[1]))
}
year

state_geo_census <- data.frame(state_name = toupper(rep(state_geometry_main$state_name,14)),
                                     YEAR = year,
                                     geometry = rep(state_geometry_main$geometry,14))

state_census <- reduce(list(state_geo_census,
                            corn_state_harvest_census_acres,
                            corn_state_harvest_census_operation,
                            corn_state_sales_census_operation,
                            corn_state_sales_census_dollar,
                            corn_state_production_census_bu,
                            corn_state_production_census_ton,
                            corn_state_production_census_lb,
                            
                            
                            soybeans_state_harvest_census_acres,
                            soybeans_state_harvest_census_operation,
                            soybeans_state_sales_census_operation,
                            soybeans_state_sales_census_dollar,
                            soybeans_state_production_census_bu,
                            soybeans_state_production_census_ton,
                            soybeans_state_production_census_lb,
                            
                            
                            wheat_state_harvest_census_acres,
                            wheat_state_harvest_census_operation,
                            wheat_state_sales_census_operation,
                            wheat_state_sales_census_dollar,
                            wheat_state_production_census_bu,
                            wheat_state_production_census_ton,
                            wheat_state_production_census_lb,
                            
                            
                            potatoes_state_harvest_census_acres,
                            potatoes_state_harvest_census_operation,
                            potatoes_state_sales_census_operation,
                            potatoes_state_sales_census_dollar,
                            potatoes_state_production_census_bu,
                            potatoes_state_production_census_ton,
                            potatoes_state_production_census_lb), 
                             dplyr::left_join, by = c("state_name", "YEAR"))


length(corn_state_harvest_census_acres$state_name)
length(unique(corn_state_sales_census_operation$state_name))
corn_state_sales_census_operation



length(state_geo_census$state_name)
length(unique(state_geo_census$state_name))
str(state_geo_census)
state_census

nrow(corn_state_harvest_census_acres)
setwd("/Users/andrewthompson/Desktop/NASS-FA24/Complete_data/App/Data")
save(state_census, file = "smaller_state_census.rda")
state_census
setwd("/Users/andrewthompson/Desktop/NASS-FA24")
# CHECK IF THERE ARE COLUMNS WITHOUT MISSING VALUES
corn_state_census %>% filter(!is.na(corn_state_sales_census_operation))
