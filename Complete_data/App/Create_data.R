library(tidyverse)
library(data.table)
library(sf)

setwd("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\App\\")
load("selected_crop_data.rda")
load("County_geometry.rda")
load("states_geometry.rda")

###############################################
  
crop_data <- selected_crop_data
crop_data <- data.frame(crop_data, county_state = paste0(str_to_title(crop_data$COUNTY_NAME, locale = "en"), " County, ", 
                                     str_to_title(crop_data$STATE_NAME, locale = "en")))

crop_data[crop_data == "(D)"] <- NA
crop_data$VALUE <- as.numeric(gsub(",","",crop_data$VALUE))

#####################################

county_geometry_main <- County_geometry %>% 
  select(county_state, geometry) %>% 
  distinct()
########################################################

#HARVEST
corn_county_harvest_survey_acres <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "AREA HARVESTED",
         SOURCE_DESC == 'SURVEY',
         UNIT_DESC == "ACRES") %>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_harvest_survey_acres = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)



corn_county_harvest_census_acres <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "AREA HARVESTED",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "ACRES") %>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_harvest_census_acres = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_harvest_census_operation <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "AREA HARVESTED",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "OPERATIONS") %>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_harvest_census_operation = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)




############################################
#SALES
corn_county_sales_census_operation <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "SALES",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "OPERATIONS")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_sales_census_operation = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_sales_census_dollor <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "SALES",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "$")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_sales_census_dollor = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

# sales is only for cencus data
corn_county_sales_survey <- c()


##############################################
#Production

corn_county_production_census_bu <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "PRODUCTION",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "BU")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_census_bu = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_production_census_ton <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "PRODUCTION",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "TONS")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_census_ton = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_production_census_lb <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "PRODUCTION",
         SOURCE_DESC == 'CENSUS',
         UNIT_DESC == "LB")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_census_lb = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)


corn_county_production_survey_bu <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "PRODUCTION",
         SOURCE_DESC == 'SURVEY',
         UNIT_DESC == "BU")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_survey_bu = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_production_survey_ton <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "PRODUCTION",
         SOURCE_DESC == 'SURVEY',
         UNIT_DESC == "TONS")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_production_survey_ton = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

#########################################################
#  YIELD: Only survey
corn_county_yield_census <- c()

corn_county_yield_survey_bu_acre <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "YIELD",
         SOURCE_DESC == 'SURVEY',
         UNIT_DESC == "BU / ACRE")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_yield_survey_bu_acre = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

corn_county_yield_survey_ton_acre <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "YIELD",
         SOURCE_DESC == 'SURVEY',
         UNIT_DESC == "TONS / ACRE")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_yield_survey_ton_acre = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)


###########################################
#AREA PLANTED: ONLY SURVEY
corn_county_planted_census <- c()  # empty

corn_county_planted_survey_acre <- crop_data %>% 
  filter(COMMODITY_DESC == "CORN",
         AGG_LEVEL_DESC == 'COUNTY',
         STATISTICCAT_DESC == "AREA PLANTED",
         SOURCE_DESC == 'SURVEY',
         UNIT_DESC == "ACRES")%>% 
  select(county_state, YEAR, VALUE) %>% 
  group_by(county_state, YEAR) %>% 
  summarise(corn_county_planted_survey_acre = sum(VALUE, na.rm=T)) %>% 
  arrange(YEAR)

##############################################################


#Merging

# Survey

# corn_county_sales_survey <- c() # empty
# corn_county_production_survey_bu
# corn_county_production_survey_ton
# corn_county_yield_survey_bu_acre
# corn_county_yield_survey_ton_acre
# corn_county_planted_survey_acre


year <- c()
for(i in c(unique(corn_county_harvest_survey_acres$YEAR))){
  year = c(year, rep(i, dim(county_geometry_main)[1]))
}
county_geo_corn_survey <- data.frame(county_state = rep(county_geometry_main$county_state,24),
                                            YEAR = year,
                                            geometry = rep(county_geometry_main$geometry,24))

# corn_county_survey <- left_join(county_geo_corn_county_survey,
#                                 corn_county_harvest_survey_acres,
#                                 by = c("county_state", "YEAR"))

corn_county_survey <- reduce(list(county_geo_corn_survey,
                                  corn_county_harvest_survey_acres,
                                  corn_county_production_survey_bu,
                                  corn_county_production_survey_ton,
                                  corn_county_yield_survey_bu_acre,
                                  corn_county_yield_survey_ton_acre,
                                  corn_county_planted_survey_acre), 
                             dplyr::left_join, by = c("county_state", "YEAR"))



save(corn_county_survey, file = "corn_county_survey_all.rda")

###########
###########
###########

# Cencus

# corn_county_sales_census_operation
# corn_county_sales_census_dollor
# corn_county_production_census_bu
# corn_county_production_census_ton
# corn_county_production_census_lb
# corn_county_yield_census <- c() # empty
# corn_county_planted_census <- c()  # empty


year <- c()
for(i in unique(corn_county_harvest_census_acres$YEAR)){
  year = c(year, rep(i, dim(county_geometry_main)[1]))
}
county_geo_corn_census <- data.frame(county_state = rep(county_geometry_main$county_state,5),
                                     YEAR = year,
                                     geometry = rep(county_geometry_main$geometry,5))


corn_county_cencus <- reduce(list(county_geo_corn_census,
                                  corn_county_harvest_census_acres,
                                  corn_county_harvest_census_operation,
                                  corn_county_sales_census_operation,
                                  corn_county_sales_census_dollor,
                                  corn_county_production_census_bu,
                                  corn_county_production_census_ton,
                                  corn_county_production_census_lb), 
                             dplyr::left_join, by = c("county_state", "YEAR"))

save(corn_county_cencus, file = "corn_county_cencus_all.rda")

#########################
corn_county_cencus <- corn_county_cencus %>% 
  select(-geometry)

corn_county <- left_join(corn_county_survey,
                         corn_county_cencus,
                         by = c("county_state", "YEAR"))

corn_county <- data.frame(corn_county,
                          corn_county_harvest_survey_operation = rep(NA, nrow(corn_county)),
                          corn_county_yield_census_bu_acre = rep(NA, nrow(corn_county)),
                          corn_county_yield_census_ton_acre = rep(NA, nrow(corn_county)),
                          corn_county_planted_census_acre  = rep(NA, nrow(corn_county)),
                          corn_county_sales_survey_dollor = rep(NA, nrow(corn_county)),
                          corn_county_sales_survey_operation = rep(NA, nrow(corn_county)),
                          corn_county_production_survey_lb = rep(NA, nrow(corn_county)))
                          

save(corn_county, file = "corn_county.rda")





# popup_msg <- reactive({
#   if(length(strsplit(as.character(req(input$unit)), ""))!=0){
#     if(as.character(input$source) == "CENSUS"){
#       str_c("<strong>", dates()$county_state, #", ", dates()$State,
#             "</strong><br /><strong>", dates()$YEAR, "</strong>",
#             "<br /> ACRES_harvested: ", ifelse(is.zero(dates()$corn_county_harvest_census_acres), "(D)", dates()$corn_county_harvest_census_acres),
#             "<br /> OPERATIONS_harvested: ", ifelse(is.zero(dates()$corn_county_harvest_census_operation), "(D)", dates()$corn_county_harvest_census_operation),
#             "<br /> ACRES_plated: ",  ifelse(is.zero(dates()$corn_county_planted_census_acre), "(D)", dates()$corn_county_planted_census_acre),
#             "<br /> BU_production: ",  ifelse(is.zero(dates()$corn_county_production_census_bu), "(D)", dates()$corn_county_production_census_bu),
#             "<br /> TONS_production: ",  ifelse(is.zero(dates()$corn_county_production_census_ton), "(D)", dates()$corn_county_production_census_ton),
#             "<br /> LB_production: ",  ifelse(is.zero(dates()$corn_county_production_census_lb), "(D)", dates()$corn_county_production_census_lb),
#             "<br /> BU_ACRE_yield: ",  ifelse(is.zero(dates()$corn_county_yield_census_bu_acre), "(D)", dates()$corn_county_yield_census_bu_acre),
#             "<br /> TONS_ACRE_yield: ",  ifelse(is.zero(dates()$corn_county_yield_census_ton_acre), "(D)", dates()$corn_county_yield_census_ton_acre),
#             "<br /> OPERATIONS_sales: ",  ifelse(is.zero(dates()$corn_county_sales_census_operation), "(D)", dates()$corn_county_sales_census_operation),
#             "<br /> Dollar_sales: ",  ifelse(is.zero(dates()$corn_county_sales_census_dollor), "(D)", dates()$corn_county_sales_census_dollor))
# 
#     }else if(as.character(input$source) == "SURVEY"){
#       str_c("<strong>", dates()$county_state, #", ", dates()$State,
#             "</strong><br /><strong>", dates()$YEAR, "</strong>",
#             "<br /> ACRES_harvested: ",  ifelse(is.zero(dates()$corn_county_harvest_survey_acres), "(D)", dates()$corn_county_harvest_survey_acres),
#             "<br /> ACRES_harvested: ",  ifelse(is.zero(dates()$corn_county_harvest_survey_operation), "(D)", dates()$corn_county_harvest_survey_operation),
#             "<br /> ACRES_plated: ",  ifelse(is.zero(dates()$corn_county_planted_survey_acre), "(D)", dates()$corn_county_planted_survey_acre),
#             "<br /> BU_production: ",  ifelse(is.zero(dates()$corn_county_production_survey_bu), "(D)", dates()$corn_county_production_survey_bu),
#             "<br /> TONS_production: ",  ifelse(is.zero(dates()$corn_county_production_survey_ton), "(D)", dates()$corn_county_production_survey_ton),
#             "<br /> LB_production: ",  ifelse(is.zero(dates()$corn_county_production_survey_lb), "(D)", dates()$corn_county_production_survey_lb),
#             "<br /> BU_ACRE_yield: ",  ifelse(is.zero(dates()$corn_county_yield_survey_bu_acre), "(D)", dates()$corn_county_yield_survey_bu_acre),
#             "<br /> TONS_ACRE_yield: ",  ifelse(is.zero(dates()$corn_county_yield_survey_ton_acre), "(D)", dates()$corn_county_yield_survey_ton_acre),
#             "<br /> OPERATIONS_sales: ",  ifelse(is.zero(dates()$corn_county_sales_survey_operation), "(D)", dates()$corn_county_sales_survey_operation),
#             "<br /> Dollar_sales: ",  ifelse(is.zero(dates()$corn_county_sales_survey_dollor), "(D)", dates()$corn_county_sales_survey_dollor))
#     }
#   }
# 
# 
# })
