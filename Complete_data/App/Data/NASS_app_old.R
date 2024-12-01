options(timeout=300)

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)
library(bslib)
library(imputeTS)
library(data.table)
# install.packages("imputeTS")

# setwd("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\App")
setwd("/Users/andrewthompson/Desktop/NASS-FA24/Complete_data/App")


#######################  Loading the data  #############################


load("states_geometry.rda")  # states_geometry data
states_map2 <- states_geometry %>%
  sf::st_set_crs(4326) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

#load("corn_county_cencus.rda")
#load("corn_county_survey.rda")
load("corn_county.rda")

# corn_county_survey <- corn_county_survey %>%
#   mutate(Production = rep(NA, nrow(corn_county_survey)))



#layer_county <- unique(corn_county_cencus$county_state)
layer_county <- unique(corn_county$county_state)

###################### Setting the color range ##############################


color_pal1 <- colorRampPalette(colors = c("springgreen4", "yellow3"), space = "Lab")(2)

## Make vector of colors for second bin
#color_pal2 <- colorRampPalette(colors = c("yellow3", "orange"), space = "Lab")(5)

## Make vector of colors for third bin
color_pal3 <- colorRampPalette(colors = c("orange", "red3"), space = "Lab")(15)

## Make vector of colors for fourth bin
#color_pal4 <- colorRampPalette(colors = c("red3", "darkred"), space = "Lab")(5)

## Make vector of colors for last bin
color_pal5 <- colorRampPalette(colors = c("darkred", "black"), space = "Lab")(5)

## Combine the five color palettes
color_pal <- c(color_pal1, color_pal3, color_pal5)#, color_pal4, color_pal5)
#color_pal <- c(color_pal1, color_pal2, color_pal3, color_pal4, color_pal5)


################################## Helper functions  #############################

## https://gist.github.com/addiversitas/d2659ff553f702d60105a97fe46261a0

#helper functions for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

setShapeLabel <- function(map, data = getMapData(map), 
                          layerId,
                          label = NULL,
                          options = NULL){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)


validate_input <- function(input_value) {
  if (length(input_value) != 1) {
    return(NULL)  # You can return NULL or an appropriate error message
  }
  return(TRUE)  # If valid, return TRUE
}
########################################################################
# Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, 
# lumen, paper, readable, sandstone, simplex, slate, spacelab, 
# superhero, united, yeti.

ui <- navbarPage(leafletjs, theme = shinytheme("cosmo"),
                title = "Agriculture Data Analysis Portal",
                tabPanel('Crops',
                         #titlePanel("Crops"),
                         fluidRow( column(3,),
                                   column(6,sliderInput(inputId = "dates", "Timeline of Selected Parameter", 
                                                        min = 2000L, #min(c(corn_county_cencus$YEAR, corn_county_survey$YEAR)),
                                                        max = 2023L, #max(c(corn_county_cencus$YEAR, corn_county_survey$YEAR)),
                                                        value = 2002L,
                                                        sep = "",
                                                        #timeFormat = "%m-%d-%Y",
                                                        step = 1,
                                                        ticks = FALSE,
                                                        animate = animationOptions(interval = 1000),
                                   ),
                                   ),
                                   column(3,)        
                         ),
                         fluidRow(
                           column(3,
                                  wellPanel(
                                    selectInput(inputId = "level", "Choose a level", 
                                                c("County", "State"),
                                                selected = "County"),
                                    
                                    selectInput(inputId = "crop", "Choose a crop", 
                                                c("Corn", "Soybean"),
                                                selected = "Corn"),
                                    
                                    selectInput(inputId = "stat", "Choose a STATISTICCAT_DESC", 
                                                c("PRODUCTION", "YIELD", "AREA PLANTED", "AREA HARVESTED", "SALES"),
                                                selected = "AREA HARVESTED"),
                                    
                                    selectInput(inputId = "source", "Choose a SOURCE_DESC", 
                                                c("","CENSUS", "SURVEY"),
                                                selected = NULL),
                                    
                                    # selectInput(inputId = "unit", "Choose a UNIT_DESC",
                                    #             c("ACRES", "OPERATIONS"),
                                    #             selected = "ACRES"),
                                    # 
                                    uiOutput("unit"),
                                  )
                                  
                           ),
                           column(9,
                                  leafletOutput("map_pop"),
                                  
                                  
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  DT::dataTableOutput("tab1"),
                                  br(),
                                  textOutput("test"),
                                  
                                  )
                         ),
                         fluidRow(
                           column(2),
                           column(8,
                                  h5("Plot of Acres in the selected county"),
                                  plotOutput("plot_acres"),
                                  br(),
                                  br(),
                                  h5("Plot of Operations in the selected county"),
                                  plotOutput("plot_operations"),
                                  br(),
                                  br(),
                                  h5("Sales Plot in the selected county"),
                                  plotOutput("plot_sales"),
                                  br(),
                                  br(),
                                  h5("Normalized Sales Plot in the selected county"),
                                  plotOutput("plot_acres_norm"),
                                  br(),
                                  br(),
                                  h5("Yield Plot in the selected county"),
                                  plotOutput("plot_yield"),
                                  br(),
                                  br(),
                                  h5("Harvested Area Plot in the selected county"),
                                  plotOutput("plot_harvested"),
                                  br(),
                                  br(),
                                  h5("Planted Area Plot in the selected county"),
                                  plotOutput("plot_planted"),
                                  br(),
                                  br(),
                                  h5("Production Plot in the selected county"),
                                  plotOutput("plot_production"),
                                  
                           ),
                           column(2)
                         ),
                         
                         
                         
                         )
                )


server <- function(input, output,session) {
  ### Map Interface & Climate Grid Selection
  GRIDrv <- reactiveVal()
  
  #c("PRODUCTION", "YIELD", "AREA PLANTED", "AREA HARVESTED", "SALES")
  output$unit <- renderUI({
    if(length(strsplit(as.character(req(input$source)), ""))!=0){ #"CENSUS", "SURVEY"
      if(input$stat == 'AREA HARVESTED'){
        x = c("","ACRES_harvested", "OPERATIONS_harvested")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                       selected = NULL)
      }else if(input$stat == 'AREA PLANTED'){
        x = c(NULL,"ACRES_plated")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                       choices = x,
                       selected = "")
      }else if(input$stat == 'PRODUCTION'){
        x = c(NULL,"BU_production", "TONS_production", "LB_production")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                    selected = "")
      }else if(input$stat == 'YIELD'){
        x = c(NULL,"BU_ACRE_yield", "TONS_ACRE_yield")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                    selected = "")
      }else if(input$stat == 'SALES'){
        x = c(NULL,"OPERATIONS_sales", "Dollar_sales")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                    selected = "")
      }
    }
    
    
  })
  
  data_new4 <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      st_as_sf(corn_county) %>%
        sf::st_set_crs(4326) %>% 
        sf::st_transform('+proj=longlat +datum=WGS84')
      
      # if(as.character(input$source) == "CENSUS"){
      #   st_as_sf(corn_county_cencus) %>%
      #     sf::st_set_crs(4326) %>% 
      #     sf::st_transform('+proj=longlat +datum=WGS84')
      # }else if(as.character(input$source) == "SURVEY"){
      #   st_as_sf(corn_county_survey) %>%
      #     sf::st_set_crs(4326) %>% 
      #     sf::st_transform('+proj=longlat +datum=WGS84')
      # }
    }
    
  })
  
  
  # Update slider choices based on valid years
  # observe({
  #   if(as.character(input$source) == "CENSUS"){
  #     updateSliderInput(session, "dates", 
  #                       min = min(data_new4()$YEAR), 
  #                       max = max(data_new4()$YEAR), 
  #                       value = min(data_new4()$YEAR), 
  #                       step = 5)
  #   }else if(as.character(input$source) == "SURVEY"){
  #     updateSliderInput(session, "dates", 
  #                       min = min(data_new4()$YEAR), 
  #                       max = max(data_new4()$YEAR), 
  #                       value = min(data_new4()$YEAR), 
  #                       step = 1)
  #   }
  # })
  
  dates <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      data_new4() %>% 
        filter(YEAR == as.numeric(input$dates))
    }
    
  })
  
  reactive_data <-  reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(as.character(input$source) == "CENSUS"){
        switch(input$unit,
               ACRES_harvested = data_new4()$corn_county_harvest_census_acres,
               OPERATIONS_harvested = data_new4()$corn_county_harvest_census_operation,
               ACRES_plated = data_new4()$corn_county_planted_census_acre,
               BU_production = data_new4()$corn_county_production_census_bu,
               TONS_production = data_new4()$corn_county_production_census_ton,
               LB_production = data_new4()$corn_county_production_census_lb,
               BU_ACRE_yield = data_new4()$corn_county_yield_census_bu_acre,
               TONS_ACRE_yield = data_new4()$corn_county_yield_census_ton_acre,
               OPERATIONS_sales = data_new4()$corn_county_sales_census_operation,
               Dollar_sales = data_new4()$corn_county_sales_census_dollor
               
        )
      }else if(as.character(input$source) == "SURVEY"){
        switch(input$unit,
               ACRES_harvested = data_new4()$corn_county_harvest_survey_acres,
               OPERATIONS_harvested = data_new4()$corn_county_harvest_survey_operation,
               ACRES_plated = data_new4()$corn_county_planted_survey_acre,
               BU_production = data_new4()$corn_county_production_survey_bu,
               TONS_production = data_new4()$corn_county_production_survey_ton,
               LB_production = data_new4()$corn_county_production_survey_lb,
               BU_ACRE_yield = data_new4()$corn_county_yield_survey_bu_acre,
               TONS_ACRE_yield = data_new4()$corn_county_yield_survey_ton_acre,
               OPERATIONS_sales = data_new4()$corn_county_sales_survey_operation,
               Dollar_sales = data_new4()$corn_county_sales_survey_dollor
        )
      }
      
    }
     
  })
  
  reactive_stat <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(as.character(input$source) == "CENSUS"){
        switch(input$unit,
               ACRES_harvested = dates()$corn_county_harvest_census_acres,
               OPERATIONS_harvested = dates()$corn_county_harvest_census_operation,
               ACRES_plated = dates()$corn_county_planted_census_acre,
               BU_production = dates()$corn_county_production_census_bu,
               TONS_production = dates()$corn_county_production_census_ton,
               LB_production = dates()$corn_county_production_census_lb,
               BU_ACRE_yield = dates()$corn_county_yield_census_bu_acre,
               TONS_ACRE_yield = dates()$corn_county_yield_census_ton_acre,
               OPERATIONS_sales = dates()$corn_county_sales_census_operation,
               Dollar_sales = dates()$corn_county_sales_census_dollor
        )
      }else if(as.character(input$source) == "SURVEY"){
        switch(input$unit,
               ACRES_harvested = dates()$corn_county_harvest_survey_acres,
               OPERATIONS_harvested = dates()$corn_county_harvest_survey_operation,
               ACRES_plated = dates()$corn_county_planted_survey_acre,
               BU_production = dates()$corn_county_production_survey_bu,
               TONS_production = dates()$corn_county_production_survey_ton,
               LB_production = dates()$corn_county_production_survey_lb,
               BU_ACRE_yield = dates()$corn_county_yield_survey_bu_acre,
               TONS_ACRE_yield = dates()$corn_county_yield_survey_ton_acre,
               OPERATIONS_sales = dates()$corn_county_sales_survey_operation,
               Dollar_sales = dates()$corn_county_sales_survey_dollor
        )
      }
    }
     
  })
  
  counties <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      data_new4()$county_state 
    }
    
  })
  
  pal_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      #rdata = reactive_data()
      colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
      #colorNumeric(palette = color_pal, domain = rdata)#reactive_data())
    }
    
  })
  
  popup_msg <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(as.character(input$source) == "CENSUS"){
        str_c("<strong>", dates()$county_state, #", ", dates()$State,
              "</strong><br /><strong>", dates()$YEAR, "</strong>",
              "<br /> ACRES_harvested: ", ifelse(is.na(dates()$corn_county_harvest_census_acres), "(D)", dates()$corn_county_harvest_census_acres),
              "<br /> OPERATIONS_harvested: ", ifelse(is.na(dates()$corn_county_harvest_census_operation), "(D)", dates()$corn_county_harvest_census_operation),
              "<br /> ACRES_plated: ",  ifelse(is.na(dates()$corn_county_planted_census_acre), "(D)", dates()$corn_county_planted_census_acre),
              "<br /> BU_production: ",  ifelse(is.na(dates()$corn_county_production_census_bu), "(D)", dates()$corn_county_production_census_bu),
              "<br /> TONS_production: ",  ifelse(is.na(dates()$corn_county_production_census_ton), "(D)", dates()$corn_county_production_census_ton),
              "<br /> LB_production: ",  ifelse(is.na(dates()$corn_county_production_census_lb), "(D)", dates()$corn_county_production_census_lb),
              "<br /> BU_ACRE_yield: ",  ifelse(is.na(dates()$corn_county_yield_census_bu_acre), "(D)", dates()$corn_county_yield_census_bu_acres),
              "<br /> TONS_ACRE_yield: ",  ifelse(is.na(dates()$corn_county_yield_census_ton_acre), "(D)", dates()$corn_county_yield_census_ton_acres),
              "<br /> OPERATIONS_sales: ",  ifelse(is.na(dates()$corn_county_sales_census_operation), "(D)", dates()$corn_county_sales_census_operation),
              "<br /> Dollar_sales: ",  ifelse(is.na(dates()$corn_county_sales_census_dollor), "(D)", dates()$corn_county_sales_census_dollor))
        
      }else if(as.character(input$source) == "SURVEY"){
        str_c("<strong>", dates()$county_state, #", ", dates()$State,
              "</strong><br /><strong>", dates()$YEAR, "</strong>",
              "<br /> ACRES_harvested: ",  ifelse(is.na(dates()$corn_county_harvest_survey_acres), "(D)", dates()$corn_county_harvest_survey_acres),
              "<br /> ACRES_harvested: ",  ifelse(is.na(dates()$corn_county_harvest_survey_operation), "(D)", dates()$corn_county_harvest_survey_operation),
              "<br /> ACRES_plated: ",  ifelse(is.na(dates()$corn_county_planted_survey_acre), "(D)", dates()$corn_county_planted_survey_acre),
              "<br /> BU_production: ",  ifelse(is.na(dates()$corn_county_production_survey_bu), "(D)", dates()$corn_county_production_survey_bu),
              "<br /> TONS_production: ",  ifelse(is.na(dates()$corn_county_production_survey_ton), "(D)", dates()$corn_county_production_survey_ton),
              "<br /> LB_production: ",  ifelse(is.na(dates()$corn_county_production_survey_lb), "(D)", dates()$corn_county_production_survey_lb),
              "<br /> BU_ACRE_yield: ",  ifelse(is.na(dates()$corn_county_yield_survey_bu_acre), "(D)", dates()$corn_county_yield_survey_bu_acres),
              "<br /> TONS_ACRE_yield: ",  ifelse(is.na(dates()$corn_county_yield_survey_ton_acre), "(D)", dates()$corn_county_yield_survey_ton_acres),
              "<br /> OPERATIONS_sales: ",  ifelse(is.na(dates()$corn_county_sales_survey_operation), "(D)", dates()$corn_county_sales_survey_operation),
              "<br /> Dollar_sales: ",  ifelse(is.na(dates()$corn_county_sales_survey_dollor), "(D)", dates()$corn_county_sales_survey_dollor))
      }
    }
     
    
  })
  
  output$map_pop <- renderLeaflet({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      leaflet(width = "100%",
              options = leafletOptions(zoomSnap = 0,
                                       zoomDelta = 0.25)) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>% 
        setView(lat = 41.550835, lng = -88.100409, zoom = 5) %>% #41.550835, -86.897873
        addPolygons(data = states_map2,#st_transform(states_map2, crs = "+init=epsg:4326"),
                    group = "state",
                    color = "black",
                    fill = FALSE,
                    weight = 3) %>%
        addPolygons(data = st_transform(filter(data_new4(), YEAR == 2002), crs = "+init=epsg:4326"), #filter(data_new4, YEAR == 2002),#
                    layerId = layer_county,
                    color = "white",
                    weight = 1,
                    smoothFactor = 0,
                    fillOpacity = 0.7)
    }
    
  })
  
  
  
  observe({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      leafletProxy("map_pop", data = dates()) %>% 
        setShapeStyle(layerId = layer_county, 
                      fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
    }
    
  })
  
  observe({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      leafletProxy("map_pop", data = dates()) %>%
        setShapeLabel(layerId = layer_county,
                      label = popup_msg())
    }
    
  })
  
  observe({ 
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      leafletProxy("map_pop") %>% 
        clearControls() %>% 
        addLegend("bottomleft",
                  pal = pal_data(),
                  values = na.omit(reactive_data()),
                  title = str_to_title(str_replace_all(input$unit, "_", " ")),
                  na.label = "",
                  opacity = 5)
    }
    
  })
  
  
  observeEvent(input$map_pop_shape_click, {
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      GRIDrv(input$map_pop_shape_click$id)
    }
    
  })
  
  county_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      validate(
        need(GRIDrv() != "", "Please select a county to generate analyses.")
      )
      
      Data = data_new4() %>% 
        subset(county_state == GRIDrv()) 
      Data
    }
    

  })
  
  output$tab1 <- DT::renderDataTable({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      print(county_data())
      
    }
    
  })
  
  output$test <- renderPrint({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      names(county_data())
    }
  })
  
  output$plot_acres <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      Data_harvest <- county_data() %>% 
        select(YEAR, "corn_county_harvest_survey_acres",
               "corn_county_harvest_census_acres",
#               "corn_county_harvest_census_operation",
#               "corn_county_harvest_survey_operation"
               ) %>% 
        group_by(YEAR) %>% 
        summarise(survey_acres = sum(corn_county_harvest_survey_acres, na.rm = T),
#                  survey_operation = sum(corn_county_harvest_survey_operation, na.rm = T),
                  census_acres = sum(corn_county_harvest_census_acres, na.rm = T),
#                  census_operation = sum(corn_county_harvest_census_operation, na.rm = T),
                  )
      
      # Reshape data to long format
      long_data <- Data_harvest %>%
        pivot_longer(cols = c(survey_acres, census_acres), 
                     names_to = "Variable", 
                     values_to = "Value")
      
      # Create side-by-side bar graph
      ggplot(subset(long_data, Value != 0), aes(x = as.numeric(YEAR), y = Value, color = Variable)) +
        geom_line() +  # "dodge" places bars side by side
        labs(title = paste0("Number of Harvest Survey Acres in ", dates()$county_state),
             x = "Year", 
             y = "Value") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_brewer(palette = "Set2") 
     
      }
  })
  
  output$plot_operations <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      Data_harvest <- county_data() %>% 
        select(YEAR,
               #"corn_county_harvest_survey_acres",
               #"corn_county_harvest_census_acres",
               "corn_county_harvest_census_operation",
               "corn_county_harvest_survey_operation"
        ) %>% 
        group_by(YEAR) %>% 
        summarise(#survey_acres = sum(corn_county_harvest_survey_acres, na.rm = T),
                  survey_operation = sum(corn_county_harvest_survey_operation, na.rm = T),
                  #census_acres = sum(corn_county_harvest_census_acres, na.rm = T),
                  census_operation = sum(corn_county_harvest_census_operation, na.rm = T),
        )
      
      # Reshape data to long format
      long_data <- Data_harvest %>%
        pivot_longer(cols = c(survey_operation, census_operation), 
                     names_to = "Variable", 
                     values_to = "Value")
      print(long_data)
      
      # Create side-by-side bar graph
      ggplot(subset(long_data, Value != 0), aes(x = as.numeric(YEAR), y = Value, color = Variable)) +
        geom_line() +  # "dodge" places bars side by side
        labs(title = paste0("Number of Harvest Operations in ", dates()$county_state),
             x = "Year", 
             y = "Value") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_brewer(palette = "Set2") 
      
    }
  })Ω
  
  
  output$plot_sales <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      Data_harvest <- county_data() %>% 
        select(YEAR, "corn_county_harvest_survey_acres" ,
               "corn_county_harvest_census_acres",
               "corn_county_harvest_census_operation",
               "corn_county_harvest_survey_operation") %>% 
        group_by(YEAR) %>% 
        summarise(survey_acres = sum(corn_county_harvest_survey_acres, na.rm = T),
                  survey_operation = sum(corn_county_harvest_survey_operation, na.rm = T),
                  census_acres = sum(corn_county_harvest_census_acres, na.rm = T),
                  census_operation = sum(corn_county_harvest_census_operation, na.rm = T),)
      
      # Reshape data to long format
      long_data <- Data_harvest %>%
        pivot_longer(cols = c(survey_acres, survey_operation, census_acres, census_operation), 
                     names_to = "Variable", 
                     values_to = "Value")
      
      # Create side-by-side bar graph
      ggplot(long_data, aes(x = as.numeric(YEAR), y = Value, fill = Variable)) +
        geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
        labs(title = "Side-by-Side Bar Graph of Variables by Year",
             x = "Year", 
             y = "Value") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_brewer(palette = "Set2") 
      
      
      
    }
  })
  
  
  
  output$plot_acres_norm <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      Data_harvest <- county_data() %>% 
        select(YEAR, "corn_county_harvest_survey_acres" ,
               "corn_county_harvest_census_acres",
               "corn_county_harvest_census_operation",
               "corn_county_harvest_survey_operation") %>%
        group_by(YEAR) %>% 
        summarise(survey_acres = sum(corn_county_harvest_survey_acres, na.rm = T),
                  survey_operation = sum(corn_county_harvest_survey_operation, na.rm = T),
                  census_acres = sum(corn_county_harvest_census_acres, na.rm = T),
                  census_operation = sum(corn_county_harvest_census_operation, na.rm = T),)
      
      
      # Reshape data to long format
      long_data <- Data_harvest %>%
        pivot_longer(cols = c(survey_acres, survey_operation, census_acres, census_operation), 
                     names_to = "Variable", 
                     values_to = "Value")
      
      total_survey_acres = sum(Data_harvest$survey_acres) 
      total_survey_operations = sum(Data_harvest$survey_operation) 
      total_census_acres = sum(Data_harvest$census_acres) *5 # account for the data being recorded every 5 years
      total_census_operations = sum(Data_harvest$census_operation) *5 # account for the data being recorded every 5 years
      
      long_data <- long_data %>% 
        mutate(
          Value = case_when(
            Variable == "survey_acres" ~ Value / total_survey_acres,
            Variable == "survey_operation" ~ Value / total_survey_operations,
            Variable == "census_acres" ~ Value / total_census_acres,
            Variable == "census_operation" ~ Value / total_census_operations,
            TRUE ~ Value  # Default to original value if no match
          )
        )
      
      # Create side-by-side bar graph
      ggplot(long_data, aes(x = as.numeric(YEAR), y = Value, fill = Variable)) +
        geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
        labs(title = "Side-by-Side Bar Graph of Variables Percentage of Total Across Time",
             x = "Year", 
             y = "Proportion") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_brewer(palette = "Set2") 
      
    }
  })
  
  
  
  output$plot_yield <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      
    }
  })
  
  output$plot_harvested <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      
    }
  })
  
  output$plot_planted <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      ggplot()+
        geom_point() + 
        geom_line(x_intercept = 0)
    }
  })
  
  output$plot_production <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
      
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


