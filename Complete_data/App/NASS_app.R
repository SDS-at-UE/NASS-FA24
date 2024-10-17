options(timeout=300)

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)
library(bslib)
library(imputeTS)
library(data.table)
library(purrr) 

setwd("/Users/andrewthompson/Desktop/NASS-FA24/Complete_data/App")

#######################  Loading the data  #############################


load("states_geometry.rda")  # states_geometry data
states_map2 <- states_geometry %>%
  sf::st_set_crs(4326) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

load("corn_county.rda")
load("smaller_corn_state_census.rda")
# view(corn_state_census)
# view(corn_county)

for(i in 4:dim(corn_county)[2]){
  corn_county[,i][is.na(corn_county[,i])] <- 0
}
for(i in 4:dim(corn_state_census)[2]){
  corn_state_census[,i][is.na(corn_state_census[,i])] <- 0
}

is.zero <- function(x) {
  x == 0
}

layer_county <- unique(corn_county$county_state)
layer_state <- unique(corn_state_census$state_name)

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

ui <- navbarPage(leafletjs, theme = shinytheme("spacelab"),
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
                                                        animate = animationOptions(interval = 2000),
                                   ),
                                   ),
                                   column(3,)        
                         ),
                         fluidRow(
                           column(3,
                                  wellPanel(
                                    selectInput(inputId = "level", "Choose a level", 
                                                c("County", "State"),
                                                selected = "State"),
                                    
                                    selectInput(inputId = "crop", "Choose a crop", 
                                                c("Corn", "Soybean"),
                                                selected = "Corn"),
                                    
                                    selectInput(inputId = "stat", "Choose a stat", 
                                                c("PRODUCTION", "YIELD", "AREA PLANTED", "AREA HARVESTED", "SALES"),
                                                selected = "AREA HARVESTED"),

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
                                  #br(),
                                  #textOutput("test"),
                                  
                           )
                         ),
                         
                         fluidRow(
                           column(2),
                           column(8,
                                  textOutput("label"),
                                  h5("Plot of Acres in the selected county"),
                                  plotOutput("plot1"),
                                  br(),
                                  br(),
                                  h5("Plot of Operations in the selected county"),
                                  plotOutput("plot2"),
                                  br(),
                                  br(),
                                  #h5("Sales Plot in the selected county"),
                                  plotOutput("plot3"),
                                  br(),
                                  br(),
                                  #h5("Yield Plot in the selected county"),
                                  plotOutput("plot4"),
                                  br(),
                                  br(),
                                  
######################################################################################################
# CONDITIONAL PLOT - UTILIZE THESE
                                  conditionalPanel(
                                    condition = "input.level == 'State'",  # Replace 'SomeValue' with your condition
                                    h5("Additional Plot in the selected county"),
                                    plotOutput("plot7")
                                  )
######################################################################################################
                           ),
                           column(2)
                         ),
                        
                         
                         
                         
                         )
                )


server <- function(input, output,session) {
  ### Map Interface & Climate Grid Selection
  GRIDrv <- reactiveVal()
  STATErv <- reactiveVal()
  
  observeEvent(input$level, {
    updateSelectInput(session, "unit", selected = "")
  })
  #stat = c("PRODUCTION", "YIELD", "AREA PLANTED", "AREA HARVESTED", "SALES")
  output$unit <- renderUI({
    if(length(strsplit(as.character(req(input$stat)), ""))!=0){
      if(input$stat == 'AREA HARVESTED'){
        x = c("","ACRES_harvested", "OPERATIONS_harvested")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                       selected = "")
      }else if(input$stat == 'AREA PLANTED'){
        x = c("","ACRES_plated")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                       choices = x,
                       selected = "")
      }else if(input$stat == 'PRODUCTION'){
        x = c("","BU_production", "TONS_production", "LB_production")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                    selected = "")
      }else if(input$stat == 'YIELD'){
        x = c("","BU_ACRE_yield", "TONS_ACRE_yield")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                    selected = "")
      }else if(input$stat == 'SALES'){
        x = c("","OPERATIONS_sales", "Dollar_sales")
        selectInput(inputId = "unit", label ="Choose a UNIT_DESC",
                    choices = x,
                    selected = "")
      }
    }
    
  })
  
  
  
  data_new4 <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(input$level == "County") {
      return(st_as_sf(corn_county) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
        sf::st_set_crs(4326) %>% 
        sf::st_transform('+proj=longlat +datum=WGS84'))
      }
      if(input$level == "State") {
        return(st_as_sf(corn_state_census) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
          sf::st_set_crs(4326) %>% 
          sf::st_transform('+proj=longlat +datum=WGS84'))
      }
    }
    
  })
  
  # data_new5 <- reactive({
  #   if(length(strsplit(as.character(req(input$unit)), ""))!=0){
  #     if(input$level == "State") {
  #     st_as_sf(corn_state_census) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
  #       sf::st_set_crs(4326) %>% 
  #       sf::st_transform('+proj=longlat +datum=WGS84')
  #     }
  #   }
  # })
  
  
  ########################################################################
  ########################################################################
  
  
  
  
  
  ########################################################################
  ########################################################################
  
  
  
  dates <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      data = data_new4() %>%
        filter(YEAR == as.numeric(input$dates))
      
      # Validate that data is not empty
      validate(
        need(nrow(data) > 0, "No data available for the selected year.")
      )
      data
    }
    
  })
  
  
  
  reactive_data <-  reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(as.character(input$level) == "State"){
        result <-switch(input$unit,
                 ACRES_harvested = data_new4()$corn_state_harvest_census_acres,
                 OPERATIONS_harvested = data_new4()$corn_state_harvest_census_operation,
                 ACRES_plated = data_new4()$corn_state_planted_census_acre,
                 BU_production = data_new4()$corn_state_production_census_bu,
                 TONS_production = data_new4()$corn_state_production_census_ton,
                 LB_production = data_new4()$corn_state_production_census_lb,
                 BU_ACRE_yield = data_new4()$corn_state_yield_census_bu_acre,
                 TONS_ACRE_yield = data_new4()$corn_state_yield_census_ton_acre,
                 OPERATIONS_sales = data_new4()$corn_state_sales_census_operation,
                 Dollar_sales = data_new4()$corn_state_sales_census_dollor
        )
      }
      if(as.character(input$level) == "County"){
        result <-switch(input$unit,
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
      }
      return(result)
    }
     
  })
  
  
  reactive_stat <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        if (as.character(input$level) == "State") {
          result <- switch(input$unit,
                           ACRES_harvested = dates()$corn_state_harvest_census_acres,
                           OPERATIONS_harvested = dates()$corn_state_harvest_census_operation,
                           ACRES_plated = dates()$corn_state_planted_census_acre,
                           BU_production = dates()$corn_state_production_census_bu,
                           TONS_production = dates()$corn_state_production_census_ton,
                           LB_production = dates()$corn_state_production_census_lb,
                           BU_ACRE_yield = dates()$corn_state_yield_census_bu_acre,
                           TONS_ACRE_yield = dates()$corn_state_yield_census_ton_acre,
                           OPERATIONS_sales = dates()$corn_state_sales_census_operation,
                           Dollar_sales = dates()$corn_state_sales_census_dollar
          )
        } else if (as.character(input$level) == "County") {
          result <- switch(input$unit,
                           ACRES_harvested = dates()$corn_county_harvest_census_acres,
                           OPERATIONS_harvested = dates()$corn_county_harvest_census_operation,
                           ACRES_plated = dates()$corn_county_planted_census_acre,
                           BU_production = dates()$corn_county_production_census_bu,
                           TONS_production = dates()$corn_county_production_census_ton,
                           LB_production = dates()$corn_county_production_census_lb,
                           BU_ACRE_yield = dates()$corn_county_yield_census_bu_acre,
                           TONS_ACRE_yield = dates()$corn_county_yield_census_ton_acre,
                           OPERATIONS_sales = dates()$corn_county_sales_census_operation,
                           Dollar_sales = dates()$corn_county_sales_census_dollar
          )
        }

        return(result)  
      }
  })
  
  counties <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      data_new4()$county_state
    }
    
  })
  
  pal_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      validate(
        need(nrow(dates()) > 0, "No data to apply color palette.")
      )
      rdata = reactive_data()
      if(max(reactive_stat())==0){
        colorNumeric(palette = color_pal, domain = 0.0001:10000)
      }else{
        colorNumeric(palette = color_pal, domain = rdata)
      }
      
      #colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
      #colorNumeric(palette = color_pal, domain = rdata)#reactive_data())
    }
    
  })
  
  
  popup_msg <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(input$level == "County"){
        str_c("<strong>", dates()$county_state, #", ", dates()$State,
              "</strong><br /><strong>", dates()$YEAR, "</strong>",
              "<br /> ACRES_harvested: ", dates()$corn_county_harvest_census_acres,
              "<br /> OPERATIONS_harvested: ", dates()$corn_county_harvest_census_operation,
              "<br /> ACRES_plated: ",  dates()$corn_county_planted_census_acre,
              "<br /> BU_production: ",  dates()$corn_county_production_census_bu,
              "<br /> TONS_production: ",  dates()$corn_county_production_census_ton,
              "<br /> LB_production: ",  dates()$corn_county_production_census_lb,
              "<br /> BU_ACRE_yield: ",  dates()$corn_county_yield_census_bu_acre,
              "<br /> TONS_ACRE_yield: ",  dates()$corn_county_yield_census_ton_acre,
              "<br /> OPERATIONS_sales: ",  dates()$corn_county_sales_census_operation,
              "<br /> Dollar_sales: ",  dates()$corn_county_sales_census_dollor)
      }
      if(input$level == "State"){
        ######################################################################################
       # fill with a popup
      }
    }
  })
  
 
  
  output$map_pop <- renderLeaflet({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0) {
      validate(
        need(nrow(dates()) > 0, "No data available for the selected year.")
      )
      
      # Initialize leaflet map (this part is common to both conditions)
      map <- leaflet(width = "100%",
                     options = leafletOptions(zoomSnap = 0,
                                              zoomDelta = 0.25)) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(lat = 41.550835, lng = -88.100409, zoom = 5) %>%
        addPolygons(data = states_map2,
                    group = "state",
                    color = "black",
                    fill = FALSE,
                    weight = 3)
      
      # Conditionally add county polygons if input$level is "County"
      if(input$level == "County") {
        map <- map %>%
          addPolygons(data = st_transform(filter(data_new4(), YEAR == 2002), crs = "+init=epsg:4326"),
                      layerId = layer_county,
                      color = "white",
                      weight = 1,
                      smoothFactor = 0,
                      fillOpacity = 0.7)
      }
      if(input$level == "State") {
        map <- map %>%
          addPolygons(data = states_map2,
                      layerId = layer_state,  # Assign layer IDs for states
                      color = "black",         # Customize state polygon colors
                      weight = 2,
                      fillOpacity = 0.4)
      }
      
      # Return the leaflet map
      map
    }
  })
  
  
  
  observe({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(input$level == "County"){
      leafletProxy("map_pop", data = dates()) %>% 
        setShapeStyle(layerId = layer_county, 
                      fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
      }
      if(input$level == "State"){
        leafletProxy("map_pop", data = dates()) %>% 
          setShapeStyle(layerId = layer_state, 
                        fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
        }
      }
    
  })
  
  observe({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(input$level == "County"){
        chosen_layers = layer_county
        
      }
      if(input$level == "State") {
        chosen_layers = layer_state
        
      }
      leafletProxy("map_pop", data = dates()) %>%
        setShapeLabel(layerId = chosen_layers,
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
      if(input$level == "State"){
        STATErv(input$map_pop_shape_click$id)
        print(paste(input$map_pop_shape_click$id, "clicked"))
      }
      if(input$level == "County"){
        GRIDrv(input$map_pop_shape_click$id)
        print(paste(input$map_pop_shape_click$id, "clicked"))
      }
    }
  })
  

  
  county_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      
        validate(
          need(GRIDrv() != "", "Please select a county cell to generate analysis")
        )
        
        Data = data_new4() %>% 
          subset(county_state == GRIDrv()) 
        return(Data)
      
    }
  })
  
  state_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      validate(
        need(STATErv() != "", "Please select a state to generate analysis")
      )
      Data = data_new4() %>% 
        subset(state_name == STATErv())
      return(Data)
    }
  })
  
  output$label <- renderPrint({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0){
      if(input$level == "State"){
        data = state_data()
        print(paste("Currently Selected:", input$map_pop_shape_click$id))
      }
      if(input$level == "County"){
        data = county_data()
        print(paste("Currently Selected:", input$map_pop_shape_click$id))
      }
    }
  })
  
  
  output$tab1 <- DT::renderDataTable({
    req(input$unit)
    if (input$level == "State") {
      result <- head(state_data())
    } 
    if (input$level == "County") {
      result <- head(county_data())
    }
    return(result)
  })
  
  
  # observeEvent(input$level, {
  #   if (input$level == "State") {
  #     plot_county()  # Calls the first set of observe/reactive/etc. for value1
  #   } else if (input$level == "County") {
  #     plot_state()  # Calls the second set of observe/reactive/etc. for value2
  #   }
  # })
  
  plot_county <- function() {
    
    output$tab1 <- DT::renderDataTable({
      if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        if(input$level == "State"){
          head(county_data())
        }
        if(input$level == "County"){
          head(state_data())
        }
        
      }
      
    })
    
    output$test <- renderPrint({
      if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        if(input$level == "State"){
          names(county_data())
        }
        if(input$level == "County"){
          names(county_data())
        }
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
          labs(title = paste0("Number of Harvest Survey Acres in ", county_data()$county_state),
               x = "Year", 
               y = "Value") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2") 
        
      }
    })
    
    
    #############################################################################
    # UNNECESSARY GRAPH
    #
    #############################################################################
    
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
          labs(title = paste0("Number of Harvest Operations in ", county_data()$county_state),
               x = "Year", 
               y = "Value") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2") 
        
      }
    })
    
    
    
    
    output$plot_harvested <- renderPlot({
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
        ggplot(long_data, aes(x = factor(YEAR), y = Value, fill = Variable)) +
          geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
          labs(title = paste0("Side-by-side bar graph of harvested areas in ", county_data()$county_state),
               x = "Year", 
               y = "Harvested Area") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2") 
        
      }
    })
    
    output$plot_yield <- renderPlot({
      if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        Data_yield <- county_data() %>% 
          select(YEAR, "corn_county_yield_survey_bu_acre" ,
                 "corn_county_yield_survey_ton_acre",
                 "corn_county_yield_census_bu_acre",
                 "corn_county_yield_census_ton_acre") %>% 
          group_by(YEAR) %>% 
          summarise(survey_bu_acre = sum(corn_county_yield_survey_bu_acre, na.rm = T),
                    survey_ton_acre = sum(corn_county_yield_survey_ton_acre, na.rm = T),
                    census_bu_acre = sum(corn_county_yield_census_bu_acre, na.rm = T),
                    census_ton_acre = sum(corn_county_yield_census_ton_acre, na.rm = T),)
        
        # Reshape data to long format
        long_data <- Data_yield %>%
          pivot_longer(cols = c(survey_bu_acre, survey_ton_acre, census_bu_acre, census_ton_acre), 
                       names_to = "Variable", 
                       values_to = "Value")
        
        # Create side-by-side bar graph
        ggplot(long_data, aes(x = factor(YEAR), y = Value, fill = Variable)) +
          geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
          labs(title = paste0("Side-by-side bar graph of yield in ", county_data()$county_state),
               x = "Year", 
               y = "Yield") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2")
        
      }
    })
    
    output$plot_sales <- renderPlot({
      if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        Data_sales <- county_data() %>% 
          select(YEAR, "corn_county_sales_census_operation",
                 "corn_county_sales_census_dollor") %>% 
          group_by(YEAR) %>% 
          summarise(census_operation = sum(corn_county_sales_census_operation, na.rm = T),
                    census_dollar = sum(corn_county_sales_census_dollor, na.rm = T),)
        
        
        print("DATA SALES")
        print(Data_sales)
        
        # Reshape data to long format
        long_data <- Data_sales %>%
          pivot_longer(cols = c(census_operation, census_dollar), 
                       names_to = "Variable", 
                       values_to = "Value")
        
        # Create side-by-side bar graph
        ggplot(long_data, aes(x = na.omit(as.factor(YEAR)), y = Value)) +
          geom_bar(stat = "identity") +  # "dodge" places bars side by side
          labs(title = paste0("Bar graph of sales (Census) in ", county_data()$county_state),
               x = "Year", 
               y = "Sales") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2")
        
      }
    })
    
    output$plot_planted <- renderPlot({
      if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        Data_planted <- county_data() %>% 
          select(YEAR, "corn_county_planted_survey_acre" ,
                 "corn_county_planted_census_acre") %>% 
          group_by(YEAR) %>% 
          summarise(survey_acre = sum(corn_county_planted_survey_acre, na.rm = T),
                    census_acre = sum(corn_county_planted_census_acre, na.rm = T),)
        
        # Reshape data to long format
        long_data <- Data_planted %>%
          pivot_longer(cols = c(survey_acre , census_acre), 
                       names_to = "Variable", 
                       values_to = "Value")
        
        # Create side-by-side bar graph
        ggplot(long_data, aes(x = factor(YEAR), y = Value, fill = Variable)) +
          geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
          labs(title = paste0("Side-by-side bar graph of planted area in ", county_data()$county_state),
               x = "Year", 
               y = "Sales") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2")
        
      }
    })
    
    output$plot_production <- renderPlot({
      if(length(strsplit(as.character(req(input$unit)), ""))!=0){
        Data_production <- county_data() %>% 
          select(YEAR,
                 "corn_county_production_census_bu",
                 "corn_county_production_census_ton",
                 "corn_county_production_census_lb") %>% 
          group_by(YEAR) %>% 
          summarise(census_bu = sum(corn_county_production_census_bu, na.rm = T),
                    census_ton = sum(corn_county_production_census_ton, na.rm = T),
                    census_lb = sum(corn_county_production_census_lb, na.rm = T),)
        
        # Reshape data to long format
        long_data <- Data_production %>%
          pivot_longer(cols = c(census_bu, census_ton, census_lb), 
                       names_to = "Variable", 
                       values_to = "Value")
        
        # Create side-by-side bar graph
        ggplot(long_data, aes(x = as.numeric(YEAR), y = Value, fill = Variable)) +
          geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
          labs(title = paste0("Side-by-side bar graph of production in ", county_data()$county_state),
               x = "Year", 
               y = "Production") +
          theme_minimal() +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_brewer(palette = "Set2")
        
      }
    })
    
  } # End plot_county bracket
  
  plot_state <- function() {
    
    
      
        
      }
   
    
    
  
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
