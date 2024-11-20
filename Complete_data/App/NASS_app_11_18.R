
if ("raster" %in% .packages()) {
  detach("package:raster", unload = TRUE)
}


options(timeout=300)

# Check and install necessary packages, Load libraries
list.of.packages <- c('shiny', 'shinythemes', 'tidyverse', 'shinyWidgets', 'leaflet', 'sf',
                      'bslib', 'imputeTS', 'data.table', 'purrr', 'shinyjs')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(list.of.packages, library, character.only = TRUE)

# library(shiny)
# library(tidyverse)
# library(leaflet)
# library(sf)
# library(shinythemes)
# library(bslib)
# library(imputeTS)
# library(data.table)
# library(purrr) 
# library(shinyjs)  # For using the delay
# library(shinyWidgets)

#setwd("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\NASS\\Data\\Final_data\\")

#######################  Loading the data  #############################
url_1 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/corn_state_geometry.rda?raw=true"
load(url(url_1))

url_2 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/corn_survey_final1.rda?raw=true"
load(url(url_2))

url_3 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/corn_census_final.rda?raw=true"
load(url(url_3))

url_4 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/corn_survey.rda?raw=true"
load(url(url_4))

url_5 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/soybeans_census_final.rda?raw=true"
load(url(url_5))

url_6 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/soybeans_state_geometry.rda?raw=true"
load(url(url_6))

url_7 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/soybeans_survey_final.rda?raw=true"
load(url(url_7))

url_8 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/soybeans_survey.rda?raw=true"
load(url(url_8))

url_9 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/potatoes_census_final.rda?raw=true"
load(url(url_9))

url_10 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/potatoes_state_geometry.rda?raw=true"
load(url(url_10))

url_11 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/potatoes_survey.rda?raw=true"
load(url(url_11))

url_12 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/potatoes_survey_final.rda?raw=true"
load(url(url_12))

url_13 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/wheat_census_final.rda?raw=true"
load(url(url_13))

url_14 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/wheat_state_geometry.rda?raw=true"
load(url(url_14))

url_15 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/wheat_survey.rda?raw=true"
load(url(url_15))

url_16 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/app_dev/Complete_data/App/Data/wheat_survey_final.rda?raw=true"
load(url(url_16))
#load("corn_survey_final1.rda")


#load("corn_state_geometry.rda")  # states_geometry data
# states_map2 <- corn_state_geometry %>%
#   sf::st_set_crs(4326) %>% 
#   sf::st_transform('+proj=longlat +datum=WGS84')
# states_map2 <- states_map2 %>% arrange(STATE_NAME)

#load("corn_survey_final.rda")
#load("corn_census_final.rda")


# for(i in 4:dim(corn_county)[2]){
#   corn_county[,i][is.na(corn_county[,i])] <- 0
# }

is.zero <- function(x) {
  x == 0
}


# Function to extract unique left-hand side of column names with "_"
get_left_side <- function(df) {
  column_names <- colnames(df)
  # Filter columns that contain "_"
  columns_with_underscore <- column_names[grepl("_", column_names)]
  # Split column names by "_"
  split_names <- strsplit(columns_with_underscore, "_")
  # Extract and return unique left-hand side
  unique_left <- unique(sapply(split_names, `[`, 1))
  return(unique_left)
}

# Function to extract unique right-hand side of column names with "_"
get_right_side <- function(df, target_left_value = NULL) {
  # Get column names that contain "_"
  col_names_with_underscore <- grep("_", colnames(df), value = TRUE)
  
  # Split column names into left and right sides of "_"
  split_names <- strsplit(col_names_with_underscore, "_")
  
  # Filter based on the target_left_value if provided
  if (!is.null(target_left_value)) {
    split_names <- split_names[sapply(split_names, function(x) x[1] == target_left_value)]
  }
  
  # Extract the right-hand side of "_" for the filtered column names
  right_sides <- sapply(split_names, function(x) x[2])
  
  # Return unique right-hand side values
  unique(right_sides)
}



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

ui <- fluidPage(
  
  fluidRow(
    # Adding the logo at the top using img tag
    column(3, div(style="text-align: center;",img(src = "NASS.jpg", height = "30%", width = "30%")  )),
    #column(2, span(h5(img(src = "Logo_US_Forest.jpg", height = "30%", width = "30%")  ,"US Forest Service" ))),
    column(4, div(style = "font-size: 18px; color: black;",
                  "This page depicts the data analysis for",
                  br(),
                  strong("United States Department of Agriculture"),
                  br(),
                  strong("National Agricultural Statistics Service")
    )), 
    column(3, br(), div(style="text-align: left;",img(src = "UE_logo.jpg", height = "70%", width = "70%"))),
    column(2, div(style="text-align: top;",img(src = "new_stat_300.jpg", height = '100px', width = '190px')))
    #Stat300_2
  ),
  

  navbarPage(leafletjs, theme = shinytheme("cosmo"),
                title = "Agriculture Data Analysis Portal",
                tabPanel('Crops',
                         #titlePanel("Crops"),
                         fluidRow(
                           column(9,
                                  fluidRow(
                                    column(4,
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           wellPanel(
                                             useShinyjs(),
                                             selectInput(inputId = "level", "Choose a level", 
                                                         c("County", "State"),
                                                         selected = "State"),
                                             
                                             selectInput(inputId = "crop", "Choose a crop", 
                                                         c("Corn", "Soybeans", "Potatoes", "Wheat"),
                                                         selected = "Corn"),
                                             
                                             uiOutput("stat"),
                                             
                                             # selectInput(inputId = "stat", "Choose a STATISTICCAT_DESC",
                                             #            choices = get_left_side()  #c("PRODUCTION", "YIELD", "AREA PLANTED", "AREA HARVESTED", "SALES"),
                                             #             selected = "AREA HARVESTED"),
                                             
                                             # selectInput(inputId = "source", "Choose a SOURCE_DESC", 
                                             #             c("","CENSUS", "SURVEY"),
                                             #             selected = NULL),
                                             
                                             # selectInput(inputId = "unit", "Choose a UNIT_DESC",
                                             #             c("ACRES", "OPERATIONS"),
                                             #             selected = "ACRES"),
                                             # 
                                             uiOutput("unit"),
                                             br(),
                                             switchInput(inputId = "toggle_map", label = "Map",
                                                         onLabel = "Creating Map...", offLabel = "Updating Filters...", 
                                                         value = FALSE),
                                             
                                             hr(),
                                             hr(),
                                             uiOutput("var2")
                                           )
                                           
                                    ),
                                    column(8,sliderInput(inputId = "dates", "Timeline of Selected Parameter", 
                                                         min = 2000L, #min(c(corn_county_cencus$YEAR, corn_county_survey$YEAR)),
                                                         max = 2023L, #max(c(corn_county_cencus$YEAR, corn_county_survey$YEAR)),
                                                         value = 2022L,
                                                         sep = "",
                                                         #timeFormat = "%m-%d-%Y",
                                                         step = 1,
                                                         ticks = FALSE,
                                                         width = '100%',
                                                         animate = animationOptions(interval = 3000),
                                    ),
                                    br(),
                                    br(),
                                    leafletOutput("map_pop"),
                                    )
                                  ),
                                  
                                  ),
                           column(3,
                                  uiOutput("text_output"), 
                                  tableOutput("msg_pop")
                                  
                                  )        
                         ),
                         fluidRow(
                           column(12,
                                  fluidRow(
                                    column(4,
                                           
                                           tableOutput("tab1")
                                    ),
                                    column(8,
                                           fluidRow(
                                             column(9,
                                                    plotOutput("boxplot")
                                                    ),
                                             column(3,
                                                    textOutput("zscore_box")
                                             )
                                           ),
                                           fluidRow(
                                             column(6,
                                                    plotOutput("bar1")
                                             ),
                                             column(6,
                                                    plotOutput("bar2")
                                             )
                                             
                                           ),
                                           fluidRow(
                                             column(12,
                                                    plotOutput("scatter2")
                                             )
                                           )
                                           
                                    )
                                  )
                                  
                                  
                                  
                           )
                         ),
                         fluidRow(column(12,
                                         h2("Crop Progress and Condition in the selected states"),
                                         br(),
                                         textOutput("main_states"),
                                         br(),
                                         plotOutput("excellent_good"),
                                         br(),
                                         plotOutput("condition"),
                                         br(),
                                         plotOutput("progress"),
                                         
                                         ))

                         )
                )
)


server <- function(input, output,session) {
  ### Map Interface & Climate Grid Selection
  GRIDrv <- reactiveVal()
  STATErv <- reactiveVal()
  
  

  
  states_map2 <- reactive({
    switch(input$crop,
           "Corn" = corn_state_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84')%>% arrange(STATE_NAME),
           "Soybeans" = soybeans_state_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME),
           "Potatoes" = {
             if(input$level == "County"){
               potatoes_state_geometry %>%
                 sf::st_set_crs(4326) %>% 
                 sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME)
             }else if(input$level == "State"){
               potatoes_state_geometry[-6,] %>%
                 sf::st_set_crs(4326) %>% 
                 sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME)
             }
            },
           "Wheat" = wheat_state_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME)
    )
  })
  
  
  
  #if(!is.null(input$unit) && input$unit != ""){}
  data_new4 <- reactive({
    switch(input$crop,
           "Corn" = switch(input$level,
                           "County" = st_as_sf(corn_census_final) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                             sf::st_set_crs(4326) %>% 
                             sf::st_transform('+proj=longlat +datum=WGS84'),
                           "State" = st_as_sf(corn_survey_final) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                             sf::st_set_crs(4326) %>% 
                             sf::st_transform('+proj=longlat +datum=WGS84')),
           "Soybeans" = switch(input$level,
                               "County" = st_as_sf(soybeans_census_final) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                                 sf::st_set_crs(4326) %>% 
                                 sf::st_transform('+proj=longlat +datum=WGS84'),
                               "State" = st_as_sf(soybeans_survey_final) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                                 sf::st_set_crs(4326) %>% 
                                 sf::st_transform('+proj=longlat +datum=WGS84')),
           "Potatoes" = switch(input$level,
                               "County" = st_as_sf(potatoes_census_final) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                                 sf::st_set_crs(4326) %>% 
                                 sf::st_transform('+proj=longlat +datum=WGS84'),
                               "State" = st_as_sf(potatoes_survey_final %>% arrange(STATE_NAME)) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                                 sf::st_set_crs(4326) %>% 
                                 sf::st_transform('+proj=longlat +datum=WGS84')),
           "Wheat" = switch(input$level,
                               "County" = st_as_sf(wheat_census_final) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                                 sf::st_set_crs(4326) %>% 
                                 sf::st_transform('+proj=longlat +datum=WGS84'),
                               "State" = st_as_sf(wheat_survey_final %>% arrange(STATE_NAME)) %>% # Turns the geometry column into geometry, rather than observations (not column anymore, rather, characteristic)
                                 sf::st_set_crs(4326) %>% 
                                 sf::st_transform('+proj=longlat +datum=WGS84'))
    )
  })
  
  observeEvent(input$level, {
    if(input$level =="County"){
      updateSliderInput(session, "dates", step = 5)
      updateSliderInput(session, "dates", min = 2002L)
      updateSliderInput(session, "dates", max = 2022L)
    }
    if(input$level == "State"){
      updateSliderInput(session, "dates", step = 1)
      updateSliderInput(session, "dates", min = 2000L)
      updateSliderInput(session, "dates", max = 2023L)}
  })
  
  # Observe 'level' and 'crop' changes to reset 'stat' and 'unit'
  observeEvent(input$level, {
    updateSelectInput(session, "stat", selected = "")
    updateSelectInput(session, "unit", selected = "")
  })
  
  observeEvent(input$crop, {
    updateSelectInput(session, "stat", selected = "")
    updateSelectInput(session, "unit", selected = "")
  })
  
  # Dynamically generate 'stat' selectInput
  output$stat <- renderUI({
    selectInput(inputId = "stat", label = "Choose a STATISTICCAT_DESC",
                choices = c("", get_left_side(data_new4()[,-1])),  # Dynamically retrieve choices
                selected = "")
    
  })
  
  # Observe 'stat' changes to update 'unit'
  observeEvent(input$stat, {
    req(input$stat)  # Ensure 'stat' is selected
    updateSelectInput(session, "unit", selected = "")
  })
  
  # Dynamically generate 'unit' selectInput based on 'stat'
  output$unit <- renderUI({
    req(input$stat)  # Ensure 'stat' is selected
    x <- c("", get_right_side(data_new4()[,-1], as.character(input$stat)))
    selectInput(inputId = "unit", label = "Choose a UNIT_DESC",
                choices = x,
                selected = "")

  })
  
  # Disable 'unit' initially
  shinyjs::disable("unit")
  
  # Observer to update 'unit' choices based on 'stat'
  observeEvent(input$stat, {
    # Reset unit choices first
    updateSelectInput(session, "unit", choices = c(""), selected = "")
    
    # Get the relevant choices for 'unit' based on selected 'stat'
    unit_choices <- get_right_side(data_new4()[, -1], as.character(input$stat))
    
    # If valid choices exist, update 'unit' and enable it
    if (!is.null(unit_choices) && length(unit_choices) > 0) {
      updateSelectInput(session, "unit", choices = c("", unit_choices), selected = "")
      shinyjs::enable("unit")  # Enable 'unit' input
    } else {
      updateSelectInput(session, "unit", choices = c("No valid options"), selected = "")
      shinyjs::disable("unit")  # Disable 'unit' if no valid options
    }
  })
  
  # Observer for the toggle switch (on-off switch)
  observeEvent(input$toggle_map, {
    if (input$toggle_map) {
      # Toggle ON: Lock filters and generate the map
      shinyjs::disable("level")
      shinyjs::disable("crop")
      shinyjs::disable("stat")
      shinyjs::disable("unit")
      showNotification("Creating map...")
      
      # Insert your map generation code here
      # create_map_function()
      
    } else {
      # Toggle OFF: Unlock the filters for updating
      shinyjs::enable("level")
      shinyjs::enable("crop")
      shinyjs::enable("stat")
      shinyjs::enable("unit")
      showNotification("Filters unlocked for updating.")
    }
  })
  
  

  dates <- reactive({
    req(input$unit)

    data <- data_new4() %>%
      filter(YEAR == as.numeric(input$dates))

    validate(need(nrow(data) > 0, "No data available for the selected month."))

    return(data)
  })

  reactive_data <-  reactive({
    req(input$unit)
    
    a = input$stat
    b = input$unit
    e = paste0(a,"_",b)
    
    data_new4()[[e]]
    
  })
  
 
  reactive_stat <-  reactive({
    req(input$unit)
    
    a = input$stat
    b = input$unit
    e = paste0(a,"_",b)
    
    dates()[[e]]
    
  })
  

  pal_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0 &
       input$toggle_map == TRUE){
      validate(
        need(nrow(dates()) > 0, "No data to apply color palette.")
      )
      rdata = reactive_data()
      if(max(reactive_stat())==0){
        colorNumeric(palette = color_pal, domain = 0.01:10000)
      }else{
        colorNumeric(palette = color_pal, domain = rdata+1)
      }

      #colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
      #colorNumeric(palette = color_pal, domain = rdata)#reactive_data())
    }

  })

  
  popup_msg <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0&
       input$toggle_map == TRUE){
      if(input$level == "County") {
        str_c("<strong>", dates()$county_state, #", ", dates()$State,
              "</strong><br /><strong>", dates()$YEAR, "</strong>")
      }else if(input$level == "State") {
        str_c("<strong>", dates()$STATE_NAME, #", ", dates()$State,
              "</strong><br /><strong>", dates()$YEAR, "</strong>")
      }
      
    } 
  })
  
  
  output$map_pop <- renderLeaflet({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 &
       input$toggle_map == TRUE) {
      
      validate(
        need(nrow(dates()) > 0, "No data available for the selected year.")
      )
      
      # Initialize leaflet map (this part is common to both conditions)
      map <- leaflet(width = "100%",
                     options = leafletOptions(zoomSnap = 0,
                                              zoomDelta = 0.25)) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(lat = 41.550835, lng = -92.811029, zoom = 3.8) %>% # # 39.881612, -90.811029; 41.431141, -92.851743
        addPolygons(data = states_map2(),
                    group = "state",
                    color = "black",
                    fill = FALSE,
                    weight = 3)
      
      # Conditionally add county polygons if input$level is "County"
      if(input$level == "County") {
        layer_county <- unique(data_new4()$county_state)
        map <- map %>%
          addPolygons(data = st_transform(filter(data_new4(), YEAR == 2002), crs = "+init=epsg:4326"),
                      layerId = layer_county,
                      color = "white",
                      weight = 1,
                      smoothFactor = 0,
                      fillOpacity = 0.7)
      }
      if(input$level == "State") {
        layer_state <- unique(data_new4()$STATE_NAME)
        #layer_state <- unique(states_map2()$STATE_NAME)
        map <- map %>%
          addPolygons(data = states_map2(),
                      layerId = layer_state,  # Assign layer IDs for states
                      color = "black",         # Customize state polygon colors
                      weight = 2,
                      fillOpacity = 0.7)
      }
      
      # Return the leaflet map
      map
    }
  })

  
  observe({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0&
       input$toggle_map == TRUE){
      if(input$level == "County"){
        layer_county <- unique(data_new4()$county_state)
        leafletProxy("map_pop", data = dates()) %>% 
          setShapeStyle(layerId = layer_county, 
                        fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
      }
      if(input$level == "State"){
        layer_state <- unique(data_new4()$STATE_NAME)
        leafletProxy("map_pop", data = dates()) %>% 
          setShapeStyle(layerId = layer_state, 
                        fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
      }
    }
    
  })
  
  
  observe({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0&
       input$toggle_map == TRUE){
      if(input$level == "County"){
        layer_county <- unique(data_new4()$county_state)
        chosen_layers = layer_county
        leafletProxy("map_pop", data = dates()) %>%
          setShapeLabel(layerId = chosen_layers,
                        label = popup_msg())
      }
      if(input$level == "State") {
        layer_state <- unique(data_new4()$STATE_NAME)
        chosen_layers = layer_state
        leafletProxy("map_pop", data = dates()) %>%
          setShapeLabel(layerId = chosen_layers,
                        label = popup_msg())
      }
      
    }
    
    
  })
  
  observe({ 
    if(length(strsplit(as.character(req(input$unit)), ""))!=0 &
       input$toggle_map == TRUE){
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
    if(length(strsplit(as.character(req(input$unit)), ""))!=0&
       input$toggle_map == TRUE){
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
  

  updated_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), ""))!=0 &
       input$toggle_map == TRUE){
      if (input$level == "County"){
        validate(
          need(GRIDrv() != "" && !is.null(GRIDrv()), "Please select a county cell to generate analysis")
        )
        
        Data = data_new4() %>% 
          subset(county_state == GRIDrv()) 
        return(Data)
      }
      if (input$level == "State"){
        validate(
          need(STATErv() != "", "Please select a state to generate analysis")
        )
        Data = data_new4() %>% 
          subset(STATE_NAME == STATErv())
        return(Data)
      }
    }
  })
  
  output$text_output <- renderUI({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
      textOutput("display_text")
    }
    
  })
  
  output$display_text <- renderText({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
       
      paste0(sf::st_drop_geometry(updated_data()[1,1]), ", year ", input$dates)
    }
    
  })
  
  output$msg_pop <- renderTable({
    # Check if input$unit has valid value and toggle_map is set to TRUE
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
      
      # Get column names containing "_"
      ttt <- grep("_", colnames(updated_data()[, -1]), value = TRUE)
      
      Data <- updated_data() %>% filter(YEAR == as.numeric(input$dates))
      Data <- sf::st_drop_geometry(Data)

      tt <- t(Data[, ttt])

      if (ncol(tt) > 0) {
        colnames(tt) <- rep("", ncol(tt))  
      }
      
      # Replace 0 values with "(D)"
      tt[tt == 0] <- "(D)"
      
      # Format values: Keep "(D)" as is, and apply formatting to numeric values
      tt <- ifelse(tt == "(D)", "(D)", format(as.numeric(tt), big.mark = ",", scientific = FALSE))
      

      df_tt <- data.frame(
        STATISTICCAT_UNIT = row.names(tt),  
        Value = as.vector(tt)               
      )
      
      df_tt
      
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  output$var2 <- renderUI({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
      
      a = paste0(input$stat,"_", input$unit)
      Data = sf::st_drop_geometry(updated_data())
      ttt <- grep("_", colnames(Data[, -1]), value = TRUE)
      ttt = ttt[-which(ttt == a)]
      
      selectInput(inputId = "var2", label = "Choose another variable",
                  choices = c("", ttt),  # Dynamically retrieve choices
                  selected = "")
    }
    
  })
  
  tab_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      
      a = paste0(input$stat,"_", input$unit)
      b = input$var2
      Data = sf::st_drop_geometry(updated_data())
      df = Data[,c('YEAR', a,b)]
      
      #colnames(df) <- gsub("_", "<br>", colnames(df))
      
      df
    }
  })
  
  output$tab1 <- renderTable({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){

      df = tab_data()
      colnames(df) <- gsub("_", "<br>", colnames(df))
      df
    }

  }, striped = TRUE, hover = TRUE, bordered = TRUE,, sanitize.text.function = identity,
  table.attr = 'style="font-size:9px; width:50%;"')
  
  
  output$boxplot <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      
      selected_state <- unique(updated_data()$STATE_NAME)
      
      wheat_long <- sf::st_drop_geometry(data_new4()) %>%
        filter(STATE_NAME == selected_state) %>%
        #select(-geometry) %>% 
        pivot_longer(cols = -c(STATE_NAME, YEAR), 
                     names_to = "Category", values_to = "Value") %>%
        group_by(Category) %>%
        mutate(Value = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) %>%
        ungroup()
      
      
      # Replace Inf and -Inf with NA, and remove rows with NA values in Value
      wheat_long <- wheat_long %>%
        mutate(Value = ifelse(is.finite(Value), Value, NA)) %>%
        drop_na(Value)
      
      
      # Create a boxplot
      ggplot(wheat_long, aes(x = Category, y = Value)) +
        geom_boxplot() +
        labs(title = paste("Boxplot for", selected_state, "(Z-Score Standardized)"),
             x = "Category", y = "Standardized Value (Mean = 0, SD = 1)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
  })
  
  
  
  output$bar1 <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      Data = tab_data()
      names(Data) = c("Year", "a", "b")
      ggplot(Data) + 
        geom_bar(aes(x = as.factor(Year), y = a), stat = "identity", position = "dodge") +
        labs(title = "Bar graph of the first selected variable",
             x = "Year", 
             y = names(tab_data()[2])) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

  })
  
  output$bar2 <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      Data = tab_data()
      names(Data) = c("Year", "a", "b")
      ggplot(Data) + 
        geom_bar(aes(x = as.factor(Year), y = b), stat = "identity", position = "dodge") +
        labs(title = "Bar graph of the second selected variable",
             x = "Year", 
             y = names(tab_data()[3])) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
  })
  
  output$scatter2 <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      
      Data <- tab_data()
      names(Data) <- c("Year", "a", "b")
      
      # Define scale factor (ensure it's valid)
      scale <- 1000  # Adjust the scale factor as needed
      
      if(sum(Data$b, na.rm = TRUE) / sum(Data$a, na.rm = TRUE) > 100){
        ggplot(Data, aes(x = Year)) +
          geom_line(aes(y = a, color = names(tab_data()[2]))) + # First y-axis
          geom_line(aes(y = b / scale, color = names(tab_data()[3]))) + # Second y-axis, scaled
          scale_y_continuous(
            name = names(tab_data()[2]),
            sec.axis = sec_axis(~.*scale, name = names(tab_data()[3])),
            limits = c(0, max(c(max(Data$a, na.rm = TRUE), max(Data$b / scale, na.rm = TRUE))))
          ) +
          labs(x = "Year", color = "Variable") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom" # Move legend to the bottom
          )
        
      } else if(sum(Data$a, na.rm = TRUE) / sum(Data$b, na.rm = TRUE) > 100){
        ggplot(Data, aes(x = Year)) +
          geom_line(aes(y = b, color = names(tab_data()[3]))) + # First y-axis
          geom_line(aes(y = a / scale, color = names(tab_data()[2]))) + # Second y-axis, scaled
          scale_y_continuous(
            name = names(tab_data()[3]),
            sec.axis = sec_axis(~.*scale, name = names(tab_data()[2])),
            limits = c(0, max(c(max(Data$b, na.rm = TRUE), max(Data$a / scale, na.rm = TRUE))))
          ) +
          labs(x = "Year", color = "Variable") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom" # Move legend to the bottom
          )
      } else {
        ggplot(Data, aes(x = Year)) +
          geom_line(aes(y = b, color = names(tab_data()[3]))) + # First y-axis
          geom_line(aes(y = a, color = names(tab_data()[2]))) + # Second y-axis, scaled
          labs(x = "Year", color = "Variable") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom" # Move legend to the bottom
          )
        
      }
      
    }
  })
  
  plots_data <- reactive({
    switch(input$crop,
           "Corn" =  {
             # Replace "(D)" with 0 in the entire data frame
             corn_survey[corn_survey == "(D)"] <- 0
             
             # Remove commas and convert VALUE to numeric
             corn_survey$VALUE <- as.numeric(gsub(",", "", corn_survey$VALUE))
             
             # Apply the condition to set VALUE to 0 where conditions are met
             corn_survey %>%
               mutate(VALUE = if_else(
                 STATISTICCAT_DESC %in% c("PROGRESS", "PROGRESS, PREVIOUS YEAR", "PROGRESS, 5 YEAR AVG") &
                   UNIT_DESC == "PCT HARVESTED" &
                   UTIL_PRACTICE_DESC == 'SILAGE',
                 0,  # Set Value to 0 if conditions are met
                 VALUE  # Keep the original Value otherwise
               ))
           },
           "Soybeans" = soybeans_survey,
           "Potatoes" = potatoes_survey,
           "Wheat" = wheat_survey
    )
  })
  
  
  data_condition <- reactive({
    
    c_survey <- plots_data()
    
    c_survey[c_survey == "(D)"] <- 0
    c_survey$VALUE <- as.numeric(gsub(",","",c_survey$VALUE))

    
    inx <- grep("WEEK", c_survey$REFERENCE_PERIOD_DESC, value = FALSE)
    
    c_survey_week <- c_survey[inx,] %>% 
      arrange(WEEK_ENDING) %>% 
      filter(YEAR>=2000,
             FREQ_DESC == "WEEKLY",
             #UTIL_PRACTICE_DESC == 'GRAIN',
             STATISTICCAT_DESC %in% c("PROGRESS",
                                      "PROGRESS, PREVIOUS YEAR",
                                      "PROGRESS, 5 YEAR AVG",
                                      "CONDITION",
                                      "CONDITION, PREVIOUS YEAR",
                                      "CONDITION, 5 YEAR AVG",
                                      "MOISTURE",
                                      "MOISTURE, PREVIOUS YEAR",
                                      "MOISTURE, 5 YEAR AVG",
                                      "HEIGHT, AVG",
                                      "HEIGHT, AVG, PREVIOUS YEAR",
                                      "HEIGHT, AVG, 5 YEAR AVG")) %>% 
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
    
    
    wide_data_week <- c_survey_week %>% 
      pivot_wider(
        names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
        values_from = sum_value
      ) %>% 
      arrange(WEEK_ENDING)
    
    c_survey_week_final <- wide_data_week %>% 
      mutate(good_excellent = `CONDITION_PCT EXCELLENT` + `CONDITION_PCT GOOD`,
             Year = year(WEEK_ENDING),
             Week_Num = as.numeric(gsub("WEEK #", "", REFERENCE_PERIOD_DESC))) %>%  # Extract numeric week number)
      arrange(STATE_NAME, Week_Num)
    
    return(c_survey_week_final)
    
  })
  
  output$main_states <- renderText({
    req(data_condition())
    states <- unique(na.omit(data_condition()$STATE_NAME))
    paste("The following plots are available only for the following states:\n", 
          paste(states, collapse = ", "))
  })
  
  output$zscore_box <- renderText({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0)
      paste("Z-standardization allows for comparison between data for different variables.
    Z-scores represent the number of standard deviations the data point
                                         is from the mean.")
  })
  
  
  output$excellent_good <- renderPlot({
    if(input$toggle_map == TRUE & input$level == "State"){
      name = unique(updated_data()$STATE_NAME)
      year = as.numeric(input$dates)
      
      df1 <- data_condition() %>% 
        ungroup() %>% 
        dplyr::select(REFERENCE_PERIOD_DESC, STATE_NAME, Year, Week_Num, good_excellent) %>% 
        filter(STATE_NAME == name, 
               Year %in% c((year-4):year)) %>% 
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
          title = "Good Excellent Values of the crop by Week for selected state",
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
  
      
    }
    
  })
  
  output$condition <- renderPlot({
    if(input$toggle_map == TRUE& input$level == "State"){
      name = unique(updated_data()$STATE_NAME)
      year = as.numeric(input$dates)
      
      df2 <- data_condition() %>% 
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
        filter(STATE_NAME == name, 
               Year == year) %>% 
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
          title = "Cumulative Crop Condition by Week in the selected state and year"
        ) +
        theme_minimal()

    }
    
  })
  
  
  output$progress <- renderPlot({
    if(input$toggle_map == TRUE& input$level == "State"){
      name = unique(updated_data()$STATE_NAME)
      year = as.numeric(input$dates)
      
      t2 = grep("PROGRESS", names(data_condition()))
      t1 = which(names(data_condition()) %in% c("STATE_NAME",
                                                      "REFERENCE_PERIOD_DESC",
                                                      "Year", 
                                                      "Week_Num"))
      
      df3 <- data_condition()[, c(t1,t2)]
      df3 <- df3 %>% 
        ungroup() %>% 
        filter(STATE_NAME == name, 
               Year == input$dates) %>% 
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
          title = "Progress of Crop by Category Over Weeks",
          x = "Week Number",
          y = "Percentage",
          color = "Category",
          linetype = "Legend"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("PLANTED" = "darkolivegreen", "MILK" = "cyan3", "DENTED" = "chartreuse3", "MATURE" = "black",
                                      "EMERGED" = "red" ,  "SILKING" = "gold" ,  "DOUGH" = 'purple', "HARVESTED" = "blue")) +
        scale_linetype_manual(values = c("Current Year" = "solid", "Previous Year" = "dashed", "5 Year Avg" = "dotted")) +
        theme(legend.position = "bottom")
      
      
    }
    
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)


