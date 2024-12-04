
if ("raster" %in% .packages()) {
  detach("package:raster", unload = TRUE)
}


options(timeout=300)

# Check and install necessary packages, Load libraries
list.of.packages <- c('shiny', 'shinythemes', 'tidyverse', 'shinyWidgets', 'leaflet', 'sf',
                      'bslib', 'imputeTS', 'data.table', 'purrr', 'shinyjs', 'scales')
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
url_1 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/corn_census_final.rda?raw=true"
url_var = url(url_1)
load(url_var)
close(url_var)

url_2 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/corn_county_geometry.rda?raw=true"
url_var = url(url_2)
load(url_var)
close(url_var)

url_3 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/corn_state_geometry.rda?raw=true"
url_var = url(url_3)
load(url_var)
close(url_var)

url_4 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/corn_survey_final.rda?raw=true"
url_var = url(url_4)
load(url_var)
close(url_var)

url_5 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/corn_survey_week_final.rda?raw=true"
url_var = url(url_5)
load(url_var)
close(url_var)

url_6 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/potatoes_census_final.rda?raw=true"
url_var = url(url_6)
load(url_var)
close(url_var)

url_7 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/potatoes_county_geometry.rda?raw=true"
url_var = url(url_7)
load(url_var)
close(url_var)

url_8 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/potatoes_state_geometry.rda?raw=true"
url_var = url(url_8)
load(url_var)
close(url_var)

url_9 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/potatoes_survey_final.rda?raw=true"
url_var = url(url_9)
load(url_var)
close(url_var)

url_10 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/potatoes_survey_week_final.rda?raw=true"
url_var = url(url_10)
load(url_var)
close(url_var)

url_11 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/soybeans_census_final.rda?raw=true"
url_var = url(url_11)
load(url_var)
close(url_var)

url_12 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/soybeans_county_geometry.rda?raw=true"
url_var = url(url_12)
  load(url_var)
close(url_var)

url_13 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/soybeans_state_geometry.rda?raw=true"
url_var = url(url_13)
load(url_var)
close(url_var)

url_14 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/soybeans_survey_final.rda?raw=true"
url_var = url(url_14)
load(url_var)
close(url_var)

url_15 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/soybeans_survey_week_final.rda?raw=true"
url_var = url(url_15)
load(url_var)
close(url_var)

url_16 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/wheat_census_final.rda?raw=true"
url_var = url(url_16)
load(url_var)
close(url_var)

url_17 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/wheat_county_geometry.rda?raw=true"
url_var = url(url_17)
load(url_var)
close(url_var)

url_18 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/wheat_state_geometry.rda?raw=true"
url_var = url(url_18)
load(url_var)
close(url_var)

url_19 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/wheat_survey_final.rda?raw=true"
url_var = url(url_19)
load(url_var)
close(url_var)

url_20 <- "https://github.com/SDS-at-UE/NASS-FA24/blob/main/Complete_data/App/new_data/wheat_survey_week_final.rda?raw=true"
url_var = url(url_20)
load(url_var)
close(url_var)

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
    column(3, div(style="text-align: center;",a(href = "https://www.nass.usda.gov/", 
                                                img(src = "NASS.jpg", height = "30%", width = "30%"))
                                                )),
    #column(2, span(h5(img(src = "Logo_US_Forest.jpg", height = "30%", width = "30%")  ,"US Forest Service" ))),
    column(4, div(style = "font-size: 18px; color: black;",
                  "This page depicts the Crop data analysis for",
                  br(),
                  strong("United States Department of Agriculture"),
                  br(),
                  strong("National Agricultural Statistics Service")
    )), 
    column(3, br(), div(style="text-align: left;",a(href = "https://www.evansville.edu/index.cfm", 
                                                    img(src = "UE_logo.jpg", height = "70%", width = "70%"))
                                                    )),
    column(2, div(style="text-align: top;",a(href = "https://www.evansville.edu/majors/mathematics/changelab-research.cfm", 
                                             img(src = "new_stat_300.jpg", height = "100px", width = "190px"))))
    #Stat300_2
  ),
  

  navbarPage(leafletjs, theme = shinytheme("cosmo"),
                title = "Agriculture Data Analysis Portal",
             tabPanel('Introduction',
                      fluidRow(
                        column(7,  # Adjusted width to span both columns
                               div(
                                 # First Section: General Information
                                 div(
                                   HTML(
                                     "The data analysis for the crop dataset was conducted during the fall semester of 2024.
      The dataset was obtained from "
                                   ),
                                   tags$a(
                                     href = "https://www.nass.usda.gov/datasets/",
                                     "the NASS website.",
                                     style = "text-decoration: underline; color: blue;"
                                   ),
                                   style = "font-size: 15px; color: rgb(78,53,36); margin-bottom: 0px;"
                                 ),
                                 div(
                                   HTML(
                                     "The states included in this analysis are part of the Objective Yield (OY) program.
                                        For more information, visit "
                                   ),
                                   tags$a(
                                     href = "https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Objective_Yield/index.php",
                                     "the official page.",
                                     style = "text-decoration: underline; color: blue;"
                                   ),
                                   HTML(
                                     "<br><b>States included in this analysis:</b>
                                      <ul style='margin: 0; padding-left: 20px;'>
                                        <li style='color: rgb(78,53,36);'><b><span style='color: green;'>Corn:</span></b> Illinois, Indiana, Iowa, Kansas, Minnesota, Missouri, Nebraska, Ohio, South Dakota, Wisconsin, Michigan, Kentucky</li>
                                        <li style='color: rgb(78,53,36);'><b><span style='color: green;'>Soybeans:</span></b> Arkansas, Illinois, Indiana, Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, Ohio, South Dakota, Michigan, Kentucky</li>
                                        <li style='color: rgb(78,53,36);'><b><span style='color: green;'>Wheat:</span></b> Colorado, Illinois, Kansas, Missouri, Montana, Nebraska, Ohio, Oklahoma, Texas, Washington, Michigan, Kentucky, Indiana</li>
                                        <li style='color: rgb(78,53,36);'><b><span style='color: green;'>Potatoes:</span></b> Idaho, Maine, Minnesota, North Dakota, Oregon, Washington, Wisconsin, Illinois, Missouri, Michigan, Kentucky, Indiana, Ohio</li>
                                      </ul>"
                                   ),
                                   style = "font-size: 15px; color: rgb(78,53,36); margin-top: 10px;"
                                 ),

                                 # Overall Styling for the Container
                                 style = "display: flex; flex-direction: column; gap: 20px; line-height: 1.5;"
                               ),
                               div(
                                 div(
                                   HTML(
                                     "<br /><b>Our Project with NASS: Making Data Accessible Through Visualization</b><br />"
                                   ),
                                   HTML(
                                     "As a team of dedicated students from the STAT 300: Data Analysis in the Real World course at the University of Evansville, we are pleased to present our experimental visualization project in collaboration with the USDA's National Agricultural Statistics Service (NASS). 
                                     This valuable experience allowed us to apply our skills to a substantial challenge with a focus on making national agricultural data accessible for farmers, businesses, and others who may benefit from data accessibility.<br /> 
                                      Our central objective was to develop impactful data visualizations that effectively communicate insights from agricultural data to a wider audience. Utilizing advanced software and programming like R, RStudio, Shiny, Leaflet, and more, we transformed vast complex datasets into graphics designed to simplify comprehension and emphasize geographical trends.<br />
This project has marked a crucial milestone in our educational journey, solidifying the necessity of data visualization in making analytical information accessible, interpretable, and relevant. We are eager to share our findings and aim to foster a deeper appreciation for the data that informs and drives U.S. agriculture."
                                   ),
                                   style = "font-size: 15px; color: rgb(78,53,36); margin-bottom: 0px;"
                                 ),
                                 div(
                                   
                                   tags$a(
                                     href = "https://www.evansville.edu/majors/mathematics/changelab-research.cfm",
                                     "Other Changelab Projects from the University of Evansville",
                                     style = "text-decoration: underline; color: blue;"
                                   ),
                                   style = "font-size: 15px; color: rgb(78,53,36); margin-top: 10px;" # applies to everything within this div()
                                 ),
                                 
                                 # Overall Styling for the Container
                                 style = "display: flex; flex-direction: column; gap: 20px; line-height: 1.5;"
                               )
                                   
                        ),
                        column(5,
                               div(style="text-align: right;",
                                   img(src = "agriculture.jpg", 
                                       height = 'auto', 
                                       width = '100%',
                                       style = "float: right; object-fit: cover;"))
                        ),
                        
                      ),
                      
                      
                      
                      
                      hr(style = "margin-top: 5px; margin-bottom: 5px; border-width: 2px;; border-color: black;"),
                      
                      h3("Total and average yield (BU/ACRE) of the crops in the selected states:"),
                      fluidRow(
                        column(3,
                               div(style="text-align: center;",
                                   img(src = "corn.jpg", 
                                       height = '300px',
                                       width = '100%',
                                       style = "object-fit: cover; display: block; max-height: 300px;")),
                               br(),
                               plotOutput("corn_plot"),
                               div(class = "scrollable-container",
                                   tableOutput("corn_intro")),
                               tags$style(HTML("
                                #corn_intro table {  /* Target only the table inside 'corn_intro' */
                                  font-size: 10px;  /* Adjust font size */
                                  text-align: center;
                                  margin: 0 auto;
                                }
                              "))
                                
                        ),
                        column(3,
                               div(style="text-align: center;",
                                   img(src = "soybeans.jpg", 
                                       height = '300px',
                                       width = '100%',
                                       style = "object-fit: cover; display: block; max-height: 300px;" )),
                               
                               br(),
                               plotOutput("soybeans_plot"),
                               div(class = "scrollable-container",
                                   tableOutput("soybeans_intro")),
                               tags$style(HTML("
                                #soybeans_intro table {  /* Target only the table inside 'soybeans_intro' */
                                  font-size: 10px;  /* Adjust font size */
                                  text-align: center;
                                  margin: 0 auto;
                                }
                              "))
                        ),
                        column(3,
                               div(style="text-align: center;",
                                   img(src = "wheat.jpg", 
                                       height = '300px',
                                       width = '100%',
                                       style = "object-fit: cover; display: block; max-height: 300px;")),
                               
                               br(),
                               plotOutput("wheat_plot"),
                               div(class = "scrollable-container",
                                   tableOutput("wheat_intro")),
                               tags$style(HTML("
                                #wheat_intro table {  /* Target only the table inside 'wheat_intro' */
                                  font-size: 10px;  /* Adjust font size */
                                  text-align: center;
                                  margin: 0 auto;
                                }
                              "))
                        ),
                        column(3,
                               div(style="text-align: center;",
                                   img(src = "potatoes.jpg", 
                                       height = '300px', 
                                       width = '100%',
                                       style = "object-fit: cover; display: block; max-height: 300px;")),
                               br(),
                               plotOutput("potatoes_plot"),
                               div(class = "scrollable-container",
                                   tableOutput("potatoes_intro")),
                               tags$style(HTML("
                                #potatoes_intro table {  /* Target only the table inside 'potatoes_intro' */
                                  font-size: 10px;  /* Adjust font size */
                                  text-align: center;
                                  margin: 0 auto;
                                }
                              "))
                               
                        ),
                      ),
                      tags$style(HTML("
                        .scrollable-container {
                          height: 300px;  /* Set a fixed height */
                          overflow-y: auto;  /* Enable vertical scrolling */
                          overflow-x: hidden;  /* Prevent horizontal scrolling */
                        }
                      ")),
                      br(),
                      
                      fluidRow(
                        div(
                          align = "center",
                          class = "footer",
                          wellPanel(
                            helpText(
                              HTML(
                                "This app was developed by students at the 
                                <a href='https://www.evansville.edu/'><b style='font-family: \"Arial Black\", Gadget, sans-serif;'>University of Evansville</b></a>, led by Dr. Omid Khormali:<br /><br />"
                              ),
                              column(4,
                                     HTML(
                                       "<table style='width: 100%; font-size: 12px; color: rgb(76, 38, 131); font-family: Arial, sans-serif; border-collapse: collapse; text-align: right;'>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Andrew Thompson,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Ethan John,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Abbie Jahn,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Josie Pagano,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Max Hansmann,</td>
                                 </tr>
                                 </table>"),
                              ),
                              column(8, 
                                     HTML(
                                       "<table style='width: 100%; font-size: 12px; color: rgb(76, 38, 131); font-family: Arial, sans-serif; border-collapse: collapse;'>
                                   <tr>
                                   <td style='padding: 2px;'>B.S. Statistics and Data Science, B.S. Computer Science, Class of 2027</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>B.S. Neuroscience, Class of 2026</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>B.S. Biology, Class of 2026</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>B.S. Biology, Class of 2024</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>B.S. Statistics and Data Science, Class of 2026</td>
                                   </tr>
                                   </table><br />"
                                     )
                              )
                            ),
                            br(),
                            helpText(
                              HTML(
                                "<b style='font-family: \"Arial Black\", Gadget, sans-serif;'>Special thanks</b> to NASS employees who provided us with feedback in this project:<br /><br />"
                              ),
                              column(4,
                                     HTML(
                                       "<table style='width: 100%; font-size: 12px; color: rgb(76, 38, 131); font-family: Arial, sans-serif; border-collapse: collapse; text-align: right;'>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Virginia Harris,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Nathanial Warenski,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Brad Summa,</td>
                                 </tr>
                                 <tr>
                                 <td style='font-weight: bold; text-align: right; padding: 2px;'>Chris Hawthorn,</td>
                                 </tr>
                                 </table>"),
                              ),
                              column(8, 
                                     HTML(
                                       "<table style='width: 100%; font-size: 12px; color: rgb(76, 38, 131); font-family: Arial, sans-serif; border-collapse: collapse;'>
                                   <tr>
                                   <td style='padding: 2px;'>Economic, Environmental and Demographic Section, Statistics Division</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>Indiana State Statistician</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>Director, Heartland Regional Field Office</td>
                                   </tr>
                                   <tr>
                                   <td style='padding: 2px;'>Acting Head, Field Crops Section</td>
                                   </tr>
                                   </table><br />"
                                     )
                              )
                            ),
                            
                            HTML(
                              "<p style='text-align: left; font-size: 12px; font-family: Arial, sans-serif; margin-top: 10px;'>* The images used in this application are sourced from <a href='https://pixabay.com/' target='_blank'>Pixabay</a>, a platform offering free, high-quality images. </p>"
                            )
                          )
                        )
                        
                        
                        ),
                      
                      
                        
                      
                      ),
                tabPanel('Interactive Map',
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
                                             # switchInput(inputId = "toggle_map", label = "Mode", 
                                             #             onLabel = "Create Map", offLabel = "Update Filters", 
                                             #             value = FALSE),
                                             prettySwitch(
                                               inputId = "toggle_map",
                                               label = "Create the Map!",
                                               status = "success",
                                               fill = TRUE
                                             ),
                                             
                                             hr(),
                                             hr(),
                                             uiOutput("var2"),
                                             br(),
                                             br(),
                                             uiOutput("distance"),
                                             
                                             br(),
                                             uiOutput("update_map"),
                                             
                                           )
                                           
                                    ),
                                    # column(8,sliderInput(inputId = "dates", "Timeline of Selected Parameter", 
                                    #                      min = 2000L, #min(c(corn_county_cencus$YEAR, corn_county_survey$YEAR)),
                                    #                      max = 2023L, #max(c(corn_county_cencus$YEAR, corn_county_survey$YEAR)),
                                    #                      value = 2022L,
                                    #                      sep = "",
                                    #                      #timeFormat = "%m-%d-%Y",
                                    #                      step = 1,
                                    #                      ticks = FALSE,
                                    #                      width = '100%',
                                    #                      animate = animationOptions(interval = 3000),
                                    # ),
                                    # br(),
                                    # br(),
                                    # leafletOutput("map_pop"),
                                    # 
                                    # box(
                                    #   h3("Explanation of the Plot"),
                                    #   p("In this plot, the values of the variables are scaled using a statistical method called z-scores. This means that instead of showing the raw values (e.g., yield, production), the values are adjusted to show how far they are from the average (mean) value for each variable. The y-axis represents standard deviations:"),
                                    #   tags$ul(
                                    #     tags$li("A value of 0 means the data point is equal to the average."),
                                    #     tags$li("Positive values (e.g., +2) mean the data point is above the average."),
                                    #     tags$li("Negative values (e.g., -2) mean the data point is below the average.")
                                    #   ),
                                    #   p("This scaling allows us to compare variables with different units (e.g., bushels, acres) on the same scale, making it easier to identify patterns and trends."),
                                    #   
                                    #   plotOutput("boxplot")
                                    # )
                                    # 
                                    # )
                                    column(
                                      8,
                                      # Timeline Slider
                                      sliderInput(
                                        inputId = "dates", 
                                        label = "Timeline of Selected Parameter", 
                                        min = 2000L,#min(corn_county_census$YEAR, corn_county_survey$YEAR, na.rm = TRUE), 
                                        max = 2023L, #max(corn_county_census$YEAR, corn_county_survey$YEAR, na.rm = TRUE), 
                                        value = 2022L,
                                        sep = "",
                                        step = 1,
                                        ticks = FALSE,
                                        width = '100%',
                                        animate = animationOptions(interval = 3000)
                                      ),
                                      br(),
                                      br(),
                                      
                                      # Leaflet Map
                                      leafletOutput("map_pop"),
                                      
                                      # Explanation and Plot Box
                                      # box(
                                      #   title = "Understanding the Plot",
                                      #   status = "primary",
                                      #   solidHeader = TRUE,
                                      #   collapsible = TRUE,
                                      #   width = NULL,
                                        uiOutput("boxplot_explanation"),
                                        # Boxplot Output
                                        plotOutput("boxplot")
                                      #)
                                    )
                                    
                                  ),
                                  
                                  ),
                           column(3,
                                  uiOutput("text_output"), 
                                  tableOutput("msg_pop"),
                                  br(),
                                  uiOutput("text_output_total"), 
                                  tableOutput("msg_pop_total"),
                                  uiOutput("text_output_average"), 
                                  tableOutput("msg_pop_average"),
                                  
                                  )        
                         ),
                         fluidRow(
                           column(12,
                                  fluidRow(
                                    column(4,
                                           
                                           tableOutput("tab1")
                                    ),
                                    column(8,
                                           # fluidRow(
                                           #   column(8,
                                           #          plotOutput("boxplot")
                                           #          ),
                                           #   column(4,
                                           #          #textOutput("zscore_box")
                                           #          h3("Explanation of the Plot"),
                                           #          p("In this plot, the values of the variables are scaled using a statistical method called z-scores. This means that instead of showing the raw values (e.g., yield, production), the values are adjusted to show how far they are from the average (mean) value for each variable. The y-axis represents standard deviations:"),
                                           #          tags$ul(
                                           #            tags$li("A value of 0 means the data point is equal to the average."),
                                           #            tags$li("Positive values (e.g., +2) mean the data point is above the average."),
                                           #            tags$li("Negative values (e.g., -2) mean the data point is below the average.")
                                           #          ),
                                           #          p("This scaling allows us to compare variables with different units (e.g., bushels, acres) on the same scale, making it easier to identify patterns and trends."),
                                           #          
                                           #   )
                                           # ),
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
                         conditionalPanel(
                           condition = "input.level == 'State'", 
                         fluidRow(column(12,
                                         h2("Crop Progress and Condition in the selected states"),
                                         br(),
                                         textOutput("main_states"),
                                         br(),
                                         hr(),
                                         HTML(
                                           'This chart displays the percentage of the selected crop that was rated good or excellent 
                                           over several weeks for the selected state, comparing data from the past 4 years. 
                                           This highlights variability in crop quality over time and and between subsequent years. '
                                         ),
                                         br(),
                                         br(),
                                         plotOutput("excellent_good"),
                                         br(),
                                         hr(),
                                         HTML(
                                           'This plot displays crop condition. The colors represent the proportion of crop status between
                                           Excellent, Good, Fair, Poor, and Very Poor ratings over time. 
                                           This can be used to determine the portion of healthy crop in a given year for the selected state. '
                                         ),
                                         br(),
                                         br(),
                                         plotOutput("condition"),
                                         br(),
                                         hr(),
                                         HTML(
                                           'This plot provides a comprehensive look at crop progress throughout the stages of production, 
                                           showcasing the weekly development of each stage compared to previous years and the five year average. 
                                           This can be used to highlight trends in crop development. '
                                         ),
                                         br(),
                                         br(),
                                         plotOutput("progress"),
                                         
                                         ))
                         )

                         ),
                     tabPanel('Parameter Comparison',
                            fluidRow(
                                     column(3,
                                            h4("The comparision will do for the selected crop in the Interactive Map tab!")
                                            ),
                                     column(6,
                                            fluidRow(
                                              column(6, 
                                                     h4("Group 1"),
                                                     uiOutput("state1"),
                                                     #uiOutput("county1")
                                                     ),
                                              column(6,
                                                     h4("group 2"),
                                                     uiOutput("state2"),
                                                     #uiOutput("county2")
                                                     )
                                            )
                                            ),
                                     column(3,
                                            selectInput(inputId = "level2", "Choose a level", 
                                                        c("County", "State"),
                                                        selected = "State"),
                                            
                                            selectInput(inputId = "crop2", "Choose a crop", 
                                                        c("Corn", "Soybeans", "Potatoes", "Wheat"),
                                                        selected = "Corn"),
                                            
                                            uiOutput("parameters"),
                                            
                                            # br(),
                                            # prettySwitch(
                                            #   inputId = "county_level",
                                            #   label = "Compare counties!",
                                            #   status = "success",
                                            #   fill = TRUE
                                            # ),
                                            
                                            )
                                     ),
                            fluidRow( 
                                     column(8,
                                            plotOutput("comparison")
                                            ),
                                     column(4,
                                            tableOutput("table_comparison")
                                            )
                                     )
                     ),
                )
)


server <- function(input, output,session) {
  ### Map Interface & Climate Grid Selection
  GRIDrv <- reactiveVal()
  STATErv <- reactiveVal()
  
  

  
  states_map2 <- reactive({
    switch(input$crop,
           "Corn" = corn_state_geometry %>% mutate(STATE_NAME = toupper(STATE_NAME)) %>% 
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84')%>% arrange(STATE_NAME),
           "Soybeans" = soybeans_state_geometry %>% mutate(STATE_NAME = toupper(STATE_NAME)) %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME),
           "Potatoes" = {
             if(input$level == "County"){
               potatoes_state_geometry %>% mutate(STATE_NAME = toupper(STATE_NAME)) %>%
                 sf::st_set_crs(4326) %>% 
                 sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME)
             }else if(input$level == "State"){
               potatoes_state_geometry[-6,] %>% mutate(STATE_NAME = toupper(STATE_NAME)) %>%
                 sf::st_set_crs(4326) %>% 
                 sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME)
             }
            },
           "Wheat" = wheat_state_geometry %>% mutate(STATE_NAME = toupper(STATE_NAME)) %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84') %>% arrange(STATE_NAME)
    )
  })

  
  data_new4 <- reactive({
    switch(input$crop,
           "Corn" = switch(input$level,
                           "County" = corn_census_final,
                           "State" = corn_survey_final),
           "Soybeans" = switch(input$level,
                               "County" = soybeans_census_final,
                               "State" = soybeans_survey_final),
           "Potatoes" = switch(input$level,
                               "County" = potatoes_census_final,
                               "State" = potatoes_survey_final),
           "Wheat" = switch(input$level,
                            "County" = wheat_census_final,
                            "State" = wheat_survey_final)
    )
  })
  
  
  
  data_new2 <- reactive({
    switch(input$crop2,
           "Corn" = switch(input$level2,
                           "County" = corn_census_final,
                           "State" = corn_survey_final),
           "Soybeans" = switch(input$level2,
                               "County" = soybeans_census_final,
                               "State" = soybeans_survey_final),
           "Potatoes" = switch(input$level2,
                               "County" = potatoes_census_final,
                               "State" = potatoes_survey_final),
           "Wheat" = switch(input$level2,
                            "County" = wheat_census_final,
                            "State" = wheat_survey_final)
    )
  })
  
  county_geometry <- reactive({
    switch(input$crop,
           "Corn" = corn_county_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84'),
           "Soybeans" = soybeans_county_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84'),
           "Potatoes" = potatoes_county_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84'),
           "Wheat" = wheat_county_geometry %>%
             sf::st_set_crs(4326) %>% 
             sf::st_transform('+proj=longlat +datum=WGS84')
    )
  })
  
  # county_geo <- reactive({
  #   if(input$level == "County"){
  #     D <- data_new4() %>%
  #       left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state")
  #     
  #     st_as_sf(D) %>% 
  #       sf::st_set_crs(4326) %>%
  #       sf::st_transform('+proj=longlat +datum=WGS84')
  #   }
  # })
  
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
  
  
  
  
  
  output$distance <- renderUI({
    if(input$level == "County"){
      sliderInput("distance", "Select Distance (miles):", min = 10, max = 500, value = 200, step = 10)
      } 
  })
  
  output$update_map <- renderUI({
    if(input$level == "County"){
      prettySwitch(
        inputId = "update_map",
        label = "Update the map!",
        status = "success",
        fill = TRUE
      )
    } 
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
  
  ##############################################
  pal_data <- reactive({
    req(input$unit, input$toggle_map == TRUE)
    validate(need(nrow(dates()) > 0, "No data to apply color palette."))
    
    colorNumeric(palette = color_pal, domain = reactive_stat() + 1)
  })
  
  ###########################################

  # pal_data <- reactive({
  #   if(length(strsplit(as.character(req(input$unit)), ""))!=0 &
  #      input$toggle_map == TRUE){
  #     validate(
  #       need(nrow(dates()) > 0, "No data to apply color palette.")
  #     )
  #     rdata = reactive_data()
  #     if(max(reactive_stat())==0){
  #       colorNumeric(palette = color_pal, domain = 0.01:10000)
  #     }else{
  #       colorNumeric(palette = color_pal, domain = rdata+1)
  #     }
  # 
  #     #colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
  #     #colorNumeric(palette = color_pal, domain = rdata)#reactive_data())
  #   }
  # })
  ############################################################
  
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
        
        county_geo <- data_new4() %>%
          left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state")
        
        county_geo <- st_as_sf(county_geo) %>% 
          sf::st_set_crs(4326) %>%
          sf::st_transform('+proj=longlat +datum=WGS84')
        
        layer_county <- unique(data_new4()$county_state)
        map <- map %>%
          addPolygons(data = st_transform(filter(county_geo, YEAR == 2002), crs = "+init=epsg:4326"),
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
  
  ################################################
  # observeEvent(input$map_pop_shape_click, {
  #   print(input$map_pop_shape_click$id)
  #   
  #   if (input$level == "County") {
  #     GRIDrv(input$map_pop_shape_click$id)
  #     print(paste("Selected County:", GRIDrv()))
  #   } else if (input$level == "State") {
  #     STATErv(input$map_pop_shape_click$id)
  #     print(paste("Selected State:", STATErv()))
  #   }
  # })
  
  
  #####################################
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
  #############################################
  selected_data <- reactiveVal(NULL)

  observeEvent(GRIDrv(), {
    req(input$level == "County", GRIDrv())

    # Fetch data for the selected county
    selected_data(data_new4() %>% filter(county_state == GRIDrv()))
  })

  observeEvent(STATErv(), {
    req(input$level == "State", STATErv())

    # Fetch data for the selected state
    selected_data(data_new4() %>% filter(STATE_NAME == STATErv()))
  })

  updated_data <- reactive({
    req(selected_data())
    selected_data()
  })

  #############################################
  # updated_data <- reactive({
  #   req(input$unit, input$toggle_map)
  #   # if(length(strsplit(as.character(req(input$unit)), ""))!=0 &
  #   #    input$toggle_map == TRUE){
  #     if (input$level == "County"){
  #       validate(
  #         need(GRIDrv() != "" && !is.null(GRIDrv()), "Please select a county cell to generate analysis")
  #       )
  # 
  #       Data = data_new4() %>%
  #         subset(county_state == GRIDrv())
  #       return(Data)
  #     }
  #     if (input$level == "State"){
  #       validate(
  #         need(STATErv() != "", "Please select a state to generate analysis")
  #       )
  #       Data = data_new4() %>%
  #         subset(STATE_NAME == STATErv())
  #       return(Data)
  #     }
  #   #}
  # })
  ####################################################
  
  
  output$text_output <- renderUI({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & 
       input$toggle_map == TRUE){
      #textOutput("display_text")
      paste0(updated_data()[1,1], ", year ", input$dates)
    }
    
  })
  
  # output$display_text <- renderText({
  #   if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
  # 
  #     paste0(updated_data()[1,1], ", year ", input$dates)
  #   }
  # 
  # })
  
  output$msg_pop <- renderTable({
    # Check if input$unit has valid value and toggle_map is set to TRUE
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
      
      # Get column names containing "_"
      ttt <- grep("_", colnames(updated_data()[, -1]), value = TRUE)
      
      Data <- updated_data() %>% filter(YEAR == as.numeric(input$dates))
      #Data <- sf::st_drop_geometry(Data)

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
  
  
  output$text_output_total <- renderUI({
    req(input$level == "County", input$toggle_map == TRUE, filtered_counties())
    if (input$update_map)  {
      h4("The total values of parameters in all the selected counties:")
    }
  })
  
  output$text_output_average <- renderUI({
    req(input$level == "County", input$toggle_map == TRUE, filtered_counties())
    if (input$update_map)  {
      h4("Unweighted average values of parameters in the selected counties:")
    }
  })

  output$var2 <- renderUI({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE){
      
      a = paste0(input$stat,"_", input$unit)
      Data = updated_data()
      ttt <- grep("_", colnames(Data[, -1]), value = TRUE)
      ttt = ttt[-which(ttt == a)]
      
      selectInput(inputId = "var2", label = "Choose another variable",
                  choices = c("", ttt),  # Dynamically retrieve choices
                  selected = "")
    }
    
  })
  
  
  ###########################################################
  # county_geometry_preprocessed <- reactive({
  #   req(input$level == "County")
  #   county_geo <- county_geometry() %>%
  #     mutate(geometry = st_make_valid(geometry)) %>% # Ensure valid geometries
  #     st_set_crs(4326) %>%
  #     st_transform(crs = '+proj=longlat +datum=WGS84')
  #   county_geo
  # })
  # 
  # filtered_counties <- reactive({
  #   req(updated_data(), input$distance, GRIDrv())
  #   county_geo <- data_new4() %>%
  #     left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state")
  # 
  #   county_geo <- st_as_sf(county_geo) %>%
  #     sf::st_set_crs(4326) %>%
  #     sf::st_transform('+proj=longlat +datum=WGS84')
  #   
  #   # Get geometry of the selected county
  #   selected_geom <- county_geometry_preprocessed() %>%
  #     filter(county_state == GRIDrv()) %>%
  #     st_geometry()
  # 
  #   # Filter counties within distance
  #   valid_counties <- county_geometry_preprocessed()
  #   valid_counties[st_distance(valid_counties, selected_geom) <= units::set_units(input$distance, "miles"), ]
  # })
  
  
  ############################################################
  # Calculate counties within the selected distance
  # filtered_counties <- reactive({
  #   req(updated_data(), input$distance)
  #   county_geo <- data_new4() %>%
  #     left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state")
  #   
  #   county_geo <- st_as_sf(county_geo) %>%
  #     sf::st_set_crs(4326) %>%
  #     sf::st_transform('+proj=longlat +datum=WGS84')
  #   
  #   selected_county <- unique(county_geo$county_state)
  #   
  #   # Remove empty geometries
  #   valid_counties <- county_geo[!st_is_empty(county_geo$geometry), ]
  #   
  #   
  #   # Get the selected county geometry
  #   selected_geom <- valid_counties %>%
  #     filter(county_state %in% selected_county) %>%
  #     distinct(county_state, .keep_all = TRUE) %>%
  #     st_geometry() %>%
  #     .[[1]]
  #   
  #   selected_geom <- st_sf(geometry = st_sfc(selected_geom), crs = st_crs(valid_counties))
  #   #selected_geom <- st_transform(selected_geom, crs = "+init=epsg:4326")
  #   # Calculate distances
  #   distances <- st_distance(valid_counties, selected_geom)
  #   
  #   
  #   distance_threshold <- units::as_units(input$distance, "miles")
  #   
  #   D <- valid_counties[distances <= distance_threshold, ]
  #   
  #   print(dim(D))
  #   return(D)
  # })
  ###################################
  
  # Calculate counties within the selected distance
  filtered_counties <- reactive({
    req(updated_data(), input$distance)
    county_geo <- data_new4() %>%
      left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state")

    county_geo <- st_as_sf(county_geo) %>%
      sf::st_set_crs(4326) %>%
      sf::st_transform('+proj=longlat +datum=WGS84')

    #selected_county <- unique(county_geo$county_state)

    # Remove empty geometries
    valid_counties <- county_geo[!st_is_empty(county_geo$geometry), ]
    
    
    selected_geom <- valid_counties %>%
      filter(county_state == GRIDrv()) %>%
      st_geometry()

    # Filter counties within distance
    valid_counties <- county_geo
    valid_counties[st_distance(valid_counties, selected_geom) <= units::set_units(input$distance, "miles"), ]
      
    
  })
  # 
  ######################################################
  
  # pal_data_cache <- reactiveVal(NULL)
  # 
  # observe({
  #   req(input$unit, input$toggle_map == TRUE)
  #   
  #   if (is.null(pal_data_cache())) {
  #     pal_data_cache(
  #       colorNumeric(
  #         palette = color_pal,
  #         domain = reactive_stat() + 1
  #       )
  #     )
  #   }
  # })
  
  ##########################################################
  observe({
    req(input$level == "County", input$toggle_map == TRUE)
    
    if (input$update_map) {
      req(filtered_counties())
      
      # # Validate filtered_counties() output
      # validate(need("sf" %in% class(filtered_counties()), "filtered_counties() must return an sf object"))
      # validate(need(!is.null(filtered_counties()$geometry), "Geometry column is missing in filtered_counties()"))
      
      leafletProxy("map_pop") %>%
        clearGroup("county") %>%  # Clear only the county group
        addPolygons(
          data = filtered_counties(),
          group = "county",
          layerId = ~county_state,
          fillColor = "green",
          color = "white",
          weight = 1,
          smoothFactor = 0,
          fillOpacity = 0.7,
          popup = ~paste("<strong>County:</strong>", county_state,
                         "<br><strong>Year:</strong>", YEAR)
        )
    } else {
      req(data_new4())
      
      # Merge geometry with data_new4()
      county_geo <- data_new4() %>%
        left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state") %>%
        st_as_sf() %>%
        sf::st_set_crs(4326) %>%
        sf::st_transform('+proj=longlat +datum=WGS84')
      
      # # Validate the merged sf object
      # validate(need("sf" %in% class(county_geo), "Merged county_geo must be an sf object"))
      # validate(need(!is.null(county_geo$geometry), "Geometry column is missing in county_geo"))
      
      leafletProxy("map_pop") %>%
        clearGroup("county") %>%  # Clear only the county group
        addPolygons(
          data = county_geo,
          group = "county",
          layerId = ~county_state,
          fillColor = ~pal_data()(reactive_stat()),
          color = "white",
          weight = 1,
          smoothFactor = 0,
          fillOpacity = 0.7,
          popup = ~paste("<strong>County:</strong>", county_state,
                         "<br><strong>Year:</strong>", YEAR)
        )
    }
  })
  
  
  
  ###########################################################
  # observe({
  #   req(input$level == "County", input$toggle_map == TRUE)
  # 
  #   if (input$update_map) {
  #     # Update map to show only filtered counties
  #     req(filtered_counties())
  #     validate(
  #       need(nrow(filtered_counties()) > 0, "No counties within the selected distance.")
  #     )
  # 
  #     leafletProxy("map_pop") %>%
  #       clearGroup("county") %>%
  #       addPolygons(
  #         data = filtered_counties(),
  #         group = "county",
  #         layerId = ~county_state,
  #         color = "green",
  #         weight = 1,
  #         smoothFactor = 0,
  #         fillOpacity = 0.7,
  #         popup = ~paste("<strong>County:</strong>", county_state,
  #                        "<br><strong>Year:</strong>", YEAR)
  #       )
  #   } else {
  #     county_geo <- data_new4() %>%
  #       left_join(county_geometry()[, c("county_state", "geometry")], by = "county_state")
  # 
  #     county_geo <- st_as_sf(county_geo) %>%
  #       sf::st_set_crs(4326) %>%
  #       sf::st_transform('+proj=longlat +datum=WGS84')
  # 
  #     # Restore the original map with all counties
  #     leafletProxy("map_pop") %>%
  #       clearGroup("county") %>%
  #       addPolygons(
  #         data = county_geo,
  #         group = "county",
  #         layerId = ~county_state,
  #         fillColor = ~pal_data()(reactive_stat()),
  #         color = "white",
  #         weight = 1,
  #         smoothFactor = 0,
  #         fillOpacity = 0.7,
  #         popup = ~paste("<strong>County:</strong>", county_state,
  #                        "<br><strong>Year:</strong>", YEAR)
  #       )
  #   }
  # })
  ######################################################
  
  output$msg_pop_total <- renderTable({
    req(input$level == "County", input$toggle_map == TRUE, filtered_counties())
    if (input$update_map) {
      filtered_counties()
      # Get column names containing "_"
      ttt <- grep("_", colnames(filtered_counties()[, -1]), value = TRUE)
      
      Data <- filtered_counties() %>% filter(YEAR == as.numeric(input$dates))
      Data <- sf::st_drop_geometry(Data)
      
      # D <- as.data.frame(lapply(Data, function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)))
      # names(D) <- names(Data)
      tt <- as.data.frame(colSums(Data[, ttt], na.rm = TRUE))
      names(tt) <- "Values"
      
      # Replace 0 values with "(D)"
      tt$Values[tt$Values == 0] <- "(D)"
      
      # Format values: Keep "(D)" as is, and apply formatting to numeric values
      tt$Values <- ifelse(tt$Values == "(D)", "(D)", format(as.numeric(tt$Values), big.mark = ",", scientific = FALSE))
      
      
      df_tt <- data.frame(
        STATISTICCAT_UNIT = rownames(tt),  
        Value = tt$Values               
      )
      
      df_tt[df_tt$STATISTICCAT_UNIT != "YIELD_BU / ACRE",]
      
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  output$msg_pop_average <- renderTable({
    req(input$level == "County", input$toggle_map == TRUE, filtered_counties())
    if (input$update_map) {
      filtered_counties()
      # Get column names containing "_"
      ttt <- grep("_", colnames(filtered_counties()[, -1]), value = TRUE)
      
      Data <- filtered_counties() %>% filter(YEAR == as.numeric(input$dates))
      Data <- sf::st_drop_geometry(Data)
      
      # D <- as.data.frame(lapply(Data, function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)))
      # names(D) <- names(Data)
      
      tt <- as.data.frame(colMeans(Data[, ttt], na.rm = TRUE))
      names(tt) <- "Values"
      
      # Replace 0 values with "(D)"
      tt$Values[tt$Values == 0] <- "(D)"
      
      # Format values: Keep "(D)" as is, and apply formatting to numeric values
      tt$Values <- ifelse(tt$Values == "(D)", "(D)", format(as.numeric(tt$Values), big.mark = ",", scientific = FALSE))
      
      
      df_tt <- data.frame(
        STATISTICCAT_UNIT = rownames(tt),  
        Value = tt$Values               
      )
      
      df_tt
      
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  tab_data <- reactive({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      if(input$level == "State"){
        a = paste0(input$stat,"_", input$unit)
        b = input$var2
        Data = updated_data()#sf::st_drop_geometry(updated_data())
        df = Data[,c('YEAR', a,b)]
        
        #colnames(df) <- gsub("_", "<br>", colnames(df))
        
        df
      } else if (input$level == "County" & input$update_map) {
        req(filtered_counties())
        
        a = paste0(input$stat,"_", input$unit)
        b = input$var2
        Data = sf::st_drop_geometry(filtered_counties())
        df = Data[,c('YEAR', a,b)] 
        names(df) <- c('Year', "a", "b")
        
        df <- df %>% 
          group_by(Year) %>% 
          summarise(a1 = sum(a, na.rm = T),
                    b1 = sum(b, na.rm = T))
        names(df) <- c('Year', a, b)
        
        na.omit(df)
        
      } else if (input$level == "County" & input$update_map == FALSE) {
        
        a = paste0(input$stat,"_", input$unit)
        b = input$var2
        Data = updated_data()#sf::st_drop_geometry(updated_data())
        df = Data[,c('YEAR', a,b)]
        
        #colnames(df) <- gsub("_", "<br>", colnames(df))
        
        df
      }
      
    }
  })
  
  output$tab1 <- renderTable({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){

      df = tab_data()
      colnames(df) <- gsub("_", "<br>", colnames(df))
      df
    }

  }, striped = TRUE, hover = TRUE, bordered = TRUE, sanitize.text.function = identity,
  table.attr = 'style="font-size:9px; width:50%;"')
  
  
  output$boxplot_explanation <- renderUI({
    if (length(strsplit(as.character(req(input$unit)), "")) != 0 && 
        input$toggle_map == TRUE && 
        length(strsplit(as.character(req(input$var2)), "")) != 0) {
      
      tagList(
        h4("Explanation of the following box Plot"),
        p("In this plot, the values of the variables are scaled using a 
        statistical method called z-scores. This means that instead of 
        showing the raw values (e.g., yield, production), the values are 
        adjusted to show how far they are from the average (mean) 
        value for each variable. The y-axis represents standard deviations:"),
        tags$ul(
          tags$li("A value of 0 means the data point is equal to the average."),
          tags$li("Positive values (e.g., +2) mean the data point is above the average."),
          tags$li("Negative values (e.g., -2) mean the data point is below the average.")
        ),
        p("This scaling allows us to compare variables with different units 
        (e.g., bushels, acres) on the same scale, making it easier to 
        identify patterns and trends.")
      )
    }
  })
  
  
  output$boxplot <- renderPlot({
    if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
       length(strsplit(as.character(req(input$var2)), "")) != 0){
      
      if(input$level == "State"){
        selected_state <- unique(updated_data()$STATE_NAME)
        
        wheat_long <- data_new4() %>%#sf::st_drop_geometry(data_new4()) %>%
          filter(STATE_NAME == selected_state) %>%
          #select(-geometry) %>% 
          pivot_longer(cols = -c(STATE_NAME, YEAR), 
                       names_to = "Category", values_to = "Value") %>%
          group_by(Category) %>%
          mutate(Value = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) %>%
          ungroup()
        
      }else if(input$level == "County" & input$update_map){
        selected_state <- unique(filtered_counties()$county_state)
        
        wheat_long <- data_new4() %>%#sf::st_drop_geometry(data_new4()) %>%
          filter(county_state == selected_state) %>%
          #select(-geometry) %>% 
          pivot_longer(cols = -c(county_state, YEAR), 
                       names_to = "Category", values_to = "Value") %>%
          group_by(Category) %>%
          mutate(Value = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) %>%
          ungroup()
      }else if(input$level == "County" & input$update_map == FALSE){
        selected_state <- unique(updated_data()$county_state)
        
        wheat_long <- data_new4() %>%#sf::st_drop_geometry(data_new4()) %>%
          filter(county_state == selected_state) %>%
          #select(-geometry) %>% 
          pivot_longer(cols = -c(county_state, YEAR), 
                       names_to = "Category", values_to = "Value") %>%
          group_by(Category) %>%
          mutate(Value = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) %>%
          ungroup()
      }
      
      
      
      
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
        geom_bar(aes(x = as.factor(Year), y = a), stat = "identity", position = "dodge", color = "red") +
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
        geom_bar(aes(x = as.factor(Year), y = b), stat = "identity", position = "dodge", color = "blue") +
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
  
  data_condition <- reactive({
    switch(input$crop,
           "Corn" =  corn_survey_week_final,
           "Soybeans" = soybeans_survey_week_final,
           "Potatoes" = potatoes_survey_week_final,
           "Wheat" = wheat_survey_week_final
    )
  })
  
  # 
  # data_condition <- reactive({
  #   
  #   c_survey <- plots_data()
  #   
  #   c_survey[c_survey == "(D)"] <- 0
  #   c_survey$VALUE <- as.numeric(gsub(",","",c_survey$VALUE))
  # 
  #   
  #   inx <- grep("WEEK", c_survey$REFERENCE_PERIOD_DESC, value = FALSE)
  #   
  #   c_survey_week <- c_survey[inx,] %>% 
  #     arrange(WEEK_ENDING) %>% 
  #     filter(YEAR>=2000,
  #            FREQ_DESC == "WEEKLY",
  #            #UTIL_PRACTICE_DESC == 'GRAIN',
  #            STATISTICCAT_DESC %in% c("PROGRESS",
  #                                     "PROGRESS, PREVIOUS YEAR",
  #                                     "PROGRESS, 5 YEAR AVG",
  #                                     "CONDITION",
  #                                     "CONDITION, PREVIOUS YEAR",
  #                                     "CONDITION, 5 YEAR AVG",
  #                                     "MOISTURE",
  #                                     "MOISTURE, PREVIOUS YEAR",
  #                                     "MOISTURE, 5 YEAR AVG",
  #                                     "HEIGHT, AVG",
  #                                     "HEIGHT, AVG, PREVIOUS YEAR",
  #                                     "HEIGHT, AVG, 5 YEAR AVG")) %>% 
  #     dplyr::select(WEEK_ENDING,
  #                   REFERENCE_PERIOD_DESC,
  #                   STATE_NAME,
  #                   STATISTICCAT_DESC, 
  #                   UNIT_DESC, 
  #                   VALUE) %>% 
  #     group_by(WEEK_ENDING,
  #              REFERENCE_PERIOD_DESC,
  #              STATE_NAME,
  #              STATISTICCAT_DESC, 
  #              UNIT_DESC) %>% 
  #     summarise(sum_value = sum(VALUE))
  #   
  #   
  #   wide_data_week <- c_survey_week %>% 
  #     pivot_wider(
  #       names_from = c(STATISTICCAT_DESC, UNIT_DESC), 
  #       values_from = sum_value
  #     ) %>% 
  #     arrange(WEEK_ENDING)
  #   
  #   c_survey_week_final <- wide_data_week %>% 
  #     mutate(good_excellent = `CONDITION_PCT EXCELLENT` + `CONDITION_PCT GOOD`,
  #            Year = year(WEEK_ENDING),
  #            Week_Num = as.numeric(gsub("WEEK #", "", REFERENCE_PERIOD_DESC))) %>%  # Extract numeric week number)
  #     arrange(STATE_NAME, Week_Num)
  #   
  #   return(c_survey_week_final)
  #   
  # })
  
  output$main_states <- renderText({
    req(data_condition())
    states <- unique(na.omit(data_condition()$STATE_NAME))
    paste("The following plots are available only for the following states:\n", 
          paste(states, collapse = ", "))
  })
  
  
  output$excellent_good <- renderPlot({
    if(input$toggle_map == TRUE & input$level == "State"){
      name = unique(updated_data()$STATE_NAME)
      year = as.numeric(input$dates)
      
      df1 <- data_condition() %>% 
        ungroup() %>% 
        dplyr::select(REFERENCE_PERIOD_DESC, STATE_NAME, Year, Week_Num, good_excellent) %>% 
        filter(STATE_NAME == name, 
               Year %in% c((year-5):year)) %>% 
        arrange(STATE_NAME, Week_Num)
      
     
      if(input$crop == "Wheat"){
        
        df1 <- df1 %>%
          mutate(Year = ifelse(df1$Week_Num >= 35, Year+1, Year)) %>% 
          mutate(adjusted_week = ifelse(Week_Num >= 35, Week_Num - 34, Week_Num + 18))
        df1$Week_Num = df1$adjusted_week
        df1 <- df1 %>% 
          filter(Year != max(Year, na.rm = TRUE) & Year != min(Year, na.rm = TRUE))
      }
      df1$Year <- as.factor(df1$Year) 
      
      
      # Get the last point with non-NA values for each year to add the label at the end of the line
      df_last <- df1 %>%
        group_by(Year) %>%
        filter(!is.na(good_excellent)) %>%  # Filter out NA values
        filter(Week_Num == max(Week_Num))   # Select the last non-NA week for each year
      
      # plot
      ggplot(data = df1, 
             aes(x = Week_Num, y = good_excellent, color = factor(Year), group = Year)) +
        geom_line() +  # Add lines for each year
        geom_point() +  # Add points for better visualization, but it can be removed
        # Add year label at the end of each line for every year
        geom_text(data = df_last, aes(label = Year), 
                  hjust = -0.3,  # Adjust the horizontal position
                  vjust = 0.5,   # Adjust the vertical position
                  size = 4) +    # Size of the text
        labs(
          title = "Good Excellent Values of the crop by Week for selected state",
          x = "Week Number (Wheat Adjusted to Begin Week 35)",
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
               Year %in% c(year, year - 1)) %>% 
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
      
      if(input$crop == "Wheat"){
        
        df2 <- df2 %>%
          mutate(Year = ifelse(df2$Week_Num >= 35, Year+1, Year)) %>% 
          mutate(adjusted_week = ifelse(Week_Num >= 35, Week_Num - 34, Week_Num + 18))
        df2$Week_Num = df2$adjusted_week
        df2 <- df2 %>% 
          filter(Year != max(Year, na.rm = TRUE) & Year != min(Year, na.rm = TRUE))
      }
      df2 <- df2 %>% 
        filter(Year == max(Year))
      
      
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
          x = "Week Number (Wheat Adjusted to Begin Week 35)",
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
               Year %in% c(year, year-1)) %>% 
        arrange(STATE_NAME, Week_Num)
      
      if(input$crop == "Wheat"){
        
        df3 <- df3 %>%
          mutate(Year = ifelse(df3$Week_Num >= 35, Year+1, Year)) %>% 
          mutate(adjusted_week = ifelse(Week_Num >= 35, Week_Num - 34, Week_Num + 18))
        df3$Week_Num = df3$adjusted_week
        df3 <- df3 %>% 
          filter(Year != max(Year, na.rm = TRUE) & Year != min(Year, na.rm = TRUE))
      }
      
      df3 <- df3 %>% 
        filter(Year == max(Year))
      
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
          x = "Week Number (Wheat Adjusted to Begin Week 35)",
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
  
  
  output$state1 <- renderUI({
    if(length(strsplit(as.character(req(input$crop2)), "")) != 0 &
       input$level2 == "State"){
      selectInput("state1", 
                  "Select State 1:", 
                  choices = unique(data_new2()$STATE_NAME),# unique_products,
                  multiple = F,
                  selected = NULL)
    } 
  })
  
  output$state2 <- renderUI({
    if(length(strsplit(as.character(req(input$crop2)), "")) != 0 &
       input$level2 == "State"){
      selectInput("state2", 
                  "Select State 2:", 
                  choices = unique(data_new2()$STATE_NAME),# unique_products,
                  multiple = F,
                  selected = NULL)
    } 
  })
  
  output$parameters <- renderUI({
    if(input$level2 == "State"){
      print(names(data_new2())[3:(ncol(data_new2()))])
      selectInput("parameters", 
                  "Select the Parameter:", 
                  choices = names(data_new2())[3:(ncol(data_new2()))],# unique_products,
                  multiple = F,
                  selected = NULL)
    } 
  })

  
  output$comparison <- renderPlot({
    if(length(strsplit(as.character(req(input$state1)), "")) != 0 &
       length(strsplit(as.character(req(input$state2)), "")) != 0 &
       length(strsplit(as.character(req(input$parameters)), "")) != 0){
      
      k <-  which(names(data_new2()) == input$parameters)
      
      
      D <- data_new2() %>% 
        filter(STATE_NAME %in% c(input$state1, input$state2))
      
      #D <- sf::st_drop_geometry(D)
      
      
      DD <- D[, c(1,2,k)] 
      names(DD) <- c("STATE_NAME", "Year", "Var")
      
      
      ggplot(data = DD, aes(x = Year, y = Var, color = STATE_NAME, group = STATE_NAME)) +
        geom_line(size = 1.2) +            # Add lines for each state
        geom_point(size = 3) +            # Add points for each year
        labs(
          title = "Comparison of selected parameter Over Years",
          x = "Year",
          y = "Parameter",
          color = "State"
        ) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +                 # Apply a minimal theme
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        scale_x_continuous(breaks = seq(min(DD$Year), max(DD$Year), by = 1))
        
      
    }
    
  })
  
  output$table_comparison <- renderTable({
    if(length(strsplit(as.character(req(input$state1)), "")) != 0 &
       length(strsplit(as.character(req(input$state2)), "")) != 0 &
       length(strsplit(as.character(req(input$parameters)), "")) != 0 &
       input$state1 != input$state2){
      
      k <-  which(names(data_new2()) == input$parameters)
      print(k)
      
      D <- data_new2() %>% 
        filter(STATE_NAME %in% c(input$state1, input$state2))
      
      #D <- sf::st_drop_geometry(D)
      
      print(names(D))
      
      DD <- D[, c(1,2,k)] 
      names(DD) <- c("STATE_NAME", "Year", "Var")
      
      DD$Var <- format(as.numeric(DD$Var), big.mark = ",", scientific = FALSE)
      
      
      reshaped_data <- DD %>%
        pivot_wider(
          names_from = STATE_NAME,
          values_from = Var,
          names_prefix = ""
        ) %>%
        rename(
          Ohio_var1 = unique(D$STATE_NAME)[1],
          Indiana_var1 = unique(D$STATE_NAME)[2]
        )
      
      names(reshaped_data) <- c("Year", "Values of Group 1", "Values of Group 2")
      
      reshaped_data
    }
    
  })
  
  # output$zscore_box <- renderText({
  #   if(length(strsplit(as.character(req(input$unit)), "")) != 0 & input$toggle_map == TRUE &
  #      length(strsplit(as.character(req(input$var2)), "")) != 0)
  #     paste("Z-standardization allows for comparison between data for different variables.
  #   Z-scores represent the number of standard deviations the data point
  #                                        is from the mean.")
  # })
  
  output$corn_intro <- renderTable({
    df <- corn_survey_final %>% 
      select(YEAR, `YIELD_BU / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_BU / ACRE`, na.rm = T),
                Average_YIELD = round(mean(`YIELD_BU / ACRE`, na.rm = T), digits = 0)) %>% 
      arrange(desc(YEAR))
    df
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  
  output$soybeans_intro <- renderTable({
    df <- soybeans_survey_final %>% 
      select(YEAR, `YIELD_BU / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_BU / ACRE`, na.rm = T),
                Average_YIELD = round(mean(`YIELD_BU / ACRE`, na.rm = T), digits = 0)) %>% 
      arrange(desc(YEAR))
    df
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  output$wheat_intro <- renderTable({
    df <- wheat_survey_final %>% 
      select(YEAR, `YIELD_BU / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_BU / ACRE`, na.rm = T),
                Average_YIELD = round(mean(`YIELD_BU / ACRE`, na.rm = T), digits = 0)) %>% 
      arrange(desc(YEAR))
    df
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  output$potatoes_intro <- renderTable({
    df <- potatoes_survey_final %>% 
      select(YEAR, `YIELD_CWT / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_CWT / ACRE`, na.rm = T)*1.667,
                Average_YIELD = round(mean(`YIELD_CWT / ACRE`, na.rm = T), digits = 0) * 1.667) %>% 
      arrange(desc(YEAR))
    df
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  
  output$potatoes_plot <- renderPlot({
    df <- potatoes_survey_final %>% 
      select(YEAR, `YIELD_CWT / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_CWT / ACRE`, na.rm = T)*1.667,
                Average_YIELD = round(mean(`YIELD_CWT / ACRE`, na.rm = T), digits = 0) * 1.667) %>% 
      arrange(desc(YEAR))
    
    data_long <- df %>%
      pivot_longer(cols = c(Total_YIELD, Average_YIELD), 
                   names_to = "Parameter", 
                   values_to = "Value")
    
    ggplot(data_long, aes(x = YEAR, y = Value, color = Parameter)) +
      geom_line(linewidth = 1) +       # Use linewidth instead of size for lines
      geom_point(size = 2) +          # Add points for both metrics
      labs(
        title = "Total and Average Yields Over Years",
        x = "Year",
        y = "Yield",
        color = "Parameters:"
      ) +
      scale_color_manual(
        values = c("Total_YIELD" = "blue", "Average_YIELD" = "red")  # Match exact Metric names
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "bottom",  # Place legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9)    # Adjust legend text size
      ) +
      scale_x_continuous(breaks = seq(min(data_long$YEAR), max(data_long$YEAR), by = 1))  # Show every year on x-axis
  })
  
  output$corn_plot <- renderPlot({
    df <- corn_survey_final %>% 
      select(YEAR, `YIELD_BU / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_BU / ACRE`, na.rm = T),
                Average_YIELD = round(mean(`YIELD_BU / ACRE`, na.rm = T), digits = 0)) %>% 
      arrange(desc(YEAR))
    
    data_long <- df %>%
      pivot_longer(cols = c(Total_YIELD, Average_YIELD), 
                   names_to = "Parameter", 
                   values_to = "Value")
    
    ggplot(data_long, aes(x = YEAR, y = Value, color = Parameter)) +
      geom_line(linewidth = 1) +       # Use linewidth instead of size for lines
      geom_point(size = 2) +          # Add points for both metrics
      labs(
        title = "Total and Average Yields Over Years",
        x = "Year",
        y = "Yield",
        color = "Parameters:"
      ) +
      scale_color_manual(
        values = c("Total_YIELD" = "blue", "Average_YIELD" = "red")  # Match exact Metric names
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "bottom",  # Place legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9)    # Adjust legend text size
      ) +
      scale_x_continuous(breaks = seq(min(data_long$YEAR), max(data_long$YEAR), by = 1))  # Show every year on x-axis
  })
  
  output$soybeans_plot <- renderPlot({
    df <- soybeans_survey_final %>% 
      select(YEAR, `YIELD_BU / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_BU / ACRE`, na.rm = T),
                Average_YIELD = round(mean(`YIELD_BU / ACRE`, na.rm = T), digits = 0)) %>% 
      arrange(desc(YEAR))
    
    data_long <- df %>%
      pivot_longer(cols = c(Total_YIELD, Average_YIELD), 
                   names_to = "Parameter", 
                   values_to = "Value")
    
    ggplot(data_long, aes(x = YEAR, y = Value, color = Parameter)) +
      geom_line(linewidth = 1) +       # Use linewidth instead of size for lines
      geom_point(size = 2) +          # Add points for both metrics
      labs(
        title = "Total and Average Yields Over Years",
        x = "Year",
        y = "Yield",
        color = "Parameters:"
      ) +
      scale_color_manual(
        values = c("Total_YIELD" = "blue", "Average_YIELD" = "red")  # Match exact Metric names
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "bottom",  # Place legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9)    # Adjust legend text size
      ) +
      scale_x_continuous(breaks = seq(min(data_long$YEAR), max(data_long$YEAR), by = 1))  # Show every year on x-axis
  })
  
  output$wheat_plot <- renderPlot({
    df <- wheat_survey_final %>% 
      select(YEAR, `YIELD_BU / ACRE`) %>% 
      group_by(YEAR) %>% 
      summarise(Total_YIELD = sum(`YIELD_BU / ACRE`, na.rm = T),
                Average_YIELD = round(mean(`YIELD_BU / ACRE`, na.rm = T), digits = 0)) %>% 
      arrange(desc(YEAR))
    
    data_long <- df %>%
      pivot_longer(cols = c(Total_YIELD, Average_YIELD), 
                   names_to = "Parameter", 
                   values_to = "Value")
    
    ggplot(data_long, aes(x = YEAR, y = Value, color = Parameter)) +
      geom_line(linewidth = 1) +       # Use linewidth instead of size for lines
      geom_point(size = 2) +          # Add points for both metrics
      labs(
        title = "Total and Average Yields Over Years",
        x = "Year",
        y = "Yield",
        color = "Parameters:"
      ) +
      scale_color_manual(
        values = c("Total_YIELD" = "blue", "Average_YIELD" = "red")  # Match exact Metric names
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "bottom",  # Place legend at the bottom
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 9)    # Adjust legend text size
      ) +
      scale_x_continuous(breaks = seq(min(data_long$YEAR), max(data_long$YEAR), by = 1))  # Show every year on x-axis
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


