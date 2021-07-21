#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
.

# To DO-
# Outline of the 16 counties in the map
# Add base layer of FCC and Microsoft connectivity
# Add green and red color functionality in library map
# 



library(shiny)
library(shinydashboard)
library(leaflet)
library(stringr)
library(dplyr)
library(sp)
library(readr)
library(RColorBrewer)

# setwd("~/Sites/Broadband_Asset_Inventory/broadband_app")
# libdata <- readxl::read_xlsx("library_database_v2.xlsx")
# location_data <- readxl::read_xlsx("location_database.xlsx")


# Calling different RScripts
source("forword_geocoding.R")
source('broadband_usage_layer.R')


# Define UI for application
ui <- bootstrapPage(
    title = "BAI Map",
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(
        # Include custom CSS
        includeCSS("styles.css")
    ),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(id = "input-panel",
                  fixed = TRUE,
                  draggable = TRUE,
                  top = 150,
                  left = "auto",
                  right = 20,
                  bottom = "auto",
                  width = 330,
                  height = "auto",
                  
                  selectInput(
                      inputId = "library_resources",
                      label = "Library Resources Availability",
                      choices = c("Internet Access" = "internet_acc",
                                  "Wifi Access" = "wifi_acc",
                                  "Computer Classes" = "computer_classes",
                                  "Laptop Lending" = "lend_laptop",
                                  "EReader Lending" = "lend_ereader",
                                  "Wifi Printing" = "wifi_print"),
                      
                  ),
                  checkboxGroupInput(
                      inputId = "asset_selection",
                      label = "Broadband Assets",
                      choices = c("Library",
                                  "ISP"
                      ),
                  ),
                  
                  checkboxGroupInput(
                      inputId = "layer_selection",
                      label = "Broadband Availability and Usage",
                      choices = c("Broadband Availability (FCC)",
                                  "Broadband Usage (Microsoft)"
                      ),
                  ),
                  
                  
                  
                  checkboxInput("legend", "Show legend", TRUE)
    )
    
)


# ---------------------------------------------


# Define server logic
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        # Making the Map
        
        leaflet() %>% 
            # Base Groups
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            setView(lng = -85.14954, lat = 35.09463, zoom = 9) %>%
            # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
            
            # Layer Groups
            addLayersControl(
                baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                options = layersControlOptions(collapsed = TRUE)
            )
        
    })
    
    # observeEvent(input$map_click, {
    #     message('Map center is:')
    #     print(input$map_center)
    # })
    
    observeEvent(c(
        input$asset_selection,
        input$library_resources
    ), {
        message("Asset selection was changed")
        lbs <- input$library_resources
        message('library broadband selection is ', lbs)
        # catpure the selected asset 
        selected_asset <- input$asset_selection
        message('selected asset is :')
        # print(selected_asset)
        if(is.null(selected_asset)){
            message('Removing some points')
            # remove some points
            leafletProxy("map", data = libdata) %>%
                clearMarkers()
        } else {
            if( 'Library' %in% selected_asset){
                # make some points
                # Creating Icons for use in the library map
                library_marker_icon <- awesomeIcons(icon = "book",
                                                    library = "fa",
                                                    markerColor = ifelse(libdata[,which(names(libdata)==input$library_resources)] == 'Y', 'green', 'red')
                )
                
                leafletProxy("map", data = libdata) %>%
                    # clearShapes() %>%
                    addAwesomeMarkers(
                        group = "library_markers",
                        icon = library_marker_icon,
                        popup = paste0("Potential Partner: ", libdata$name,
                                       "<br/>",
                                       "Director: ", libdata$director,
                                       "<br/>",
                                       "Email: ", libdata$email,
                                       "<br/>",
                                       "Phone Number: ", libdata$phoneNo,
                                       "<br/>",
                                       "Address: ", libdata$street, " ", libdata$city, " ", libdata$county, " ", libdata$state,
                                       "<br/>",
                                       "BroadBand Information: ",
                                       "<br/>",
                                       "Internet Access: ", libdata$internet_acc,
                                       "<br/>",
                                       "WiFi Access: ", libdata$wifi_acc,
                                       "<br/>",
                                       "Computer Classes: ", libdata$computer_classes,
                                       "<br/>",
                                       "Laptop Lending: ", libdata$lend_laptop,
                                       "<br/>",
                                       "EReader Lending: ", libdata$lend_ereader,
                                       "<br/>",
                                       "Wifi Printing: ", libdata$wifi_print
                        ),
                    )
            } else {
                # Clear just the library markers
                leafletProxy("map", data = libdata) %>%
                    clearGroup(group = "library_markers")
            }
            # a new if-else for a new asset goes here
        } 
    },
    ignoreNULL = FALSE)
    
?clearMarkers
    observeEvent(input$layer_selection,{
        
        selected_layer <- input$layer_selection
        
        if(is.null(selected_layer)){
            leafletProxy("map", data = zipcode_shp) %>%
                clearShapes() %>% 
                removeControl(layerId = 'legend_control')
            
        }else{
            
            if("Broadband Availability (FCC)" %in% selected_layer){
                
                map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"),
                                            domain=zipcode_shp@data$broadband_usage,
                                            na.color="#CECECE")
                
                leafletProxy("map", data = zipcode_shp) %>%
                    addPolygons(
                        group = "fcc_layer",
                        fillColor = ~map_palette(broadband_usage),
                        fillOpacity = 0.7,
                        dashArray = '5,5',
                        # dashOffset = '10',
                        color = "black",
                        weight = 2,
                        label = paste0(zipcode_shp@data$county_name , " County, ",
                                       "<br/>",
                                       zipcode_shp@data$GEOID10,
                                       "<br/>",
                                       'Broadband Usage: ', zipcode_shp@data$broadband_usage*100, '%'),
                        
                        
                        ) %>%
                    addLegend( 
                        layerId = "legend_control",
                        pal=map_palette,
                        values=zipcode_shp$broadband_usage,
                        opacity=0.5,
                        position = "bottomleft",
                        na.label = "NA" )
                
                
            }else{
                leafletProxy("map", data = zipcode_shp) %>%
                    clearGroup(group = 'fcc_layer') %>%
                    removeControl(layerId = 'legend_control')
            }
            # a new if-else for a new layer goes here
            
        }
        
        
        
    },
    ignoreNULL = FALSE)


    observe({



    })
    
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
