


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
    
    layer_watcher <- reactiveVal(value = list())
    # Updates the layer watcher
    observeEvent(input$layer_selection, {
        r <- layer_watcher()
        most_recent <- input$layer_selection
        if(is.null(most_recent)){
            most_recent <- 'nothing!'
        }
        r[[length(r) + 1]] <- most_recent
        layer_watcher(r) # overwrite layer_watcher
        message('Here is your history of layer selections')
        print(layer_watcher())
    },
    ignoreNULL = FALSE)
    
    observeEvent(input$layer_selection,{
        
        selected_layer <- input$layer_selection
        message('---pravesh: current selected layer is: ', selected_layer)
        r <- layer_watcher()
        if(length(r) > 1){
            # get the penultimate layer selection
            old_layer <- r[[length(r)-1]]
        } else {
            old_layer <- c()
        }
        message('---pravesh: previously selected layer is: ', old_layer)
        # see if something was already in
        fcc_already <- "Broadband Availability (FCC)" %in% selected_layer &
            "Broadband Availability (FCC)" %in% old_layer
        if(fcc_already){
            message('---pravesh: do NOT add the polygons for fcc again, it is already drawn!')
        }
        
        # save(selected_layer, file = '/tmp/prav.RData')
        
        if(is.null(selected_layer)){
            leafletProxy("map", data = county_shp) %>%
                clearShapes() %>% 
                removeControl(layerId = 'fcc_legend_control')
            
            leafletProxy("map", data = zipcode_shp) %>%
                clearShapes() %>% 
                removeControl(layerId = 'microsoft_legend_control')
            
        }else{
            # Broadband Availability (FCC) Layer
            # str(county_shp@data)
            if("Broadband Availability (FCC)" %in% selected_layer & !fcc_already){
                county_shp@data$fcc_broadband <- as.numeric(county_shp@data$fcc_broadband)
                fcc_map_palette <- colorNumeric(palette = brewer.pal(5, "Reds"),
                                            domain=county_shp@data$fcc_broadband,
                                            na.color="#CECECE")
                
                leafletProxy("map", data = county_shp) %>% 
                    addPolygons(
                        group = "fcc_layer",
                        fillColor = ~fcc_map_palette(county_shp@data$fcc_broadband),
                        dashArray = '5,5',
                        weight = 3,
                        color = "black",
                        label = paste0(county_shp@data$county_name , " ", county_shp@data$st,
                                                                  "<br/>",
                                                                  # county_shp@data$GEOID10,
                                                                  "<br/>",
                                                                  'Broadband Usage: ', county_shp@data$fcc_broadband*100, '%')

                    )
                
            }
            
            # if("Broadband Availability (FCC)" %in% selected_layer){
            #     
            #     # map_palette <- colorNumeric(palette = brewer.pal(9, "Reds"),
            #     #                             domain=county_shp@data$fcc_broadband,
            #     #                             na.color="#CECECE")
            #     
            #     leafletProxy("map", data = county_shp) %>%
            #         addPolygons(
            #             group = "fcc_layer",
            #             # fillColor = ~map_palette(fcc_broadband),
            #             # fillOpacity = 0.7,
            #             dashArray = '5,5',
            #             # dashOffset = '10',
            #             color = "black",
            #             weight = 4,
            #             label = paste0(county_shp@data$county_name , " ", county_shp@data$st,
            #                            "<br/>",
            #                            # county_shp@data$GEOID10,
            #                            "<br/>",
            #                            'Broadband Usage: ', county_shp@data$fcc_broadband*100, '%')
            #             
            #             
            #             ) %>%
            #         addLegend(
            #             layerId = "fcc_legend_control",
            #             pal=map_palette,
            #             values=county_shp$fcc_broadband,
            #             opacity=0.5,
            #             position = "bottomleft",
            #             na.label = "NA" )
            #     
            #     
            # }else{
            #     leafletProxy("map", data = county_shp) %>%
            #         clearGroup(group = 'fcc_layer') %>%
            #         removeControl(layerId = 'fcc_legend_control')
            # }
            

           # a new if-else for a new layer goes here
            
            # Broadband Usage (Microsoft) Layer
            if("Broadband Usage (Microsoft)" %in% selected_layer){
                
                map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"),
                                            domain=zipcode_shp@data$broadband_usage,
                                            na.color="#CECECE")
                
                leafletProxy("map", data = zipcode_shp) %>%
                    addPolygons(
                        group = "microsoft_layer",
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
                                       'Broadband Usage: ', zipcode_shp@data$broadband_usage*100, '%')
                        
                        
                        ) %>%
                    addLegend( 
                        layerId = "microsoft_legend_control",
                        pal=map_palette,
                        values=zipcode_shp$broadband_usage,
                        opacity=0.5,
                        position = "bottomleft",
                        na.label = "NA" )
                
                
            }else{
                leafletProxy("map", data = zipcode_shp) %>%
                    clearGroup(group = 'microsoft_layer') %>%
                    removeControl(layerId = 'microsoft_legend_control')
            }
            # a new if-else for a new layer goes here
            
        }
        
        
        
    },
    ignoreNULL = FALSE)


}

# Run the application
shinyApp(ui = ui, server = server)
