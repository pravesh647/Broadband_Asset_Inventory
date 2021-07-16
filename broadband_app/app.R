#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# To DO-
# Outline of the 16 counties in the map
# Add base layer of FCC and Microsoft connectivity
# Add green and red color functionality in library map
# 



library(shiny)
library(shinydashboard)
library(gsheet)
library(leaflet)
library(stringr)
library(dplyr)

# setwd("~/Sites/Broadband_Asset_Inventory/broadband_app")
# libdata <- readxl::read_xlsx("library_database_v2.xlsx")
# location_data <- readxl::read_xlsx("location_database.xlsx")


# Calling different RScripts
source("forword_geocoding.R")





# Define UI for application that draws a histogram


# Header of the App---------------------------------
header <- dashboardHeader(
    # Add image and link with title
    title = tags$a(href='https://www.thriveregionalpartnership.org/projects/regional-broadband-alliance',
                   tags$img(src="thrive_logo.png", height = '50', width = '50'),  target="_blank"),
    titleWidth = 100,
    
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Author",
            message = "This is an web app"
        )
    )
)











# SideBar of the App---------------------------------
sidebar <- dashboardSidebar(
    width = 200,
    sidebarMenu(
        menuItem(
            "Maps", 
            tabName = "maps", 
            icon = icon("globe"),
            menuSubItem("Library", tabName = "librarymap", icon = icon("map")),
            menuSubItem("ISP", tabName = "ispmap", icon = icon("map"))
        ),
        menuItem(
            "MapBaseLayer", 
            tabName = "baselayer", 
            icon = icon("globe"),
            menuSubItem("FCC Connectivity", tabName = "fcc_base", icon = icon("map")),
            menuSubItem("Microsoft Connectivity", tabName = "microsoft_base", icon = icon("map"))
        ),
        menuItem(
            "Charts", 
            tabName = "charts", 
            icon = icon("bar-chart"),
            menuSubItem("Library", tabName = "librarychart", icon = icon("area-chart")),
            menuSubItem("ISP", tabName = "ispchart", icon = icon("area-chart"))
        )
    )
)


# Body of the App---------------------------------
body <- dashboardBody(
    
    tabItems(
        tabItem(
            tabName = "librarymap",
        
            fluidRow(
            # box(
            #     title = "Libraries",
            #     collapsible = TRUE,
                width = "100%",
                selectInput(
                    inputId = "library_broadband_selection",
                    label = "Broadband Resources Availability",
                    choices = c("Internet Access" = "internet_acc",
                                "Wifi Access" = "wifi_acc",
                                "Computer Classes" = "computer_classes",
                                "Laptop Lending" = "lend_laptop",
                                "EReader Lending" = "lend_ereader",
                                "Wifi Printing" = "wifi_print"),
                    
                ),
                leafletOutput("librarymapPlot", height = "84vh"),
                
            # )
            )
        ),
        tabItem(
            tabName = "ispmap",
            # Map in Dashboard
            leafletOutput("")
        ),
        tabItem(
            tabName = "charts",
            h2("Second tab content")
        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin = 'black')



# ---------------------------------------------



# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$librarymapPlot <- renderLeaflet({
        
        # Creating Icons for use in the library map
        library_marker_icon <- awesomeIcons(icon = "book",
                                            library = "fa",
                                            markerColor = ifelse(libdata[,which(names(libdata)==input$library_broadband_selection)] == 'Y', 'green', 'red')
        )
        
        # Making the Map
        libdata %>%
            mutate(address = paste0(street, ', ', city, ' ', state)) %>% 
            left_join(librarylocation_data, by = "address") %>% 
            filter(asset_type == "library") %>% 
            leaflet() %>% 
                # Base Groups
                addTiles(group = "OSM (default)") %>%
                addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
                addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
                
                # Overlay Groups
                # Library Markers
                addAwesomeMarkers( 
                    group = "Library",
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
                    
                    # clusterOptions = markerClusterOptions(),
                ) %>% 
                # addAwesomeMarkers(
                #     group = "ISP",
                #     icon = 
                # )
            
                
                # Layer Groups
                addLayersControl(
                    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                    
                    overlayGroups = c("Assets","Library"),
                    options = layersControlOptions(collapsed = TRUE)
                )
                
                   
           
        # }
    })
}
?addMarkers
# Run the application
shinyApp(ui = ui, server = server)
