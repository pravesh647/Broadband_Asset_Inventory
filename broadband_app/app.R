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
# 
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

header <- dashboardHeader(
    title = "Chattanooga Tri-state Area Broadband Assets",
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Author",
            message = "This is an web app"
        )
    )
)
    
sidebar <- dashboardSidebar(
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
            menuSubItem("Library", tabName = "librarymap", icon = icon("map")),
            menuSubItem("ISP", tabName = "ispmap", icon = icon("map"))
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
    
    
body <- dashboardBody(
    
    tabItems(
        tabItem(
            tabName = "librarymap",
            box(
                title = "Libraries",
                collapsible = TRUE,
                width = "100%",
                tags$style(type = "text/css", "#librarymapPlot {height: calc(100vh - 165px) !important;}"),
                leafletOutput("librarymapPlot")
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
    

ui <- dashboardPage(header, sidebar, body)

?left_join

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$librarymapPlot <- renderLeaflet({
        # Making the Map
        libdata %>%
            mutate(address = paste0(street, ', ', city, ' ', state)) %>% 
            left_join(location_data, by = "address") %>% 
            filter(asset_type == "library") %>% 
        # if ( tolower(input$asset) == 'library'){
            # selected_data %>% 
            leaflet(height = 1000) %>% 
                # addTiles() %>% 
                addProviderTiles(providers$Stamen.Toner) %>% 
                addMarkers( popup = paste0("Potential Partner: ", libdata$name,
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
                                           "EReader Lending: ", libdata$lead_ereader,
                                           "<br/>",
                                           "Wifi Printing: ", libdata$wifi_print
                                           ),
                            clusterOptions = markerClusterOptions()
                            )
            
        # }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
