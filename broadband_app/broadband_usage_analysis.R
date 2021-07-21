

getwd()

library(readr)
library(dplyr)
library(stringr)
library(leaflet)
library(RColorBrewer)
library(tigris)
library(rgeos)
library(sp)
library(rgdal)

# Loading Data on Broadband Usage provided by FCC and Mircosoft
# Source: https://github.com/microsoft/USBroadbandUsagePercentages
county_usage <- read.csv("datasets/broadband_data_2020October.csv")
zipcode_usage <- read.csv("datasets/broadband_data_zipcode.csv")


# Getting Heading
county_usage <- county_usage[18:nrow(county_usage),]

colnames(county_usage) <- as.character(unlist(county_usage[1,]))
county_usage <-  county_usage[-1, ]

# Adjusting column names
colnames(county_usage) <- str_trim( tolower(colnames(county_usage)))

county_usage <- county_usage %>% 
  rename( countyid = `county id`,
          county_name = `county name`,
          fcc_broadband = 'broadband availability per fcc',
          usage = 'broadband usage')

colnames(zipcode_usage) <- str_trim( tolower(colnames(zipcode_usage)))

zipcode_usage <- zipcode_usage %>% 
  dplyr::select(-error.range..95......., -error.range..mae......, -msd) %>% 
  rename(countyid = county.id,
         county_name = county.name,
         postal_code = postal.code,
         broadband_usage = broadband.usage)

# Cleaning countyId to remove state codes in some observations
str(county_usage)
county_usage$countyid <- as.character(county_usage$countyid)
county_usage$countyid <- substr(county_usage$countyid, str_length(county_usage$countyid)-2, str_length(county_usage$countyid) )
str(zipcode_usage)
zipcode_usage$countyid <- as.character(zipcode_usage$countyid)
zipcode_usage$countyid <- substr(zipcode_usage$countyid, str_length(zipcode_usage$countyid)-2, str_length(zipcode_usage$countyid) )
# county_usage$countyid <- as.numeric(county_usage$countyid)





write.csv(county_usage, "datasets/cleaned_broadband_data_2020October.csv")
write.csv(zipcode_usage, "datasets/cleaned_broadband_data_zipcode.csv")


# End cleaning data ----------------------------------------------------






# GETTTING TN BOUNDARY

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolylines(data = tnx)
# Pull in the data from the fcc into the tnx spatial dataframe
tnx@data <- left_join(tnx@data,
                      county_usage %>%
                        mutate(county_name = gsub(' County', '', county_name)),
                      by = c('NAME' = 'county_name'))
tnx@data$fcc_broadband <- as.numeric(tnx@data$fcc_broadband)



library(RColorBrewer)

# map text
map_palette <- colorNumeric(palette = brewer.pal(15, "Greens"), 
                            domain=pd_tn_sub@data$broadband_usage, 
                            na.color="#CECECE")
county_palette <- colorNumeric(palette = colorRampPalette(brewer.pal(9, 'Reds'))(25), 
                            domain=tnx@data$fcc_broadband, 
                            na.color="#CECECE")
head(tnx)


# Making the map

leaflet(options = leafletOptions(minZoom = 1, maxZoom = 10)) %>% 
  # Base Groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  
  # Adding zip code boundary
  addPolygons( 
    data = "zipcode_shp",
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
    
    
    group = "BroadbandUsage") %>% 
  addLegend( pal=map_palette, values=zipcode_shp$broadband_usage, opacity=0.5, position = "bottomleft", na.label = "NA" )

  # Adding county boundary
  addPolygons(data = tnx,
              fillColor = ~county_palette(fcc_broadband),
              fillOpacity = 0.5,
              color = "black",
              weight = 2,
              label = paste0(pd_tn_sub@data$county_name , " County, ",
                             "<br/>",
                             'Broadband Usage: ', pd_tn_sub@data$broadband_usage*100, '%'),
              
              group = "fccBroadbandUsage") %>% 
  
  # Adding county boundary lines
  addPolylines(data = tnx,
               color = 'black',
               weight = 3) %>% 
  
  
  # Layer Groups
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    
    overlayGroups = c("Assets","BroadbandUsage", "fccBroadbandUsage"),
    options = layersControlOptions(collapsed = TRUE)
  )


