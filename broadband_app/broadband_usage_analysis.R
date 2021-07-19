

setwd("~/Sites/Broadband_Asset_Inventory/broadband_app")
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
# install.packages("usmap")
# library(usmap)
# ?usmap
# 
# ??rgeos



# Loading Data on Broadband Usage provided by FCC and Mircosoft
# Source: https://github.com/microsoft/USBroadbandUsagePercentages
county_usage <- read.csv("datasets/broadband_data_2020October.csv")
zipcode_usage <- read.csv("datasets/broadband_data_zipcode.csv")


# Cleaning Data
county_usage <- county_usage[18:nrow(county_usage),]

colnames(county_usage) <- as.character(unlist(county_usage[1,]))
county_usage <-  county_usage[-1, ]

colnames(county_usage) <- str_trim( tolower(colnames(county_usage)))

county_usage <- county_usage %>% 
  rename( countyid = `county id`,
          county_name = `county name`,
          fcc_broadband = 'broadband availability per fcc',
          usage = 'broadband usage')

str(county_usage)
if( str_length(county_usage$countyid) == 4 ){
  county_usage <- county_usage %>% 
    mutate(countyid = str_sub(countyid, 2, str_length(countyid)))
}


colnames(zipcode_usage) <- str_trim( tolower(colnames(zipcode_usage)))

zipcode_usage <- zipcode_usage %>% 
  dplyr::select(-error.range..95......., -error.range..mae......, -msd) %>% 
  rename(county_id = county.id,
         county_name = county.name,
         postal_code = postal.code,
         broadband_usage = broadband.usage)

write.csv(county_usage, "datasets/cleaned_broadband_data_2020October.csv")
write.csv(zipcode_usage, "datasets/cleaned_broadband_data_zipcode.csv")


# End cleaning data ----------------------------------------------------




# library(ggplot2)
# plot_usmap(include = c("CA", "NV", "ID", "OR", "WA")) +
#   labs(title = "Western States")

# ?plot_usmap
# View(str(usmap::us_map()))
# str(plot_usmap)
# plot_usmap("county")
# plot_usmap(regions = "county")
# 
# county_usage %>% 
# plot_usmap(regions = 'county',values = fcc_broadband)

# Loading shapefile data on usa legal territory boundaries
# Source: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html



options(tigris_use_cache = TRUE)
# ?zctas
# # Get a SpatialPolygonsDataFrame of ZCTAs for TN ( 370:385)
# df <- zctas(cb = FALSE, starts_with = c(370:385), states)
# plot(df)
# # Get a SpatialPolygonsDataFrame for Tennessee
# # tn <- zctas(cb = FALSE, state = 'Tennessee')
# tn <- states(cb=FALSE)
# 
# tn %>% 
#   filter(STUSPS == 'TN') %>% 
#   plot()
# ?states

# Get County boundary shapefile
# county <-  counties(state = 'TN')
# plot(county)





# 
# library(raster)
# tn <- getData('GADM', country = 'USA', level = 3)

pd = readOGR('datasets/tl_2019_us_zcta510/')
# county = readOGR('datasets/tl_2016_47_cousub/')
# plot(county)
# head(pd@data)

# Subset just for areas of interest
tn_zipcode_usage <- zipcode_usage %>% 
  filter(st == 'TN')

pd_tn_sub <- pd[pd@data$ZCTA5CE10 %in% tn_zipcode_usage$postal_code,]
plot(pd_tn_sub)

zipcode_usage$postal_code <- as.character(zipcode_usage$postal_code)
tn_zipcode_usage$postal_code <- as.character(tn_zipcode_usage$postal_code)


pd_sub@data <- left_join(pd_sub@data, zipcode_usage, by=c("ZCTA5CE10" = "postal_code"))
pd_tn_sub@data <- left_join(pd_tn_sub@data, tn_zipcode_usage, by=c("ZCTA5CE10" = "postal_code"))

TGA <- pd_sub %>% 
  filter(pd_subST == 'TN')
  


# GETTTING TN BOUNDARY

library(tigris)
library(sp)
tn <- counties(state = 'TN', cb = TRUE, resolution = '20m')
tnx <- as(tn, 'Spatial')
plot(tnx)

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
    data = pd_tn_sub,
    fillColor = ~map_palette(broadband_usage),
    fillOpacity = 0.7,
    dashArray = '5,5',
    # dashOffset = '10',
    color = "black",
    weight = 2,
    label = paste0(pd_tn_sub@data$county_name , " County, ",
                   "<br/>",
                   pd_tn_sub@data$GEOID10,
                   "<br/>",
                   'Broadband Usage: ', pd_tn_sub@data$broadband_usage*100, '%'),
    
    
    group = "BroadbandUsage") %>% 
  addLegend( pal=map_palette, values=pd_tn_sub$broadband_usage, opacity=0.5, position = "bottomleft", na.label = "NA" ) %>% 
  
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


