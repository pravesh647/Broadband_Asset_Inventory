if(!dir.exists('datasets')){
  message('Creating a datasets directory')
  dir.create('datasets')
}
shp_file <- 'datasets/broadband_data.RData'
if(file.exists(shp_file)){
  message('Loading a cached file: ', shp_file)
  load(shp_file)
} else {
  message('Creating a ', shp_file, ' file for the first time.')
  library(readr)
  library(dplyr)
  library(stringr)
  library(leaflet)
  library(RColorBrewer)
  library(tigris)
  library(rgeos)
  library(sp)
  library(rgdal)
  library(usmap)
  # source("broadband_usage_analysis.R")
  # Reading county and zip code level broadband availability and usage data
  zipcode_usage <- readr::read_csv("datasets/cleaned_broadband_data_zipcode.csv")
  # county_usage <- read.csv("datasets/cleaned_broadband_data_2020October.csv", stringsAsFactors=FALSE)
  county_usage <- readr::read_csv("datasets/cleaned_broadband_data_2020October.csv")
  
  # Selecting observations in TN, AL and GA states
  zipcode_usage <- zipcode_usage %>% 
    filter(st %in% c('TN', 'AL', 'GA'))
  
  county_usage <- county_usage %>% 
    filter(st %in% c('TN', 'AL', 'GA'))
  
  
  
  # Saving data from tigris in cache
  options(tigris_use_cache = TRUE)
  
  
  # Getting TN, Al, GA zipcode spatial polygon data
  tn_zipcode <- zctas(cb = FALSE, state = "TN", year = 2010)
  al_zipcode <- zctas(cb = FALSE, state = "AL", year = 2010)
  ga_zipcode <- zctas(cb = FALSE, state = "GA", year = 2010)
  
  zipcode_shp <- rbind(al_zipcode, ga_zipcode, tn_zipcode)
  zipcode_shp <- as(zipcode_shp, 'Spatial')
  
  
  # GETTTING TN, AL, GA counties spatial polygon data
  tn_county <- counties(state = 'TN', cb = TRUE, resolution = '20m')
  al_county <- counties(state = 'AL', cb = TRUE, resolution = '20m')
  ga_county <- counties(state = 'GA', cb = TRUE, resolution = '20m')
  
  county_shp <- rbind(al_county, ga_county, tn_county)
  county_shp <- as(county_shp, 'Spatial')
  
  # plot(tnx)
  
  
  # Confirming which variable is the postal code in zipcode_shp
  
  sum(zipcode_usage$postal_code %in% zipcode_shp@data$ZCTA5CE10)
  nrow(zipcode_usage)
  sum(county_usage$countyid %in% county_shp@data$COUNTYFP)
  nrow(county_usage)
  str(county_usage)
  str(zipcode_usage)
  # Changing datatype for left-join
  zipcode_usage$postal_code <- as.character(zipcode_usage$postal_code)
  
  
  
  # Left-joining zipcode broadband usage data to zipcode boundary
  zipcode_shp@data <- left_join(zipcode_shp@data, zipcode_usage, by=c("ZCTA5CE10" = "postal_code"))
  county_shp@data <- left_join(county_shp@data, county_usage, by=c("COUNTYFP" = "countyid"))
  

  
  # Shinking the shapefile zipcode_shp
  zip_data <- zipcode_shp@data
  z <- gSimplify(zipcode_shp, tol=0.01, topologyPreserve = TRUE)
  # z <- gSimplify(zipcode_shp, tol=10, topologyPreserve = TRUE)
  # z <- zipcode_shp
  z <- SpatialPolygonsDataFrame(Sr = z, data = zip_data)
  coords <- coordinates(z)
  z <- z[coords[,2] <= 36 &
           coords[,2] >= 34 & 
           coords[,1] <= -84 &
           coords[,1] >= -86,]
  # plot(z)
  zipcode_shp <- z

  # Saving final dataset as RObject to be used in shiny app
  save(zipcode_shp, county_shp, file = shp_file)
  
}
