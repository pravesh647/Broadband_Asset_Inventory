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
  
  # Getting concerned counties
  concerned_counties <- readxl::read_xlsx("datasets/concerned_counties.xlsx")
  concerned_counties <- concerned_counties[1:(nrow(concerned_counties)-2),]
  concerned_counties <- concerned_counties %>% 
    select(county, FIPS, state)
  
  # concerned_counties <- concerned_counties %>% 
  #   mutate(FIPS = substr(FIPS, str_length(FIPS)-2, str_length(FIPS)))
  
  # Selecting observations in TN, AL and GA states
  zipcode_usage <- zipcode_usage %>% 
    filter(st %in% c('TN', 'AL', 'GA'))
  
  county_usage <- county_usage %>% 
    filter(st %in% c('TN', 'AL', 'GA'))
  
  # Selecting observations in only the concerned counties
  county_usage <- county_usage %>% 
    filter(county_usage$countyid %in% concerned_counties$FIPS)
  
  # zipcode_usage <- zipcode_usage %>% 
    # filter(zipcode_usage$countyid %in% concerned_counties$FIPS)
  
  
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
  
  # Joining STATEFP and COUNTYFP to get countyid
  county_shp@data <- county_shp@data %>% 
    mutate(countyid = paste(county_shp@data$STATEFP, county_shp@data$COUNTYFP, sep = ""))
  
  str(county_shp@data)
  # Removing the initial zeros
  county_shp@data$countyid <- as.numeric(county_shp@data$countyid)
  county_usage$countyid <- as.numeric(county_usage$countyid)
  str(county_usage)
  
  # Confirming which variable is the postal code in zipcode_shp
  sum(zipcode_usage$postal_code %in% zipcode_shp@data$ZCTA5CE10)
  nrow(zipcode_usage)
  sum(county_usage$countyid %in% county_shp@data$COUNTYFP)
  nrow(county_usage)
  str(county_usage)
  str(zipcode_usage)
  
  # Changing datatype for left-join
  zipcode_usage$postal_code <- as.character(zipcode_usage$postal_code)
  county_shp@data$fcc_broadband <- as.numeric(county_shp@data$fcc_broadband)

  # Left-joining zipcode broadband usage data to zipcode boundary
  zipcode_shp@data <- left_join(zipcode_shp@data, zipcode_usage, by=c("ZCTA5CE10" = "postal_code"))
  county_shp@data <- left_join(county_shp@data, county_usage, by="countyid")
  
  
  

  # Keeping only boundaries for concerned counties  
  # zipcode_shp <- zipcode_shp[!is.na(zipcode_shp@data$countyid),]
  county_shp <- county_shp[!is.na(county_shp@data$county_name),]
  
  # see intersections of all zipcodes with county borders
  x <- sp::over(zipcode_shp, polygons(county_shp))
  zipcode_shp <- zipcode_shp[!is.na(x),]
  
  # Simplifying the resolution of the boundaries 
  zipcode_shp_d <- zipcode_shp@data
  zipcode_shp <- gSimplify(zipcode_shp, tol = 0.01, topologyPreserve = TRUE)
  zipcode_shp <- SpatialPolygonsDataFrame(Sr = zipcode_shp, data = zipcode_shp_d)
  
  # plot(zipcode_shp)
  # plot(county_shp, col = adjustcolor('red', alpha.f = 0.3), add = T)
  

  # Saving final dataset as RObject to be used in shiny app
  save(zipcode_shp, county_shp, file = shp_file)
  
}
