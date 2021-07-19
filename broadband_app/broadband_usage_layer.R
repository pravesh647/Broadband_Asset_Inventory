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

# Reading county and zip code level broadband availability and usage data
county_usage <- read.csv("datasets/cleaned_broadband_data_2020October.csv")
zipcode_usage <- read.csv("datasets/cleaned_broadband_data_zipcode.csv")



options(tigris_use_cache = TRUE)


# Reading zipcode level data
# pd = readOGR('datasets/tl_2019_us_zcta510/')


# Getting TN zipcode spatial polygon data
tn_zipcode <- zctas(cb = FALSE, state = 'TN', year = 2010)
tn_zipcode <- as(tn_zipcode, 'Spatial')
# plot(tn_zipcode)


# GETTTING TN spatial polygon data
tn_county <- counties(state = 'TN', cb = TRUE, resolution = '20m')
tn_county <- as(tn_county, 'Spatial')
# plot(tnx)


# changing datatype
tn_county@data$GEOID <- as.numeric(tn_county@data$GEOID)
tn_zipcode@data$GEOID <- as.numeric(tn_zipcode@data$GEOID)

# Subsetting dataset 


# Left-joining zipcode broadband usage data to zipcode boundary
tn_zipcode@data <- left_join(tn_zipcode@data, zipcode_usage, by=c("ZCTA5CE10" = "postal_code"))



