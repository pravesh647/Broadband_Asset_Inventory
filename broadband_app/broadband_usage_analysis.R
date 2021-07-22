

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
# county_usage$countyid <- substr(county_usage$countyid, str_length(county_usage$countyid)-2, str_length(county_usage$countyid) )
str(zipcode_usage)
zipcode_usage$countyid <- as.character(zipcode_usage$countyid)
# zipcode_usage$countyid <- substr(zipcode_usage$countyid, str_length(zipcode_usage$countyid)-2, str_length(zipcode_usage$countyid) )
# county_usage$countyid <- as.numeric(county_usage$countyid)





write.csv(county_usage, "datasets/cleaned_broadband_data_2020October.csv")
write.csv(zipcode_usage, "datasets/cleaned_broadband_data_zipcode.csv")


# End cleaning data ----------------------------------------------------
