library(gsheet)
library(dplyr)
library(stringr)
library(tidygeocoder)


# setwd("~/Sites/Broadband_Asset_Inventory/broadband_app")

# Importing data to rstudio 

# libdata <- readxl::read_xlsx("library_database_v1.xlsx")
libdata <- readxl::read_xlsx("library_database_v2.xlsx")
# libdata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/12EMjsbAVGZPo92nIE2MCATfKF1iIVFWy/edit#gid=1848942392")
location_data <- readxl::read_xlsx("location_database.xlsx")


# Checking if any new data has been added

if (nrow(libdata) != nrow(location_data)) {

  # Forward geo coding the physical address --??tidygeocoder, ?geo ------------
  location_data_new <- libdata[(nrow(location_data)+1):nrow(libdata),] %>% 
    mutate(address = paste0(street, ', ', city, ' ', state)) %>% 
    select(address) %>% 
    geocode(address, method = "arcgis")

location_data <- rbind(location_data, location_data_new)
  
}

# ----------------------------------------------------------------------------

# exporting the data
writexl::write_xlsx(location_data, "location_database.xlsx")


