library(gsheet)
library(dplyr)
library(stringr)
library(tidygeocoder)


# setwd("~/Sites/Broadband_Asset_Inventory/broadband_app")

# Importing data to rstudio 

# libdata <- readxl::read_xlsx("library_database_v1.xlsx")
libdata <- readxl::read_xlsx("library_database_v2.xlsx")
# libdata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/12EMjsbAVGZPo92nIE2MCATfKF1iIVFWy/edit#gid=1848942392")
librarylocation_data <- readxl::read_xlsx("librarylocation_database.xlsx")


# Checking if any new data has been added

if (nrow(libdata) != nrow(librarylocation_data)) {

  # Forward geo coding the physical address of library
  librarylocation_data_new <- libdata[(nrow(librarylocation_data)+1):nrow(libdata),] %>% 
    mutate(address = paste0(street, ', ', city, ' ', state)) %>% 
    select(address) %>% 
    geocode(address, method = "arcgis")

librarylocation_data <- rbind(librarylocation_data, librarylocation_data_new)
  
}

# ----------------------------------------------------------------------------

# exporting the data
writexl::write_xlsx(librarylocation_data, "librarylocation_database.xlsx")


