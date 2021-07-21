

library(gsheet)
library(dplyr)
library(stringr)
library(tidygeocoder)


if(!dir.exists('datasets')){
  message('Creating a datasets directory')
  dir.create('datasets')
}

library_file <- 'datasets/library_data.RData'

# Importing data to rstudio 
libdata <- readxl::read_xlsx("datasets/library_database_v2.xlsx")
# libdata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/12EMjsbAVGZPo92nIE2MCATfKF1iIVFWy/edit#gid=1848942392")
librarylocation_data <- readxl::read_xlsx("datasets/librarylocation_database.xlsx")

# Checking if any new data has been added

if (nrow(libdata) != nrow(librarylocation_data) & !file.exists(library_file)) {

  # Forward geo coding the physical address of library
  librarylocation_data_new <- libdata[(nrow(librarylocation_data)+1):nrow(libdata),] %>% 
    mutate(address = paste0(street, ', ', city, ' ', state)) %>% 
    geocode(address, method = "arcgis") %>% 
    select(street, city, state, lat, long)

  librarylocation_data <- rbind(librarylocation_data, librarylocation_data_new)
  
  # exporting the data
  writexl::write_xlsx(librarylocation_data, "datasets/librarylocation_database.xlsx")
  
  libdata <- libdata %>%
    left_join(librarylocation_data, by = c("street", "city", "state"))
  
  save(libdata, file = library_file)
  
}else{
  message('Loading a cached file: ', library_file)
  load(library_file)
} 

# ----------------------------------------------------------------------------






