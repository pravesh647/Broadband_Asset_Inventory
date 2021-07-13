library(gsheet)
library(dplyr)
library(stringr)
library(tidygeocoder)

# setwd("~/Sites/Broadband_Asset_Inventory/broadband_app")
# Importing data to rstudio
libdata <- readxl::read_xlsx("Regional Library Database.xlsx")

# Getting familiar with the dataset
ncol(libdata)
nrow(libdata)
head(libdata)

colnames(libdata) = tolower( colnames(libdata))
names(libdata)

# Renaming column headers
libdata <- libdata %>% 
  rename(phoneNo = `phone number`,
         paddress = `physical address`,
         mailaddress = `mailing address (if different)`,
         isp = `isp provider`,
         internet_acc = `internet access`,
         wifi_acc = `wifi access`,
         computer_classes = `computer classes`,
         lend_laptop = `laptop lending`,
         lead_ereader = `ereader lending`,
         wifi_print = `wifi printing`,
         name = library,
         )

# Breaking single line physical address to street, city, state, zip code ------
libdata <- libdata %>% 
  mutate(paddress = str_trim(paddress),
         zipcode = str_trim(substr(paddress, str_length(libdata$paddress)-5, str_length(libdata$paddress))),
         paddress = str_trim(substr(paddress, 1, str_length(libdata$paddress)-5)),
         asset_type = "library",
         asset_category = "potential_partners"
        )

libdata <- libdata %>% 
  mutate(paddress = str_trim(substr(libdata$paddress, 1, str_length(libdata$paddress)-2))) 

libdata <- libdata %>% 
  mutate(asset_info = NA)
  
libdata <- libdata %>% 
  mutate(city = unlist(lapply(str_split(libdata$paddress, ","), function(x){x[length(x)]})),
         street = lapply(str_split(libdata$paddress, ","), function(x){x[1:(length(x)-1)]})
         )


for (i in 1:length(libdata$street)) {
  the_val <- unlist(libdata$street[i])
  if(length(the_val) > 1){
    libdata$street[i] <- paste0(the_val, collapse = '')
  }
}

libdata$street <- str_trim(unlist(libdata$street))
libdata$street


libdata <- libdata %>%  
  select( -paddress)

# ----------------------------------------------------------------------------

# Forward geocoding the physical address -------------------------------------
# Moving it to forword_geocoding.R

# ??tidygeocoder
# ?geocode

# location_data <- libdata %>% 
#   mutate(address = paste0(street, ', ', city, ' ', state)) %>% 
#   select(address) %>% 
#   geocode(address, method = "arcgis")



# ----------------------------------------------------------------------------


# exporting the data
writexl::write_xlsx(libdata, "library_database_v1.xlsx")



