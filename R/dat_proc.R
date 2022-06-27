library(jsonlite)
library(tibble)
library(tidyverse)
library(readxl)
library(here)
library(ggmap)
library(sf)
library(purrr)
library(lubridate)

google_key <- Sys.getenv('google_key')
register_google(google_key)

##
# location meta

# site id from original file from SS
locsinit <- read_excel(here('data/raw/Site List (1).xlsx')) %>% 
  select(Site = Name, lon = Long, lat = Lat)

# locations
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/locations'

locs <- fromJSON(url, flatten = T) %>% 
  select(
    Site = name, 
    Address = address, 
    City = city,
    County = county
    ) %>%
  mutate(
    Site = gsub('\r\n$', '', Site), 
    County = gsub('\\sCounty$', '', County)
    ) %>% 
  unite('fulladdress', Address, City, sep = ', ', remove = F) %>% 
  left_join(locsinit, by = 'Site') %>% 
  nest(latlon = c('lon', 'lat')) %>% 
  mutate(
    fulladdress = case_when(
      is.na(Address) ~ NA_character_, 
      T ~ fulladdress
    ),
    latlon = pmap(list(fulladdress, latlon), function(fulladdress, latlon){
      out <- latlon
      if(anyNA(latlon) & !is.na(fulladdress))
        out <- geocode(fulladdress)
      return(out)
    })
  ) %>% 
  unnest('latlon', keep_empty = T) %>% 
  mutate(
    lat = ifelse(grepl('BayboroMarina', Site), 27.75848, lat), 
    lon = ifelse(grepl('BayboroMarina', Site), -82.63635, lon) 
  )

# tmp <- locs %>% 
#   filter(!is.na(lon)) %>% 
#   st_as_sf(coords = c('lon', 'lat'), crs = 4326)

save(locs, file = here('data/locs.RData'))

##
# complete event data
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/publicapi/cleanupevents?eventState=Complete'
jsn <- fromJSON(url, flatten = T)

evnt <- jsn %>%
  as_tibble() %>% 
  select(
    Date = eventDate, 
    DeviceID = collectionDeviceId, 
    Org = organization.orgName,
    Site = location.name, 
    # LitterRecyclablesVolumeL = litter_Recycleables.volume, 
    LitterRecyclablesWeightLb = litter_Recycleables.weight,
    # LitterTrashVolumeL = litter_Trash.volume,
    LitterTrashWeightLb = litter_Trash.weight, 
    DebrisRecyclablesWeightLb = debris_Recycleables.weight,
    DebrisTrashWeightLb = debris_Trash.weight,
    dataCards
  ) %>% 
  mutate(
    Site = gsub('\r\n$', '', Site), 
    Date = gsub('^0021', '2021', Date),
    Date = as.Date(Date), 
    dataCards = map(dataCards, function(x){
      if(length(x) > 0)
        x %>% 
          select(itemName) %>% 
          mutate(
            itemName = ifelse(!grepl(' > ', itemName), paste('WriteIn >', itemName), itemName)
          ) %>% 
          separate(itemName, into = c('item1', 'item2'), sep = ' > ', remove = T)
    }), 
    Org = gsub('\\s+$', '', Org)
  )

##
# combine location and event data

tfwdat <- evnt %>% 
  left_join(locs, by = 'Site') %>% 
  select(-fulladdress, -Address)

save(tfwdat, file = here('data/tfwdat.RData'))
