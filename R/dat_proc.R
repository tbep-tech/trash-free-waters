
library(jsonlite)
library(tibble)
library(tidyverse)
library(readxl)
library(here)
library(ggmap)
library(sf)
library(mapview)
library(purrr)

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
  mutate(Site = gsub('\r\n$', '', Site)) %>% 
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

tmp <- locs %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)



# complete event data
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?eventState=Complete'
jsn <- fromJSON(url, flatten = T)

evnt <- jsn %>%
  select(
    Date = eventDate, 
    DeviceID = collectionDeviceId, 
    Org = organization.orgName,
    Site = location.name, 
    LitterRecyclablesVolumeL = litter_Recycleables.volume, 
    LitterRecyclablesBags = litter_Recycleables.bags,
    LitterRecyclablesWeightLb = litter_Recycleables.weight,
    LitterTrashVolumeL = litter_Trash.volume,
    LitterTrashBags = litter_Trash.bags,
    LitterTrashWeightLb = litter_Trash.weight, 
    DebrisRecyclablesWeightLb = debris_Recycleables.weight,
    DebrisTrashWeightLb = debris_Trash.weight
  ) %>% 
  mutate(Site = gsub('\r\n$', '', Site))






