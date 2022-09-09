library(jsonlite)
library(tibble)
library(tidyverse)
library(here)
library(ggmap)
library(sf)
library(purrr)
library(lubridate)
library(googlesheets4)
library(googledrive)

google_key <- Sys.getenv('google_key')
register_google(google_key)

gs4_deauth()

##
# location meta

# site id from Google Drive
# https://docs.google.com/spreadsheets/d/1LJ20HgCeN5lkgYITKGidrOXevVOEkqurf9z6tLQ-PU4/edit#gid=1663489699
locsinit <- read_sheet('1LJ20HgCeN5lkgYITKGidrOXevVOEkqurf9z6tLQ-PU4') %>% 
  select(Site = `Site ID`, lon = Long, lat = Lat, device = `Device Type`) %>% 
  mutate(
    device = case_when(
      grepl('^Originally', device) ~ 'Litter Gitter/Boom', 
      T ~ device
    )
  )

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
          select(itemName, itemCondition.intactCount, itemCondition.partialIntactCount, itemCondition.degradedCount) %>% 
          mutate(
            itemName = ifelse(!grepl(' > ', itemName), paste('WriteIn >', itemName), itemName), 
          ) %>% 
          separate(itemName, into = c('item1', 'item2'), sep = ' > ', remove = T) %>% 
          rowwise() %>% 
          mutate(
            itemcnt = sum(itemCondition.intactCount, itemCondition.partialIntactCount, itemCondition.degradedCount, na.rm = T)
          ) %>% 
          select(item1, item2, itemcnt)
    }), 
    Org = gsub('\\s+$', '', Org)
  )

##
# combine location and event data

tfwdat <- evnt %>% 
  left_join(locs, by = 'Site') %>% 
  select(-fulladdress, -Address)

save(tfwdat, file = here('data/tfwdat.RData'))
