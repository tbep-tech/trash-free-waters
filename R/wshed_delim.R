library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)
library(jsonlite)
library(tibble)
library(readxl)
library(here)
library(ggmap)
library(mapview)
library(purrr)
library(units)

google_key <- Sys.getenv('google_key')
register_google(google_key)

whitebox::wbt_init()

# from here https://www.fgdl.org/metadataexplorer/explorer.jsp
dem <- raster('~/Desktop/usgsdem1.tif')

# https://vt-hydroinformatics.github.io/rgeowatersheds.html

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

locstmp <- locs %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

bbox <- st_bbox(locstmp) %>% 
  st_as_sfc %>%
  st_buffer(dist = set_units(0.5, degree)) %>%
  st_bbox %>% 
  st_as_sfc() %>% 
  st_transform(crs = crs(dem)) %>% 
  as_Spatial()

demcrp <- crop(dem, bbox)
writeRaster(demcrp, '~/Desktop/demcrp.tif', options=c('TFW=YES'))

tm_shape(demcrp)+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE)+
  tm_scale_bar()

wbt_hillshade(dem = '~/Desktop/demcrp.tif',
              output = "~/Desktop/brush_hillshade.tif",
              azimuth = 115)

hillshade <- raster("~/Desktop/brush_hillshade.tif")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()
