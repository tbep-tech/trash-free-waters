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
# note that the crs was not immediately obvious, so I imported the original into ArcGIS and exported as tiff
# the crs was then applied
# it 92m cell size, maybe 5m here? https://www.fgdl.org/metadata/fgdc_xml/flidar_mosaic_m.shp.xml
# dem <- raster('~/Desktop/usgsdem1.tif')
dem <- raster('~/Desktop/Topobathy_SPW1.tif')

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

## expanded bounding box for locations used to clip raster
locstmp <- locs %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

bbox <- st_bbox(locstmp) %>% 
  st_as_sfc %>%
  st_buffer(dist = set_units(0.3, degree)) %>%
  st_bbox %>% 
  st_as_sfc() %>% 
  st_transform(crs = crs(dem)) %>% 
  as_Spatial()

demcrp <- dem#crop(dem, bbox)

writeRaster(demcrp, here('T:/05_GIS/TRASH_FREE_WATERS/demcrp.tif'), options=c('TFW=YES'), overwrite = T)

tm_shape(demcrp)+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE)+
  tm_scale_bar()

##
# create hillshade

wbt_hillshade(dem = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp.tif'),
              output = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_hillshade.tif'),
              azimuth = 115)

hillshade <- raster(here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_hillshade.tif'))

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()

##
# fill holes

wbt_fill_single_cell_pits(
  dem = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp.tif'),
  output = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_nopits.tif')
)

wbt_breach_depressions_least_cost(
  dem = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_nopits.tif'),
  output = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_breached_nopits.tif'),
  dist = 5,
  fill = TRUE)

wbt_fill_depressions_wang_and_liu(
  dem = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_breached_nopits.tif'),
  output = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_filled_breached_nopits.tif')
)

demcrp_filled_breached <- raster(here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_filled_breached_nopits.tif'))
tm_shape(demcrp_filled_breached)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()

##
# create flow accumulation and pointer grid

wbt_d8_flow_accumulation(input = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_filled_breached_nopits.tif'),
                         output = here('T:/05_GIS/TRASH_FREE_WATERS/D8FA.tif'))

wbt_d8_pointer(dem = here('T:/05_GIS/TRASH_FREE_WATERS/demcrp_filled_breached_nopits.tif'),
               output = here('T:/05_GIS/TRASH_FREE_WATERS/D8pointer.tif'))

##
# create pour points

ppointsSP <- locstmp %>% 
  st_transform(crs = crs(dem)) %>% 
  as_Spatial()
shapefile(ppointsSP, filename = here('T:/05_GIS/TRASH_FREE_WATERS/pourpoints.shp'), overwrite = TRUE)

##
# extract streams from raster

wbt_extract_streams(flow_accum = here('T:/05_GIS/TRASH_FREE_WATERS/D8FA.tif'),
                    output = here('T:/05_GIS/TRASH_FREE_WATERS/raster_streams.tif'),
                    threshold = 6000)

raster_streams <- raster(here('T:/05_GIS/TRASH_FREE_WATERS/raster_streams.tif'))
mapview(raster_streams)
