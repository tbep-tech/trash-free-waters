# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/locations
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/organizations
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/itemtypes
# 
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?year=2021
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?orgID=6
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?locationID=9
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?eventState=New
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?eventState=QAReady
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?eventState=QAComplete
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?eventState=Complete
# https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?year=2021&eventState=Complete

library(jsonlite)
library(tibble)
library(tidyverse)

# locations
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/locations'
jsn <- fromJSON(url)

out <- jsn %>%
  as_tibble()

# organizations
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/organizations'
jsn <- fromJSON(url)

out <- jsn %>%
  as_tibble()

# item types
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/itemtypes'
jsn <- fromJSON(url)

out <- jsn %>%
  as_tibble()

# 2021 cleanup events
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?year=2021'
jsn <- fromJSON(url)

out <- jsn %>%
  as_tibble()

# complete events 
url <- 'https://dev.tampabay.wateratlas.usf.edu/trash-free-waters/api/cleanupevents?eventState=Complete'
jsn <- fromJSON(url)

out <- jsn %>%
  as_tibble()
