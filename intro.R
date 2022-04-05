library(sf)
library(dplyr)

# https://www.r-project.org/
# https://wiki.openstreetmap.org/wiki/Overpass_API
# https://overpass-turbo.eu/
# https://wiki.openstreetmap.org/wiki/Key:highway
# https://wiki.openstreetmap.org/wiki/Tag:leisure%3Dfitness_station


regso <- st_read("data/RegSO_2018_v1.gpkg")

regso <- regso %>% 
  filter(kommunnamn == "Stockholm")
plot(st_geometry(regso))

# regso <- regso %>% 
#   filter(st_area(.) > mean(st_area(.)))

center <- st_centroid(st_union(regso))
circle <- st_buffer(center, 5000)

# Dense logical matrix
logical_m <- st_intersects(circle, regso, sparse = FALSE)

plot(st_geometry(regso[logical_m,]))
plot(st_geometry(circle), add = TRUE, lty = 5, lwd = 3)

# Kommuner från geojson
# kommuner <- jsonlite::fromJSON("https://segregationsbarometern.delmos.se/geojson/kommuner.geojson")

kommuner <- geojsonsf::geojson_sf("https://segregationsbarometern.delmos.se/geojson/kommuner.geojson")
plot(st_geometry(kommuner))