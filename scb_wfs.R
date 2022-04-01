library(dplyr)
library(sf)
library(ows4R)
library(httr)
library(purrr)

wfs_msb <- "https://geodata.scb.se/geoserver/stat/wfs"

url <- parse_url(wfs_msb)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
                  request = "GetCapabilities"
)
request <- build_url(url)
request

scb_client <- WFSClient$new(wfs_msb, 
                            serviceVersion = "1.0.0")

scb_client$getFeatureTypes(pretty = TRUE)

# scb_client$getCapabilities()

scb_client$
  getCapabilities()$ 
  getFeatureTypes() %>%  
  map_chr(function(x){x$getAbstract()})

url$query <- list(service = "wfs",
                  version = "1.0.0", 
                  request = "GetFeature",
                  typename = "stat:DeSO.2018"#,
                  # srsName = "EPSG:3006"
)
request <- build_url(url)

wfs_features <- read_sf(request)

# 5984
# nrow(wfs_features)

st_write(wfs_features, "data/DeSO_2018.gpkg")

plot(st_geometry(wfs_features))