library(dplyr)
library(jsonlite)
library(httr)
library(sf)

my_url <- "https://api.skolverket.se/planned-educations/school-units?geographicalAreaCode=1780"
httpResponse <- GET(my_url, 
                    add_headers(Accept = "application/vnd.skolverket.plannededucations.api.v2.hal+json"))

info = fromJSON(content(httpResponse, "text"))

total_pages <- info$body$page$totalPages-1

datalist <- list()

for (i in 0:total_pages){
  my_url <- 
    paste("https://api.skolverket.se/planned-educations/school-units?geographicalAreaCode=1780&page=",i,sep="")
  httpResponse <- GET(my_url, 
                      add_headers(Accept = "application/vnd.skolverket.plannededucations.api.v3.hal+json"))
  
  info = fromJSON(content(httpResponse, "text"))
  df <- info$body$`_embedded`$listedSchoolUnits
  
  df$`_links` <- NA
  
  datalist[[i+1]] <- df
}
school_units = do.call(rbind, datalist) %>% rename(schoolunitcode = code)

#####################################

l <- list()
for (i in 1:nrow(school_units)){
  info_url <- paste("https://api.skolverket.se/planned-educations/school-units/", 
                    school_units[i,]$schoolunitcode ,sep = "")
  httpResponse <- GET(info_url, 
                      add_headers(Accept = "application/vnd.skolverket.plannededucations.api.v2.hal+json"))
  info = fromJSON(content(httpResponse, "text"))
  
  # Hämta statistik för skolenhetskoden
  stats_url <- paste("https://api.skolverket.se/planned-educations/school-units/", 
                     school_units[i,]$schoolunitcode, "/statistics/gr" ,sep = "")
  httpResponse <- GET(stats_url, 
                      add_headers(Accept = "application/vnd.skolverket.plannededucations.api.v2.hal+json"))
  stats = fromJSON(content(httpResponse, "text"))
  
  df <- data.frame(
    schoolUnitCode = info$body$code,
    name = info$body$name,
    lon = info$body$wgs84_Long,
    lat = info$body$wgs84_Lat,
    totalNumberOfPupils = ifelse(stats$status != "NOT_FOUND", 
                                 stats$body$totalNumberOfPupils$value, ""))
  l[[i]] <- df
}
school_units_df <- do.call("rbind", l)
school_units_df <- school_units_df %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)

school_units_df <- school_units_df %>% 
  mutate(totalNumberOfPupils = gsub(".* (\\d{1,})$", "\\1" , totalNumberOfPupils)) %>% 
  mutate(totalNumberOfPupils = as.numeric(totalNumberOfPupils)) %>% 
  filter(!is.na(totalNumberOfPupils))