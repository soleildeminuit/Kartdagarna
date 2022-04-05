source("key.R")
library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(tmap)
library(RColorBrewer)
library(rayshader)
library(hereR)
set_key(key)

# Läs in geodata (Geopackage), bara geodata - inga data/statistik
regso <- st_read("data/RegSO_2018_v1.gpkg")
deso <- st_read("data/DeSO_2018.gpkg")

# Filtrera fram DeSO för Karlstad
pxdf <- readRDS("data/pxdf.rds")

deso_karlstad_df <- pxdf %>% 
  filter(grepl("^\\d{4}[A-C]\\d{4}$", region) == TRUE,
         substr(region, 1, 4) == "1780",
         ålder == "totalt", kön == "totalt") %>% 
  select(-ålder, -kön) %>% 
  rename(deso = region) %>% 
  rename(bef_antal = `Folkmängden per region`)

# Läg till geografi
deso_karlstad_sf <- left_join(deso_karlstad_df, deso, by = "deso") %>% 
  st_as_sf()


gg <- ggplot(deso_karlstad_sf) + 
  geom_sf(aes(fill=bef_antal)) +
  # scale_fill_gradient2(low = "darkred", mid = "grey85", high = "darkgreen", midpoint = mean(deso_karlstad$bef_antal)) +
  ggthemes::theme_few()

gg


# 3D
plot_gg(gg,multicore = TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)


library(classInt)
ci <- classIntervals(deso_karlstad_sf$bef_antal, n = 5, style = "jenks")

deso_karlstad_sf <- deso_karlstad_sf %>%
  mutate(breaks = cut(bef_antal, ci$brks, include.lowest = T, dig.lab=10))


gg <- ggplot(deso_karlstad_sf) + 
  geom_sf(aes(fill=breaks)) +
  # OpenStreetMap
  geom_sf(data = big_streets, inherit.aes = FALSE, color = "black", size = 0.5, alpha = 0.6) +
  geom_sf(data = med_streets, inherit.aes = FALSE, color = "black", size = 0.3, alpha = 0.5) +
  geom_sf(data = small_streets, inherit.aes = FALSE, color = "black", size = 0.2, alpha = 0.3) +
  # scale_fill_gradient2(low = "darkred", mid = "grey85", high = "darkgreen", midpoint = mean(deso_karlstad$bef_antal)) +
  scale_fill_brewer(palette = "RdYlBu", name = "Antal") + 
  labs(caption = "Datakälla: SCB", title = "Befolkning", subtitle = "Antal invånare") +
  theme_void()

gg

# Spara som pdf
ggsave(plot = gg, filename = "figs/ggplot_ex.pdf", width = 297, height = 210, units = "mm")
ggsave(plot = gg, filename = "figs/ggplot_ex.png", width = 297, height = 210, units = "mm", dpi = 600)

###########################################################################

# Skapa PowerPoint-presentation
library(officer)

p_dml <- rvg::dml(ggobj = gg)

# initiera ppt-slide
officer::read_pptx() %>%
  # lägg till
officer::add_slide() %>%
  # ange objekt
officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # exportera
base::print(
  target = here::here(
    "slides",
    "",
    "",
    "karta_ppt_demo.pptx"
  )
)

###########################################################################
library(osmdata)

plot(st_geometry(regso_karlstad_sf))
bb <- st_bbox(regso_karlstad_sf %>% st_transform(crs = 4326))

big_streets <- bb %>% 
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

big_streets <- big_streets$osm_lines
big_streets$name <- iconv(big_streets$name, "UTF-8")

med_streets <- bb %>% 
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

med_streets <- med_streets$osm_lines
med_streets$name <- iconv(med_streets$name, "UTF-8")

# Längst gatan
x <- med_streets[which.max(st_length(med_streets)),]
x_buff <- st_buffer(x %>% st_transform(crs = 3006), 500)

small_streets <- bb %>% 
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

small_streets <- small_streets$osm_lines

###########################################################################

# Routing
# library(osrm)
# 
center <- st_centroid(st_union(regso_karlstad_sf)) %>% st_transform(crs = 4326) %>%
  st_as_sf()

sthlm <- data.frame(lat = 59.3124615, lon = 18.0699127) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ccc <- data.frame(lat = 59.38372, lon = 13.50940) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 
# isoc <- osrmIsochrone(loc = center, returnclass="sf",
#                       breaks = seq(from = 0, to = 15, by = 5))

# Isochrone for 5, 10, 15, 20, 25 and 30 minutes driving time
isolines <- isoline(
  poi = ccc,
  transport_mode = "car",
  range = seq(5, 15, 5) * 60,
  url_only = FALSE
)

tm_shape(isolines) + tm_fill("range", style = "equal", palette = "plasma", alpha = 0.7)

