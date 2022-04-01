library(sf)
library(dplyr)
library(tmap)

# https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/regso/

# Läs in geodata (Geopackage)
regso <- st_read("data/RegSO_2018_v1.gpkg")
deso <- st_read("data/DeSO_2018.gpkg")

str(regso)
str(deso)

# Välj Karlstad
regso_karlstad <- regso %>%
  filter(kommun == "1780")

bb <- st_bbox(regso_karlstad) %>% 
  st_as_sfc() %>% st_as_sf()

bb_extended <- bb %>% st_buffer(5000)

plot(st_geometry(regso_karlstad))

# Rita kartlager
tmap_mode("plot")
# tmap_mode("view")

t <- tm_shape(regso_karlstad) + 
  tm_borders()
t

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(regso_karlstad) + 
  tm_borders() + tm_text("regso", 
                         size = 0.5,
                         auto.placement = TRUE,
                         remove.overlap = FALSE)
t

tmap_save(t, "figs/karta_regso_karlstad.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)

############# Statistik #############

# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/
# https://www.scb.se/hitta-statistik/regional-statistik-och-kartor/regionala-indelningar/regso---regionala-statistikomraden/regso-tabellerna-i-statistikdatabasen---information-och-instruktioner/

# Koppla geodata till deso
deso_karlstad <- deso %>% right_join(., deso_karlstad, by = c("deso" = "region"))

# "sf"         "data.frame"
# class(deso_karlstad)

bef_per_regso <- deso %>% 
  st_drop_geometry() %>% 
  group_by(RegSOkod, RegSO) %>% 
  summarise(`Folkmängden per Regso` = sum(`Folkmängden per region`))

regso <- regso %>% right_join(., bef_per_regso, by = c("regsokod" = "RegSOkod"))

regso <- regso %>% rename(bef_antal = `Folkmängden per Regso`)
deso <- deso %>% rename(bef_antal = `Folkmängden per region`)

# Karta
t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(regso) + 
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(regso) + 
  tm_borders() + tm_text("regso", 
                         size = 0.5,
                         auto.placement = TRUE,
                         remove.overlap = FALSE) +
  tm_layout(
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_regso_karlstad_filled.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)
