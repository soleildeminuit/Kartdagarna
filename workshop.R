library(sf)
library(dplyr)
library(tmap)
library(viridis)

# https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/regso/

# Läs in geodata (Geopackage), bara geodata - inga data/statistik
regso_sf <- st_read("data/RegSO_2018_v1.gpkg")
deso_sf <- st_read("data/DeSO_2018.gpkg")

# Titta på strukturen
str(regso_sf)
str(deso_sf)

# Vi vil koppla DeSO till motsvarande RegSO.
# Läs in kopplingstabell DeSO - RegSO, från SCB
koppling <- openxlsx::read.xlsx("data/kopplingstabell-deso_regso_20211004.xlsx",
                                "Blad1", 
                                startRow = 4) %>% select(-Kommun, -Kommunnamn)

deso_sf <- deso_sf %>% left_join(., koppling, by = c("deso" = "DeSO"))

# 3363
# n_distinct(deso$RegSOkod)

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
deso_karlstad_sf <- left_join(deso_karlstad_df, deso_sf, by = "deso") %>% 
  st_as_sf()

# Omskrivande rektangel
bb <- st_bbox(deso_karlstad_sf) %>% 
  st_as_sfc() %>% st_as_sf()

bb_extended <- bb %>% st_buffer(5000)

# GSD Terrängkarta
mv_17 <- st_read("data/terrang/17/mv_17.shp")
mv_17 <- mv_17 %>% 
  st_crop(deso_karlstad) %>% 
  filter(KATEGORI == "Vattenyta") %>% 
  st_intersection(deso_karlstad)

skolenheter_sf <- st_read("data/skolenheter_sf.geojson")

###########################################################################
t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  # tm_shape(deso_karlstad) +
  # tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad_sf) + 
  tm_borders() + 
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_layout(
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_1.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad_sf) +
  tm_fill("bef_antal", style = "equal")+#, title = "folkmängd") +
  # tm_shape(deso_karlstad) + 
  # tm_borders() + 
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_layout(
    # legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_2.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad_sf) +
  tm_fill("bef_antal", style = "equal", title = "folkmängd") +
  tm_shape(deso_karlstad_sf) +
  tm_borders() +
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_layout(
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_3.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)
###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad_sf) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad_sf) +
  tm_borders() +
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_layout(
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_4.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad_sf) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad_sf) +
  tm_borders() +
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängden per region",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_5.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)
###########################################################################

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad_sf) +
  tm_fill("bef_antal", 
          style = "jenks", 
          palette = "viridis",
          title = "folkmängd") +
  tm_shape(deso_karlstad_sf) +
  tm_borders() +
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängden per region",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_6.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)
###########################################################################

# A: till största delen utanför större befolkningskoncentrationer eller tätorter.
# B: tätortsområden som inte delas in i mindre områden. Runt existerande tätorter har ett område på 600 meter omkring och de bebyggelser inom detta områdena inkluderats och getts ett eget DeSO område, om invånarantalet överstiger 1000 och det inte är aktuellt indela dessa ytterligare, har färre än 2700 invånare.
# C: Delområden inom stora tätorter.

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    deso_karlstad_sf %>% 
      filter(substr(deso,5,5) == "C")
  ) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad_sf) +
  tm_borders() +
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängden per region",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_7.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################

t <- #tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    deso_karlstad_sf %>% 
      filter(substr(deso,5,5) == "C")
  ) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(
    deso_karlstad_sf %>% 
      filter(substr(deso,5,5) == "C")
  ) +
  tm_borders() +
  # tm_text(
  #   "regso", 
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE) +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängden per region",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_8.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)


###########################################################################

# RegSO

regso_karlstad_sf <- deso_karlstad_sf %>% 
  group_by(RegSOkod, RegSO) %>% 
  summarise(bef_antal = sum(bef_antal)) %>% 
  ungroup()

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    regso_karlstad_sf
  ) +
  tm_fill("bef_antal", 
          style = "jenks", 
          title = "folkmängd") +
  tm_shape(
    regso_karlstad_sf
  ) +
  tm_borders() +
  tm_text(
    "RegSO",
    size = 0.5,
    auto.placement = TRUE,
    remove.overlap = FALSE) +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängden per region",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_9.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################

# RegSO

regso_karlstad_sf <- deso_karlstad_sf %>% 
  group_by(RegSOkod, RegSO) %>% 
  summarise(bef_antal = sum(bef_antal)) %>% 
  ungroup()

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    regso_karlstad_sf
  ) +
  tm_fill("bef_antal", 
          style = "jenks", 
          title = "folkmängd") +
  tm_shape(
    regso_karlstad_sf
  ) +
  tm_borders() +
  tm_shape(mv_17) + tm_fill(col = "cadetblue1", alpha = 0.7) +
  tm_shape(regso_karlstad_sf) +
  tm_text(
    "RegSO",
    size = 0.5,
    auto.placement = TRUE,
    remove.overlap = FALSE,
    col = "black") +
  tm_credits("Kartdagarna 2022 | Datakälla: SCB",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängden per region",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_10.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################

# RegSO

regso_karlstad_sf <- deso_karlstad_sf %>% 
  group_by(RegSOkod, RegSO) %>% 
  summarise(bef_antal = sum(bef_antal)) %>% 
  ungroup()

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    regso_karlstad_sf
  ) +
  tm_fill("bef_antal", 
          style = "jenks", 
          title = "Folkmängd") +
  tm_shape(
    regso_karlstad_sf
  ) +
  tm_borders() +
  tm_shape(mv_17) + tm_fill(col = "cadetblue1", alpha = 0.7) +
  # tm_shape(regso_karlstad) +
  # tm_text(
  #   "RegSO",
  #   size = 0.5,
  #   auto.placement = TRUE,
  #   remove.overlap = FALSE,
  #   col = "black") +
  tm_shape(skolenheter_sf) +
  tm_symbols(
    title.size = "Antal elever",
    size = "totalNumberOfPupils", 
    col = "gray15", 
    border.col = "grey15", 
    alpha = 0.7) +
  tm_text(
    "name",
    size = 0.5,
    auto.placement = TRUE,
    remove.overlap = TRUE,
    col = "black") +
  tm_credits("Kartdagarna 2022 | Datakällor: SCB, Lantmäteriet, Skolverket",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Folkmängd per regionalt statistikområde (RegSO)\nskolenheter_sf och antal elever",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_11.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###########################################################################

# Beräkna andel av resp. åldersgrupp
bef_stat_sf <- pxdf %>% 
  filter(grepl("^\\d{4}[A-C]\\d{4}$", region) == TRUE) %>% 
  filter(
    år == "2021", 
    kön == "totalt", 
    ålder != "totalt"
  ) %>% 
  select(-c(kön, år)) %>% 
  group_by(region) %>% 
  mutate(andel = round(100*(`Folkmängden per region` / sum(`Folkmängden per region`)),1)) %>% 
  ungroup() %>% 
  left_join(., koppling, by = c("region" = "DeSO")) %>% 
  right_join(., deso_sf, by = c("region" = "deso")) %>% 
  arrange(region) %>% 
  st_as_sf(.)

bef_stat_5_9_sf <- bef_stat_sf %>% 
  filter(
    ålder == "5-9 år", 
    substr(region,1,4)=="1780"
  )

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    bef_stat_5_9_sf
  ) +
  tm_fill("andel", 
          style = "jenks", 
          title = "Andel 5-9 år (%)") +
  tm_shape(
    bef_stat_5_9_sf
  ) +
  tm_borders() +
  tm_shape(mv_17) + tm_fill(col = "cadetblue1", alpha = 0.7) +
  tm_shape(skolenheter_sf) +
  tm_symbols(
    title.size = "Antal elever",
    size = "totalNumberOfPupils", 
    col = "gray15", 
    border.col = "grey15", 
    alpha = 0.7) +
  tm_text(
    "name",
    size = 0.5,
    auto.placement = TRUE,
    remove.overlap = TRUE,
    col = "black") +
  tm_credits("Kartdagarna 2022 | Datakällor: SCB, Lantmäteriet, Skolverket",
             position=c("right", "bottom")) +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(type = "arrow", position=c("right", "top"), show.labels = 3) +
  tm_layout(
    main.title = "Andel 5-9 åringar per DeSO\nskolenheter_sf och antal elever",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_12.png", 
          units = "mm",
          width = 210, height = 297, 
          dpi = 300)

###############################################

