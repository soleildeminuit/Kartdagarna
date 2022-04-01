library(sf)
library(dplyr)
library(tmap)
library(viridis)

# https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/regso/

# Läs in geodata (Geopackage), bara geodata - inga data/statistik
regso <- st_read("data/RegSO_2018_v1.gpkg")
deso <- st_read("data/DeSO_2018.gpkg")

str(regso)
str(deso)

# Vi vil koppla DeSO till motsvarande RegSO.
# Läs in kopplingstabell DeSO - RegSO, från SCB
koppling <- openxlsx::read.xlsx("data/kopplingstabell-deso_regso_20211004.xlsx",
                                "Blad1", 
                                startRow = 4) %>% select(-Kommun, -Kommunnamn)

deso <- deso %>% left_join(., koppling, by = c("deso" = "DeSO"))

# 3363
# n_distinct(deso$RegSOkod)

# Filtrera fram DeSO för Karlstad
pxdf <- readRDS("data/pxdf.rds")

deso_karlstad <- pxdf %>% 
  filter(grepl("^\\d{4}[A-C]\\d{4}$", region) == TRUE,
         substr(region, 1, 4) == "1780",
         ålder == "totalt", kön == "totalt") %>% 
  select(-ålder, -kön) %>% 
  rename(deso = region) %>% 
  rename(bef_antal = `Folkmängden per region`)

# Läg till geografi
deso_karlstad <- left_join(deso_karlstad, deso, by = "deso")

bb <- st_bbox(deso_karlstad) %>% 
  st_as_sfc() %>% st_as_sf()

bb_extended <- bb %>% st_buffer(5000)


# "data.frame"
# class(deso_karlstad)
deso_karlstad <- st_as_sf(deso_karlstad)
# "sf"         "data.frame"

###########################################################################
t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  # tm_shape(deso_karlstad) +
  # tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad) + 
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
          width = 297, height = 210, 
          dpi = 300)

###########################################################################

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad) +
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
          width = 297, height = 210, 
          dpi = 300)

###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad) +
  tm_fill("bef_antal", style = "equal", title = "folkmängd") +
  tm_shape(deso_karlstad) +
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
          width = 297, height = 210, 
          dpi = 300)
###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad) +
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
          width = 297, height = 210, 
          dpi = 300)

###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad) +
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
    main.title = "Folkmängden per region efter ålder och kön",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_5.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)
###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(deso_karlstad) +
  tm_fill("bef_antal", 
          style = "jenks", 
          palette = "viridis",
          title = "folkmängd") +
  tm_shape(deso_karlstad) +
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
    main.title = "Folkmängden per region efter ålder och kön",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_6.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)
###########################################################################


t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    deso_karlstad %>% 
      filter(substr(deso_karlstad$deso,5,5) == "C")
    ) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(deso_karlstad) +
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
    main.title = "Folkmängden per region efter ålder och kön",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_7.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)

###########################################################################

t <- #tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    deso_karlstad %>% 
      filter(substr(deso_karlstad$deso,5,5) == "C")
  ) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(
    deso_karlstad %>% 
      filter(substr(deso_karlstad$deso,5,5) == "C")
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
    main.title = "Folkmängden per region efter ålder och kön",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_8.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)


###########################################################################

# RegSO

regso_karlstad <- deso_karlstad %>% 
  group_by(RegSOkod, RegSO) %>% 
  summarise(bef_antal = sum(bef_antal))

t <- tm_shape(bb_extended) + tm_borders(alpha = 0) + 
  tm_shape(
    regso_karlstad
  ) +
  tm_fill("bef_antal", style = "jenks", title = "folkmängd") +
  tm_shape(
    regso_karlstad
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
    main.title = "Folkmängden per region efter ålder och kön",
    legend.format=list(fun=function(x) formatC(x, digits=0, format="d", big.mark = " "), text.separator = "-")
  )
t

tmap_save(t, "figs/karta_deso_9.png", 
          units = "mm",
          width = 297, height = 210, 
          dpi = 300)