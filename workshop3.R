library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(rayshader)

# Läs in geodata (Geopackage), bara geodata - inga data/statistik
regso <- st_read("data/RegSO_2018_v1.gpkg")
deso <- st_read("data/DeSO_2018.gpkg")

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
deso_karlstad <- left_join(deso_karlstad, deso, by = "deso") %>% 
  st_as_sf()


library(classInt)
ci <- classIntervals(deso_karlstad$bef_antal, n = 5, style = "jenks")

deso_karlstad <- deso_karlstad %>%
  mutate(breaks = cut(bef_antal, ci$brks, include.lowest = T, dig.lab=10))

gg <- ggplot(deso_karlstad) + 
  geom_sf(aes(fill=breaks)) +
  # scale_fill_gradient2(low = "darkred", mid = "grey85", high = "darkgreen", midpoint = mean(deso_karlstad$bef_antal)) +
  scale_fill_brewer(palette = "RdYlBu") +
  ggthemes::theme_few()

gg

plot_gg(gg,multicore = TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)