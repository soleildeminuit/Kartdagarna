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

library(classInt)
ci <- classIntervals(deso_karlstad_sf$bef_antal, n = 5, style = "jenks")

deso_karlstad_sf <- deso_karlstad_sf %>%
  mutate(breaks = cut(bef_antal, ci$brks, include.lowest = T, dig.lab=10))


gg <- ggplot(deso_karlstad_sf) + 
  geom_sf(aes(fill=breaks)) +
  # scale_fill_gradient2(low = "darkred", mid = "grey85", high = "darkgreen", midpoint = mean(deso_karlstad$bef_antal)) +
  scale_fill_brewer(palette = "RdYlBu") + 
  ggthemes::theme_few()

gg

plot_gg(gg,multicore = TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)

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
