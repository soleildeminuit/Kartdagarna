library(sf)
library(cartography)
library(dplyr)

png("figs/pop.png", width = 200, height = 200, units = "mm", res = 300)


# import to an sf object
pop <- deso_karlstad_sf %>% 
  filter(substr(deso_karlstad_sf$deso,5,5) == "C")
# plot municipalities (only borders are plotted)
plot(st_geometry(pop), col = "grey80", border = "grey")
# plot population
propSymbolsLayer(
  x = pop, 
  var = "bef_antal", 
  inches = 0.25, 
  col = "brown4",
  legend.pos = "topright",  
  legend.title.txt = "Befolkning"
)
# layout
layoutLayer(title = "Befolkningssiffror Karlstad",
            sources = "Datakälla: SCB",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE)
# north arrow
north(pos = "topleft")
dev.off()