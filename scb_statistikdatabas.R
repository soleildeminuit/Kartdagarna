library(pxweb)
library(dplyr)

#  "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101Y/FolkmDesoAldKonN"
# d <- pxweb_interactive("http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101Y/FolkmDesoAldKonN")
d <- pxweb_interactive("api.scb.se")

# query <- pxweb_query_as_json(d$query, pretty = TRUE)

pxq <- pxweb_query("data/query_befstat.json")

pxd <- pxweb_get("http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101Y/FolkmDesoAldKonN",
                 pxq)
pxd

pxdf <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")
head(pxdf)

# saveRDS(pxdf, "data/pxdf.rds")
pxdf <- readRDS("data/pxdf.rds")

deso_karlstad <- pxdf %>% 
  filter(grepl("^\\d{4}[A-C]\\d{4}$", region) == TRUE,
         substr(region, 1, 4) == "1780",
         ålder == "totalt", kön == "totalt") %>% 
  select(-ålder, -kön)

# 53
# n_distinct(deso_karlstad$region)

# https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/regso/
# Läs in kopplingstabell DeSO - RegSO, från SCB
koppling <- openxlsx::read.xlsx("data/kopplingstabell-deso_regso_20211004.xlsx",
                                "Blad1", 
                                startRow = 4) %>% select(-Kommun, -Kommunnamn)

deso_karlstad <- deso_karlstad %>% left_join(., koppling, by = c("region" = "DeSO"))

# 31
n_distinct(deso_karlstad$RegSOkod)

