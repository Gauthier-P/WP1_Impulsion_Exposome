########################
###-----Chainage-----###
########################


library(questionr)
library(rmapshaper)
library(sf)
library(dplyr)


rm(list=ls())
graphics.off()
###---Données merged---###

Merged_AIR_EDU_2021 <- readRDS("0_input/Merged/Merged_2021.rds")
Merged_AIR_EDU_2015 <- readRDS("0_input/Merged/Merged_2015.rds")
Merged_AIR_EDU_2011 <- readRDS("0_input/Merged/Merged_2011.rds")
################################################################################

communes_sf_2024 <- st_read("0_Input/Contour_commune/2024/COMMUNE.shp")
communes_sf_2018 <- st_read("0_Input/Contour_commune/2018/COMMUNE.shp")


reg_mapping <- c(
  "1" = "Guadeloupe",
  "2" = "Martinique",
  "3" = "Guyane",
  "4" = "La Réunion",
  "6" = "Mayotte",
  "11" = "Île-de-France",
  "24" = "Centre-Val de Loire",
  "27" = "Bourgogne-Franche-Comté",
  "28" = "Normandie",
  "32" = "Hauts-de-France",
  "44" = "Grand Est",
  "52" = "Pays de la Loire",
  "53" = "Bretagne",
  "75" = "Nouvelle-Aquitaine",
  "76" = "Occitanie",
  "84" = "Auvergne-Rhône-Alpes",
  "93" = "Provence-Alpes-Côte d'Azur",
  "94" = "Corse"
)

cols_to_remove <- c("ID", "NOM_M", "INSEE_COM", "STATUT", "INSEE_CAN",
                    "INSEE_REG", "INSEE_ARR", "INSEE_DEP", "SIREN_EPCI")

###--- 2021 ---###
map_data_2021 <- communes_sf_2024 %>%
  mutate(CODGEO = as.character(INSEE_COM)) %>%
  inner_join(Merged_AIR_EDU_2021, by = "CODGEO") %>%
  mutate(LIBREG = reg_mapping[as.character(REG)]) %>%
  select(-any_of(cols_to_remove))

###--- 2015 ---###
map_data_2015 <- communes_sf_2024 %>%
  mutate(CODGEO = as.character(INSEE_COM)) %>%
  inner_join(Merged_AIR_EDU_2015, by = "CODGEO") %>%
  mutate(LIBREG = reg_mapping[as.character(REG)]) %>%
  select(-any_of(cols_to_remove))

###--- 2011 ---###
map_data_2011 <- communes_sf_2024 %>%
  mutate(CODGEO = as.character(INSEE_COM)) %>%
  inner_join(Merged_AIR_EDU_2011, by = "CODGEO") %>%
  mutate(LIBREG = reg_mapping[as.character(REG)]) %>%
  select(-any_of(cols_to_remove))

saveRDS(map_data_2021,"0_Input/map_data_COG2024_C_2021.rds" )
saveRDS(map_data_2015,"0_Input/map_data_COG2024_C_2015.rds" )
saveRDS(map_data_2011,"0_Input/map_data_COG2024_C_2011.rds" )

