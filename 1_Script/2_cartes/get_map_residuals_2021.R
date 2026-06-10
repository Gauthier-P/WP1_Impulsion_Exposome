########################
###-----Chainage-----###
########################

rm(list=ls())
graphics.off()
set.seed(22071998)

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggtext)
library(grid)
library(gridtext)
library(cowplot)
library(gtable)
library(grid)
library(ggrepel)
library(rmapshaper)
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(patchwork)
library(COGugaison)


###---Données merged---###

Residuals_all <- readRDS("2_Results/Regression/All_resid_air_pollution.rds")
communes_sf_2024 <- st_read("0_Input/Contour_commune/2024/COMMUNE.shp")
map_data_2024 <- readRDS("0_input/map_data_COG2024_C_2021.rds")
map_data_2024 <-st_drop_geometry(map_data_2024)

###############################################################################

resid_M0 <- subset(Residuals_all,Residuals_all$Annee =="2021" & Residuals_all$model == "M0" & Residuals_all$Urbanicity =="All" & Residuals_all$Sociale =="High_edu_prop" & Residuals_all$norm == FALSE)

Resid_M0_NO2  <- subset(resid_M0, resid_M0$Exposure == "MOY.NO2")  %>% select(c("CODGEO", "residual"))
Resid_M0_PM25 <- subset(resid_M0, resid_M0$Exposure == "MOY.PM25") %>% select(c("CODGEO", "residual"))
Resid_M0_PM10 <- subset(resid_M0, resid_M0$Exposure == "MOY.PM10") %>% select(c("CODGEO", "residual"))

missing_codgeo <- c("60694", "85165", "85212", "55239", "55139", "55307", "55039", "55050", "55189")
missing_rows <- data.frame(CODGEO = missing_codgeo, 
                           residual = NA)

Resid_M0_NO2 <- bind_rows(Resid_M0_NO2, missing_rows)
Resid_M0_PM25 <- bind_rows(Resid_M0_PM25, missing_rows)
Resid_M0_PM10 <- bind_rows(Resid_M0_PM10, missing_rows)

Resid_M0_NO2  <- Resid_M0_NO2 %>% rename(residual_NO2  = residual)
Resid_M0_PM25 <- Resid_M0_PM25 %>% rename(residual_PM25 = residual)
Resid_M0_PM10 <- Resid_M0_PM10 %>% rename(residual_PM10 = residual)

map_data_2024 <- map_data_2024 %>% distinct(CODGEO, .keep_all = TRUE)

Resid_M0_map <- merge(map_data_2024, Resid_M0_NO2, by ="CODGEO")
Resid_M0_map <- merge(Resid_M0_map, Resid_M0_PM25, by ="CODGEO")
Resid_M0_map <- merge(Resid_M0_map, Resid_M0_PM10, by ="CODGEO")

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

Resid_M0_map <- communes_sf_2024 %>%
  mutate(CODGEO = as.character(INSEE_COM)) %>%
  inner_join(Resid_M0_map, by = "CODGEO") %>%
  mutate(LIBREG = reg_mapping[as.character(REG)]) %>%
  select(-any_of(cols_to_remove))

###############################################################################

villes <- Resid_M0_map %>%
  filter(Pop_tot > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])

map_Res_edu_NO2 <- ggplot(Resid_M0_map) +
  geom_sf(aes(fill = residual_NO2), color = NA) +
  geom_sf(data = villes.sf, colour = "black", size = 0.5) +
  geom_label_repel(
    data = villes.sf,
    aes(x = long, y = lat, label = NOM.x),
    fill = scales::alpha("white", 0.5),  # fond semi-transparent
    color = "black",
    label.size = 0,                      # pas de bordure
    box.padding = 0.1,
    point.padding = 0.1,
    max.overlaps = Inf,
    size = 1.5
  )+
  
  coord_sf(
    expand = FALSE) + 
  
  scale_fill_distiller(
    palette = "RdYlGn",
    type = "div",
    direction = -1,              
    na.value = "grey90",
    name = "**Résidus Education et NO2**",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8)
    )
  ) +
  labs(title= "Residus Education et NO2")+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#fbf9f4",color=NA),
    legend.position = "bottom",
    legend.text = element_text(),
    legend.key.height = unit(0.15, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    plot.title = element_markdown(hjust=0.5, face="bold"),
    plot.subtitle = element_text(hjust=0.5,color="grey40"),
    plot.caption = element_markdown(color="grey20",hjust=0.5)
  )


ggsave("2_results/Map/Residus/map_NO2_edu_2021.png", map_Res_edu_NO2, width = 5,height = 5)


map_Res_edu_PM25 <- ggplot(Resid_M0_map) +
  geom_sf(aes(fill = residual_PM25), color = NA) +
  geom_sf(data = villes.sf, colour = "black", size = 0.5) +
  geom_label_repel(
    data = villes.sf,
    aes(x = long, y = lat, label = NOM.x),
    fill = scales::alpha("white", 0.5),  # fond semi-transparent
    color = "black",
    label.size = 0,                      # pas de bordure
    box.padding = 0.1,
    point.padding = 0.1,
    max.overlaps = Inf,
    size = 1.5
  )+
  
  coord_sf(
    expand = FALSE) + 
  
  
  scale_fill_gradient2(
    low = "#1D3557",
    mid = "#BAB1DC",
    high = "#E63946",
    midpoint = 0,
    limits = c(-6, 6),
    na.value = "grey90",
    name = "**Residus Education et NO2",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8)
    )
  ) +
  
  labs(title= "Residus Education et PM25")+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#fbf9f4",color=NA),
    legend.position = "bottom",
    legend.text = element_text(),
    legend.key.height = unit(0.15, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    plot.title = element_markdown(hjust=0.5, face="bold"),
    plot.subtitle = element_text(hjust=0.5,color="grey40"),
    plot.caption = element_markdown(color="grey20",hjust=0.5)
  )


ggsave("2_results/Map/Residus/map_PM25_edu_2021.png", map_Res_edu_PM25, width = 5,height = 5)


###############################################################################

map_Res_edu_PM10 <- ggplot(Resid_M0_map) +
  geom_sf(aes(fill = residual_PM10), color = NA) +
  geom_sf(data = villes.sf, colour = "black", size = 0.5) +
  geom_label_repel(
    data = villes.sf,
    aes(x = long, y = lat, label = NOM.x),
    fill = scales::alpha("white", 0.5),  # fond semi-transparent
    color = "black",
    label.size = 0,                      # pas de bordure
    box.padding = 0.1,
    point.padding = 0.1,
    max.overlaps = Inf,
    size = 1.5
  )+
  
  coord_sf(
    expand = FALSE) + 
  
  
  scale_fill_gradient2(
    low = "#1D3557",
    mid = "#BAB1DC",
    high = "#E63946",
    midpoint = 0,
    limits = c(-10, 10),
    na.value = "grey90",
    name = "**Residus Education et NO2",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8)
    )
  ) +
  
  labs(title= "Residus Education et PM10")+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#fbf9f4",color=NA),
    legend.position = "bottom",
    legend.text = element_text(),
    legend.key.height = unit(0.15, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    plot.title = element_markdown(hjust=0.5, face="bold"),
    plot.subtitle = element_text(hjust=0.5,color="grey40"),
    plot.caption = element_markdown(color="grey20",hjust=0.5)
  )



ggsave("2_results/Map/Residus/map_PM10_edu_2021.png", map_Res_edu_PM10, width = 5,height = 5)



