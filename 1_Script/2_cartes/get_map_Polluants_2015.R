########################
###-----Chainage-----###
########################

rm(list=ls())
graphics.off()

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(grid)
library(gridtext)
library(cowplot)
library(gtable)
library(grid)
library(rmapshaper)
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(patchwork)
library(COGugaison)


###---Données merged---###

map_data_2015<- readRDS("0_input/map_data_COG2024_C_2015.rds")
# INERIS_Air_2015 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2015.rds")
################################################################################

# communes_sf_2024 <- st_read("0_Input/Contour_commune/2024/COMMUNE.shp")
# communes_sf_2018 <- st_read("0_Input/Contour_commune/2018/COMMUNE.shp")
# 
# # map_data_2018 <- communes_sf_2018 %>%
# #   mutate(CODGEO = as.character(INSEE_COM)) %>%
# #   inner_join(Merged_2015, by = "CODGEO")
# 
# map_data_2015 <- communes_sf_2024 %>%
#   mutate(CODGEO = as.character(INSEE_COM)) %>%
#   inner_join(Merged_2015, by = "CODGEO")
# 

villes <- map_data_2015 %>%
  filter(POPULATION.x > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])

################################################################################


map_NO2_2015 <- ggplot(map_data_2015) +
  geom_sf(aes(fill = MOY.NO2), color = NA) +
  scale_fill_viridis_c(
    option = "turbo",
    na.value = "grey90",
    name ="**Concentration**<br><span style='color:grey'>µg/m<sup>3</sup></span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  labs(
    title= "NO<sub>2</sub>")+
  theme_void()+
  theme(
    plot.margin = margin(1,1,1,1,"cm"),
    plot.background = element_rect(fill="#fbf9f4",color=NA),
    
    legend.position = "bottom",
    legend.text = element_text(),
    legend.key.height = unit(0.15, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    plot.title = element_markdown(hjust=0.5, face="bold"),
    plot.subtitle = element_text(hjust=0.5,color="grey40"),
    plot.caption = element_markdown(color="grey20",hjust=0.5)
  )


f.polluant <- function(plot.data, polluant, p.title){
  
  ggplot(plot.data) +
    geom_sf(aes(fill = .data[[polluant]]), color = NA) +
    geom_sf(data = villes.sf, colour = "black", size = 0.5) +
    geom_label_repel(
      data = villes.sf,
      aes(x = long, y = lat, label = NOM),
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
    scale_fill_viridis_c(
      option = "turbo",
      na.value = "grey90",
      name ="**Concentration**<br><span style='color:grey'>µg/m<sup>3</sup></span>",
      guide = guide_colorbar(
        title.position = "top",   
        title.hjust = 0,  
        label.theme = element_text(size = 6), 
        direction = "horizontal",
        title.theme = element_markdown(size = 8))
    ) +
    labs(title = p.title) +
    theme_void() +
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
}


plot.data <- map_data_2015
liste.polluants <- c("MOY.NO2", "MOY.O3","MOY.PM25","MOY.PM10")
liste.polluants.legend <- c("NO<sub>2</sub>", "O<sub>3</sub>","PM<sub>2.5</sub>","PM<sub>10</sub>")

plots <- list()

for(i in seq_along(liste.polluants)){
  
  polluant <- liste.polluants[i]
  titre <- liste.polluants.legend[i]
  
  plots[[polluant]] <- f.polluant(
    plot.data = plot.data,
    polluant = polluant,
    p.title = titre
  )
}

ggsave("2_results/Map/2015/Air_pollution/map_NO2_2015.png", plots$MOY.NO2, width = 5,height = 5)
ggsave("2_results/Map/2015/Air_pollution/map_O3_2015.png", plots$MOY.O3, width = 5,height = 5)
ggsave("2_results/Map/2015/Air_pollution/map_PM25_2015.png", plots$MOY.PM25, width = 5,height = 5)
ggsave("2_results/Map/2015/Air_pollution/map_PM10_2015.png", plots$MOY.PM10, width = 5,height = 5)

map_combined <-
  wrap_plots(
    plots$MOY.NO2,
    plots$MOY.O3,
    plots$MOY.PM25,
    plots$MOY.PM10,
    ncol = 2,
    nrow = 2
  ) +
  plot_annotation(
    title = "Moyenne annuelle des polluants atmosphériques par commune (2015)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2015 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")


#
ggsave("2_results/Map/2015/Air_pollution/map_combined.png", map_combined, width = 10,height = 10)

