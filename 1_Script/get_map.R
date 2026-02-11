########################
###-----Chainage-----###
########################

rm(list=ls())

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
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(COGugaison)


###---Données merged---###

Merged_AIR_EDU_2021 <- readRDS("0_input/Merged/Merged_AIR_EDU_2021.rds")
INERIS_Air_2021 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021.rds")
################################################################################

communes_sf_2024 <- st_read("0_Input/Contour_commune/2024/COMMUNE.shp")
communes_sf_2018 <- st_read("0_Input/Contour_commune/2018/COMMUNE.shp")

# map_data_2018 <- communes_sf_2018 %>%
#   mutate(CODGEO = as.character(INSEE_COM)) %>%
#   inner_join(Merged_AIR_EDU_2021, by = "CODGEO")

map_data_2024 <- communes_sf_2024 %>%
  mutate(CODGEO = as.character(INSEE_COM)) %>%
  inner_join(Merged_AIR_EDU_2021, by = "CODGEO")


################################################################################


map_NO2_2021 <- ggplot(map_data_2024) +
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


plot.data <- map_data_2024
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

ggsave("2_results/Map/map_NO2_2021.png", plots$MOY.NO2)
ggsave("2_results/Map/map_O3_2021.png", plots$MOY.O3)
ggsave("2_results/Map/map_PM25_2021.png", plots$MOY.PM25)
ggsave("2_results/Map/map_PM10_2021.png", plots$MOY.PM10)

titre <- richtext_grob(
  "**Moyenne annuelle des polluants atmosphériques par commune (2021)**<br>
   <span style='color:grey50'>µg/m<sup>3</sup> · COG 2024</span>",
  hjust = 0.5,
  gp = gpar(fontsize = 16)
)

source <- textGrob(
  "Données : INERIS 2021 — Qualité de l’air France métropolitaine",
  gp = gpar(fontface = 3, fontsize = 7, col="grey30"),
  hjust = 1,
  x = 1
)


cartes <- arrangeGrob(
  plots$MOY.NO2, plots$MOY.O3,
  plots$MOY.PM25, plots$MOY.PM10,
  ncol = 2,
  padding = unit(0.2, "line")   
)


map_combined <- arrangeGrob(
  titre,
  cartes,
  source,
  ncol = 1,
  heights = unit.c(
    unit(1.2, "cm"),   # hauteur titre
    unit(1, "npc") - unit(2.2, "cm"),  # zone cartes
    unit(1, "cm")      # hauteur source -> évite la coupe
  )
)


ggsave(
  "2_results/Map/map_combined.png",
  map_combined,
  width = 10,
  height = 10,
  bg = "#fbf9f4"
)

# ggsave("2_results/Map/map_combined.png", map_combined)

