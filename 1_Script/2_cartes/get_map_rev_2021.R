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


map_data_2024 <- readRDS("0_input/map_data_COG2024_C_2021.rds")

villes <- map_data_2024 %>%
  filter(Pop_tot > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])
################################################################################

map_Rev_D1_2021 <- ggplot(map_data_2024) +
  geom_sf(aes(fill = Q121), color = NA) +
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
    name ="**1er décile de revenu**<br><span style='color:grey'>(€)</span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  
  labs(title= "1er décile de revenu")+
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


ggsave("2_results/Map/Sociales/Revenu/map_Rev_D1_2021.png", map_Rev_D1_2021, width = 5,height = 5)

map_Rev_D9_2021 <- ggplot(map_data_2024) +
  geom_sf(aes(fill = D921), color = NA) +
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
    name ="**9e décile de revenu**<br><span style='color:grey'>(€)</span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  labs(title= "9e décile de revenu")+
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


ggsave("2_results/Map/Sociales/Revenu/map_Rev_D9_2021.png", map_Rev_D9_2021, width = 5,height = 5)

map_Rev_M_2021 <- ggplot(map_data_2024) +
  geom_sf(aes(fill =Q221), color = NA) +
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
    name ="**Revenu médian**<br><span style='color:grey; font-size:7pt;'>(€)</span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  labs(title= "Revenu médian")+
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


ggsave("2_results/Map/Sociales/Revenu/map_Rev_M_2021.png", map_Rev_M_2021, width = 5,height = 5)

map_Rev_D91_2021 <- ggplot(map_data_2024) +
  geom_sf(aes(fill = RD), color = NA) +
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
    name ="**Rapport interdécile D9/D1**<br><span style='color:grey; font-size:7pt;'>(€)</span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  labs(title= "Rapport interdécile D9/D1")+
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


ggsave("2_results/Map/Sociales/Revenu/map_Rev_D91_2021.png", map_Rev_D91_2021, width = 5,height = 5)


map_combined <-
  wrap_plots(
    map_Rev_D1_2021,
    map_Rev_M_2021,
    map_Rev_D9_2021,
    map_Rev_D91_2021,
    ncol = 2,
    nrow = 2
  ) +
  plot_annotation(
    title = "Revenu par commune (2021)",
    caption = "Données : INSEE 2021",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave(
  "2_results/Map/Sociales/Revenu/map_combined_rev.png",
  map_combined,
  width = 10,
  height = 10,
  bg = "#fbf9f4"
)

################################################################################
