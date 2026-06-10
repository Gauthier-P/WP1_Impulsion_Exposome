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

map_immi_2021 <- ggplot(map_data_2024) +
  geom_sf(aes(fill = Prop_Immigre), color = NA) +
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
    name ="**Proportion d'individus immigrés**<br><span style='color:grey'>%</span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  theme_void()+
  labs(title= "Ensemble des communes")+
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

ggsave("2_results/Map/Sociales/Immigration/map_immi_2021.png", map_immi_2021, width = 5,height = 5)


map_data_2024_comm2000 <- map_data_2024
map_data_2024_comm2000$Prop_Immigre[map_data_2024_comm2000$population_totale <= 2000] <- NA

map_immi_comm2000_2021 <- ggplot(map_data_2024_comm2000) +
  geom_sf(aes(fill = Prop_Immigre), color = NA) +
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
    name ="**Proportion d'individus immigrés**<br><span style='color:grey'>%</span>",
    guide = guide_colorbar(
      title.position = "top",   
      title.hjust = 0,  
      label.theme = element_text(size = 6), 
      direction = "horizontal",
      title.theme = element_markdown(size = 8))
  ) +
  theme_void()+
  labs(title= "Communes > 2000 hab.")+
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

ggsave("2_results/Map/Sociales/Immigration/map_immi_comm2000_2021.png", map_immi_comm2000_2021, width = 5,height = 5)



map_combined <-
  (map_immi_2021|map_immi_comm2000_2021) +
  plot_annotation(
    title = "Immigration par commune (2021)",
    subtitle = "Proportion d'individus immigrés (%)",
    caption = "Données : INSEE 2021",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave(
  "2_results/Map/Sociales/Immigration/map_combined_immi.png",
  map_combined,
  width = 7.5,
  height = 5,
  bg = "#fbf9f4"
)

################################################################################
