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


map_data_2021 <- readRDS("0_input/map_data_COG2024_C_2021.rds")

villes <- map_data_2021 %>%
  filter(Pop_tot > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])

################################################################################

map_dens_2021 <- ggplot(map_data_2021) +
  geom_sf(aes(fill = LIBDENS), color = NA) +
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
  scale_fill_manual(
    values = c(
      "Rural" = "#FAEDCD",
      "Urbain intermédiaire" = "#00B4D8",
      "Urbain dense" = "#03045E"
    ),
    na.value = "grey90",
    name = "**Densité communale classification**",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0,
      direction = "horizontal",
      title.theme = element_markdown(size = 8)
    )
  ) +
  theme_void()+
  labs(title= "Densité communale classification")+
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

ggsave("2_results/Map/Sociales/densite/map_dens_2021.png", map_dens_2021, width = 5,height = 5)

