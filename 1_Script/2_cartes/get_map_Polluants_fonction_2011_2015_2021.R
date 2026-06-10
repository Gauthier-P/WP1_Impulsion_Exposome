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

map_data_2011 <- readRDS("0_input/map_data_COG2024_C_2011.rds")
map_data_2015 <- readRDS("0_input/map_data_COG2024_C_2015.rds")
map_data_2021 <- readRDS("0_input/map_data_COG2024_C_2021.rds")


scales_polluants <- list(
  "MOY.NO2"  = range(
    c(map_data_2011$MOY.NO2,map_data_2015$MOY.NO2,map_data_2021$MOY.NO2),
    na.rm = TRUE
  ),
  "MOY.O3"   = range(
    c(map_data_2011$MOY.O3, map_data_2015$MOY.O3, map_data_2021$MOY.O3),
    na.rm = TRUE
  ),
  "MOY.PM25" =  range(
    c(map_data_2011$MOY.PM25, map_data_2015$MOY.PM25, map_data_2021$MOY.PM25),
    na.rm = TRUE
  ),
  "MOY.PM10" =  range(
    c(map_data_2011$MOY.PM10, map_data_2015$MOY.PM10, map_data_2021$MOY.PM10),
    na.rm = TRUE
  )
)


###--- Fonction principale ---###


f.map.polluants <- function(annee) {
  
  # Chargement des données
  map_data <- readRDS(paste0("0_input/map_data_COG2024_C_", annee, ".rds"))
  
  villes <- map_data %>% filter(POPULATION.x > 160000)
  villes.sf <- villes %>% st_centroid()
  coords <- st_coordinates(villes.sf)
  villes.sf <- villes.sf %>%
    mutate(long = coords[,1], lat = coords[,2])
  
  f.polluant <- function(plot.data, polluant, p.title, limits) {
    ggplot(plot.data) +
      geom_sf(aes(fill = .data[[polluant]]), color = NA) +
      geom_sf(data = villes.sf, colour = "black", size = 0.5) +
      geom_label_repel(
        data = villes.sf,
        aes(x = long, y = lat, label = NOM),
        fill = scales::alpha("white", 0.5),
        color = "black",
        label.size = 0,
        box.padding = 0.1,
        point.padding = 0.1,
        max.overlaps = Inf,
        size = 1.5
      ) +
      coord_sf(expand = FALSE) +
      scale_fill_viridis_c(
        option = "turbo",
        limits = limits,   
        na.value = "grey90",
        name = "**Concentration**<br><span style='color:grey'>µg/m<sup>3</sup></span>",
        guide = guide_colorbar(
          title.position = "top",
          title.hjust = 0,
          label.theme = element_text(size = 6),
          direction = "horizontal",
          title.theme = element_markdown(size = 8)
        )
      ) +
      labs(title = p.title) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#fbf9f4", color = NA),
        legend.position = "bottom",
        legend.key.height = unit(0.15, "cm"),
        legend.key.width  = unit(0.8, "cm"),
        plot.title = element_markdown(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
        plot.caption = element_markdown(color = "grey20", hjust = 0.5)
      )
  }
  
  liste.polluants        <- c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
  liste.polluants.legend <- c("NO<sub>2</sub>", "O<sub>3</sub>", "PM<sub>2.5</sub>", "PM<sub>10</sub>")
  
  plots <- list()
  for (i in seq_along(liste.polluants)) {
    plots[[liste.polluants[i]]] <- f.polluant(
      plot.data = map_data,
      polluant  = liste.polluants[i],
      p.title   = liste.polluants.legend[i],
      limits    = scales_polluants[[liste.polluants[i]]]
    )
  }
  
  output_dir <- paste0("2_results/Map/", annee, "/Air_pollution/")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(paste0(output_dir, "map_NO2_", annee, ".png"),  plots$MOY.NO2,  width = 5, height = 5)
  ggsave(paste0(output_dir, "map_O3_", annee, ".png"),   plots$MOY.O3,   width = 5, height = 5)
  ggsave(paste0(output_dir, "map_PM25_", annee, ".png"), plots$MOY.PM25, width = 5, height = 5)
  ggsave(paste0(output_dir, "map_PM10_", annee, ".png"), plots$MOY.PM10, width = 5, height = 5)
  
  map_combined <- wrap_plots(
    plots$MOY.NO2, plots$MOY.O3,
    plots$MOY.PM25, plots$MOY.PM10,
    ncol = 2, nrow = 2
  ) +
    plot_annotation(
      title    = paste0("Moyenne annuelle des polluants atmosphériques par commune (", annee, ")"),
      subtitle = "COG 2024",
      caption  = "Données : INERIS - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
      theme = theme(
        plot.background = element_rect(fill = "#fbf9f4", color = NA),
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
        plot.caption  = element_text(hjust = 1, size = 6, color = "grey30")
      )
    ) &
    theme(legend.position = "bottom")
  
  ggsave(paste0(output_dir, "map_combined.png"), map_combined, width = 10, height = 10)
  
  message("Cartes ", annee, " sauvegardées dans : ", output_dir)
}

f.map.polluants(2011)
f.map.polluants(2015)
f.map.polluants(2021)
