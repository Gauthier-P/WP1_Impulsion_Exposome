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
library(ggrepel)
library(rmapshaper)
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(patchwork)
library(COGugaison)

YEARS <- c(2021, 2015, 2011)  

EDU_LEVELS <- list(
  Low    = list(col   = "Low_edu_prop",
                title = "Niveau faible d'éducation",
                sub   = "Sans diplôme, CAP, BEPC, Brevet ou DNB",
                leg   = "Proportion d'individus avec un niveau faible d'éducation"),
  Medium = list(col   = "Medium_edu_prop",
                title = "Niveau moyen d'éducation",
                sub   = "CAP, BEP, BAC",
                leg   = "Proportion d'individus avec un niveau moyen d'éducation"),
  High   = list(col   = "High_edu_prop",
                title = "Niveau haut d'éducation",
                sub   = ">BAC+2",
                leg   = "Proportion d'individus avec un niveau haut d'éducation")
)

BG_COLOR  <- "#fbf9f4"
POP_THRESHOLD <- 160000


load_year_data <- function(year) {
  path <- paste0("0_input/map_data_COG2024_C_", year, ".rds")
  readRDS(path)
}


get_city_centroids <- function(map_data) {
  villes <- map_data %>% filter(POPULATION.x > POP_THRESHOLD)
  villes_sf <- st_centroid(villes)
  coords <- st_coordinates(villes_sf)
  villes_sf %>% mutate(long = coords[, 1], lat = coords[, 2])
}


make_edu_map <- function(map_data, villes_sf, level_cfg, year) {
  ggplot(map_data) +
    geom_sf(aes(fill = .data[[level_cfg$col]]), color = NA) +
    geom_sf(data = villes_sf, colour = "black", size = 0.5) +
    geom_label_repel(
      data        = villes_sf,
      aes(x = long, y = lat, label = NOM),
      fill        = scales::alpha("white", 0.5),
      color       = "black",
      label.size  = 0,
      box.padding = 0.1,
      point.padding = 0.1,
      max.overlaps  = Inf,
      size          = 1.5
    ) +
    coord_sf(expand = FALSE) +
    scale_fill_viridis_c(
      option   = "turbo",
      na.value = "grey90",
      name     = paste0("**", level_cfg$leg, "**<br><span style='color:grey'>%</span>"),
      guide    = guide_colorbar(
        title.position = "top",
        title.hjust    = 0,
        label.theme    = element_text(size = 6),
        direction      = "horizontal",
        title.theme    = element_markdown(size = 8)
      )
    ) +
    labs(title = paste0(
      level_cfg$title,
      "<br><span style='color:grey; font-size:7pt;'>(", level_cfg$sub, ")</span>"
    )) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = BG_COLOR, color = NA),
      legend.position  = "bottom",
      legend.key.height = unit(0.15, "cm"),
      legend.key.width  = unit(0.80, "cm"),
      legend.text       = element_text(size = 6),
      plot.title        = element_markdown(hjust = 0.5, face = "bold")
    )
}


for (year in YEARS) {
  
  message("Annee: ", year)
  
  map_data  <- load_year_data(year)
  villes_sf <- get_city_centroids(map_data)
  
  maps <- lapply(EDU_LEVELS, function(cfg) {
    make_edu_map(map_data, villes_sf, cfg, year)
  })
  
  for (level_name in names(maps)) {
    out_path <- paste0( "2_results/Map/",year, sprintf(
      "/Sociales/Education/map_edu_%s_%d.png",
      level_name, year
    ))
    
    ggsave(out_path, maps[[level_name]], width = 5, height = 5, bg = BG_COLOR)
    message("  Saved: ", out_path)
  }
  
  map_combined <- (maps$Low | maps$Medium | maps$High) +
    plot_annotation(
      title    = paste("Education par commune", paste0("(", year, ")")),
      subtitle = "Proportion d'individus par niveau d'éducation",
      caption  = paste("Données : INSEE", year),
      theme    = theme(
        plot.background = element_rect(fill = BG_COLOR, color = NA),
        plot.title      = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle   = element_text(hjust = 0.5, color = "grey40"),
        plot.caption    = element_text(hjust = 1, size = 6, color = "grey30")
      )
    ) &
    theme(legend.position = "bottom")
  
  out_combined <- paste0(
    "2_results/Map/",year,"/Sociales/Education/map_combined_edu_", year, ".png"
  )
  ggsave(out_combined, map_combined, width = 10, height = 5, bg = BG_COLOR)
  message("  Sauvegarder: ", out_combined)
}


