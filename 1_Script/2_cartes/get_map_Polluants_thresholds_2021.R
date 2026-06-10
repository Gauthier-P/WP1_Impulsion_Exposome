########################
###-----Chainage-----###
########################

rm(list=ls())


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
library(MetBrewer)
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(patchwork)
library(COGugaison)


###---Données merged---###

map_data_2021 <- readRDS("0_input/map_data_COG2024_C_2021_biv_seuil.rds")
map_data_2015 <- readRDS("0_input/map_data_COG2024_C_2015_biv_seuil.rds")
map_data_2011 <- readRDS("0_input/map_data_COG2024_C_2011_biv_seuil.rds")

################################################################################
 

villes <- map_data_2021 %>%
  filter(Pop_tot > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])

c1 <- rev(met.brewer("Hiroshige"))
# swatchplot(c1)

polluant <- "MOY.NO2"
p.title <- "test"

################################################################################
f.polluant <- function(polluant, p.title, annee){

  plot.data <- readRDS(paste0("0_input/map_data_COG2024_C_", annee, ".rds"))
  
  
  plot.data[[polluant]] <- factor(
    plot.data[[polluant]],
    levels = c(0, 1),
    labels = c("below", "above")
  )
  

  ggplot(plot.data) +
    geom_sf(aes(fill =  plot.data[[polluant]]), color = NA) +
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
      values = c("below" = c1[2], "above" = c1[10]),
      limits = c("below", "above"),# force les deux catégories
      breaks = c("below", "above"), 
      drop = FALSE,                   # ne supprime pas les catégories absentes
      na.value = "grey90",
      name = "**Regulatory threshold**",
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal",
        title.theme = element_markdown(size = 8),
        label.theme = element_text(size = 6)
      )
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


# plot.data <- map_data_2024

liste.polluants <- c("NO2.WHO.target1", "NO2.WHO.target2","NO2.WHO.target3","NO2.WHO.AQG",
                     "PM25.WHO.target1","PM25.WHO.target2", "PM25.WHO.target3", "PM25.WHO.target4", "PM25.WHO.AQG",
                     "PM10.WHO.target1", "PM10.WHO.target2", "PM10.WHO.target3", "PM10.WHO.target4", "PM10.WHO.AQG")

liste.polluants.legend <- c("Target 1 WHO NO<sub>2</sub>",
                            "Target 2 WHO NO<sub>2</sub>",
                            "Target 3 WHO NO<sub>2</sub>",
                            "AQG WHO NO<sub>2</sub>",
                            
                            "Target 1 WHO PM<sub>2.5</sub>",
                            "Target 2 WHO PM<sub>2.5</sub>",
                            "Target 3 WHO PM<sub>2.5</sub>",
                            "Target 4 WHO PM<sub>2.5</sub>",
                            "AQG WHO PM<sub>2.5</sub>",
                            
                            "Target 1 WHO PM<sub>10</sub>",
                            "Target 2 WHO PM<sub>10</sub>",
                            "Target 3 WHO PM<sub>10</sub>",
                            "Target 4 WHO PM<sub>10</sub>",
                            "AQG WHO PM<sub>10</sub>")

plots_2021 <- list()
plots_2015 <- list()
plots_2011 <- list()

for(i in seq_along(liste.polluants)){
  
  polluant <- liste.polluants[i]
  titre <- liste.polluants.legend[i]
  
  plots_2021[[polluant]] <- f.polluant(
    polluant = polluant,
    p.title = titre,
    annee = 2021
  )
  output.file <- paste0("2_results/Map/2021/Seuil/", liste.polluants[i], ".jpeg")
  ggsave(output.file, width = 5,height = 5)
  
  plots_2015[[polluant]] <- f.polluant(
    polluant = polluant,
    p.title = titre,
    annee = 2015
  )
  output.file <- paste0("2_results/Map/2015/Seuil/", liste.polluants[i], ".jpeg")
  ggsave(output.file, width = 5,height = 5)
  
  
  plots_2011[[polluant]] <- f.polluant(
    polluant = polluant,
    p.title = titre,
    annee = 2011
  )
  output.file <- paste0("2_results/Map/2011/Seuil/", liste.polluants[i], ".jpeg")
  ggsave(output.file, width = 5,height = 5)

}


# ggsave("2_results/Map/map_NO2_Seuil_F_2021.png", plots$THRESHOLD.F.NO2, width = 5,height = 5)
# ggsave("2_results/Map/map_PM25_Seuil_F_2021.png", plots$THRESHOLD.F.PM25, width = 5,height = 5)
# ggsave("2_results/Map/map_PM10_Seuil_F_2021.png", plots$THRESHOLD.F.PM10, width = 5,height = 5)
# 
# ggsave("2_results/Map/map_NO2_Seuil_WHO_2021.png", plots$THRESHOLD.WHO.NO2, width = 5,height = 5)
# ggsave("2_results/Map/map_PM25_Seuil_WHO_2021.png", plots$THRESHOLD.WHO.PM25, width = 5,height = 5)
# ggsave("2_results/Map/map_PM10_Seuil_WHO_2021.png", plots$THRESHOLD.WHO.PM10, width = 5,height = 5)


map_NO2_2021 <-
  wrap_plots(
    plots_2021$NO2.WHO.target1,
    plots_2021$NO2.WHO.target2,
    plots_2021$NO2.WHO.target3,
    plots_2021$NO2.WHO.AQG,
    ncol = 4,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire NO2 par commune (2021)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2021 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave("2_results/Map/Seuil/map_combined_seuil_NO2_2021.png", map_NO2, width = 20,height = 10)

map_PM25_2021 <-
  wrap_plots(
    plots_2021$PM25.WHO.target1,
    plots_2021$PM25.WHO.target2,
    plots_2021$PM25.WHO.target3,
    plots_2021$PM25.WHO.target4,
    plots_2021$PM25.WHO.AQG,
    ncol = 5,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire PM25 par commune (2021)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2021 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave("2_results/Map/Seuil/map_combined_seuil_PM25_2021.png", map_PM25, width = 25,height = 10)

map_PM10_2021 <-
  wrap_plots(
    plots_2021$PM10.WHO.target1,
    plots_2021$PM10.WHO.target2,
    plots_2021$PM10.WHO.target3,
    plots_2021$PM10.WHO.target4,
    plots_2021$PM10.WHO.AQG,
    ncol = 5,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire PM10 par commune (2021)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2021 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave("2_results/Map/Seuil/map_combined_seuil_PM10_2021.png", map_PM10, width = 25,height = 10)


map_NO2_2015 <-
  wrap_plots(
    plots_2015$NO2.WHO.target1,
    plots_2015$NO2.WHO.target2,
    plots_2015$NO2.WHO.target3,
    plots_2015$NO2.WHO.AQG,
    ncol = 4,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire NO2 par commune (2015)",
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

ggsave("2_results/Map/Seuil/map_combined_seuil_NO2_2015.png", map_NO2, width = 20,height = 10)

map_PM25_2015 <-
  wrap_plots(
    plots_2015$PM25.WHO.target1,
    plots_2015$PM25.WHO.target2,
    plots_2015$PM25.WHO.target3,
    plots_2015$PM25.WHO.target4,
    plots_2015$PM25.WHO.AQG,
    ncol = 5,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire PM25 par commune (2015)",
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

ggsave("2_results/Map/Seuil/map_combined_seuil_PM25_2015.png", map_PM25, width = 25,height = 10)

map_PM10_2015 <-
  wrap_plots(
    plots_2015$PM10.WHO.target1,
    plots_2015$PM10.WHO.target2,
    plots_2015$PM10.WHO.target3,
    plots_2015$PM10.WHO.target4,
    plots_2015$PM10.WHO.AQG,
    ncol = 5,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire PM10 par commune (2015)",
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

ggsave("2_results/Map/Seuil/map_combined_seuil_PM10_2015.png", map_PM10, width = 25,height = 10)


map_NO2_2011 <-
  wrap_plots(
    plots_2011$NO2.WHO.target1,
    plots_2011$NO2.WHO.target2,
    plots_2011$NO2.WHO.target3,
    plots_2011$NO2.WHO.AQG,
    ncol = 4,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire NO2 par commune (2011)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2011 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave("2_results/Map/Seuil/map_combined_seuil_NO2_2011.png", map_NO2, width = 20,height = 10)

map_PM25_2011 <-
  wrap_plots(
    plots_2011$PM25.WHO.target1,
    plots_2011$PM25.WHO.target2,
    plots_2011$PM25.WHO.target3,
    plots_2011$PM25.WHO.target4,
    plots_2011$PM25.WHO.AQG,
    ncol = 5,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire PM25 par commune (2011)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2011 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave("2_results/Map/Seuil/map_combined_seuil_PM25_2011.png", map_PM25, width = 25,height = 10)

map_PM10_2011 <-
  wrap_plots(
    plots_2011$PM10.WHO.target1,
    plots_2011$PM10.WHO.target2,
    plots_2011$PM10.WHO.target3,
    plots_2011$PM10.WHO.target4,
    plots_2011$PM10.WHO.AQG,
    ncol = 5,
    nrow = 1
  ) +
  plot_annotation(
    title = "Seuils Reglementaire PM10 par commune (2011)",
    subtitle = "COG 2024",
    caption = "Données : INERIS 2011 - www.ineris.fr/fr/recherche-appui/risques-chroniques/mesure-prevision-qualite-air/qualite-air-france-metropolitaine",
    theme = theme(
      plot.background = element_rect(fill="#fbf9f4", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
      plot.caption = element_text(hjust = 1, size = 6, color="grey30")
    )
  ) &
  theme(legend.position = "bottom")

ggsave("2_results/Map/Seuil/map_combined_seuil_PM10_2011.png", map_PM10, width = 25,height = 10)


