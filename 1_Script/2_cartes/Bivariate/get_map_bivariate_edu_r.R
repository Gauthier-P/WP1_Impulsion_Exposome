library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(cowplot)
library(classInt)
library(grid)
library(gridtext)
library(cowplot)
library(gtable)
library(grid)
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(tidyverse)
library(patchwork)


rm(list=ls())
graphics.off()

###---Données merged---###


map_data_biv_2021 <-  readRDS("0_input/map_data_COG2024_C_2021_biv.rds")
map_data_biv_seuil_2021 <-  readRDS("0_input/map_data_COG2024_C_2021_biv_seuil.rds")

map_data_biv_2015 <-  readRDS("0_input/map_data_COG2024_C_2015_biv.rds")
map_data_biv_seuil_2015 <-  readRDS("0_input/map_data_COG2024_C_2015_biv_seuil.rds")

map_data_biv_2011 <-  readRDS("0_input/map_data_COG2024_C_2011_biv.rds")
map_data_biv_seuil_2011 <-  readRDS("0_input/map_data_COG2024_C_2011_biv_seuil.rds")

villes <- map_data_biv_2021 %>%
  filter(Pop_tot > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])

polluants <- c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
edu_vars  <- c( "High_edu_prop", "edu_ratio")
immi_var  <- "Prop_Immigre"

polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
immi_cl      <- paste0(str_remove(immi_var, "_prop"),".cl")

edu_pol  <- as.vector(outer(polluants_cl, edu_cl, paste, sep = "_"))
immi_pol <- as.vector(outer(polluants_cl, immi_cl, paste, sep = "_"))

################################################################################

biv_palette_blured <-  c(
  "1-1" = "#CBBED0", "2-1" = "#BB7C8F", "3-1" = "#AE3A4C",
  "1-2" = "#89A0C9", "2-2" = "#7F6A89", "3-2" = "#77334B",
  "1-3" = "#4985C1", "2-3" = "#415786", "3-3" = "#3E2848"
)

plot_bivar <- function(data, biv_var, titre){
  ggplot(data) +
    geom_sf(aes(fill = .data[[biv_var]]), color = NA) +
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
    coord_sf(expand = FALSE)+
    scale_fill_manual(values = biv_palette_blured,limits = names(biv_palette_blured), na.value = "grey90") +
    labs(title = titre) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#fbf9f4", color = NA),
      panel.background = element_rect(fill = "#fbf9f4", colour = NA),
      plot.title = element_markdown(hjust = 0.5, size = 9,face = "bold"),
      plot.margin = margin(0, 0, 0, 0, "pt"), 
      legend.position = "none"
    )+
    annotate(
      "text",
      x = -Inf, y = -Inf,  # Position en haut à gauche
      label = "Source : INERIS 2021|INSEE 2021",
      hjust = 0, vjust = -0.75,
      size = 2, color = "black", fontface = "italic")
}

### --- Fonction légende --- ###
make_legend <- function(Sociale, Polluant){
  expand.grid(Pollut = 1:3, Edu = 1:3) %>%
    mutate(cat = paste0(Pollut, "-", Edu)) %>%
    ggplot(aes(Pollut, Edu, fill = cat)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = biv_palette_blured) +
    scale_x_continuous(breaks = 1:3) +
    scale_y_continuous(breaks = 1:3) +
    labs(x = paste0(Polluant," polluant →"), y = Sociale) +
    theme_minimal(base_size = 6) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "#fbf9f4", color = NA)
    )
}

################################################################################

legend  <- make_legend("Tercile ratio →", "Tercile")

polluants_label <- c("NO<sub>2</sub>","O<sub>3</sub>","PM<sub>2.5</sub>","PM<sub>10</sub>")
polluants <- c("NO2","O3","PM25","PM10")
edu_levels <- c("_r")
edu_label <- c("High" = "élevé", "_r" = "ratio")

edu_pol  <- as.vector(outer(polluants_cl, edu_cl, paste, sep = "_"))
immi_pol <- as.vector(outer(polluants_cl, immi_cl, paste, sep = "_"))

titles <- expand.grid(pol = polluants, edu = edu_levels) %>%
  mutate(
    var   = paste0(pol, "_Edu", edu),
    titre = paste0(
      "Exposition au ", polluants_label[pol],
      " et ratio low/high educated "
    )
  )


for(i in seq_len(nrow(titles))){
  
  base_plot <- plot_bivar(map_data_biv_2021, titles$var[i], paste0(titles$titre[i], " (2021)"))
  
  legend_use <- if(grepl("Low", titles$var[i])) legend else legend
  
  final_plot <- ggdraw(base_plot) +
    draw_plot(legend_use, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
  
  ggsave(
    paste0("2_results/Map/2021/Bivariate/Tercile/Education/map_", titles$var[i], "_2021.png"),
    final_plot,
    width = 5, height = 5,
    bg = "#fbf9f4", dpi = 300
  )
}

for(i in seq_len(nrow(titles))){
  
  base_plot <- plot_bivar(map_data_biv_2015, titles$var[i], paste0(titles$titre[i], " (2015)"))
  
  legend_use <- if(grepl("Low", titles$var[i])) legend else legend
  
  final_plot <- ggdraw(base_plot) +
    draw_plot(legend_use, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
  
  ggsave(
    paste0("2_results/Map/2015/Bivariate/Tercile/Education/map_", titles$var[i], "_2015.png"),
    final_plot,
    width = 5, height = 5,
    bg = "#fbf9f4", dpi = 300
  )
}
for(i in seq_len(nrow(titles))){
  
  base_plot <- plot_bivar(map_data_biv_2011, titles$var[i], paste0(titles$titre[i], " (2021)"))
  
  legend_use <- if(grepl("Low", titles$var[i])) legend else legend
  
  final_plot <- ggdraw(base_plot) +
    draw_plot(legend_use, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
  
  ggsave(
    paste0("2_results/Map/2011/Bivariate/Tercile/Education/map_", titles$var[i], "_2011.png"),
    final_plot,
    width = 5, height = 5,
    bg = "#fbf9f4", dpi = 300
  )
}

################################################################################

edu_vars  <- c("High_edu_prop")

polluants_label <- c("NO<sub>2</sub>","PM<sub>2.5</sub>","PM<sub>10</sub>")
polluants <- c("NO2","PM25","PM10")
edu_levels <- c( "High")
edu_label <- c("High" = "élevé")

edu_pol  <- as.vector(outer(polluants_cl, edu_cl, paste, sep = "_"))
immi_pol <- as.vector(outer(polluants_cl, immi_cl, paste, sep = "_"))

titles <- expand.grid(pol = polluants, edu = edu_levels) %>%
  mutate(
    var   = paste0(pol, "_Edu", edu),
    titre = paste0(
      "Exposition au ", polluants_label[pol],
      " et proportion d'individus avec un niveau d’éducation ",
      edu_label[edu]
    )
  )


for(i in seq_len(nrow(titles))){
  
  base_plot <- plot_bivar(map_data_biv_seuil_2021, titles$var[i], paste0(titles$titre[i], " (2021)"))
  
  legend_use <- if(grepl("Low", titles$var[i])) legend else legend
  
  final_plot <- ggdraw(base_plot) +
    draw_plot(legend_use, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
  
  ggsave(
    paste0("2_results/Map/2021/Bivariate/Seuil/Education/map_", titles$var[i], "_2021.png"),
    final_plot,
    width = 5, height = 5,
    bg = "#fbf9f4", dpi = 300
  )
}

for(i in seq_len(nrow(titles))){
  
  base_plot <- plot_bivar(map_data_biv_seuil_2015, titles$var[i], paste0(titles$titre[i], " (2015)"))
  
  legend_use <- if(grepl("Low", titles$var[i])) legend else legend
  
  final_plot <- ggdraw(base_plot) +
    draw_plot(legend_use, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
  
  ggsave(
    paste0("2_results/Map/2015/Bivariate/Seuil/Education/map_", titles$var[i], "_2015.png"),
    final_plot,
    width = 5, height = 5,
    bg = "#fbf9f4", dpi = 300
  )
}

for(i in seq_len(nrow(titles))){
  
  base_plot <- plot_bivar(map_data_biv_seuil_2011, titles$var[i], paste0(titles$titre[i], " (2011)"))
  
  legend_use <- if(grepl("Low", titles$var[i])) legend else legend
  
  final_plot <- ggdraw(base_plot) +
    draw_plot(legend_use, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
  
  ggsave(
    paste0("2_results/Map/2011/Bivariate/Seuil/Education/map_", titles$var[i], "_2011.png"),
    final_plot,
    width = 5, height = 5,
    bg = "#fbf9f4", dpi = 300
  )
}

