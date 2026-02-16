rm(list=ls())
graphics.off()

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
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
library(patchwork)



###---Données merged---###

Merged_AIR_EDU_2021 <- readRDS("0_input/Merged/Merged_AIR_EDU_2021.rds")
INERIS_Air_2021 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021.rds")
################################################################################

communes_sf_2024 <- st_read("0_Input/Contour_commune/2024/COMMUNE.shp")
communes_sf_2018 <- st_read("0_Input/Contour_commune/2018/COMMUNE.shp")

map_data_2024 <- communes_sf_2024 %>%
  mutate(CODGEO = as.character(INSEE_COM)) %>%
  inner_join(Merged_AIR_EDU_2021, by = "CODGEO")

################################################################################

map_data_biv <- map_data_2024 %>%
  mutate(
    NO2.cl = cut(
      MOY.NO2,
      breaks = classIntervals(MOY.NO2, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    O3.cl = cut(
      MOY.O3,
      breaks = classIntervals(MOY.O3, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    PM25.cl = cut(
      MOY.PM25,
      breaks = classIntervals(MOY.PM25, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    PM10.cl = cut(
      MOY.PM10,
      breaks = classIntervals(MOY.PM10, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    Edu.low.cl = cut(
      Low_edu_prop,
      breaks = classIntervals(Low_edu_prop, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ), 
    Edu.med.cl = cut(
      Medium_edu_prop,
      breaks = classIntervals(Medium_edu_prop, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    Edu.high.cl = cut(
      High_edu_prop,
      breaks = classIntervals(High_edu_prop, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    )
  ) 

breaks_NO2     <- classIntervals(map_data_2024$MOY.NO2[!is.na(map_data_2024$MOY.NO2)], n = 3, style = "quantile")$brks
breaks_O3      <- classIntervals(map_data_2024$MOY.O3[!is.na(map_data_2024$MOY.O3)], n = 3, style = "quantile")$brks
breaks_PM25    <- classIntervals(map_data_2024$MOY.PM25[!is.na(map_data_2024$MOY.PM25)], n = 3, style = "quantile")$brks
breaks_PM10    <- classIntervals(map_data_2024$MOY.PM10[!is.na(map_data_2024$MOY.PM10)], n = 3, style = "quantile")$brks
breaks_EduLow  <- classIntervals(map_data_2024$Low_edu_prop[!is.na(map_data_2024$Low_edu_prop)], n = 3, style = "quantile")$brks
breaks_EduMed  <- classIntervals(map_data_2024$Medium_edu_prop[!is.na(map_data_2024$Medium_edu_prop)], n = 3, style = "quantile")$brks
breaks_EduHigh <- classIntervals(map_data_2024$High_edu_prop[!is.na(map_data_2024$High_edu_prop)], n = 3, style = "quantile")$brks

map_data_biv <- map_data_2024 %>%
  mutate(
    NO2.cl = cut(MOY.NO2, breaks = breaks_NO2, include.lowest = TRUE, labels = 1:3),
    O3.cl = cut(MOY.O3, breaks = breaks_O3, include.lowest = TRUE, labels = 1:3),
    PM25.cl = cut(MOY.PM25, breaks = breaks_PM25, include.lowest = TRUE, labels = 1:3),
    PM10.cl = cut(MOY.PM10, breaks = breaks_PM10, include.lowest = TRUE, labels = 1:3),
    Edu.low.cl  = cut(Low_edu_prop, breaks = breaks_EduLow, include.lowest = TRUE, labels = 1:3),
    Edu.med.cl  = cut(Medium_edu_prop, breaks = breaks_EduMed, include.lowest = TRUE, labels = 1:3),
    Edu.high.cl = cut(High_edu_prop, breaks = breaks_EduHigh, include.lowest = TRUE, labels = 1:3)
  )%>%
  mutate(
    NO2_EduLow  = ifelse(is.na(NO2.cl)  | is.na(Edu.low.cl),  NA, paste0(NO2.cl, "-", Edu.low.cl)),
    NO2_EduMed  = ifelse(is.na(NO2.cl)  | is.na(Edu.med.cl),  NA, paste0(NO2.cl, "-", Edu.med.cl)),
    NO2_EduHigh = ifelse(is.na(NO2.cl)  | is.na(Edu.high.cl), NA, paste0(NO2.cl, "-", Edu.high.cl)),
    
    O3_EduLow  = ifelse(is.na(O3.cl)  | is.na(Edu.low.cl),  NA, paste0(O3.cl, "-", Edu.low.cl)),
    O3_EduMed  = ifelse(is.na(O3.cl)  | is.na(Edu.med.cl),  NA, paste0(O3.cl, "-", Edu.med.cl)),
    O3_EduHigh = ifelse(is.na(O3.cl)  | is.na(Edu.high.cl), NA, paste0(O3.cl, "-", Edu.high.cl)),
    
    PM25_EduLow  = ifelse(is.na(PM25.cl)  | is.na(Edu.low.cl),  NA, paste0(PM25.cl, "-", Edu.low.cl)),
    PM25_EduMed  = ifelse(is.na(PM25.cl)  | is.na(Edu.med.cl),  NA, paste0(PM25.cl, "-", Edu.med.cl)),
    PM25_EduHigh = ifelse(is.na(PM25.cl)  | is.na(Edu.high.cl), NA, paste0(PM25.cl, "-", Edu.high.cl)),
    
    PM10_EduLow  = ifelse(is.na(PM10.cl)  | is.na(Edu.low.cl),  NA, paste0(PM10.cl, "-", Edu.low.cl)),
    PM10_EduMed  = ifelse(is.na(PM10.cl)  | is.na(Edu.med.cl),  NA, paste0(PM10.cl, "-", Edu.med.cl)),
    PM10_EduHigh = ifelse(is.na(PM10.cl)  | is.na(Edu.high.cl), NA, paste0(PM10.cl, "-", Edu.high.cl))
  )

random_index <- sample(1:3000, 100)
verif_tercile <- map_data_biv[random_index, c("NO2.cl", "MOY.NO2",
                                              "O3.cl",  "MOY.O3",
                                              "PM25.cl","MOY.PM25", 
                                              "PM10.cl", "MOY.PM10",
                                              "Edu.low.cl","Low_edu_prop",
                                              "Edu.med.cl", "Medium_edu_prop", 
                                              "Edu.high.cl", "High_edu_prop")]
biv_palette <- c(
  "1-1" = "#e8e8e8",
  "2-1" = "#ace4e4",
  "3-1" = "#5ac8c8",
  "1-2" = "#dfb0d6",
  "2-2" = "#a5add3",
  "3-2" = "#5698b9",
  "1-3" = "#be64ac",
  "2-3" = "#8c62aa",
  "3-3" = "#3b4994"
)
# biv_palette_blured <-  c(
#   "1-1" = "#E8E8E8", "2-1" = "#E4ACAC", "3-1" = "#C75959",
#   "1-2" = "#AFD4DE", "2-2" = "#AD9AA5", "3-2" = "#985356",
#   "1-3" = "#64ACBE", "2-3" = "#627F8C", "3-3" = "#574942"
# )
biv_palette_blured <-  c(
  "1-1" = "#CBBED0", "2-1" = "#BB7C8F", "3-1" = "#AE3A4C",
  "1-2" = "#89A0C9", "2-2" = "#7F6A89", "3-2" = "#77334B",
  "1-3" = "#4985C1", "2-3" = "#415786", "3-3" = "#3E2848"
)


plot_bivar <- function(data, biv_var, titre){
  ggplot(data) +
    geom_sf(aes(fill = .data[[biv_var]]), color = NA) +
    coord_sf(expand = FALSE)+
    scale_fill_manual(values = biv_palette_blured, na.value = "grey90") +
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

biv_vars <- c(
  "NO2_EduLow","NO2_EduMed","NO2_EduHigh",
  "O3_EduLow","O3_EduMed","O3_EduHigh",
  "PM25_EduLow","PM25_EduMed","PM25_EduHigh",
  "PM10_EduLow","PM10_EduMed","PM10_EduHigh"
)

biv_titles <- c(
  "Exposition au NO<sub>2</sub> et proportion d'individus avec un niveau d’éducation faible",
  "Exposition au NO<sub>2</sub> et proportion d'individus avec un niveau d’éducation moyen",
  "Exposition au NO<sub>2</sub> et proportion d'individus avec un niveau d’éducation élevé",
  
  "Exposition à l'O<sub>3</sub> et proportion d'individus avec un niveau d’éducation faible",
  "Exposition à l'O<sub>3</sub> et proportion d'individus avec un niveau d’éducation moyen",
  "Exposition à l'O<sub>3</sub> et proportion d'individus avec un niveau d’éducation élevé",
  
  "Exposition aux PM<sub>2.5</sub> et proportion d'individus avec un niveau d’éducation faible",
  "Exposition aux PM<sub>2.5</sub> et proportion d'individus avec un niveau d’éducation moyen",
  "Exposition aux PM<sub>2.5</sub> et proportion d'individus avec un niveau d’éducation élevé",
  
  "Exposition aux PM<sub>10</sub> et proportion d'individus avec un niveau d’éducation faible",
  "Exposition aux PM<sub>10</sub> et proportion d'individus avec un niveau d’éducation moyen",
  "Exposition aux PM<sub>10</sub> et proportion d'individus avec un niveau d’éducation élevé"
)

plots_bivar <- list()

for(i in seq_along(biv_vars)){
  plots_bivar[[biv_vars[i]]] <- plot_bivar(map_data_biv, biv_vars[i], biv_titles[i])
}

legend_df <- expand.grid(
  Pollut = 1:3,    # horizontal = pollution
  Edu    = 1:3     # vertical = niveau d'éducation
)
legend_df$cat <- paste0(legend_df$Pollut, "-", legend_df$Edu)

# Palette 3x3 (même que les cartes)
biv_palette <- c(
  "1-1" = "#e8e8e8","2-1" = "#ace4e4","3-1" = "#5ac8c8",
  "1-2" = "#dfb0d6","2-2" = "#a5add3","3-2" = "#5698b9",
  "1-3" = "#be64ac","2-3" = "#8c62aa","3-3" = "#3b4994"
)




legend_plot_low <- ggplot(legend_df, aes(x = Pollut, y = Edu, fill = cat)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = biv_palette_blured) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  scale_y_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Tercile polluant →", y = "Tercile low éducation →") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 6),
    axis.text  = element_text(size = 5),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background  = element_rect(fill = "#fbf9f4", colour = NA),
    panel.background = element_rect(fill = "#fbf9f4", colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

legend_plot_medium <- ggplot(legend_df, aes(x = Pollut, y = Edu, fill = cat)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = biv_palette_blured) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  scale_y_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Tercile polluant →", y = "Tercile medium éducation →") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 6),
    axis.text  = element_text(size = 5),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#fbf9f4", color = NA)
  )

legend_plot_high <- ggplot(legend_df, aes(x = Pollut, y = Edu, fill = cat)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = biv_palette_blured) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  scale_y_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Tercile polluant →", y = "Tercile high éducation →") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 6),
    axis.text  = element_text(size = 5),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background  = element_rect(fill = "#fbf9f4", colour = NA),
    panel.background = element_rect(fill = "#fbf9f4", colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  )


NO2_EduLow <- ggdraw(plots_bivar$NO2_EduLow) +
  draw_plot(legend_plot_low, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
NO2_EduHigh <- ggdraw(plots_bivar$NO2_EduHigh) +
  draw_plot(legend_plot_high, x = 0.05, y = 0.25, width = 0.20, height = 0.20)

O3_EduLow <- ggdraw(plots_bivar$O3_EduLow)  +
  draw_plot(legend_plot_low, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
O3_EduHigh <- ggdraw(plots_bivar$O3_EduHigh)+
  draw_plot(legend_plot_high, x = 0.05, y = 0.25, width = 0.20, height = 0.20)

PM25_EduLow <- ggdraw(plots_bivar$PM25_EduLow) +
  draw_plot(legend_plot_low, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
PM25_EduHigh <- ggdraw(plots_bivar$PM25_EduHigh)+
  draw_plot(legend_plot_high, x = 0.05, y = 0.25, width = 0.20, height = 0.20)

PM10_EduLow <- ggdraw(plots_bivar$PM10_EduLow) +
  draw_plot(legend_plot_low, x = 0.05, y = 0.25, width = 0.20, height = 0.20)
PM10_EduHigh <- ggdraw(plots_bivar$PM10_EduHigh)+
  draw_plot(legend_plot_high, x = 0.05, y = 0.25, width = 0.20, height = 0.20)


ggsave("2_results/Map/Bivariate/map_NO2_Low_2021.png", NO2_EduLow, width = 5, height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_NO2_High_2021.png", NO2_EduHigh, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_O3_Low_2021.png", O3_EduLow, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_O3_High_2021.png", O3_EduHigh, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_PM25_Low_2021.png", PM25_EduLow, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_PM25_High_2021.png", PM25_EduHigh, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_PM10_Low_2021.png", PM10_EduLow, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)
ggsave("2_results/Map/Bivariate/map_PM10_High_2021.png", PM10_EduHigh, width = 5,height = 5, bg = "#fbf9f4", dpi = 300)


################################################################################
combined_plot <- NO2_EduLow + legend_plot_low +
  plot_layout(ncol = 2, widths = c(4, 1))

ggsave(
  "2_results/Map/Bivariate/NO2_EduLow_with_legend.png",
  combined_plot,
  width = 8,
  height = 5,
  bg = "#fbf9f4",
  dpi = 300
)
