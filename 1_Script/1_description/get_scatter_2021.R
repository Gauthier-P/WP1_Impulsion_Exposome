###############################
####------Regression-------####
###############################

library(questionr)
library(gtsummary)
library(ggplot2)
library(ggrepel)
library(rmapshaper)
library(sf)
library(dplyr)
library(naniar)
library(openxlsx)
library(geomtextpath)
library(cowplot)
library(patchwork)
library(broom)
library(MetBrewer)


rm(list=ls())
graphics.off()
###---Données merged---###

map_data_biv <- readRDS("0_input/map_data_COG2024_C_2021_biv.rds")
map_data <- readRDS("0_input/map_data_COG2024_C_2021.rds")
map_data_biv <-st_drop_geometry(map_data_biv)
map_data <-st_drop_geometry(map_data)

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10",
             "MOY.NO2_norm" = "NO2", "MOY.O3_norm" = "O3", "MOY.PM25_norm" ="PM2.5" ,"MOY.PM10_norm"= "PM10")

Seuil <-c("THRESHOLD.WHO.NO2", "THRESHOLD.WHO.PM25","THRESHOLD.WHO.PM10",
          "THRESHOLD.F.NO2", "THRESHOLD.F.PM25","THRESHOLD.F.PM10")

sociales <-c( "High_edu_prop","Q221")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
             "Prop_Immigre" = "Immigration",
             "D121" = "1er Décile","Q221" = "Médiane","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")

urbanicity <-c("All","Urbain", "Urbain intermédiaire", "Rural")

################################################################################
##-normalized--##

vars_to_transform <- map_data_biv[, c(Polluants,sociales)] 
vars_transformed <- scale(vars_to_transform)
colnames(vars_transformed) <- paste0(colnames(vars_to_transform), "_norm")
map_data_biv <- cbind(map_data_biv, vars_transformed)

Polluants_norm <- paste0(Polluants, "_norm")
Sociales_norm <- paste0(sociales, "_norm")

map_data_U  <- subset(map_data_biv, map_data_biv$DENS == 1)   
map_data_UI <- subset(map_data_biv, map_data_biv$DENS == 2) 
map_data_R  <- subset(map_data_biv, map_data_biv$DENS == 3)

datasets <- c("map_data_biv","map_data_U","map_data_UI","map_data_R")


################################################################################

mod0 <- lm(MOY.NO2 ~ High_edu_prop, data = map_data_biv)
summary(mod0)

mod1 <- lm(MOY.NO2 ~ High_edu_prop, data = map_data_biv, weights = POPULATION.x)
summary(mod1)

P.scatter <-  ggplot(map_data_biv, aes(x = High_edu_prop, y = MOY.NO2, color = LIBDENS))+
  geom_point() +
  geom_labelsmooth(aes(label = LIBDENS), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_light()

scatter_pol <- function(plot.data, polluant, sociale, urbanicity, ylim = NULL){
  
  plot <- ggplot(plot.data, aes(x = .data[[sociale]], y = .data[[polluant]]))+
    geom_point(colour = "#04084E", alpha = .25) +
    geom_smooth(method = "lm", color="red", fill="#69b3a2", se=TRUE) +
    theme_light()+
    ggtitle(urbanicity)+
    labs(x = Label.S[ gsub("_norm$", "", sociale)], y = Label.P[ gsub("_norm$", "", polluant)])+ 
    coord_cartesian(ylim = ylim) 
  
  output.file  <- paste0("2_Results/Regression/",urbanicity,"/",polluant,"_" ,sociale, ".png" )
  ggsave(output.file, plot, width = 4, height = 4)
  
}

scatter_pol_w <- function(plot.data, polluant, sociale, urbanicity, ylim = NULL){
  
  plot <- ggplot(plot.data, aes(x = .data[[sociale]], y = .data[[polluant]], weight = POPULATION.x))+
    geom_point(aes(size = POPULATION.x), colour = "#04084E", alpha = .25) +
    scale_size_continuous(range = c(1, 8), guide = "none") +
    geom_smooth(method = "lm", color="red", fill="#69b3a2", se=TRUE) +
    theme_light()+
    ggtitle(urbanicity)+
    labs(x = Label.S[ gsub("_norm$", "", sociale)], y = Label.P[ gsub("_norm$", "", polluant)])+ 
    coord_cartesian(ylim = ylim) 
  
  output.file  <- paste0("2_Results/Regression/Weighted/",urbanicity,"/",polluant,"_" ,sociale, "_weighted.png" )
  ggsave(output.file, plot, width = 4, height = 4)
  
}

# for (u in 1:length(urbanicity)){
#   dataset <- get(datasets[u])
# 
#   for (p in Polluants){
#     ylim_raw  <- range(sapply(datasets, function(d) range(get(d)[[p]], na.rm = TRUE)))
#     ylim_norm <- range(sapply(datasets, function(d) range(get(d)[[paste0(p, "_norm")]], na.rm = TRUE)))
#     for (s in sociales){
#       scatter_pol_w(dataset,p,s,urbanicity[u], ylim = ylim_raw)
#       scatter_pol_w(dataset,paste0(p,"_norm"),paste0(s,"_norm"),urbanicity[u], ylim = ylim_norm)
#       scatter_pol(dataset,p,s,urbanicity[u], ylim = ylim_raw)
#       scatter_pol(dataset,paste0(p,"_norm"),paste0(s,"_norm"),urbanicity[u], ylim = ylim_norm)
#     }
#   }
# }

################################################################################


cities <- c(
  "Paris", "Marseille", "Lyon", "Toulouse", "Nice", "Nantes", 
  "Montpellier", "Strasbourg", "Bordeaux", "Lille")

map_data_biv <- map_data_biv %>%
  mutate(
    label = ifelse(NOM %in% cities, NOM, "")
  )
plot.data <- map_data_biv
polluant  <- "MOY.NO2"
sociale   <- "High_edu_prop"

plot.fn <- function(plot.data, polluant, sociale){
  
  plot.all <-ggplot(plot.data, aes(x = .data[[sociale]], 
                                 y = .data[[polluant]], 
                                 weight = POPULATION.x)) +
    geom_point(aes(size = POPULATION.x), colour = "#264653", alpha = 0.25) +
    geom_smooth(method = "lm", se = TRUE, colour = "#264653",alpha = 0.1) +
    theme_light()+
    ggtitle("Ensemble") +
    geom_text_repel(
      aes(label = label),
      color = "black",
      size = 9/.pt, # font size 9 pt
      point.padding = 0.1, 
      box.padding = 0.6,
      min.segment.length = 0.1,
      max.overlaps = 1000,
      seed = 220798 # For reproducibility reasons
    ) +
    theme(legend.position = "none")+
    labs(colour = "Densité", fill = "Densité", size = "Population",
         x = Label.S[gsub("_norm$", "", sociale)],
         y = Label.P[gsub("_norm$", "", polluant)])
  
  plot.u <-ggplot(plot.data, aes(x = .data[[sociale]], 
                               y = .data[[polluant]], 
                               colour = LIBDENS, 
                               fill = LIBDENS,
                               weight = POPULATION.x)) +
    geom_point(aes(size = POPULATION.x), alpha = 0.25) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
    theme_light()+
    ggtitle("Par densité d'habitat") +
    geom_text_repel(
      aes(label = label),
      color = "black",
      size = 9/.pt, # font size 9 pt
      point.padding = 0.1, 
      box.padding = 0.6,
      min.segment.length = 0,
      max.overlaps = 1000,
      seed = 7654 # For reproducibility reasons
    ) +
    scale_color_manual(values = c("All" = "#264653",
                                  "Rural"= "#BCC184",
                                  "Urbain intermédiaire"= "#F4A261",
                                  "Urbain dense"= "#C53D1B"))+
    scale_fill_manual(values = c("All" = "#264653",
                                  "Rural"= "#BCC184",
                                  "Urbain intermédiaire"= "#F4A261",
                                  "Urbain dense"= "#C53D1B"))+    theme_light() +
    theme(legend.position = "right")+
    labs(colour = "Densité", fill = "Densité", size = "Population",
         x = Label.S[gsub("_norm$", "", sociale)],
         y = Label.P[gsub("_norm$", "", polluant)])

  plot <- wrap_plots(list(plot.all, plot.u), ncol =2) + plot_layout(guides = "collect") 
  plot
  
  output.file  <- paste0("2_Results/Regression/Scatterplot/",sociale,"/")
  ggsave(paste0(output.file,polluant, "by_u_weighted.png" ), plot, width = 10, height = 5, dpi = 250)
  ggsave(paste0(output.file,polluant, "All_weighted.png" ), plot.all, width = 5, height = 5, dpi = 250)
  
}

plots_list <- list()
for (p in Polluants){
    for (s in sociales){
      plot.fn(map_data_biv,p,s)
    }
}

