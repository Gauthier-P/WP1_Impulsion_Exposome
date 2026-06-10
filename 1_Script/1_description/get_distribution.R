###############################
####------Description------####
###############################

library(questionr)
library(patchwork)
library(gtsummary)
library(ggplot2)
library(rmapshaper)
library(sf)
library(dplyr)
library(naniar)
library(openxlsx)
library(writexl)
library(readxl)

rm(list=ls())
graphics.off()
###---Données merged---###

map_data <- readRDS("0_input/map_data_biv_seuil_2011_2015_2021_COG2024.rds")
map_data <-st_drop_geometry(map_data)

WHO.s <- read_xlsx("0_input/INERIS/Seuil_WHO.xlsx")

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10")
Seuil <-c("NO2.WHO.target1", "NO2.WHO.target2","NO2.WHO.target3","NO2.WHO.AQG",
          "PM25.WHO.target1", "PM25.WHO.target2","PM25.WHO.target3","PM2.5.WHO.target4","PM25.WHO.AQG",
          "PM10.WHO.target1", "PM10.WHO.target2","PM10.WHO.target3","PM10.WHO.target4","PM25.WHO.AQG")

sociales <-c( "High_edu_prop","Q2")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
             "Prop_Immigre" = "Immigration",
             "D121" = "1er Décile","Q2" = "Médiane","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")


################################################################################
get_distrb_pol <- function(polluant) {
  
  seuils <- WHO.s[WHO.s$Polluant == Label.P[polluant], ]
  
  seuils_labels <- colnames(seuils)[2:6]
  seuils_values <- as.numeric(seuils[1, -1])
  
  keep          <- !is.na(seuils_values)
  seuils_values <- seuils_values[keep]
  seuils_labels <- seuils_labels[keep]
  
  seuils_df <- data.frame(x = seuils_values, label = seuils_labels)
  
  make_plot <- function(data, weighted = FALSE) {
    
    base_aes <- if (weighted) {
      aes(x = .data[[polluant]], fill = factor(ANNEE),
          color = factor(ANNEE), weight = POPULATION.x)
    } else {
      aes(x = .data[[polluant]], fill = factor(ANNEE), color = factor(ANNEE))
    }
    
    ggplot(data, base_aes) +
      geom_density(alpha = 0.4) +
      geom_vline(xintercept = seuils_values,
                 linetype  = "dashed",
                 color     = "black",
                 linewidth = 0.5) +
      geom_text(
        data        = seuils_df,
        aes(x = x, y = 0, label = label),
        angle       = 90,
        vjust       = -0.5,
        color       = "black",
        size        = 3,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values  = c("2011" = "#2c7bb6", "2015" = "#69b3a2", "2021" = "#d7191c")) +
      scale_color_manual(values = c("2011" = "#2c7bb6", "2015" = "#69b3a2", "2021" = "#d7191c")) +
      labs(
        title = paste(Label.P[polluant]),
        x     = Label.P[polluant],
        y     = "Density",
        fill  = "Année",
        color = "Année"
      ) +
      theme_classic() +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position  = "bottom"
      )
  }
  
  plot   <- make_plot(map_data, weighted = FALSE)
  plot.w <- make_plot(map_data, weighted = TRUE)
  
  ggsave(
    paste0("2_results/Description/Air_pollution/Distibution_", Label.P[polluant], ".jpeg"),
    plot, bg = "#fbf9f4"
  )
  ggsave(
    paste0("2_results/Description/Air_pollution/Distibution_", Label.P[polluant], "_by_pop.jpeg"),
    plot.w, bg = "#fbf9f4"
  )
  
  return(list(plot = plot, plot_weighted = plot.w))
}

plots_list <- lapply(Polluants, function(p) get_distrb_pol(polluant = p))

plots <- lapply(plots_list, `[[`, "plot")
plots_weighted     <- lapply(plots_list, `[[`, "plot_weighted")

plot_combined <-  wrap_plots(plots)
plot_combined_w <-  wrap_plots(plots_weighted)
ggsave("2_results/Description/Air_pollution/Distibution_combined.jpeg",plot_combined, bg = "#fbf9f4", width = 8, height = 8, dpi = 300)
ggsave("2_results/Description/Air_pollution/Distibution_combined_pop.jpeg",plot_combined_w, bg = "#fbf9f4", width = 8, height = 8, dpi = 300)

get_distrb_s <- function(sociale) {
  

  make_plot <- function(data, weighted = FALSE) {
    
    base_aes <- if (weighted) {
      aes(x = .data[[sociale]], fill = factor(ANNEE),
          color = factor(ANNEE), weight = POPULATION.x)
    } else {
      aes(x = .data[[sociale]], fill = factor(ANNEE), color = factor(ANNEE))
    }
    
    ggplot(data, base_aes) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values  = c("2011" = "#2c7bb6", "2015" = "#69b3a2", "2021" = "#d7191c")) +
      scale_color_manual(values = c("2011" = "#2c7bb6", "2015" = "#69b3a2", "2021" = "#d7191c")) +
      labs(
        title = paste(Label.S[sociale]),
        x     = Label.S[sociale],
        y     = "Density",
        fill  = "Année",
        color = "Année"
      ) +
      theme_classic() +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position  = "bottom"
      )
  }
  
  plot   <- make_plot(map_data, weighted = FALSE)
  plot.w <- make_plot(map_data, weighted = TRUE)
  
  ggsave(
    paste0("2_results/Description/",sociale,"/Distibution_", Label.S[sociale], ".jpeg"),
    plot, bg = "#fbf9f4"
  )
  ggsave(
    paste0("2_results/Description/",sociale,"/Distibution_", Label.S[sociale], "_by_pop.jpeg"),
    plot.w, bg = "#fbf9f4"
  )
  
  return(list(plot = plot, plot_weighted = plot.w))
}

plots_list <- lapply(sociales, function(s) get_distrb_s(sociale = s))

plots <- lapply(plots_list, `[[`, "plot")
plots_weighted     <- lapply(plots_list, `[[`, "plot_weighted")

plot_combined <-  wrap_plots(plots)
plot_combined_w <-  wrap_plots(plots_weighted)
ggsave("2_results/Description/Distibution_combined_S.jpeg",plot_combined, bg = "#fbf9f4", width = 8, height = 5,dpi = 300)
ggsave("2_results/Description/Distibution_combined_pop_S.jpeg",plot_combined_w, bg = "#fbf9f4", width = 8, height = 5, dpi = 300)

