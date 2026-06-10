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

rm(list=ls())
graphics.off()
###---Données merged---###

map_data_biv <- readRDS("0_input/map_data_COG2024_C_2021_biv.rds")
map_data <- readRDS("0_input/map_data_COG2024_C_2021.rds")
map_data_biv <-st_drop_geometry(map_data_biv)
map_data <-st_drop_geometry(map_data)

WHO.s <- read_xlsx("0_input/INERIS/Seuil_WHO.xlsx")

source("1_Script/Fonctions/fn_description_with_missing.R")
source("1_Script/Fonctions/fn_comp_CC_test.R")

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")

Seuil <-c("NO2.WHO.target1", "NO2.WHO.target2","NO2.WHO.target3","NO2.WHO.AQG",
          "PM25.WHO.target1", "PM25.WHO.target2","PM25.WHO.target3","PM2.5.WHO.target4","PM25.WHO.AQG",
          "PM10.WHO.target1", "PM10.WHO.target2","PM10.WHO.target3","PM10.WHO.target4","PM25.WHO.AQG")

sociales <-c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","Prop_Immigre", "D121","Q221","D921","RD")


map_data_biv_cc <-  map_data_biv[complete.cases(map_data_biv), ]

data_plot <- map_data_biv_cc %>%
  group_by(map_data_biv_cc$Edu.high.cl) %>%
  summarise(mean_NO2 = mean(MOY.NO2, na.rm = F))

################################################################################

get_barplot_poll <- function(polluant, S){
  
  label <- Label.S.cl[[S]]
  
  data_plot <- map_data_biv_cc %>%
    group_by(.data[[S]]) %>%
    summarise(mean_polluant = mean(.data[[polluant]], na.rm = TRUE), .groups = "drop")
  
  plot_poll <- ggplot(data_plot, aes(x = .data[[S]], y = mean_polluant, fill = factor(.data[[S]]))) +
    geom_col() +
    scale_fill_manual(values = c("1"="#CAD5AE","2"= "#4F772D", "3"="#0D1C0D")) +
    theme_minimal() +
    labs(
      x = paste0("Tercile ", label),
      y = polluant,
      fill = paste0("Tercile ", label)
    ) +
    theme(
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  
  output.file <- paste0("2_results/Description/Bivariate/Bivariate_",Label.P[polluant],"_",Label.S.cl[[S]],".jpeg")
  ggsave(output.file, plot_poll)
  
  return(plot_poll)
}


get_distrb_pol <- function(polluant){
  
  seuils <- WHO.s[WHO.s$Polluant == Label.P[polluant], ]

  seuils_values <- as.numeric(seuils[1, -1])
  seuils_values <- seuils_values[!is.na(seuils_values)]
  
  
  plot<- ggplot(map_data, aes(x = .data[[polluant]])) +
    geom_density(fill = "#69b3a2", color = "#69b3a2", alpha = 0.8) +
    geom_vline(xintercept = seuils_values,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    geom_text(
      data = data.frame(x = seuils_values),
      aes(x = x, y = 0, label = round(x, 1)),
      angle = 90,
      vjust = -0.5,
      color = "red",
      size = 3
    )+
    labs(
      title = paste("Distribution of ",Label.P[polluant]),
      x = Label.P[polluant],
      y = "Density"
    ) +
    theme_minimal()+
    theme(
      plot.background = element_rect(fill = "#fbf9f4", color = NA),
      panel.background = element_rect(fill = "#fbf9f4", color = NA)
    )
  
  output.file <- paste0("2_results/Description/Air_pollution/Distibution_",Label.P[polluant],".jpeg")
  ggsave(output.file, plot, bg = "#fbf9f4")
  
  return(plot)
}


get_distrb_S <- function(S){
  

  plot<- ggplot(map_data_biv_cc, aes(x = .data[[S]])) +
    geom_density(fill = "#69b3a2", color = "#69b3a2", alpha = 0.8) +
    labs(
      title = paste("Distribution of ",Label.S[S]),
      x = Label.S[S],
      y = "Density"
    ) +
    theme_minimal()+
    theme(
      plot.background = element_rect(fill = "#fbf9f4", color = NA),
      panel.background = element_rect(fill = "#fbf9f4", color = NA)
    )
  
  output.file <- paste0("2_results/Description/Sociale/Distibution_",S,".jpeg")
  ggsave(output.file, plot, bg = "#fbf9f4")
  
  return(plot)
}
################################################################################

Label.S.cl <- c("High_edu.cl" = "Education", "Prop_Immigre.cl" = "Immigration")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
                "Prop_Immigre" = "Immigration",
                "D121" = "1er Décile","Q221" = "Médiane","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10")

plots_list <- lapply(Polluants, function(p) get_barplot_poll(polluant = p, S = "High_edu.cl"))
plots_list[[2]]

combined_plot <- (plots_list[[1]] | plots_list[[2]]) /
  (plots_list[[3]] | plots_list[[4]])

combined_plot <- combined_plot + plot_annotation(
  title = "Moyenne des polluants par tercile haut niveau d'éducation"
)

ggsave(
  "2_results/Description/Bivariate/Bivariate_Poll_edu.png",
  combined_plot,
  width = 8,
  height = 5,
  bg = "#fbf9f4",
  dpi = 300
)


plots_list <- lapply(Polluants, function(p) get_barplot_poll(polluant = p, S = "Prop_Immigre.cl"))
plots_list[[2]]

combined_plot <- (plots_list[[1]] | plots_list[[2]]) /
  (plots_list[[3]] | plots_list[[4]])

combined_plot <- combined_plot + plot_annotation(
  title = "Moyenne des polluants par immigration"
)

ggsave(
  "2_results/Description/Bivariate_Poll_immi.png",
  combined_plot,
  width = 8,
  height = 5,
  bg = "#fbf9f4",
  dpi = 300
)


plots_list <- lapply(Polluants, function(p) get_distrb_pol(polluant = p))
plots_list <- lapply(sociales, function(s) get_distrb_S(S = s))

################################################################################

####----Bivariate----####

ggplot(map_data_biv_cc) +
  aes(x = High_edu_prop, y = MOY.NO2) +
  geom_smooth(method = "lm") +
  geom_point(colour = "#04084E", alpha = .25) +
  theme_light()

cor(map_data_biv_cc$High_edu_prop, map_data_biv_cc$MOY.NO2)
m <- lm(MOY.NO2 ~ High_edu_prop, data = map_data_biv_cc)
summary(m)


library(GGally)
ggpairs(map_data_biv_cc[,c(Polluants,"High_edu_prop","Prop_Immigre")])
ggbivariate(map_data_biv_cc, outcome = "NO2.cl", explanatory = c("High_edu.cl", "Prop_Immigre"))
