###############################
####-------Heat map--------####
###############################

library(gtsummary)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(questionr)
library(stringr)
library(writexl)
library(readxl)
library(openxlsx)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gridExtra)
library(pheatmap)
library(weights)

rm(list=ls())

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Seuil <-c("THRESHOLD.WHO.NO2", "THRESHOLD.WHO.PM25","THRESHOLD.WHO.PM10",
          "THRESHOLD.F.NO2", "THRESHOLD.F.PM25","THRESHOLD.F.PM10")
sociales <-c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","Prop_Immigre", "D121","Q221","D921")
###---Données merged---###

map_data_biv <- readRDS("0_input/map_data_COG2024_C_2021_biv.rds")
map_data <- readRDS("0_input/map_data_COG2024_C_2021.rds")

data.M <- map_data_biv[, c(Polluants, sociales)]

Cor <- cor(map_data_biv[, c(Polluants, sociales)], use = "pairwise.complete.obs", method = "pearson")

my_palette <- colorRampPalette(c("#04084E", "white", "#B90000"))(50)

h <- pheatmap(
  Cor,
  color = my_palette,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize_row = 7,   
  fontsize_col = 7,  
  show_rownames = TRUE,
  show_colnames = TRUE,
  display_numbers = TRUE,        
  number_format = "%.2f",      
  border_color = NA, 
  main = "Heatmap of air pollutants and social",
  filename = "2_Results/heatmap.png"
)

print(h)

png("2_Results/heatmap.png", width = 2500, height = 2500, res = 300)
grid::grid.draw(h$gtable)
dev.off()

###############

##Pondérée

vars <- c(Polluants, sociales)
data.M <- map_data_biv[, c(vars, "POPULATION")]
data.M <- data.M[complete.cases(data.M), ]


result <- cov.wt(data.M[, vars], wt = data.M$POPULATION, cor = TRUE)
Cor.w <- result$cor

my_palette <- colorRampPalette(c("#04084E", "white", "#B90000"))(50)
h <- pheatmap(
  Cor.w,
  color = my_palette,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize_row = 7,
  fontsize_col = 7,
  show_rownames = TRUE,
  show_colnames = TRUE,
  display_numbers = TRUE,
  number_format = "%.2f",
  border_color = NA,
  main = "Heatmap of air pollutants and social (weighted by population)",
  filename = "2_Results/heatmap_weighted.png"
)
h


# 
# weighted_cor <- function(x, y, w) {
#   ok <- complete.cases(x, y, w)
#   x <- x[ok]; y <- y[ok]; w <- w[ok]
#   
#   mx <- weighted.mean(x, w)
#   my <- weighted.mean(y, w)
#   
#   cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
#   var_x  <- sum(w * (x - mx)^2) / sum(w)
#   var_y  <- sum(w * (y - my)^2) / sum(w)
#   
#   cov_xy / sqrt(var_x * var_y)
# }
# 
# pop <- map_data_biv$POPULATION
# 
# weighted_cor_matrix <- matrix(NA, ncol = ncol(data.M), nrow = ncol(data.M))
# colnames(weighted_cor_matrix) <- colnames(data.M)
# rownames(weighted_cor_matrix) <- colnames(data.M)
# 
# for(i in 1:ncol(data.M)){
#   for(j in 1:ncol(data.M)){
#     weighted_cor_matrix[i, j] <- weighted_cor(
#       data.M[, i],
#       data.M[, j],
#       pop
#     )
#   }
# }
# 
# png("2_Results/heatmap_weighted.png", width = 2500, height = 2500, res = 300)
# 
# h <-pheatmap(
#   weighted_cor_matrix,
#   color = my_palette,
#   cluster_rows = FALSE,
#   cluster_cols = FALSE,
#   display_numbers = TRUE,
#   number_format = "%.2f",
#   main = "Corrélations pondérées par la population",
#   filename = "2_Results/heatmap_weighted.png"
# )
# print(h)
# dev.off()
