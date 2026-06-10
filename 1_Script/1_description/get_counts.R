#######################
####-----Count-----####
#######################

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(readxl)
library(openxlsx)
library(terra)
library(COGugaison)

rm(list = ls())

map_data <- readRDS("0_Input/map_data_COG2024_C_2021_biv_seuil.rds")

map_data_U  <- subset(map_data, map_data$DENS == 1)   
map_data_UI <- subset(map_data, map_data$DENS == 2) 
map_data_R  <- subset(map_data, map_data$DENS == 3)

################################################################################

wb <- createWorkbook()
wb.p <- createWorkbook()

table_plo_Soc <-  function(wb, sheet, data, var1, var2, name, nc, nr) {
  tab <- table(data[[var1]], data[[var2]])
  tab_pct <- prop.table(tab, margin = 1) * 100
  
  tab_combined <- as.data.frame.matrix(tab)
  for (i in 1:nrow(tab_combined)) {
    for (j in 1:ncol(tab_combined)) {
      tab_combined[i, j] <- paste0(tab[i, j], " (", round(tab_pct[i, j], 1), "%)")
    }
  }
  
  writeData(wb, sheet, paste(var1),startCol = nc, startRow = nr+2)
  writeData(wb, sheet, paste(var2),startCol = nc+2, startRow = nr)
  
  writeData(wb, sheet,"3" ,startCol = nc+1, startRow = nr+2)
  writeData(wb, sheet,"2" ,startCol = nc+1, startRow = nr+3)
  writeData(wb, sheet,"1" ,startCol = nc+1, startRow = nr+4)
  
  
  writeData(wb, sheet, tab_combined, startCol = nc+2, startRow = nr+1)
}

table_plo_Soc.p <-  function(wb, sheet, data, var1, var2, name, nc, nr) {
  tab <- xtabs(data[["POPULATION.x"]] ~ data[[var1]] + data[[var2]])
  tab_pct <- prop.table(tab, margin = 1) * 100
  
  tab_combined <- as.data.frame.matrix(tab)
  for (i in 1:nrow(tab_combined)) {
    for (j in 1:ncol(tab_combined)) {
      tab_combined[i, j] <- paste0(round(tab_pct[i, j], 1), "%")
    }
  }
  
  writeData(wb, sheet, paste(var1),startCol = nc, startRow = nr+2)
  writeData(wb, sheet, paste(var2),startCol = nc+2, startRow = nr)
  
  writeData(wb, sheet,"3" ,startCol = nc+1, startRow = nr+2)
  writeData(wb, sheet,"2" ,startCol = nc+1, startRow = nr+3)
  writeData(wb, sheet,"1" ,startCol = nc+1, startRow = nr+4)
  
  
  writeData(wb, sheet, tab_combined, startCol = nc+2, startRow = nr+1)
}

datasets <- list(
  ALL = map_data,
  URBAIN = map_data_U,
  INTER = map_data_UI,
  RURAL = map_data_R
)



social <- c("High_edu.cl", "Q221.cl")
polluants <- c("NO2.cl", "PM25.cl", "PM10.cl")

# Boucle
for (urbanicity in names(datasets)) {
  data <- datasets[[urbanicity]]
  ncol <- 1
  nrow <- 1
  sheet_name <- paste(urbanicity)
  addWorksheet(wb, sheet_name)
  addWorksheet(wb.p, sheet_name)
  for (s in social) {
    for (p in polluants) {
      table_plo_Soc(wb, sheet_name, data, s, p, urbanicity, nc=ncol, nr =nrow)
      table_plo_Soc.p(wb.p, sheet_name, data, s, p, urbanicity, nc=ncol, nr =nrow)
      ncol <- ncol + 6
      
    }
    nrow <- nrow + 6
    ncol <- 1
  }
}

saveWorkbook(wb, file="2_Results/Description/table_croisee_pol_soc_raw.xlsx", overwrite = T)
saveWorkbook(wb.p, file="2_Results/Description/table_croisee_pol_soc_weighted_raw.xlsx", overwrite = T)

# 
# ###---------ALL---------###
# 
# #---NO2---#
# #colonne polluant/ligne sociale
# table(map_data$High_edu.cl,map_data$NO2.cl)
# table(map_data$Q221.cl,map_data$NO2.cl)
# 
# #---PM25---#
# table(map_data$High_edu.cl,map_data$PM25.cl)
# table(map_data$Q221.cl,map_data$PM25.cl)
# 
# #---PM10---#
# table(map_data$High_edu.cl,map_data$PM10.cl)
# table(map_data$Q221.cl,map_data$PM10.cl)
# 
# ###---------URBAIN---------###
# 
# #---NO2---#
# #colonne polluant/ligne sociale
# table(map_data_U$High_edu.cl,map_data_U$NO2.cl)
# table(map_data_U$Q221.cl,map_data_U$NO2.cl)
# 
# ###---PM25---###
# table(map_data_U$High_edu.cl,map_data_U$PM25.cl)
# table(map_data_U$Q221.cl,map_data_U$PM25.cl)
# 
# ####---PM10---###
# table(map_data_U$High_edu.cl,map_data_U$PM10.cl)
# table(map_data_U$Q221.cl,map_data_U$PM10.cl)
# 
# ###---------URBAIN INTERMEDIAIRE---------###
# 
# #---NO2---#
# #colonne polluant/ligne sociale
# table(map_data_UI$High_edu.cl,map_data_UI$NO2.cl)
# table(map_data_UI$Q221.cl,map_data_UI$NO2.cl)
# 
# ###---PM25---###
# table(map_data_UI$High_edu.cl,map_data_UI$PM25.cl)
# table(map_data_UI$Q221.cl,map_data_UI$PM25.cl)
# 
# ####---PM10---###
# table(map_data_UI$High_edu.cl,map_data_UI$PM10.cl)
# table(map_data_UI$Q221.cl,map_data_UI$PM10.cl)
# 
# ###---------RURAL---------###
# 
# #---NO2---#
# #colonne polluant/ligne sociale
# table(map_data_R$High_edu.cl,map_data_R$NO2.cl)
# table(map_data_R$Q221.cl,map_data_R$NO2.cl)
# 
# ###---PM25---###
# table(map_data_R$High_edu.cl,map_data_R$PM25.cl)
# table(map_data_R$Q221.cl,map_data_R$PM25.cl)
# 
# ####---PM10---###
# table(map_data_R$High_edu.cl,map_data_R$PM10.cl)
# table(map_data_R$Q221.cl,map_data_R$PM10.cl)
