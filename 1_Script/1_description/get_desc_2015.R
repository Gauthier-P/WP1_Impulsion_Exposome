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

map_data_biv <- readRDS("0_input/map_data_COG2024_C_2015_biv.rds")
map_data <- readRDS("0_input/map_data_COG2024_C_2015.rds")
map_data_biv <-st_drop_geometry(map_data_biv)
map_data <-st_drop_geometry(map_data)

map_data_biv <- map_data_biv %>%
  mutate(LIBDENS = factor(LIBDENS, levels = c("Urbain dense", "Urbain intermédiaire", "Rural")))

source("1_Script/Fonctions/fn_description_with_missing.R")
source("1_Script/Fonctions/fn_comp_CC_test.R")

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Seuil <- c("NO2.WHO.target1", "NO2.WHO.target2","NO2.WHO.target3","NO2.WHO.AQG",
           "PM25.WHO.target1","PM25.WHO.target2", "PM25.WHO.target3", "PM25.WHO.target4", "PM25.WHO.AQG",
           "PM10.WHO.target1", "PM10.WHO.target2", "PM10.WHO.target3", "PM10.WHO.target4", "PM10.WHO.AQG")


Seuil.Label <- c("Target 1 WHO NO<sub>2</sub>",
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

sociales <-c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","Prop_Immigre","Q215")

################################################################################
gg_miss_upset(map_data_biv,  nsets = n_var_miss(map_data_biv))
plot_NA <- gg_miss_upset(map_data,  nsets = n_var_miss(map_data))
jpeg("2_Results/plot_NA.jpeg", width = 5, height = 5, units = "in", res = 300)

print(plot_NA)

dev.off()

###--description---###

wb <- createWorkbook()
Variable <- c(Polluants,Seuil,  "High_edu_prop", "Q215")
label.Variable <- c("NO2", "O3", "PM2.5", "PM10", 
                    Seuil.Label,
                    " Prop. niveau élevé éducation", "Médiane")
data <- map_data

tab.desc.var <- desc(Variable, label.Variable, map_data)
addWorksheet(wb, sheetName = "Descritpion")
writeData(wb, sheet = "Descritpion", tab.desc.var)

# saveWorkbook(wb, "2_Results/Description/table_desc.xlsx", overwrite = T)

###--bivarié---##

##Education 
addWorksheet(wb, "Education")

vars <- c(Polluants,"NO2.WHO.target3","NO2.WHO.AQG", "PM25.WHO.target4", "PM10.WHO.AQG")
labels <- c("NO2", "O3", "PM2.5", "PM10", 
            "Target 3 WHO NO<sub>2</sub>","AQG WHO NO<sub>2</sub>",
            "Target 4 WHO PM<sub>2.5</sub>",
            "AQG WHO PM<sub>10</sub>")

comp.var <- map_data_biv[,"High_edu.cl"]
label.comp <- levels(map_data_biv[,"High_edu.cl"])
tab.comp <- comp(vars, comp.var, labels, label.comp, map_data_biv)

writeData(wb, sheet = "Education", tab.comp)

##Urbain
addWorksheet(wb, "Urbanisation")

comp.var <- map_data_biv[,"LIBDENS"]
label.comp <- levels(map_data_biv[,"LIBDENS"])
tab.comp <- comp(vars, comp.var, labels, label.comp, map_data_biv)

writeData(wb, sheet = "Urbanisation", tab.comp)

tab <- table(
  "NO2 tercile" = map_data_biv$NO2.cl,
  "Education tercile" = map_data_biv$High_edu.cl
)
prop<- prop.table(tab,margin = 1 )

tab_combined <- matrix(nrow = nrow(tab), ncol = ncol(tab))
rownames(tab_combined) <- rownames(tab)
colnames(tab_combined) <- colnames(tab)

for(i in 1:nrow(tab)) {
  for(j in 1:ncol(tab)) {
    tab_combined[i,j] <- sprintf("%.2f (n=%d)", prop[i,j], tab[i,j])
  }
}

df_tab_combined <- as.data.frame(tab_combined)
df_tab_combined <- cbind(NO2.cl = rownames(tab_combined), df_tab_combined)


writeData(wb, sheet = "Education", df_tab_combined,startRow  =30)

tab <- tbl_cross(
  data = map_data_biv,
  row = NO2.cl,
  col = High_edu.cl,
  percent = "column",
  missing ="no",
  label = list(
    NO2.cl ~ "NO2 tercile",
    High_edu.cl ~ "High education tercile"
  )
)


saveWorkbook(wb, "2_Results/Description/table_comp_raw_2015.xlsx", overwrite = T)

################################################################################
