########################
###-----Chainage-----###
########################

rm(list=ls())

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(readxl)
library(terra)
library(COGugaison)

rm(list = ls())

###---Données insee---###

Insee_C_2021 <- readRDS("0_input/INSEE/Commune/Insee_C_2021.rds")

Insee_I_2018 <- readRDS("0_input/INSEE/Commune/Insee_I_2018.rds")
Insee_I_2019 <- readRDS("0_input/INSEE/Commune/Insee_I_2019.rds")
Insee_I_2020 <- readRDS("0_input/INSEE/Commune/Insee_I_2020.rds")
Insee_I_2021 <- readRDS("0_input/INSEE/Commune/Insee_I_2021.rds")
Insee_I_2022 <- readRDS("0_input/INSEE/Commune/Insee_I_2022.rds")

###---Données INERIS---###

INERIS_Air_2019 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2019.rds")
INERIS_Air_2020 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2020.rds")
INERIS_Air_2021 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021.rds")
INERIS_Air_2022 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2022.rds")
INERIS_Air_2023 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2023.rds")
INERIS_Air_2024 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2024.rds")

################################################################################

#Harmonization

COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2024
COG_akinator(vecteur_codgeo = INERIS_Air_2021[,1]) #2018

head(diag_COG(Insee_C_2021)) #N = 34918 
head(diag_COG(INERIS_Air_2021)) #N = 35228 

apparier_COG(vecteur_codgeo = c(INERIS_Air_2021[which(INERIS_Air_2021$CODGEO!="01001"),1],"XXXXX"), COG = 2024)

appariement <- apparier_COG(vecteur_codgeo = c(INERIS_Air_2021[which(INERIS_Air_2021$CODGEO!="01001"),1],"XXXXX"), COG = 2024)
cat(appariement$absent_de_bdd)
cat(appariement$absent_de_COG)
COG2024_insee[which(COG2024_insee$CODGEO %in% appariement$absent_de_bdd),c(1,2)]

INERIS_Air_2021_COG2024 <- changement_COG_varNum(table_entree = INERIS_Air_2021, annees = c(2018:2024),
                                                    agregation = FALSE, libgeo = TRUE, donnees_insee = TRUE)

INERIS_Air_2021_COG2024<- select(INERIS_Air_2021_COG2024, c("CODGEO", "LIBGEO.x", "POPULATION","MOY.NO2","MOY.NO2.POP",
                                 "MOY.O3", "MOY.O3.POP","MOY.SOMO",  "MOY.SOMO.POP", 
                                 "MOY.AOT","MOY.PM10",  "MOY.PM10.POP", "MOY.PM25",  "MOY.PM25.POP" ))


COG_akinator(INERIS_Air_2021_COG2024$CODGEO)
head(diag_COG(INERIS_Air_2021_COG2024)) #N = 35228 

#Merging

Merged_2021 <- select(merge(Insee_C_2021, INERIS_Air_2021_COG2024, by ="CODGEO"), -c("LIBGEO.x", "POPULATION"))

saveRDS(object = Merged_2021, file = "0_input/Merged/Merged_AIR_EDU_2021.rds")





