#####################
###-----INSEE-----###
#####################

rm(list=ls())

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(readxl)
library(terra)
library(COGugaison)


###---Données insee---###

#Commune
Insee_C_2021 <- as.data.frame(read_xlsx("0_input/INSEE_IMMI/TD_IMG1A_2021.xlsx", skip = 10, col_names = TRUE))

COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2025

diagnostic <- diag_COG(Insee_C_2021)

################################################################################

Insee_C_2021 <- Insee_C_2021 %>%
  mutate(
    Immigre = rowSums(across(contains("IMMI1")), na.rm = TRUE),
    N_Immigre = rowSums(across(contains("IMMI2")), na.rm = TRUE),
    
    Immigre_H = rowSums(across(contains("IMMI1_SEXE1")), na.rm = TRUE),
    N_Immigre_H = rowSums(across(contains("IMMI2_SEXE1")), na.rm = TRUE),
    
    Immigre_F = rowSums(across(contains("IMMI1_SEXE2")), na.rm = TRUE),
    N_Immigre_F = rowSums(across(contains("IMMI2_SEXE2")), na.rm = TRUE),
    
    Prop_Immigre = Immigre/(N_Immigre + Immigre),
    Prop_Immigre_H = Immigre_H/(N_Immigre_H + Immigre_H),
    Prop_Immigre_F = Immigre_F/(N_Immigre_F + Immigre_F),
    
    Pop_tot = Immigre +N_Immigre
  )

Insee_C_2021 <- select(Insee_C_2021, c("CODGEO", "LIBGEO",
                                       "Prop_Immigre", "Prop_Immigre_H", "Prop_Immigre_F", "Pop_tot"))

#2021
COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2024
Insee_C_2021_COG2024 <- Insee_C_2021
head(diag_COG(Insee_C_2021)) #N = 34918 

appariement_2021<- apparier_COG(vecteur_codgeo = c(Insee_C_2021[which(Insee_C_2021$CODGEO!="01001"),1],"XXXXX"), COG = 2021)
cat(appariement_2021$absent_de_bdd)
cat(appariement_2021$absent_de_COG)

Insee_C_2021_COG2021 <- changement_COG_varNum(table_entree = Insee_C_2021,codgeo_entree = "CODGEO", annees = c(2024:2021),
                                              agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2021_COG2021[,1], donnees_insee = TRUE) 


saveRDS(Insee_C_2021_COG2024, "0_input/INSEE_IMMI/Insee_C_2021_COG2024.rds")
saveRDS(Insee_C_2021_COG2021, "0_input/INSEE_IMMI/Insee_C_2021_COG2021.rds")


