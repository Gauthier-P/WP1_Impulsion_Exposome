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
Insee_C_2021 <- as.data.frame(read_xlsx("0_input/Population/INSEE_densite_2021.xlsx", skip = 4, col_names = TRUE))
Insee_C_2015 <- as.data.frame(read_xlsx("0_input/Population/INSEE_densite_2015.xlsx", skip = 4, col_names = TRUE))

COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2021

Insee_C_2021 <- Insee_C_2021 %>% 
  mutate(LIBDENS.2cl = case_when(
    DENS == 1 ~ "Urbain",
    DENS == 2 ~ "Urbain",
    DENS == 3 ~ "Rural",
    TRUE ~ NA
  ))

Insee_C_2021 <- select(Insee_C_2021, c("CODGEO","LIBGEO","DENS","LIBDENS","LIBDENS.2cl"))

Insee_C_2015 <- Insee_C_2015 %>% 
  mutate(DENS3 = case_when(
    DENS == 1 ~ 1,
    DENS == 2 ~ 2,
    DENS == 3 ~ 2,
    DENS == 4 ~ 2,
    DENS == 5 ~ 3,
    DENS == 6 ~ 3,
    DENS == 7 ~ 3,
    TRUE ~ NA
  ))

Insee_C_2015 <- Insee_C_2015 %>% 
  mutate(LIBDENS.2cl = case_when(
    DENS3 == 1 ~ "Urbain",
    DENS3 == 2 ~ "Urbain",
    DENS3 == 3 ~ "Rural",
    TRUE ~ NA
  ))

Insee_C_2015 <- Insee_C_2015 %>% 
  mutate(LIBDENS = case_when(
    DENS3 == 1 ~ "Urbain dense",
    DENS3 == 2 ~ "Urbain intermédiaire",
    DENS3 == 3 ~ "Rural",
    TRUE ~ NA
  ))
Insee_C_2015 <- select(Insee_C_2015, c("CODGEO","LIBGEO","DENS3","LIBDENS","LIBDENS.2cl"))


#################################################################################

#2021
COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2021
head(diag_COG(Insee_C_2021)) #N = 34918 

appariement_2024<- apparier_COG(vecteur_codgeo = c(Insee_C_2021[which(Insee_C_2021$CODGEO!="01001"),1],"XXXXX"), COG = 2024)
cat(appariement_2024$absent_de_bdd)
cat(appariement_2024$absent_de_COG)

INSEE_Densite_2021_COG_2024 <- changement_COG_varNum(table_entree = Insee_C_2021,codgeo_entree = "CODGEO", annees = c(2021:2024),
                                                     agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = INSEE_Densite_2021_COG_2024[,1], donnees_insee = TRUE) #2016

#2015
COG_akinator(vecteur_codgeo = Insee_C_2015[,1], donnees_insee = TRUE) #2015
head(diag_COG(Insee_C_2015)) #N = 34918 

appariement_2024<- apparier_COG(vecteur_codgeo = c(Insee_C_2015[which(Insee_C_2015$CODGEO!="01001"),1],"XXXXX"), COG = 2024)
cat(appariement_2024$absent_de_bdd)
cat(appariement_2024$absent_de_COG)

INSEE_Densite_2015_COG_2024 <- changement_COG_varNum(table_entree = Insee_C_2015,codgeo_entree = "CODGEO", annees = c(2015:2024),
                                                     agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = INSEE_Densite_2015_COG_2024[,1], donnees_insee = TRUE) #2016


saveRDS(INSEE_Densite_2021_COG_2024, "0_input/Population/Densite_2021_COG2024.rds")
saveRDS(INSEE_Densite_2015_COG_2024, "0_input/Population/Densite_2015_COG2024.rds")
saveRDS(Insee_C_2021, "0_input/Population/Densite_2021.rds")
saveRDS(Insee_C_2015, "0_input/Population/Densite_2021.rds")

