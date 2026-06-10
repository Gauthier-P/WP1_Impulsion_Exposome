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
Insee_C_2021 <- as.data.frame(read_xlsx("0_input/INSEE_REVENU/FILO2021_DEC_COM.xlsx", sheet = 2, skip = 5, col_names = TRUE))
Insee_C_2015 <- as.data.frame(read_xls("0_input/INSEE_REVENU/base-cc-filosofi-2015.xls", sheet = 1, skip = 5, col_names = TRUE))
Insee_C_2011 <- as.data.frame(read_xls("0_input/INSEE_REVENU/RFDM2011COM.xls", sheet = 2, skip = 6, col_names = TRUE))

# Insee_pop_C_2021 <- as.data.frame(read_xlsx("0_input/Insee_population_iris.xlsx", skip = 5, col_names = TRUE))

COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2022

diagnostic <- diag_COG(Insee_C_2021)

################################################################################


# Insee_C_2021
Insee_C_2021 <- Insee_C_2021 %>%
  mutate(across(
    c("Q121", "Q221","Q321", 
      "D121", "D221", "D321", "D421", "D621", "D721", "D821", "D921", 
      "RD"),
    ~ as.numeric(gsub(",", ".", as.character(.)))
  ))
Insee_C_2021 <- select(Insee_C_2021, c("CODGEO", "LIBGEO",
                                       "Q121", "Q221","Q321", 
                                       "D121", "D221", "D321", "D421", "D621", "D721", "D821", "D921", 
                                       "RD"))


# Insee_C_2015
Insee_C_2015 <- Insee_C_2015 %>%
  mutate(across(
    c("MED15"),
    ~ as.numeric(gsub(",", ".", as.character(.)))
  ))
Insee_C_2015<- Insee_C_2015 %>% 
  mutate(    
    Q215 = MED15
  )
Insee_C_2015 <- select(Insee_C_2015, c("CODGEO", "LIBGEO","Q215"))

# Insee_C_2011
Insee_C_2011 <- Insee_C_2011 %>%
  mutate(across(
    c("RFMQ211"),
    ~ as.numeric(gsub(",", ".", as.character(.)))
  ))

Insee_C_2011<- Insee_C_2011 %>% 
  mutate(    
    Q211 = RFMQ211,
    CODGEO = COM
  )
Insee_C_2011 <- select(Insee_C_2011, c("CODGEO", "LIBGEO","Q211"))

################################################################################

#2021
COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2022
head(diag_COG(Insee_C_2021)) #N = 34918 

appariement_2021<- apparier_COG(vecteur_codgeo = c(Insee_C_2021[which(Insee_C_2021$CODGEO!="01001"),1],"XXXXX"), COG = 2022)
cat(appariement_2021$absent_de_bdd)
cat(appariement_2021$absent_de_COG)

Insee_C_2021_COG2021 <- changement_COG_varNum(table_entree = Insee_C_2021,codgeo_entree = "CODGEO", annees = c(2022:2021),
                                              agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2021_COG2021[,1], donnees_insee = TRUE) #2016


Insee_C_2021_COG2024 <- changement_COG_varNum(table_entree = Insee_C_2021,codgeo_entree = "CODGEO", annees = c(2021:2024),
                                              agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2021_COG2024[,1], donnees_insee = TRUE) #2016
# Insee_C_2021_COG2021<- select(Insee_C_2021_COG2021, -c("nom_commune"))

#2015
COG_akinator(vecteur_codgeo = Insee_C_2015[,1], donnees_insee = TRUE) #2016
head(diag_COG(Insee_C_2015)) #N = 34918 

appariement_2015<- apparier_COG(vecteur_codgeo = c(Insee_C_2015[which(Insee_C_2015$CODGEO!="01001"),1],"XXXXX"), COG = 2016)
cat(appariement_2015$absent_de_bdd)
cat(appariement_2015$absent_de_COG)

Insee_C_2015_COG2015 <- changement_COG_varNum(table_entree = Insee_C_2015,codgeo_entree = "CODGEO", annees = c(2016:2015),
                                              agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2015_COG2015[,1], donnees_insee = TRUE) #2016

Insee_C_2015_COG2024 <- changement_COG_varNum(table_entree = Insee_C_2015,codgeo_entree = "CODGEO", annees = c(2015:2024),
                                              agregation = F, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2015_COG2024[,1], donnees_insee = TRUE) 

# Insee_C_2015_COG2015<- select(Insee_C_2015_COG2015, -c("nom_commune"))

#2010
COG_akinator(vecteur_codgeo = Insee_C_2011[,1], donnees_insee = TRUE) #2012
head(diag_COG(Insee_C_2011)) #N = 36664 

appariement_2011<- apparier_COG(vecteur_codgeo = c(Insee_C_2011[which(Insee_C_2011$CODGEO!="01001"),1],"XXXXX"), COG = 2012)
cat(appariement_2011$absent_de_bdd)
cat(appariement_2011$absent_de_COG)

Insee_C_2011_COG2011 <- changement_COG_varNum(table_entree = Insee_C_2011,codgeo_entree = "CODGEO", annees = c(2012:2011),
                                              agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2011_COG2011[,1], donnees_insee = TRUE) #2016

Insee_C_2011_COG2024 <- changement_COG_varNum(table_entree = Insee_C_2011,codgeo_entree = "CODGEO", annees = c(2011:2024),
                                              agregation = FALSE, libgeo =F, donnees_insee = TRUE)

COG_akinator(vecteur_codgeo = Insee_C_2011_COG2024[,1], donnees_insee = TRUE) #2016

################################################################################
#cleaning 

Insee_C_2021_COG2024 <- Insee_C_2021_COG2024 %>%
  group_by(CODGEO) %>%
  summarise(
    LIBGEO = first(LIBGEO),          
    
    across(
      .cols = c(Q121, Q221, Q321, D121, D221, D321, D421, D621, D721, D821, D921, RD),
      .fns = ~ mean(., na.rm = TRUE)
    ),
    
    .groups = "drop"
  )
sum(duplicated(Insee_C_2021_COG2024$CODGEO))

Insee_C_2015_COG2024 <- Insee_C_2015_COG2024 %>%
  group_by(CODGEO) %>%
  summarise(
    LIBGEO = first(LIBGEO),          
    
    across(
      .cols = c(Q215),
      .fns = ~ mean(., na.rm = TRUE)
    ),
    
    .groups = "drop"
  )
sum(duplicated(Insee_C_2015_COG2024$CODGEO))


Insee_C_2011_COG2024 <- Insee_C_2011_COG2024 %>%
  group_by(CODGEO) %>%
  summarise(
    LIBGEO = first(LIBGEO),          
    
    across(
      .cols = c(Q211),
      .fns = ~ mean(., na.rm = TRUE)
    ),
    
    .groups = "drop"
  )
sum(duplicated(Insee_C_2011_COG2024$CODGEO))

################################################################################

saveRDS(Insee_C_2021_COG2024, "0_input/INSEE_REVENU/Insee_C_2021_COG2024.rds")
saveRDS(Insee_C_2015_COG2024, "0_input/INSEE_REVENU/Insee_C_2015_COG2024.rds")
saveRDS(Insee_C_2011_COG2024, "0_input/INSEE_REVENU/Insee_C_2011_COG2024.rds")

saveRDS(Insee_C_2021_COG2021, "0_input/INSEE_REVENU/Insee_C_2021_COG2021.rds")
saveRDS(Insee_C_2015_COG2015, "0_input/INSEE_REVENU/Insee_C_2015_COG2015.rds")
saveRDS(Insee_C_2011_COG2011, "0_input/INSEE_REVENU/Insee_C_2011_COG2011.rds")

