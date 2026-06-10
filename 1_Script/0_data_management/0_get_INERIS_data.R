#####################
###-----INERIS----###
#####################

rm(list=ls())

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(COGugaison)
library(terra)

###---Données ineris---###

polluants <- c("PM25", "PM10", "NO2", "O3")
annees <- 2018:2024
base_path  <- "0_input/INERIS/Cartotheque/Grille/2018-2024"


ineris <- list()

for (pol in polluants) {
  
  fichiers <- list.files(
    path = file.path(base_path, pol),
    pattern = "\\.nc$",
    full.names = TRUE
  )
  
  
  for (i in seq_along(fichiers)) {
    
    r <- rast(fichiers[i])
    
    df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    
    names(df)[3] <- "valeur"
    
    df$polluant <- pol
    df$annee <- annees[i]
    
    ineris[[length(ineris) + 1]] <- df
  }
}

ineris_df <- bind_rows(ineris)

################################################################################

INERIS_Air_2021 <- read.csv("0_input/INERIS/Cartotheque/Commune/Indicateurs_QualiteAir_France_Commune_2021_Ineris_v.Dec2024.csv", skip = 1, fileEncoding = "Latin1")

names(INERIS_Air_2021) <- c("CODGEO", "LIBGEO", "POPULATION", 
                            "MOY.NO2", "MOY.NO2.POP",
                            "MOY.O3", "MOY.O3.POP",
                            "MOY.SOMO", "MOY.SOMO.POP",
                            "MOY.AOT",
                            "MOY.PM10", "MOY.PM10.POP",
                            "MOY.PM25", "MOY.PM25.POP")

annees <- (2009:2024)
f.ineris <- function(annee) {
  path <- paste0(
    "0_input/INERIS/Cartotheque/Commune/",
    "Indicateurs_QualiteAir_France_Commune_",
    annee,
    "_Ineris_v.Dec2024.csv"
  )
  
  df <- read.csv(path, skip = 1, fileEncoding = "Latin1")
  
  names(df) <- c(
    "CODGEO", "LIBGEO", "POPULATION",
    "MOY.NO2", "MOY.NO2.POP",
    "MOY.O3", "MOY.O3.POP",
    "MOY.SOMO", "MOY.SOMO.POP",
    "MOY.AOT",
    "MOY.PM10", "MOY.PM10.POP",
    "MOY.PM25", "MOY.PM25.POP"
  )
  
  df$THRESHOLD.F.NO2  <- factor(ifelse(df$MOY.NO2  > 20, 1, 0))
  df$THRESHOLD.F.PM25 <- factor(ifelse(df$MOY.PM25 > 25, 1, 0))
  df$THRESHOLD.F.PM10 <- factor(ifelse(df$MOY.PM10 > 40, 1, 0))

  df$NO2.WHO.target1  <- factor(ifelse(df$MOY.NO2  > 40, 1, 0))
  df$NO2.WHO.target2  <- factor(ifelse(df$MOY.NO2  > 30, 1, 0))
  df$NO2.WHO.target3  <- factor(ifelse(df$MOY.NO2  > 20, 1, 0))
  df$NO2.WHO.AQG <- factor(ifelse(df$MOY.NO2  > 10, 1, 0))
  
  df$PM25.WHO.target1 <- factor(ifelse(df$MOY.PM25 > 35,  1, 0))
  df$PM25.WHO.target2 <- factor(ifelse(df$MOY.PM25 > 25,  1, 0))
  df$PM25.WHO.target3 <- factor(ifelse(df$MOY.PM25 > 15,  1, 0))
  df$PM25.WHO.target4 <- factor(ifelse(df$MOY.PM25 > 10,  1, 0))
  df$PM25.WHO.AQG <- factor(ifelse(df$MOY.PM25 > 5,  1, 0))

  df$PM10.WHO.target1 <- factor(ifelse(df$MOY.PM10 > 70, 1, 0))
  df$PM10.WHO.target2 <- factor(ifelse(df$MOY.PM10 > 50, 1, 0))
  df$PM10.WHO.target3 <- factor(ifelse(df$MOY.PM10 > 30, 1, 0))
  df$PM10.WHO.target4 <- factor(ifelse(df$MOY.PM10 > 20, 1, 0))
  df$PM10.WHO.AQG <- factor(ifelse(df$MOY.PM10 > 15, 1, 0))
  
  df$ANNEE <- annee
  
  return(df)
}

INERIS_Air_list <- lapply(annees, f.ineris)
names(INERIS_Air_list) <- paste0("INERIS_Air_", annees)
list2env(INERIS_Air_list, envir = .GlobalEnv)

################################################################################


#2021
COG_akinator(vecteur_codgeo = INERIS_Air_2021[,1], donnees_insee = TRUE) #2018
appariement_INERIS <- apparier_COG(vecteur_codgeo = c(INERIS_Air_2021[which(INERIS_Air_2021$CODGEO!="01001"),1],"XXXXX"), COG = 2021)
cat(appariement_INERIS$absent_de_bdd)
cat(appariement_INERIS$absent_de_COG)

COG2021_insee[which(COG2021_insee$CODGEO %in% appariement_INERIS$absent_de_bdd),c(1,2)]

INERIS_Air_2021_COG2021 <- changement_COG_varNum(table_entree = INERIS_Air_2021,codgeo_entree = "CODGEO", annees = c(2018:2021),
                                                 agregation = FALSE, libgeo = F, donnees_insee = TRUE)

COG_akinator(INERIS_Air_2021_COG2021$CODGEO)
head(diag_COG(INERIS_Air_2021_COG2021)) #N = 35228

INERIS_Air_2021_COG2024 <- changement_COG_varNum(table_entree = INERIS_Air_2021,codgeo_entree = "CODGEO", annees = c(2018:2024),
                                                 agregation = FALSE, libgeo = F, donnees_insee = TRUE)

COG_akinator(INERIS_Air_2021_COG2024$CODGEO)

#2015
COG_akinator(vecteur_codgeo = INERIS_Air_2015[,1], donnees_insee = TRUE) #2017
appariement_INERIS <- apparier_COG(vecteur_codgeo = c(INERIS_Air_2015[which(INERIS_Air_2015$CODGEO!="01001"),1],"XXXXX"), COG = 2015)
cat(appariement_INERIS$absent_de_bdd)
cat(appariement_INERIS$absent_de_COG)

COG2015_insee[which(COG2015_insee$CODGEO %in% appariement_INERIS$absent_de_bdd),c(1,2)]

INERIS_Air_2015_COG2015 <- changement_COG_varNum(table_entree = INERIS_Air_2015,codgeo_entree = "CODGEO", annees = c(2017:2015), 
                                                 agregation = FALSE, libgeo = F, donnees_insee = TRUE)

# INERIS_Air_2021_COG2021<- select(INERIS_Air_2021_COG2021, -c("nom_commune", "ANNEE"))
COG_akinator(INERIS_Air_2015_COG2015$CODGEO)
head(diag_COG(INERIS_Air_2015_COG2015)) #N = 35228 

INERIS_Air_2015_COG2024 <- changement_COG_varNum(table_entree = INERIS_Air_2015,codgeo_entree = "CODGEO", annees = c(2017:2024), 
                                                 agregation = FALSE, libgeo = F, donnees_insee = TRUE)
COG_akinator(INERIS_Air_2015_COG2024$CODGEO)


#2011
COG_akinator(vecteur_codgeo = INERIS_Air_2011[,1], donnees_insee = TRUE) #2017
appariement_INERIS <- apparier_COG(vecteur_codgeo = c(INERIS_Air_2011[which(INERIS_Air_2011$CODGEO!="01001"),1],"XXXXX"), COG = 2011)
cat(appariement_INERIS$absent_de_bdd)
cat(appariement_INERIS$absent_de_COG)

COG2011_insee[which(COG2011_insee$CODGEO %in% appariement_INERIS$absent_de_bdd),c(1,2)]

INERIS_Air_2011_COG2011 <- changement_COG_varNum(table_entree = INERIS_Air_2011,codgeo_entree = "CODGEO", annees = c(2017:2011), 
                                                 agregation = FALSE, libgeo = F, donnees_insee = TRUE)

# INERIS_Air_2021_COG2021<- select(INERIS_Air_2021_COG2021, -c("nom_commune", "ANNEE"))
COG_akinator(INERIS_Air_2011_COG2011$CODGEO)
head(diag_COG(INERIS_Air_2011_COG2011)) #N = 35228 

INERIS_Air_2011_COG2024 <- changement_COG_varNum(table_entree = INERIS_Air_2011,codgeo_entree = "CODGEO", annees = c(2017:2024), 
                                                 agregation = FALSE, libgeo = F, donnees_insee = TRUE)
COG_akinator(INERIS_Air_2011_COG2024$CODGEO)

################################################################################

#cleaning 

INERIS_Air_2021_COG2024 <- INERIS_Air_2021_COG2024 %>%
  group_by(CODGEO) %>%
  summarise(
  POPULATION = sum(POPULATION, na.rm = TRUE),
    across(
      .cols = c(LIBGEO, POPULATION, ANNEE, starts_with("THRESHOLD"), starts_with("NO2.WHO"), starts_with("PM25.WHO"), starts_with("PM10.WHO")),
      .fns = first
    ),
    
    across(
      .cols = c(MOY.NO2, MOY.O3, MOY.PM25,MOY.PM10),
      .fns = ~ mean(., na.rm = TRUE)
    ),
    
    .groups = "drop"
  )
sum(duplicated(INERIS_Air_2021_COG2024$CODGEO))

sum(duplicated(INERIS_Air_2015_COG2024$CODGEO))
INERIS_Air_2015_COG2024 <- INERIS_Air_2015_COG2024 %>%
  group_by(CODGEO) %>%
  summarise(
    POPULATION = sum(POPULATION, na.rm = TRUE),
    across(
      .cols = c(LIBGEO, POPULATION, ANNEE, starts_with("THRESHOLD"), starts_with("NO2.WHO"), starts_with("PM25.WHO"), starts_with("PM10.WHO")),
      .fns = first
    ),
    
    across(
      .cols = c(MOY.NO2, MOY.O3, MOY.PM25,MOY.PM10),
      .fns = ~ mean(., na.rm = TRUE)
    ),
    
    .groups = "drop"
  )
sum(duplicated(INERIS_Air_2015_COG2024$CODGEO))

sum(duplicated(INERIS_Air_2011_COG2024$CODGEO))
INERIS_Air_2011_COG2024 <- INERIS_Air_2011_COG2024 %>%
  group_by(CODGEO) %>%
  summarise(
    POPULATION = sum(POPULATION, na.rm = TRUE),
    across(
      .cols = c(LIBGEO, POPULATION, ANNEE, starts_with("THRESHOLD"), starts_with("NO2.WHO"), starts_with("PM25.WHO"), starts_with("PM10.WHO")),
      .fns = first
    ),
    
    across(
      .cols = c(MOY.NO2, MOY.O3, MOY.PM25,MOY.PM10),
      .fns = ~ mean(., na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

sum(duplicated(INERIS_Air_2011_COG2024$CODGEO))
################################################################################

for (i in seq_along(annees)) {
  saveRDS(
    get(paste0("INERIS_Air_", annees[i])),
    file = paste0("0_input/INERIS/Cartotheque/Commune/INERIS_Air_", annees[i], ".rds")
  )
}

saveRDS(INERIS_Air_2021_COG2021, "0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021_COG2021.rds")
saveRDS(INERIS_Air_2015_COG2015, "0_input/INERIS/Cartotheque/Commune/INERIS_Air_2015_COG2015.rds")
saveRDS(INERIS_Air_2011_COG2011, "0_input/INERIS/Cartotheque/Commune/INERIS_Air_2011_COG2011.rds")

saveRDS(INERIS_Air_2021_COG2024, "0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021_COG2024.rds")
saveRDS(INERIS_Air_2015_COG2024, "0_input/INERIS/Cartotheque/Commune/INERIS_Air_2015_COG2024.rds")
saveRDS(INERIS_Air_2011_COG2024, "0_input/INERIS/Cartotheque/Commune/INERIS_Air_2011_COG2024.rds")

