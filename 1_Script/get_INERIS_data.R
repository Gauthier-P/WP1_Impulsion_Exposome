#####################
###-----INERIS----###
#####################

rm(list=ls())

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ncdf4)
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

# 
# 
# INERIS_2015_NO2_ANNUAL_Z <- rast("0_input/INERIS/Zenodo/INERIS.REANALYSED.FRA.2015/INERIS.REANALYSED.FRA03.2015.NO2.avgannual.2gis.nc")
# INERIS_2015_NO2_ANNUAL <- rast("0_input/INERIS/Cartotheque/Grille/2018-2024/NO2/Reanalysed_FRA_2015_NO2_avgannual_Ineris_v.Jan2024.nc")
# 
# INERIS_2015_NO2_ANNUAL
# 
# plot(INERIS_2015_NO2_ANNUAL_Z,
#      main = "NO2 annuel – France 2015 (ZENODO)")
# 
# plot(INERIS_2015_NO2_ANNUAL_SI,
#      main = "NO2 annuel – France 2015 (Site internet)")
# 

################################################################################

INERIS_Air_2021 <- read.csv("0_input/INERIS/Cartotheque/Commune/Indicateurs_QualiteAir_France_Commune_2021_Ineris_v.Dec2024.csv", skip = 1, fileEncoding = "Latin1")

names(INERIS_Air_2021) <- c("CODGEO", "LIBGEO", "POPULATION", 
                            "MOY.NO2", "MOY.NO2.POP",
                            "MOY.O3", "MOY.O3.POP",
                            "MOY.SOMO", "MOY.SOMO.POP",
                            "MOY.AOT",
                            "MOY.PM10", "MOY.PM10.POP",
                            "MOY.PM25", "MOY.PM25.POP")

annees <- (2019:2024)
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
  
  df$ANNEE <- annee
  
  return(df)
}

INERIS_Air_list <- lapply(annees, f.ineris)
names(INERIS_Air_list) <- paste0("INERIS_Air_", annees)


for (i in seq_along(annees)) {
  saveRDS(
    INERIS_Air_list[[i]],
    file = paste0("0_input/INERIS/Cartotheque/Commune/INERIS_Air_", annees[i], ".rds")
  )
}

