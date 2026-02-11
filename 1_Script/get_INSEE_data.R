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

remotes::install_github("antuki/COGugaison")
library(COGugaison)


###---Données insee---###

#Commune
Insee_C_2011 <- as.data.frame(read_xls("0_input/INSEE/Commune/base-cc-diplomes-formation-Commune-2011.xls", skip = 5, col_names = TRUE))
Insee_C_2015 <- as.data.frame(read_xlsx("0_input/INSEE/Commune/base-cc-diplomes-formation-Commune-2021.xlsx", skip = 5,sheet=2, col_names = TRUE))
Insee_C_2021 <- as.data.frame(read_xlsx("0_input/INSEE/Commune/base-cc-diplomes-formation-Commune-2021.xlsx", skip = 5, col_names = TRUE))

#Harmonization 2011

Insee_C_2011 <- Insee_C_2011 %>%
  rename(P11_NSCOL15P_DIPLMIN = P11_NSCOL15P_DIPL0, 
         P11_NSCOL15P_SUP2 = P11_FNSCOL15P_BACP2, 
         P11_NSCOL15P_SUP34 = ,
         P11_NSCOL15P_SUP5 = )



#Iris
annees <- 2018:2022
base_path  <- "0_input/INSEE/IRIS"

for (i in annees) {
  # Construction dynamique du nom du fichier
  nom_fichier <- paste0("base-ic-diplomes-formation-IRIS-", i, ".xlsx")
  chemin_fichier <- file.path(base_path, nom_fichier)
  
  # Lecture du fichier Excel
  if (file.exists(chemin_fichier)) {
    # Utilisation de assign pour créer une variable dynamique
    assign(paste0("Insee_I_", i), as.data.frame(read_xlsx(chemin_fichier, skip = 5, col_names = TRUE)))
    cat("Fichier pour l'année", i, "chargé avec succès.\n")
  } else {
    cat("Le fichier pour l'année", i, "n'existe pas.\n")
  }
}

################################################################################

annees <- 2018:2022

for (i in annees) {
  
  df_name <- paste0("Insee_I_", i)
  
  if (exists(df_name)) {
    
    df <- get(df_name)
    
    yy <- substr(i, 3, 4)
    
    cols_pop <- c(
      paste0("P", yy, "_POP1517"),
      paste0("P", yy, "_POP1824"),
      paste0("P", yy, "_POP2529"),
      paste0("P", yy, "_POP30P")
    )
  
    cols_pop <- cols_pop[cols_pop %in% names(df)]
    
    df$population_totale <- rowSums(df[, cols_pop], na.rm = TRUE)
  
    df$low_edu_n <- rowSums(
      df[, c(paste0("P", yy, "_NSCOL15P_DIPLMIN"),   #Sans diplome ou CEP
             paste0("P", yy, "_NSCOL15P_BEPC"))],     #BEPC, Brevet ou DNB
      na.rm = TRUE
    )
    
    df$medium_edu_n <- rowSums(
      df[, c(paste0("P", yy, "_NSCOL15P_CAPBEP"), # CAP-BEP
             paste0("P", yy, "_NSCOL15P_BAC"))],  # BAC
      na.rm = TRUE
    )
    
    df$high_edu_n <- rowSums(
      df[, c(paste0("P", yy, "_NSCOL15P_SUP2"),
             paste0("P", yy, "_NSCOL15P_SUP34"),
             paste0("P", yy, "_NSCOL15P_SUP5"))],
      na.rm = TRUE
    )
    
    P_NSCOL15P <- df[[paste0("P", yy, "_NSCOL15P")]]
    
    df$Low_edu_prop <-  df$low_edu_n/ P_NSCOL15P
    df$Medium_edu_prop <-  df$medium_edu_n/ P_NSCOL15P
    df$High_edu_prop <-  df$high_edu_n/ P_NSCOL15P
    
    df <- select(df, c("IRIS", "REG", "DEP", "COM", "LIBCOM",
                       "population_totale","Low_edu_prop", "Medium_edu_prop", "High_edu_prop"))
    
    assign(df_name, df)
  }
}

annees <- c(2021)

for (i in annees) {
  
  df_name <- paste0("Insee_C_", i)
  
  if (exists(df_name)) {
    
    df <- get(df_name)
    
    yy <- substr(i, 3, 4)
    
    cols_pop <- c(
      paste0("P", yy, "_POP1517"),
      paste0("P", yy, "_POP1824"),
      paste0("P", yy, "_POP2529"),
      paste0("P", yy, "_POP30P")
    )
    
    cols_pop <- cols_pop[cols_pop %in% names(df)]
    
    df$population_totale <- rowSums(df[, cols_pop], na.rm = TRUE)
    
    df$low_edu_n <- rowSums(
      df[, c(paste0("P", yy, "_NSCOL15P_DIPLMIN"),   #Sans diplome ou CEP
             paste0("P", yy, "_NSCOL15P_BEPC"))],     #BEPC, Brevet ou DNB
      na.rm = TRUE
    )
    
    df$medium_edu_n <- rowSums(
      df[, c(paste0("P", yy, "_NSCOL15P_CAPBEP"), # CAP-BEP
             paste0("P", yy, "_NSCOL15P_BAC"))],  # BAC
      na.rm = TRUE
    )
    
    df$high_edu_n <- rowSums(
      df[, c(paste0("P", yy, "_NSCOL15P_SUP2"),
             paste0("P", yy, "_NSCOL15P_SUP34"),
             paste0("P", yy, "_NSCOL15P_SUP5"))],
      na.rm = TRUE
    )
    
    P_NSCOL15P <- df[[paste0("P", yy, "_NSCOL15P")]]
    
    df$Low_edu_prop <-  df$low_edu_n/ P_NSCOL15P
    df$Medium_edu_prop <-  df$medium_edu_n/ P_NSCOL15P
    df$High_edu_prop <-  df$high_edu_n/ P_NSCOL15P
    
    df <- select(df, c("CODGEO", "REG", "DEP", "LIBGEO",
                       "population_totale","Low_edu_prop", "Medium_edu_prop", "High_edu_prop"))
    
    assign(df_name, df)
  }
}



saveRDS(Insee_C_2021, "0_input/INSEE/Commune/Insee_C_2021.rds")

saveRDS(Insee_I_2018, "0_input/INSEE/Commune/Insee_I_2018.rds")
saveRDS(Insee_I_2019, "0_input/INSEE/Commune/Insee_I_2019.rds")
saveRDS(Insee_I_2020, "0_input/INSEE/Commune/Insee_I_2020.rds")
saveRDS(Insee_I_2021, "0_input/INSEE/Commune/Insee_I_2021.rds")
saveRDS(Insee_I_2022, "0_input/INSEE/Commune/Insee_I_2022.rds")

################################################################################

COG_akinator(vecteur_codgeo = Insee_C_2011[,1], donnees_insee = TRUE) #2013
COG_akinator(vecteur_codgeo = Insee_C_2015[,1], donnees_insee = TRUE) #2024
COG_akinator(vecteur_codgeo = Insee_C_2021[,1], donnees_insee = TRUE) #2024

diagnostic <- diag_COG(Insee_C_2015)








