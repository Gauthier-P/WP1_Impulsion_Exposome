########################
###-----Chainage-----###
########################

#libraries
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
#2021
Insee_EDU_C_2021 <- readRDS("0_input/INSEE_EDU/Commune/Insee_C_2021_COG2024.rds")
# Insee_IMMI_C_2021 <- readRDS("0_input/INSEE_IMMI/Insee_C_2021_COG2024.rds")
# Insee_REV_C_2021 <- readRDS("0_input/INSEE_REVENU/Insee_C_2021_COG2024.rds")
INSEE_Densite_2021 <- readRDS("0_input/Population/Densite_2021_COG2024.rds")
#2015
Insee_EDU_C_2015 <- readRDS("0_input/INSEE_EDU/Commune/Insee_C_2015_COG2024.rds")
# Insee_REV_C_2015 <- readRDS("0_input/INSEE_REVENU/Insee_C_2015_COG2024.rds")
INSEE_Densite_2015 <- readRDS("0_input/Population/Densite_2015_COG2024.rds")
#2011
Insee_EDU_C_2011 <- readRDS("0_input/INSEE_EDU/Commune/Insee_C_2011_COG2024.rds")
# Insee_REV_C_2011 <- readRDS("0_input/INSEE_REVENU/Insee_C_2011_COG2024.rds")


###---Données INERIS---###

INERIS_Air_2011 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2011_COG2024.rds")
INERIS_Air_2015 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2015_COG2024.rds")
INERIS_Air_2021 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021_COG2024.rds")


#checking duplicate
INERIS_Air_2021[duplicated(INERIS_Air_2021$CODGEO),] #none


################################################################################

####---Harmonization---###

COG_akinator(vecteur_codgeo = Insee_EDU_C_2021[,1], donnees_insee = TRUE) 
# COG_akinator(vecteur_codgeo = Insee_IMMI_C_2021[,1], donnees_insee = TRUE)
# COG_akinator(vecteur_codgeo = Insee_REV_C_2021[,1], donnees_insee = TRUE) 
COG_akinator(vecteur_codgeo = INSEE_Densite_2021[,1], donnees_insee = TRUE) 
# COG_akinator(vecteur_codgeo = INERIS_Air_2021[,1])

head(diag_COG(Insee_EDU_C_2021)) #N = 34918 
# head(diag_COG(Insee_IMMI_C_2021)) #N = 34918 
# head(diag_COG(Insee_REV_C_2021)) #N = 34884 
head(diag_COG(INERIS_Air_2021)) #N = 34806 


###---Merging---###

dedup <- function(df, key = "CODGEO") {
  n_dup <- sum(duplicated(df[[key]]))
  if (n_dup > 0) message("Doublons supprimés dans ", deparse(substitute(df)), " : ", n_dup)
  df[!duplicated(df[[key]]), ]
}

INERIS_Air_2021_clean <- dedup(INERIS_Air_2021)
INERIS_Air_2015_clean <- dedup(INERIS_Air_2015)
INERIS_Air_2011_clean <- dedup(INERIS_Air_2011)

INSEE_Densite_2021_clean <- dedup(INSEE_Densite_2021)
INSEE_Densite_2015_clean <- dedup(INSEE_Densite_2015)

Merged_Sociale_2021 <- merge(
  Insee_EDU_C_2021,
  select(INSEE_Densite_2021_clean, -c("LIBGEO")),
  by = "CODGEO", all.x = TRUE
)
Merged_2021 <- merge(
  Merged_Sociale_2021,
  select(INERIS_Air_2021_clean, -c("LIBGEO")),
  by = "CODGEO"
)

Merged_Sociale_2015 <- merge(
  Insee_EDU_C_2015,
  select(INSEE_Densite_2015_clean, -c("LIBGEO")),
  by = "CODGEO", all.x = TRUE
)
Merged_2015 <- merge(
  Merged_Sociale_2015,
  select(INERIS_Air_2015_clean, -c("LIBGEO")),
  by = "CODGEO"
)

Merged_Sociale_2011 <- merge(
  Insee_EDU_C_2011,
  select(INSEE_Densite_2015_clean, -c("LIBGEO")), 
  by = "CODGEO", all.x = TRUE
)

Merged_2011 <- merge(
  Merged_Sociale_2011,
  select(INERIS_Air_2011_clean, -c("LIBGEO")),
  by = "CODGEO"
)

################################################################################

saveRDS(object = Merged_2021, file = "0_input/Merged/Merged_2021.rds")
saveRDS(object = Merged_2015, file = "0_input/Merged/Merged_2015.rds")
saveRDS(object = Merged_2011, file = "0_input/Merged/Merged_2011.rds")





