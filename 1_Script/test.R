#####################
###------Test-----###
#####################

rm(list=ls())

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(terra)


ATMO_emm_2010 <- read.csv("0_input/ATMO/national_emissions_epci_2010.csv")
ATMO_emm_2022 <- read.csv("0_input/ATMO/national_emissions_epci_2022.csv")
EPCI_FR <- readRDS("0_input/Contour_EPCI/EPCI_FR.RDS")

freq(ATMO_emm_2010$aasqa)
ATMO_emm_2010 <-ATMO_emm_2010 %>% 
  rename("CODE_EPCI" = "code")

ATMO_emm_2022 <-ATMO_emm_2022 %>% 
  rename("CODE_EPCI" = "code")

ATMO_emm_2010$CODE_EPCI <- as.character(ATMO_emm_2010$CODE_EPCI)
EPCI_FR$CODE_EPCI<- as.character(EPCI_FR$CODE_EPCI)

ATMO_emm_2022$CODE_EPCI <- as.character(ATMO_emm_2022$CODE_EPCI)

#code_pcaet = source de pollution  
#code = Code_EPCI 


################################################################################

EPCI_ATMO_2010 <- EPCI_FR %>% 
  select(geometry,CODE_EPCI) %>%
  left_join(ATMO_emm_2010, by = "CODE_EPCI")

EPCI_ATMO_2022 <- EPCI_FR %>% 
  select(geometry,CODE_EPCI) %>%
  left_join(ATMO_emm_2022, by = "CODE_EPCI")

################################################################################


EPCI_ATMO_2010$pm25_kgkm2 <- EPCI_ATMO_2010$pm25 / EPCI_ATMO_2010$superficie

min(EPCI_ATMO_2010$pm25_kgkm2, na.rm = T)
max(EPCI_ATMO_2010$pm25_kgkm2, na.rm = T)

test <- subset(EPCI_ATMO_2022, pm25 == max(pm25, na.rm = TRUE))



################################################################################
ggplot(EPCI_ATMO_2010) +
  geom_sf(aes(fill = pm10), color = NA) +
  theme_minimal() +
  labs(
    title = "PM10"
  )
################################################################################


###---Données ineris---###


INERIS_2015_NO2_ANNUAL <- rast("0_input/INERIS/INERIS.REANALYSED.FRA.2015/INERIS.REANALYSED.FRA03.2015.NO2.avgannual.2gis.nc")
INERIS_2015_NO2_ANNUAL
plot(INERIS_2015_NO2_ANNUAL,
     main = "NO2 annuel – France 2015 (INERIS)")
