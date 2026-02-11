#####################
###------EPCI-----###
#####################

rm(list=ls())

library(questionr)
library(sf)
library(dplyr)

################################################################################

ECPI_IDF <- st_read("0_input/Contour_EPCI/11-iledefrance-reduit.geojson")
ECPI_CVDL <- st_read("0_input/Contour_EPCI/24-centrevaldeloire-reduit.geojson")
ECPI_BFC <- st_read("0_input/Contour_EPCI/27-bourgognefranchecompte-reduit.geojson")
ECPI_Normandie <- st_read("0_input/Contour_EPCI/28-normandie-reduit.geojson")
ECPI_HDF <- st_read("0_input/Contour_EPCI/32-hautsdefrance-reduit.geojson")
ECPI_Grand_est <- st_read("0_input/Contour_EPCI/44-grandest-reduit.geojson")
ECPI_PDL <- st_read("0_input/Contour_EPCI/52-paysdelaloire-reduit.geojson")
ECPI_B <- st_read("0_input/Contour_EPCI/53-bretagne-reduit.geojson")
ECPI_NA <- st_read("0_input/Contour_EPCI/75-nouvelleaquitaine-reduit.geojson")
ECPI_O <- st_read("0_input/Contour_EPCI/76-occitanie-reduit.geojson")
ECPI_ARA <- st_read("0_input/Contour_EPCI/84-auvergnerohnealpes-reduit.geojson")
ECPI_PACA <- st_read("0_input/Contour_EPCI/93-paca-reduit.geojson")
ECPI_C <- st_read("0_input/Contour_EPCI/94-corse-reduit.geojson")


EPCI_FR <- bind_rows( ECPI_IDF, ECPI_CVDL, ECPI_BFC, ECPI_Normandie,ECPI_HDF,ECPI_Grand_est,
                      ECPI_PDL, ECPI_B, ECPI_NA,ECPI_O, ECPI_ARA, ECPI_PACA,ECPI_C)


saveRDS(EPCI_FR, "0_input/Contour_EPCI/EPCI_FR.RDS" )



