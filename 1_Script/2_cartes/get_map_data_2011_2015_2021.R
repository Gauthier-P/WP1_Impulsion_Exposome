library(dplyr)
library(sf)
library(classInt)
library(purrr)
library(stringr)

rm(list=ls())

###---Loading data sets---###

map_data_2011 <-  readRDS("0_input/map_data_COG2024_C_2011_biv_seuil.rds")
map_data_2015 <-  readRDS("0_input/map_data_COG2024_C_2015_biv_seuil.rds")
map_data_2021 <-  readRDS("0_input/map_data_COG2024_C_2021_biv_seuil.rds")

#############################

###--merge--###

map_data_2011 <- map_data_2011 %>%
  mutate(ANNEE = 2011) %>%
  rename(Q2 = Q211, Q2.cl = Q211.cl, NO2_Q2 = NO2_Q211,
         PM25_Q2 = PM25_Q211, PM10_Q2 = PM10_Q211) 

map_data_2015 <- map_data_2015 %>%
  mutate(ANNEE = 2015) %>%
  rename(Q2 = Q215, Q2.cl = Q215.cl, NO2_Q2 = NO2_Q215,
         PM25_Q2 = PM25_Q215, PM10_Q2 = PM10_Q215)

map_data_2021 <- map_data_2021 %>%
  mutate(ANNEE = 2021) %>%
  rename(Q2 = Q221, Q2.cl = Q221.cl, NO2_Q2 = NO2_Q221,
         PM25_Q2 = PM25_Q221, PM10_Q2 = PM10_Q221)%>%
  select(-c(Q121, Q321, D121,D221,D321,D421,D621,D721,D821,D921,RD,
            D921.cl,D121.cl,RD.cl,
            PM10_D121,PM10_D921,PM10_RD,
            NO2_D121,NO2_D921,NO2_RD,
            PM25_D121,PM25_D921,PM25_RD))

map_data_all <- bind_rows(
  map_data_2011,
  map_data_2015,
  map_data_2021
)


saveRDS(map_data_all, "0_Input/map_data_biv_seuil_2011_2015_2021_COG2024.rds")
