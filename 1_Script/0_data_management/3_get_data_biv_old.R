rm(list=ls())
graphics.off()

library(questionr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggtext)
library(cowplot)
library(classInt)
library(grid)
library(gridtext)
library(cowplot)
library(gtable)
library(grid)
library(gridExtra)
library(ncdf4)
library(readxl)
library(terra)
library(patchwork)



###---Données merged---###


map_data_2024 <-  readRDS("0_input/map_data_COG2024_C_2021.rds")

villes <- map_data_2024 %>%
  filter(Pop_tot > 160000)

villes.sf <- villes %>%
  st_centroid()

coords <- st_coordinates(villes.sf)

villes.sf <- villes.sf %>%
  mutate(long = coords[,1],
         lat  = coords[,2])

################################################################################

map_data_biv <- map_data_2024 %>%
  mutate(
    NO2.cl = cut(
      MOY.NO2,
      breaks = classIntervals(MOY.NO2, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    O3.cl = cut(
      MOY.O3,
      breaks = classIntervals(MOY.O3, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    PM25.cl = cut(
      MOY.PM25,
      breaks = classIntervals(MOY.PM25, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    PM10.cl = cut(
      MOY.PM10,
      breaks = classIntervals(MOY.PM10, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    Edu.low.cl = cut(
      Low_edu_prop,
      breaks = classIntervals(Low_edu_prop, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ), 
    Edu.med.cl = cut(
      Medium_edu_prop,
      breaks = classIntervals(Medium_edu_prop, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    Edu.high.cl = cut(
      High_edu_prop,
      breaks = classIntervals(High_edu_prop, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ),
    Prop_Immigre.cl = cut(
      Prop_Immigre,
      breaks = classIntervals(Prop_Immigre, n = 3, style = "quantile")$brks,
      include.lowest = TRUE,
      labels = 1:3
    ) 
  ) 

breaks_NO2     <- classIntervals(map_data_2024$MOY.NO2[!is.na(map_data_2024$MOY.NO2)], n = 3, style = "quantile")$brks
breaks_O3      <- classIntervals(map_data_2024$MOY.O3[!is.na(map_data_2024$MOY.O3)], n = 3, style = "quantile")$brks
breaks_PM25    <- classIntervals(map_data_2024$MOY.PM25[!is.na(map_data_2024$MOY.PM25)], n = 3, style = "quantile")$brks
breaks_PM10    <- classIntervals(map_data_2024$MOY.PM10[!is.na(map_data_2024$MOY.PM10)], n = 3, style = "quantile")$brks

breaks_EduLow  <- classIntervals(map_data_2024$Low_edu_prop[!is.na(map_data_2024$Low_edu_prop)], n = 3, style = "quantile")$brks
breaks_EduMed  <- classIntervals(map_data_2024$Medium_edu_prop[!is.na(map_data_2024$Medium_edu_prop)], n = 3, style = "quantile")$brks
breaks_EduHigh <- classIntervals(map_data_2024$High_edu_prop[!is.na(map_data_2024$High_edu_prop)], n = 3, style = "quantile")$brks

breaks_Immi  <- classIntervals(map_data_2024$Prop_Immigre[!is.na(map_data_2024$Prop_Immigre)], n = 3, style = "quantile")$brks

map_data_biv <- map_data_2024 %>%
  mutate(
    NO2.cl = cut(MOY.NO2, breaks = breaks_NO2, include.lowest = TRUE, labels = 1:3),
    O3.cl = cut(MOY.O3, breaks = breaks_O3, include.lowest = TRUE, labels = 1:3),
    PM25.cl = cut(MOY.PM25, breaks = breaks_PM25, include.lowest = TRUE, labels = 1:3),
    PM10.cl = cut(MOY.PM10, breaks = breaks_PM10, include.lowest = TRUE, labels = 1:3),
    
    Edu.low.cl  = cut(Low_edu_prop, breaks = breaks_EduLow, include.lowest = TRUE, labels = 1:3),
    Edu.med.cl  = cut(Medium_edu_prop, breaks = breaks_EduMed, include.lowest = TRUE, labels = 1:3),
    Edu.high.cl = cut(High_edu_prop, breaks = breaks_EduHigh, include.lowest = TRUE, labels = 1:3),
    
    Prop_Immigre.cl  = cut(Low_edu_prop, breaks = breaks_Immi, include.lowest = TRUE, labels = 1:3),
  )%>%
  mutate(
    NO2_EduLow  = ifelse(is.na(NO2.cl)  | is.na(Edu.low.cl),  NA, paste0(NO2.cl, "-", Edu.low.cl)),
    NO2_EduMed  = ifelse(is.na(NO2.cl)  | is.na(Edu.med.cl),  NA, paste0(NO2.cl, "-", Edu.med.cl)),
    NO2_EduHigh = ifelse(is.na(NO2.cl)  | is.na(Edu.high.cl), NA, paste0(NO2.cl, "-", Edu.high.cl)),
    
    O3_EduLow  = ifelse(is.na(O3.cl)  | is.na(Edu.low.cl),  NA, paste0(O3.cl, "-", Edu.low.cl)),
    O3_EduMed  = ifelse(is.na(O3.cl)  | is.na(Edu.med.cl),  NA, paste0(O3.cl, "-", Edu.med.cl)),
    O3_EduHigh = ifelse(is.na(O3.cl)  | is.na(Edu.high.cl), NA, paste0(O3.cl, "-", Edu.high.cl)),
    
    PM25_EduLow  = ifelse(is.na(PM25.cl)  | is.na(Edu.low.cl),  NA, paste0(PM25.cl, "-", Edu.low.cl)),
    PM25_EduMed  = ifelse(is.na(PM25.cl)  | is.na(Edu.med.cl),  NA, paste0(PM25.cl, "-", Edu.med.cl)),
    PM25_EduHigh = ifelse(is.na(PM25.cl)  | is.na(Edu.high.cl), NA, paste0(PM25.cl, "-", Edu.high.cl)),
    
    PM10_EduLow  = ifelse(is.na(PM10.cl)  | is.na(Edu.low.cl),  NA, paste0(PM10.cl, "-", Edu.low.cl)),
    PM10_EduMed  = ifelse(is.na(PM10.cl)  | is.na(Edu.med.cl),  NA, paste0(PM10.cl, "-", Edu.med.cl)),
    PM10_EduHigh = ifelse(is.na(PM10.cl)  | is.na(Edu.high.cl), NA, paste0(PM10.cl, "-", Edu.high.cl)),
    
    
    NO2_EduImmi  = ifelse(is.na(NO2.cl)  | is.na(Prop_Immigre.cl),  NA, paste0(NO2.cl, "-", Prop_Immigre.cl)),
    O3_EduImmi  = ifelse(is.na(O3.cl)  | is.na(Prop_Immigre.cl),  NA, paste0(O3.cl, "-", Prop_Immigre.cl)),
    PM25_EduImmi  = ifelse(is.na(PM25.cl)  | is.na(Prop_Immigre.cl),  NA, paste0(PM25.cl, "-", Prop_Immigre.cl)),
    PM10_EduImmi  = ifelse(is.na(PM10.cl)  | is.na(Prop_Immigre.cl),  NA, paste0(PM10.cl, "-", Prop_Immigre.cl))
    
  )

random_index <- sample(1:3000, 100)
verif_tercile <- map_data_biv[random_index, c("NO2.cl", "MOY.NO2",
                                              "O3.cl",  "MOY.O3",
                                              "PM25.cl","MOY.PM25", 
                                              "PM10.cl", "MOY.PM10",
                                              
                                              "Edu.low.cl","Low_edu_prop",
                                              "Edu.med.cl", "Medium_edu_prop", 
                                              "Edu.high.cl", "High_edu_prop",
                                              
                                              "Prop_Immigre.cl","Prop_Immigre"
                                              )]

saveRDS(map_data_biv,"0_Input/map_data_COG2024_C_2021_biv.rds" )