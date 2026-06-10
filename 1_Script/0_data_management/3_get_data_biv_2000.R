library(dplyr)
library(sf)
library(classInt)
library(purrr)
library(stringr)

rm(list=ls())

###---Loading data sets---###

map_data_2024 <-  readRDS("0_input/map_data_COG2024_C_2021.rds")

###--- fonction terciles ---###
tercile_cut <- function(x) {
  brks <- classIntervals(x[!is.na(x)], n = 3, style = "quantile")$brks
  cut(x, breaks = brks, include.lowest = TRUE, labels = 1:3)
}

###--- variables par groupe ---###
polluants <- c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop")
immi_var  <- "Prop_Immigre"

###--- création des classes ---###
map_data_biv_2000 <- map_data_2024 %>%
  filter(population_totale > 2000) %>% 
  mutate(
    across(all_of(polluants), tercile_cut, .names =  "{str_remove(.col, 'MOY.')}.cl"),
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    across(all_of(immi_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
  )

polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
immi_cl      <- paste0(str_remove(immi_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh")
immi_label      <- paste0(str_remove(immi_var, "_prop"))

for(p in (1:length(polluants_cl))){
  
  for(e in (1:length(edu_cl))){
    
    new_name <- paste0(polluants_label[p], "_", edu_label[e])
    print(new_name)
    
    map_data_biv_2000[[new_name]] <- ifelse(
      is.na(map_data_biv_2000[[polluants_cl[p]]]) | is.na(map_data_biv_2000[[edu_cl[e]]]),
      NA,
      paste0(map_data_biv_2000[[polluants_cl[p]]], "-", map_data_biv_2000[[edu_cl[e]]])
    )
  }
  
  for(i in (1:length(immi_cl))){
    
    new_name <- paste0(polluants_label[p], "_", immi_label[i])
    print(new_name)
    
    map_data_biv_2000[[new_name]] <- ifelse(
      is.na(map_data_biv_2000[[polluants_cl[p]]]) | is.na(map_data_biv_2000[[immi_cl[i]]]),
      NA,
      paste0(map_data_biv_2000[[polluants_cl[p]]], "-", map_data_biv_2000[[immi_cl[i]]])
    )
  }
}



random_index <- sample(1:3000, 100)
verif_tercile <- map_data_biv_2000[random_index, c(polluants_cl, polluants,
                                              edu_cl, edu_vars, c("NO2_EduLow", "NO2_EduMed", "NO2_EduHigh",
                                                                  "O3_EduLow", "O3_EduMed", "O3_EduHigh",
                                                                  "PM25_EduLow", "PM25_EduMed", "PM25_EduHigh",
                                                                  "PM10_EduLow", "PM10_EduMed", "PM10_EduHigh"),
                                              immi_cl, immi_var, c("NO2_Prop_Immigre", "O3_Prop_Immigre", "PM25_Prop_Immigre", "PM10_Prop_Immigre"))]

saveRDS(map_data_biv_2000, "0_Input/map_data_COG2024_C_2021_biv2000.rds")

