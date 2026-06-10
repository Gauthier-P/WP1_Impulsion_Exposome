library(dplyr)
library(sf)
library(classInt)
library(purrr)
library(stringr)

rm(list=ls())

###---Loading data sets---###

map_data_2011 <-  readRDS("0_input/map_data_COG2024_C_2011.rds")
map_data_2015 <-  readRDS("0_input/map_data_COG2024_C_2015.rds")
map_data_2021 <-  readRDS("0_input/map_data_COG2024_C_2021.rds")

###--- fonction terciles ---###
tercile_cut <- function(x) {
  brks <- classIntervals(x[!is.na(x)], n = 3, style = "quantile")$brks
  cut(x, breaks = brks, include.lowest = TRUE, labels = 1:3)
}

###--- variables par groupe ---###
polluants <- c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","edu_ratio_lh")
# immi_var  <- "Prop_Immigre"
# rev_var <- c("D121","Q221","D921","RD")

###--- création des classes ---###

map_data_biv_2021 <- map_data_2021 %>%
  mutate(
    across(all_of(polluants), tercile_cut, .names =  "{str_remove(.col, 'MOY.')}.cl"),
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
    # across(all_of(immi_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(rev_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
  )

polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
# immi_cl      <- paste0(str_remove(immi_var, "_prop"),".cl")
# rev_cl      <- paste0(str_remove(rev_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh","Edu_r")
# immi_label      <- paste0(str_remove(immi_var, "_prop"))
# rev_label      <- rev_var

for(p in (1:length(polluants_cl))){
  
  for(e in (1:length(edu_cl))){
    
    new_name <- paste0(polluants_label[p], "_", edu_label[e])
    print(new_name)
    
    map_data_biv_2021[[new_name]] <- ifelse(
      is.na(map_data_biv_2021[[polluants_cl[p]]]) | is.na(map_data_biv_2021[[edu_cl[e]]]),
      NA,
      paste0(map_data_biv_2021[[polluants_cl[p]]], "-", map_data_biv_2021[[edu_cl[e]]])
    )
  }
  
  # for(i in (1:length(immi_cl))){
  #   
  #   new_name <- paste0(polluants_label[p], "_", immi_label[i])
  #   print(new_name)
  #   
  #   map_data_biv_2021[[new_name]] <- ifelse(
  #     is.na(map_data_biv_2021[[polluants_cl[p]]]) | is.na(map_data_biv_2021[[immi_cl[i]]]),
  #     NA,
  #     paste0(map_data_biv_2021[[polluants_cl[p]]], "-", map_data_biv_2021[[immi_cl[i]]])
  #   )
  # }
  # 
  # for(r in (1:length(rev_cl))){
  # 
  #   new_name <- paste0(polluants_label[p], "_", rev_label[r])
  #   print(new_name)
  # 
  #   map_data_biv_2021[[new_name]] <- ifelse(
  #     is.na(map_data_biv_2021[[polluants_cl[p]]]) | is.na(map_data_biv_2021[[rev_cl[r]]]),
  #     NA,
  #     paste0(map_data_biv_2021[[polluants_cl[p]]], "-", map_data_biv_2021[[rev_cl[r]]])
  #   )
  # }
  # 
  
}



###---2015---###
polluants <- c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","edu_ratio_lh")
# rev_var <- c("Q215")

###--- création des classes ---###

map_data_biv_2015 <- map_data_2015 %>%
  mutate(
    across(all_of(polluants), tercile_cut, .names =  "{str_remove(.col, 'MOY.')}.cl"),
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(rev_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
  )

polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
# immi_cl      <- paste0(str_remove(immi_var, "_prop"),".cl")
# rev_cl      <- paste0(str_remove(rev_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh","Edu_r")
# immi_label      <- paste0(str_remove(immi_var, "_prop"))
# rev_label      <- rev_var

for(p in (1:length(polluants_cl))){
  
  for(e in (1:length(edu_cl))){
    
    new_name <- paste0(polluants_label[p], "_", edu_label[e])
    print(new_name)
    
    map_data_biv_2015[[new_name]] <- ifelse(
      is.na(map_data_biv_2015[[polluants_cl[p]]]) | is.na(map_data_biv_2015[[edu_cl[e]]]),
      NA,
      paste0(map_data_biv_2015[[polluants_cl[p]]], "-", map_data_biv_2015[[edu_cl[e]]])
    )
  }
  
  # for(r in (1:length(rev_cl))){
  #   
  #   new_name <- paste0(polluants_label[p], "_", rev_label[r])
  #   print(new_name)
  #   
  #   map_data_biv_2015[[new_name]] <- ifelse(
  #     is.na(map_data_biv_2015[[polluants_cl[p]]]) | is.na(map_data_biv_2015[[rev_cl[r]]]),
  #     NA,
  #     paste0(map_data_biv_2015[[polluants_cl[p]]], "-", map_data_biv_2015[[rev_cl[r]]])
  #   )
  # }
  
  
}



###---2011---###
polluants <- c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","edu_ratio_lh")
# rev_var <- c("Q211")

map_data_biv_2011 <- map_data_2011 %>%
  mutate(
    across(all_of(polluants), tercile_cut, .names =  "{str_remove(.col, 'MOY.')}.cl"),
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(rev_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
  )



polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
# rev_cl      <- paste0(str_remove(rev_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh","Edu_r")
# rev_label      <- rev_var

for(p in (1:length(polluants_cl))){
  
  for(e in (1:length(edu_cl))){
    
    new_name <- paste0(polluants_label[p], "_", edu_label[e])
    print(new_name)
    
    map_data_biv_2011[[new_name]] <- ifelse(
      is.na(map_data_biv_2011[[polluants_cl[p]]]) | is.na(map_data_biv_2011[[edu_cl[e]]]),
      NA,
      paste0(map_data_biv_2011[[polluants_cl[p]]], "-", map_data_biv_2011[[edu_cl[e]]])
    )
  }
  
  # for(r in (1:length(rev_cl))){
  #   
  #   new_name <- paste0(polluants_label[p], "_", rev_label[r])
  #   print(new_name)
  #   
  #   map_data_biv_2011[[new_name]] <- ifelse(
  #     is.na(map_data_biv_2011[[polluants_cl[p]]]) | is.na(map_data_biv_2011[[rev_cl[r]]]),
  #     NA,
  #     paste0(map_data_biv_2011[[polluants_cl[p]]], "-", map_data_biv_2011[[rev_cl[r]]])
  #   )
  # }
  # 
  # 
}



saveRDS(map_data_biv_2011, "0_Input/map_data_COG2024_C_2011_biv.rds")
saveRDS(map_data_biv_2015, "0_Input/map_data_COG2024_C_2015_biv.rds")
saveRDS(map_data_biv_2021, "0_Input/map_data_COG2024_C_2021_biv.rds")

