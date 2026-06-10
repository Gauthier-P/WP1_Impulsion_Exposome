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


######---2021-----######

###--- variables par groupe ---###
polluants <- c("MOY.NO2", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","edu_ratio_lh")
# edu_r  <- c("edu_ratio_lh")
# immi_var  <- "Prop_Immigre"
# rev_var <- c("D121","Q221","D921","RD")

###--- création des classes ---###
map_data_biv_2021 <- map_data_2021 %>%
  mutate(
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(immi_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(rev_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
    # across(all_of(edu_r),     tercile_cut, .names = "{.col}.cl")
  )

map_data_biv_2021 <- map_data_biv_2021 %>%
  mutate(
    
    NO2.cl = case_when(
      
      NO2.WHO.AQG == 0 ~ 1,
      NO2.WHO.target3 == 0 ~ 2,
      NO2.WHO.target2 == 0 ~ 3,
      NO2.WHO.target1 == 0 ~ 3,
    ),
    
    PM25.cl = case_when(
      PM25.WHO.AQG == 0  ~ 1,
      PM25.WHO.target4 == 0 ~ 2,
      PM25.WHO.target3 == 0  ~ 3,
      PM25.WHO.target2 == 0  ~ 3,
      PM25.WHO.target1 == 0  ~ 3,
    ),
    
    PM10.cl = case_when(
      PM10.WHO.AQG == 0  ~ 1,
      PM10.WHO.target4 == 0  ~ 2,
      PM10.WHO.target3 == 0  ~ 3,
      PM10.WHO.target2 == 0  ~ 3,
      PM10.WHO.target1 == 0  ~ 3,
    )
  )

questionr::freq(map_data_biv_2021$NO2.cl)
questionr::freq(map_data_biv_2021$NO2.cl.2)
questionr::freq(map_data_biv_2021$NO2.WHO.AQG)
questionr::freq(map_data_biv_2021$NO2.WHO.AQG)
questionr::freq(map_data_biv_2021$NO2.WHO.target3)
questionr::freq(map_data_biv_2021$NO2.WHO.target2)
questionr::freq(map_data_biv_2021$NO2.WHO.target1)

questionr::freq(map_data_biv_2021$PM25.cl)
questionr::freq(map_data_biv_2021$PM25.WHO.AQG)
questionr::freq(map_data_biv_2021$PM25.WHO.target4)
questionr::freq(map_data_biv_2021$PM25.WHO.target3)
questionr::freq(map_data_biv_2021$PM25.WHO.target2)
questionr::freq(map_data_biv_2021$PM25.WHO.target1)

questionr::freq(map_data_biv_2021$PM10.cl)
questionr::freq(map_data_biv_2021$PM10.WHO.AQG)
questionr::freq(map_data_biv_2021$PM10.WHO.target4)
questionr::freq(map_data_biv_2021$PM10.WHO.target3)
questionr::freq(map_data_biv_2021$PM10.WHO.target2)
# questionr::freq(map_data_biv_2021$PM10.WHO.target1)



polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
# # immi_cl      <- paste0(str_remove(immi_var, "_prop"),".cl")
# rev_cl      <- paste0(str_remove(rev_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh","Edu_r")
edu_r_label     <- c()
# immi_label      <- paste0(str_remove(immi_var, "_prop"))
# rev_label      <- rev_var

verif_pol <-  map_data_biv_2021[, c(polluants_cl, polluants, "NO2.WHO.target1", "NO2.WHO.target2", "NO2.WHO.target3", "NO2.WHO.AQG")]

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
  # 
}

map_data_biv_2021 <- map_data_biv_2021 %>%
  mutate(High_edu.cl = factor((High_edu.cl), levels = c("3","2","1")))

# map_data_biv_2021 <- map_data_biv_2021 %>%
  # mutate(Q221.cl = factor((Q221.cl), levels = c("3","2","1")))

map_data_biv_2021 <- map_data_biv_2021 %>%
  mutate(NO2.cl = factor((NO2.cl), levels = c("1","2","3")))
map_data_biv_2021 <- map_data_biv_2021 %>%
  mutate(PM25.cl = factor((PM25.cl), levels = c("1","2","3")))
map_data_biv_2021 <- map_data_biv_2021 %>%
  mutate(PM10.cl = factor((PM10.cl), levels = c("1","2","3")))




######---2015-----######

###--- variables par groupe ---###
polluants <- c("MOY.NO2", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","edu_ratio_lh")
# rev_var <- c("Q215")

###--- création des classes ---###
map_data_biv_2015 <- map_data_2015 %>%
  mutate(
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(rev_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
  )

map_data_biv_2015 <- map_data_biv_2015 %>%
  mutate(
    
    NO2.cl = case_when(
      
      NO2.WHO.AQG == 0 ~ 1,
      NO2.WHO.target3 == 0 ~ 2,
      NO2.WHO.target2 == 0 ~ 3,
      NO2.WHO.target1 == 0 ~ 3,
    ),
    
    PM25.cl = case_when(
      PM25.WHO.AQG == 0  ~ 1,
      PM25.WHO.target4 == 0 ~ 2,
      PM25.WHO.target3 == 0  ~ 3,
      PM25.WHO.target2 == 0  ~ 3,
      PM25.WHO.target1 == 0  ~ 3,
    ),
    
    PM10.cl = case_when(
      PM10.WHO.AQG == 0  ~ 1,
      PM10.WHO.target4 == 0  ~ 2,
      PM10.WHO.target3 == 0  ~ 3,
      PM10.WHO.target2== 0  ~ 3,
      PM10.WHO.target1== 0  ~ 3,
    )
  )

# questionr::freq(map_data_biv_2015$NO2.cl)
# questionr::freq(map_data_biv_2015$NO2.cl.2)
# questionr::freq(map_data_biv_2015$NO2.WHO.AQG)
# questionr::freq(map_data_biv_2015$NO2.WHO.AQG)
# questionr::freq(map_data_biv_2015$NO2.WHO.target3)
# questionr::freq(map_data_biv_2015$NO2.WHO.target2)
# questionr::freq(map_data_biv_2015$NO2.WHO.target1)
# 
# questionr::freq(map_data_biv_2015$PM25.cl)
# questionr::freq(map_data_biv_2015$PM25.WHO.AQG)
# questionr::freq(map_data_biv_2015$PM25.WHO.target4)
# questionr::freq(map_data_biv_2015$PM25.WHO.target3)
# questionr::freq(map_data_biv_2015$PM25.WHO.target2)
# questionr::freq(map_data_biv_2015$PM25.WHO.target1)
# 
# questionr::freq(map_data_biv_2015$PM10.cl)
# questionr::freq(map_data_biv_2015$PM10.WHO.AQG)
# questionr::freq(map_data_biv_2015$PM10.WHO.target4)
# questionr::freq(map_data_biv_2015$PM10.WHO.target3)
# questionr::freq(map_data_biv_2015$PM10.WHO.target2)
# questionr::freq(map_data_biv_2015$PM10.WHO.target1)



polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
# rev_cl      <- paste0(str_remove(rev_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh","Edu_r")
# rev_label      <- rev_var

verif_pol <-  map_data_biv_2015[, c(polluants_cl, polluants, "NO2.WHO.target1", "NO2.WHO.target2", "NO2.WHO.target3", "NO2.WHO.AQG")]

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
  # 
  # 
}

map_data_biv_2015 <- map_data_biv_2015 %>%
  mutate(High_edu.cl = factor((High_edu.cl), levels = c("3","2","1")))
# 
# map_data_biv_2015 <- map_data_biv_2015 %>%
#   mutate(Q215.cl = factor((Q215.cl), levels = c("3","2","1")))

map_data_biv_2015 <- map_data_biv_2015 %>%
  mutate(NO2.cl = factor((NO2.cl), levels = c("1","2","3")))
map_data_biv_2015 <- map_data_biv_2015 %>%
  mutate(PM25.cl = factor((PM25.cl), levels = c("1","2","3")))
map_data_biv_2015 <- map_data_biv_2015 %>%
  mutate(PM10.cl = factor((PM10.cl), levels = c("1","2","3")))

######---2011-----######

###--- variables par groupe ---###
polluants <- c("MOY.NO2", "MOY.PM25", "MOY.PM10")
edu_vars  <- c("Low_edu_prop", "Medium_edu_prop", "High_edu_prop","edu_ratio_lh")
# rev_var <- c("Q211")

###--- création des classes ---###
map_data_biv_2011 <- map_data_2011 %>%
  mutate(
    across(all_of(edu_vars),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl"),
    # across(all_of(rev_var),  tercile_cut, .names = "{str_remove(.col, '_prop')}.cl")
  )

map_data_biv_2011 <- map_data_biv_2011 %>%
  mutate(
    
    NO2.cl = case_when(
      
      NO2.WHO.AQG == 0 ~ 1,
      NO2.WHO.target3 == 0 ~ 2,
      NO2.WHO.target2 == 0 ~ 3,
      NO2.WHO.target1 == 0 ~ 3,
    ),
    
    PM25.cl = case_when(
      PM25.WHO.AQG == 0  ~ 1,
      PM25.WHO.target4 == 0 ~ 2,
      PM25.WHO.target3 == 0  ~ 3,
      PM25.WHO.target2 == 0  ~ 3,
      PM25.WHO.target1 == 0  ~ 3,
    ),
    
    PM10.cl = case_when(
      PM10.WHO.AQG == 0  ~ 1,
      PM10.WHO.target4 == 0  ~ 2,
      PM10.WHO.target3 == 0  ~ 3,
      PM10.WHO.target2 == 0  ~ 3,
      PM10.WHO.target1 == 0  ~ 3,
      TRUE ~ 1
    )
  )

# questionr::freq(map_data_biv_2011$NO2.cl)
# questionr::freq(map_data_biv_2011$NO2.cl.2)
# questionr::freq(map_data_biv_2011$NO2.WHO.AQG)
# questionr::freq(map_data_biv_2011$NO2.WHO.AQG)
# questionr::freq(map_data_biv_2011$NO2.WHO.target3)
# questionr::freq(map_data_biv_2011$NO2.WHO.target2)
# questionr::freq(map_data_biv_2011$NO2.WHO.target1)
# 
# questionr::freq(map_data_biv_2011$PM25.cl)
# questionr::freq(map_data_biv_2011$PM25.WHO.AQG)
# questionr::freq(map_data_biv_2011$PM25.WHO.target4)
# questionr::freq(map_data_biv_2011$PM25.WHO.target3)
# questionr::freq(map_data_biv_2011$PM25.WHO.target2)
# questionr::freq(map_data_biv_2011$PM25.WHO.target1)
# 
# questionr::freq(map_data_biv_2011$PM10.cl)
# questionr::freq(map_data_biv_2011$PM10.WHO.AQG)
# questionr::freq(map_data_biv_2011$PM10.WHO.target4)
# questionr::freq(map_data_biv_2011$PM10.WHO.target3)
# questionr::freq(map_data_biv_2011$PM10.WHO.target2)
# questionr::freq(map_data_biv_2011$PM10.WHO.target1)



polluants_cl <- paste0(str_remove(polluants,"MOY."), ".cl")
edu_cl       <- paste0(str_remove(edu_vars, "_prop"), ".cl")
# rev_cl      <- paste0(str_remove(rev_var, "_prop"),".cl")

polluants_label <- paste0(str_remove(polluants,"MOY."))
edu_label       <- c("EduLow", "EduMed", "EduHigh","Edu_r")
# rev_label      <- rev_var

verif_pol <-  map_data_biv_2011[, c(polluants_cl, polluants, "NO2.WHO.target1", "NO2.WHO.target2", "NO2.WHO.target3", "NO2.WHO.AQG")]

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
  
# 
#   for(r in (1:length(rev_cl))){
#     
#     new_name <- paste0(polluants_label[p], "_", rev_label[r])
#     print(new_name)
#     
#     map_data_biv_2011[[new_name]] <- ifelse(
#       is.na(map_data_biv_2011[[polluants_cl[p]]]) | is.na(map_data_biv_2011[[rev_cl[r]]]),
#       NA,
#       paste0(map_data_biv_2011[[polluants_cl[p]]], "-", map_data_biv_2011[[rev_cl[r]]])
#     )
#   }
#   
#   
}

map_data_biv_2011 <- map_data_biv_2011 %>%
  mutate(High_edu.cl = factor((High_edu.cl), levels = c("3","2","1")))
# 
# map_data_biv_2011 <- map_data_biv_2011 %>%
#   mutate(Q211.cl = factor((Q211.cl), levels = c("3","2","1")))

map_data_biv_2011 <- map_data_biv_2011 %>%
  mutate(NO2.cl = factor((NO2.cl), levels = c("1","2","3")))
map_data_biv_2011 <- map_data_biv_2011 %>%
  mutate(PM25.cl = factor((PM25.cl), levels = c("1","2","3")))
map_data_biv_2011 <- map_data_biv_2011 %>%
  mutate(PM10.cl = factor((PM10.cl), levels = c("1","2","3")))



saveRDS(map_data_biv_2011, "0_Input/map_data_COG2024_C_2011_biv_seuil.rds")
saveRDS(map_data_biv_2015, "0_Input/map_data_COG2024_C_2015_biv_seuil.rds")
saveRDS(map_data_biv_2021, "0_Input/map_data_COG2024_C_2021_biv_seuil.rds")

