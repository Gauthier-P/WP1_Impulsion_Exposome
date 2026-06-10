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
library(COGugaison)


###---Données insee---###

#Commune
Insee_I_2021 <- as.data.frame(read_xlsx("0_input/INSEE_REVENU/BASE_TD_FILO_IRIS_2021_DEC.xlsx", skip = 5, col_names = TRUE))
Insee_pop_I_2021 <- as.data.frame(read_xlsx("0_input/Insee_population_iris.xlsx", skip = 5, col_names = TRUE))

COG_akinator(vecteur_codgeo = Insee_I_2021[,1], donnees_insee = TRUE) #2025

diagnostic <- diag_COG(Insee_I_2021)

################################################################################


Insee_C_2021 <- merge(Insee_I_2021,Insee_pop_I_2021[,c("IRIS","P21_POP")], by="IRIS")
Insee_C_2021 <- Insee_C_2021 %>%
  mutate(across(
    c("DEC_D121", "DEC_D221","DEC_D321", "DEC_D421", "DEC_MED21",
      "DEC_D621","DEC_D721", "DEC_D821","DEC_D921","DEC_RD21", "P21_POP"),
    ~ as.numeric(gsub(",", ".", .))
  ))

# test <- Insee_C_2021[,c("IRIS","COM","LIBCOM","DEC_D121","P21_POP")]
# 
# test_wm <- test %>%
#   group_by(COM) %>%
#   summarise(
#     LIBCOM = first(LIBCOM),
#     across(
#       where(is.numeric) & starts_with("DEC"),
#       ~ weighted.mean(.x, P21_POP, na.rm = TRUE)
#     )
#   )
# 
# test_m <- test %>%
#   group_by(COM) %>%
#   summarise(
#     LIBCOM = first(LIBCOM),
#     across(
#       where(is.numeric) & starts_with("DEC"),mean, na.rm = TRUE)
#     )

Insee_C_2021 <- Insee_C_2021 %>%
  group_by(COM) %>%
  summarise(
    LIBCOM = first(LIBCOM),
    across(
      where(is.numeric),
      ~ weighted.mean(.x, P21_POP, na.rm = TRUE)
    )
  )


Insee_I_2021 <- select(Insee_I_2021, c("IRIS",
                                       "DEC_D121","DEC_MED21","DEC_D921","DEC_RD21"))

Insee_C_2021 <- select(Insee_C_2021, c("COM","LIBCOM",
                                       "DEC_D121","DEC_MED21","DEC_D921","DEC_RD21"))

Insee_C_2021 <- Insee_C_2021 %>%
  rename(CODGEO = COM)

saveRDS(Insee_C_2021, "0_input/INSEE_REVENU/Insee_C_2021.rds")
saveRDS(Insee_I_2021, "0_input/INSEE_REVENU/Insee_I_2021.rds")


