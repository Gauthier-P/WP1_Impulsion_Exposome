###############################
####------Regression-------####
###############################

library(questionr)
library(gtsummary)
library(ggplot2)
library(rmapshaper)
library(sf)
library(dplyr)
library(naniar)
library(openxlsx)
library(geomtextpath)
library(broom)


rm(list=ls())

graphics.off()
###---Données merged---###

map_data_2011 <-  readRDS("0_input/map_data_COG2024_C_2011_biv_seuil.rds")
map_data_2015 <-  readRDS("0_input/map_data_COG2024_C_2015_biv_seuil.rds")
map_data_2021 <-  readRDS("0_input/map_data_COG2024_C_2021_biv_seuil.rds")

map_data_2011 <-st_drop_geometry(map_data_2011)
map_data_2015 <-st_drop_geometry(map_data_2015)
map_data_2021 <-st_drop_geometry(map_data_2021)

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


################################################################################

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10",
             "MOY.NO2_norm" = "NO2", "MOY.O3_norm" = "O3", "MOY.PM25_norm" ="PM2.5" ,"MOY.PM10_norm"= "PM10")

Seuil <-c("THRESHOLD.WHO.NO2", "THRESHOLD.WHO.PM25","THRESHOLD.WHO.PM10",
          "THRESHOLD.F.NO2", "THRESHOLD.F.PM25","THRESHOLD.F.PM10")

sociales <-c( "High_edu_prop","Q2", "edu_ratio_lh")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
             "Prop_Immigre" = "Immigration",
             "D121" = "1er Décile","Q2" = "Médiane","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")

Polluants_norm <- paste0(Polluants, "_norm")
Sociales_norm <- paste0(sociales, "_norm")

urbanicity <-c("A","U", "UI", "R")

Annee <-c("2011","2015","2021")

datasets <- c("map_data_2011","map_data_2011_U","map_data_2011_UI","map_data_2011_R",
              "map_data_2015","map_data_2015_U","map_data_2015_UI","map_data_2015_R",
              "map_data_2021","map_data_2021_U","map_data_2021_UI","map_data_2021_R")

################################################################################

##-normalized--##

#2011
vars_to_transform <- map_data_2011[, c(Polluants,sociales)] 
vars_transformed <- scale(vars_to_transform)
colnames(vars_transformed) <- paste0(colnames(vars_to_transform), "_norm")
map_data_2011 <- cbind(map_data_2011, vars_transformed)

map_data_2011_A <- map_data_2011 
map_data_2011_U  <- subset(map_data_2011, map_data_2011$DENS == 1)   
map_data_2011_UI <- subset(map_data_2011, map_data_2011$DENS == 2) 
map_data_2011_R  <- subset(map_data_2011, map_data_2011$DENS == 3)

#2015
vars_to_transform <- map_data_2015[, c(Polluants,sociales)] 
vars_transformed <- scale(vars_to_transform)
colnames(vars_transformed) <- paste0(colnames(vars_to_transform), "_norm")
map_data_2015 <- cbind(map_data_2015, vars_transformed)

map_data_2015_A <- map_data_2015 
map_data_2015_U  <- subset(map_data_2015, map_data_2015$DENS == 1)   
map_data_2015_UI <- subset(map_data_2015, map_data_2015$DENS == 2) 
map_data_2015_R  <- subset(map_data_2015, map_data_2015$DENS == 3)

#2021
vars_to_transform <- map_data_2021[, c(Polluants,sociales)] 
vars_transformed <- scale(vars_to_transform)
colnames(vars_transformed) <- paste0(colnames(vars_to_transform), "_norm")
map_data_2021 <- cbind(map_data_2021, vars_transformed)

map_data_2021_A <- map_data_2021
map_data_2021_U  <- subset(map_data_2021, map_data_2021$DENS == 1)   
map_data_2021_UI <- subset(map_data_2021, map_data_2021$DENS == 2) 
map_data_2021_R  <- subset(map_data_2021, map_data_2021$DENS == 3)

################################################################################

run_model <- function(dataset, annee,exposure, social, weighted, norm, urban) {
  
  f   <- as.formula(paste(exposure, social, sep = "~"))
  mod <- if (weighted)
    lm(f, data = dataset, weights = dataset$POPULATION.x)
  else
    lm(f, data = dataset)
  
  res <- as.data.frame(tidy(mod, conf.int = TRUE))
  res <- res[,c("term", "estimate", "conf.low", "conf.high", "p.value")]
  res$model      <- if (weighted) "M1" else "M0"
  res$Urbanicity <- urban
  res$R2         <- summary(mod)$r.squared
  res$Sociale    <- social
  res$Annee      <- annee
  res$Exposure   <- exposure
  res            <- res[, c("Urbanicity","Annee", "Sociale", "Exposure", "model",
                            "term", "estimate", "conf.low", "conf.high", "p.value", "R2")]
  
  rd              <- dataset %>%
    filter(!is.na(.data[[exposure]]), !is.na(.data[[social]])) %>%
    select(CODGEO)
  rd$residual     <- residuals(mod)
  rd$Urbanicity   <- urban
  rd$Exposure     <- exposure
  rd$Sociale      <- social
  rd$Annee        <- annee
  rd$model        <- res$model[[1]]
  rd$norm         <- norm
  
  list(res = res, rd = rd)
}

n_iter         <- length(urbanicity) * length(Polluants) * length(sociales) * length(Annee) * 4L
all_list       <- vector("list", n_iter)
residuals_list <- vector("list", n_iter)
idx            <- 1L

configs <- list(
  list(norm = FALSE, weighted = FALSE),
  list(norm = FALSE, weighted = TRUE),
  list(norm = TRUE,  weighted = FALSE),
  list(norm = TRUE,  weighted = TRUE)
)

for (a in (Annee)){

  for (u in (urbanicity)) {
    df <- paste0("map_data_",a,"_",u)
    print(df)
    dataset <- get(df)

    for (p in seq_along(Polluants)) {
      for (s in seq_along(sociales)) {
        for (cfg in configs) {
          exposure <- if (cfg$norm) Polluants_norm[p] else Polluants[p]
          social   <- if (cfg$norm) Sociales_norm[s]  else sociales[s]

          r <- run_model(dataset, a,exposure, social, cfg$weighted, cfg$norm, u)
          all_list[[idx]]       <- r$res
          residuals_list[[idx]] <- r$rd
          idx <- idx + 1L
        }
      }
    }
  }
} 


All           <- do.call(rbind, all_list)
Residuals_all <- do.call(rbind, residuals_list)

output.file <- file.path("2_Results","Regression")
saveRDS(All, paste0(output.file,"/All_air_pollution.rds"))
saveRDS(Residuals_all, paste0(output.file,"/All_resid_air_pollution.rds"))
write.xlsx(All, "2_Results/Regression/Table_raw_linear.xlsx", rowNames = FALSE)

