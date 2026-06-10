###############################
####------Regression-------####
###############################

library(questionr)
# library(tmap)
# library(spgwr)
# library(spdep)
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

map_data_biv <- readRDS("0_input/map_data_COG2024_C_2021_biv.rds")
map_data <- readRDS("0_input/map_data_COG2024_C_2021.rds")
map_data_biv <-st_drop_geometry(map_data_biv)
map_data <-st_drop_geometry(map_data)

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10",
             "MOY.NO2_norm" = "NO2", "MOY.O3_norm" = "O3", "MOY.PM25_norm" ="PM2.5" ,"MOY.PM10_norm"= "PM10")

Seuil <-c("THRESHOLD.WHO.NO2", "THRESHOLD.WHO.PM25","THRESHOLD.WHO.PM10",
          "THRESHOLD.F.NO2", "THRESHOLD.F.PM25","THRESHOLD.F.PM10")

sociales <-c( "High_edu_prop","Q221")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
             "Prop_Immigre" = "Immigration",
             "D121" = "1er Décile","Q221" = "Médiane","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")

urbanicity <-c("All","Urbain", "Urbain intermédiaire", "Rural")

################################################################################
##-normalized--##

vars_to_transform <- map_data_biv[, c(Polluants,sociales)] 
vars_transformed <- scale(vars_to_transform)
colnames(vars_transformed) <- paste0(colnames(vars_to_transform), "_norm")
map_data_biv <- cbind(map_data_biv, vars_transformed)

Polluants_norm <- paste0(Polluants, "_norm")
Sociales_norm <- paste0(sociales, "_norm")

map_data_U  <- subset(map_data_biv, map_data_biv$DENS == 1)   
map_data_UI <- subset(map_data_biv, map_data_biv$DENS == 2) 
map_data_R  <- subset(map_data_biv, map_data_biv$DENS == 3)

datasets <- c("map_data_biv","map_data_U","map_data_UI","map_data_R")


################################################################################

mod0 <- lm(MOY.NO2 ~ High_edu_prop, data = map_data_biv)
summary(mod0)

mod1 <- lm(MOY.NO2 ~ High_edu_prop, data = map_data_biv, weights = POPULATION.x)
summary(mod1)

P.scatter <-  ggplot(map_data_biv, aes(x = High_edu_prop, y = MOY.NO2, color = LIBDENS))+
  geom_point() +
  geom_labelsmooth(aes(label = LIBDENS), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_light()

scatter_pol <- function(plot.data, polluant, sociale, urbanicity, ylim = NULL){
  
  plot <- ggplot(plot.data, aes(x = .data[[sociale]], y = .data[[polluant]]))+
    geom_point(colour = "#04084E", alpha = .25) +
    geom_smooth(method = "lm", color="red", fill="#69b3a2", se=TRUE) +
    theme_light()+
    ggtitle(urbanicity)+
    labs(x = Label.S[ gsub("_norm$", "", sociale)], y = Label.P[ gsub("_norm$", "", polluant)])+ 
    coord_cartesian(ylim = ylim) 
  
  output.file  <- paste0("2_Results/Regression/",urbanicity,"/",polluant,"_" ,sociale, ".png" )
  ggsave(output.file, plot, width = 4, height = 4)
  
}


# 
# for (u in 1:length(urbanicity)){
#   dataset <- get(datasets[u])
#   
#   for (p in Polluants){
#     ylim_raw  <- range(sapply(datasets, function(d) range(get(d)[[p]], na.rm = TRUE)))
#     ylim_norm <- range(sapply(datasets, function(d) range(get(d)[[paste0(p, "_norm")]], na.rm = TRUE)))
#     for (s in sociales){
#       scatter_pol(dataset,p,s,urbanicity[u], ylim = ylim_raw)
#       scatter_pol(dataset,paste0(p,"_norm"),paste0(s,"_norm"),urbanicity[u], ylim = ylim_norm)
#     } 
#   } 
# }




################################################################################

run_model <- function(dataset, exposure, social, weighted, norm, urban) {
  
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
  res$Exposure   <- exposure
  res            <- res[, c("Urbanicity", "Sociale", "Exposure", "model",
                           "term", "estimate", "conf.low", "conf.high", "p.value", "R2")]
  
  rd              <- dataset %>%
    filter(!is.na(.data[[exposure]]), !is.na(.data[[social]])) %>%
    select(CODGEO)
  rd$residual     <- residuals(mod)
  rd$Urbanicity   <- urban
  rd$Exposure     <- exposure
  rd$Sociale      <- social
  rd$model        <- res$model[[1]]
  rd$norm         <- norm
  
  list(res = res, rd = rd)
}

n_iter        <- length(urbanicity) * length(Polluants) * length(sociales) * 4L
all_list       <- vector("list", n_iter)
residuals_list <- vector("list", n_iter)
idx            <- 1L

configs <- list(
  list(norm = FALSE, weighted = FALSE),
  list(norm = FALSE, weighted = TRUE),
  list(norm = TRUE,  weighted = FALSE),
  list(norm = TRUE,  weighted = TRUE)
)

for (u in seq_along(urbanicity)) {
  dataset <- get(datasets[u])
  
  for (p in seq_along(Polluants)) {
    for (s in seq_along(sociales)) {
      for (cfg in configs) {
        exposure <- if (cfg$norm) Polluants_norm[p] else Polluants[p]
        social   <- if (cfg$norm) Sociales_norm[s]  else sociales[s]
        
        r <- run_model(dataset, exposure, social, cfg$weighted, cfg$norm, urbanicity[u])
        all_list[[idx]]       <- r$res
        residuals_list[[idx]] <- r$rd
        idx <- idx + 1L
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

# 
# ################################################################################
# 
# All <- data.frame(Urbanicity = character(),
#                   Sociale =character(),
#                   Exposure = character(),
#                   model = character(),
#                   term = character(),
#                   estimate = numeric(),
#                   conf.low = numeric(),
#                   conf.high = numeric(),
#                   p.value = numeric(),
#                   R2 = numeric(),
#                   stringsAsFactors = FALSE)
# 
# Residuals_all <- data.frame()
u<-1
p<-1
s<-1

# for (u in 1:length(urbanicity)){
#   
#   dataset <- get(datasets[u])
#   
#   for (p in 1:length(Polluants)){
#     
#     for (s in 1:length(sociales)){
#       
#       #M0
#       vars <- c(sociales[s])
#       f <- paste(Polluants[p], paste0(vars,collapse="+"), sep="~")
#       mod0 <-with(dataset, lm(as.formula(f)))
#       
#       resid_df <- dataset %>%
#         filter(!is.na(.data[[Polluants[p]]]), !is.na(.data[[sociales[s]]])) %>%
#         select(CODGEO)
#       resid_df$residual   <- residuals(mod0)
#       resid_df$Urbanicity <- urbanicity[u]
#       resid_df$Exposure   <- Polluants[p]
#       resid_df$Sociale    <- sociales[s]
#       resid_df$model      <- "M0"
#       resid_df$norm      <- FALSE
#       
#       res.mod0 <- as.data.frame(tidy(mod0, conf.int=TRUE))
#       res.mod0 <- res.mod0[grep(as.character(sociales[s]), res.mod0$term),c("term", "estimate","conf.low","conf.high","p.value")]
#       res.mod0$model <- "M0"
#       res.mod0$Urbanicity <- urbanicity[u]
#       res.mod0$R2 <- summary(mod0)$r.squared
#       res.mod0$Sociale <- sociales[s]
#       res.mod0$Exposure <- Polluants[p]
#       
#       res.mod0 <- res.mod0[, c("Urbanicity","Sociale", "Exposure", "model", "term", "estimate", "conf.low", "conf.high", "p.value", "R2")]
#       
#       All <- rbind(All,res.mod0)
#       Residuals_all <- rbind(Residuals_all, resid_df)
#       
#       #M1
#       vars <- c(sociales[s])
#       f <- paste(Polluants[p], paste0(vars,collapse="+"), sep="~")
#       mod1 <-with(dataset, lm(as.formula(f), weights = POPULATION.x))
#       resid_df <- dataset %>%
#         filter(!is.na(.data[[Polluants[p]]]), !is.na(.data[[sociales[s]]])) %>%
#         select(CODGEO)
#       resid_df$residual   <- residuals(mod1)
#       resid_df$Urbanicity <- urbanicity[u]
#       resid_df$Exposure   <- Polluants[p]
#       resid_df$Sociale    <- sociales[s]
#       resid_df$model      <- "M1"
#       resid_df$norm      <- FALSE
#       
#       res.mod1 <- as.data.frame(tidy(mod1, conf.int=TRUE))
#       res.mod1 <- res.mod1[grep(as.character(sociales[s]), res.mod1$term),c("term", "estimate","conf.low","conf.high","p.value")]
#       res.mod1$model <- "M1"
#       res.mod1$Urbanicity <- urbanicity[u]
#       res.mod1$R2 <- summary(mod0)$r.squared
#       res.mod1$Sociale <- sociales[s]
#       res.mod1$Exposure <- Polluants[p]
#       
#       res.mod1 <- res.mod1[, c("Urbanicity","Sociale", "Exposure", "model", "term", "estimate", "conf.low", "conf.high", "p.value", "R2")]
#       
#       All <- rbind(All,res.mod1)
#       Residuals_all <- rbind(Residuals_all, resid_df)
#       
#     
#       #M0
#       vars <- c(Sociales_norm[s])
#       f <- paste(Polluants_norm[p], paste0(vars,collapse="+"), sep="~")
#       mod0 <-with(dataset, lm(as.formula(f)))
#       resid_df <- dataset %>%
#         filter(!is.na(.data[[Polluants[p]]]), !is.na(.data[[sociales[s]]])) %>%
#         select(CODGEO)
#       resid_df$residual   <- residuals(mod1)
#       resid_df$Urbanicity <- urbanicity[u]
#       resid_df$Exposure   <- Polluants[p]
#       resid_df$Sociale    <- sociales[s]
#       resid_df$model      <- "M0"
#       resid_df$norm      <- TRUE
#       
#       res.mod0 <- as.data.frame(tidy(mod0, conf.int=TRUE))
#       res.mod0 <- res.mod0[grep(as.character(Sociales_norm[s]), res.mod0$term),c("term", "estimate","conf.low","conf.high","p.value")]
#       res.mod0$model <- "M0"
#       res.mod0$Urbanicity <- urbanicity[u]
#       res.mod0$R2 <- summary(mod0)$r.squared
#       res.mod0$Sociale <- Sociales_norm[s]
#       res.mod0$Exposure <- Polluants_norm[p]
#       
#       res.mod0 <- res.mod0[, c("Urbanicity","Sociale", "Exposure", "model", "term", "estimate", "conf.low", "conf.high", "p.value", "R2")]
#       
#       All <- rbind(All,res.mod0)
#       Residuals_all <- rbind(Residuals_all, resid_df)
#       
#       #M1
#       vars <- c(Sociales_norm[s])
#       f <- paste(Polluants_norm[p], paste0(vars,collapse="+"), sep="~")
#       mod1 <-with(dataset, lm(as.formula(f), weights = POPULATION.x))
#       resid_df <- dataset %>%
#         filter(!is.na(.data[[Polluants[p]]]), !is.na(.data[[sociales[s]]])) %>%
#         select(CODGEO)
#       resid_df$residual   <- residuals(mod1)
#       resid_df$Urbanicity <- urbanicity[u]
#       resid_df$Exposure   <- Polluants[p]
#       resid_df$Sociale    <- sociales[s]
#       resid_df$model      <- "M1"
#       resid_df$norm      <- TRUE
#       
#       res.mod1 <- as.data.frame(tidy(mod1, conf.int=TRUE))
#       res.mod1 <- res.mod1[grep(as.character(Sociales_norm[s]), res.mod1$term),c("term", "estimate","conf.low","conf.high","p.value")]
#       res.mod1$model <- "M1"
#       res.mod1$Urbanicity <- urbanicity[u]
#       res.mod1$R2 <- summary(mod0)$r.squared
#       res.mod1$Sociale <- Sociales_norm[s]
#       res.mod1$Exposure <- Polluants_norm[p]
#       
#       res.mod1 <- res.mod1[, c("Urbanicity","Sociale", "Exposure", "model", "term", "estimate", "conf.low", "conf.high", "p.value", "R2")]
#       
#       All <- rbind(All,res.mod1)
#       Residuals_all <- rbind(Residuals_all, resid_df)
#       
#       
#     }
#   }
#   
# }
# 

# idx_used <- as.integer(names(residuals(mod0)))
# 
# map_sub.nb <- poly2nb(map_data_biv[idx_used, ])
# #Matrice
# map_sub.lw <- nb2listw(map_sub.nb, zero.policy = TRUE)
# 
# #test de Moran
# moran.test(residuals(mod0), 
#            listw       = map_sub.lw, 
#            zero.policy = TRUE)
# 
# modele_MCO <- lm(MOY.NO2 ~ High_edu_prop , data = map_data_biv)
# 
# centroids <- st_centroid(map_data_biv)
# 
# # Extraction des coordonnées
# coords <- st_coordinates(centroids)
# 
# # Ajout dans ton dataframe
# map_data_biv$X <- coords[,1]
# map_data_biv$Y <- coords[,2]
# 
# #nombre de voisin optimal
# bwCVa_voisins <- gwr.sel(MOY.NO2 ~ High_edu_prop,
#                          data = map_data_biv,
#                          method = "cv",          # Méthode cv ou AIC
#                          gweight = gwr.bisquare, # gwr.gauss ou gwr.bisquare
#                          adapt = TRUE,           # adaptatif
#                          verbose = FALSE,
#                          RMSE = TRUE,
#                          longlat = FALSE,
#                          coords=cbind(map_data_biv$X,map_data_biv$Y))
# # modele GRW
# modele_gwr <- gwr(NO2 ~ Pct0_14 + Pct_65 + Pct_Img + Pct_brevet + NivVieMed,
#                   data = LyonIris,
#                   adapt= bwCVa_voisins,
#                   gweight = gwr.bisquare,
#                   hatmatrix = TRUE,
#                   se.fit = TRUE,
#                   coords = cbind(LyonIris$X,LyonIris$Y),
#                   longlat = FALSE)
