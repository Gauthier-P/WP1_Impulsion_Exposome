###############################
####------Description------####
###############################

library(questionr)
library(patchwork)
library(gtsummary)
library(ggplot2)
library(rmapshaper)
library(sf)
library(dplyr)
library(naniar)
library(openxlsx)

rm(list=ls())
graphics.off()

###---Données---###

map_data_2021 <- readRDS("0_input/map_data_COG2024_C_2021_biv_seuil.rds")
map_data_2015 <- readRDS("0_input/map_data_COG2024_C_2015_biv_seuil.rds")
map_data_2011 <- readRDS("0_input/map_data_COG2024_C_2011_biv_seuil.rds")

map_data_2021 <-st_drop_geometry(map_data_2021)
map_data_2015 <-st_drop_geometry(map_data_2015)
map_data_2011 <-st_drop_geometry(map_data_2011)

# map_data_merged <- readRDS("0_input/map_data_biv_seuil_2011_2015_2021_COG2024.rds")
# map_data_merged <-st_drop_geometry(map_data_merged)

################################################################################

# vars <- c("High_edu_prop", "MOY.NO2", "MOY.PM25", "MOY.PM10")
# 
# map_data_2011 <- map_data_2011 %>%
#   filter(complete.cases(select(., all_of(vars))))
# 
# map_data_2021 <- map_data_2021 %>%
#   filter(complete.cases(select(., all_of(vars))))


codgeo_communs <- intersect(map_data_2011$CODGEO, map_data_2021$CODGEO)

map_data_2011 <- map_data_2011 %>%
  filter(CODGEO %in% codgeo_communs)

map_data_2021 <- map_data_2021 %>%
  filter(CODGEO %in% codgeo_communs)

# map_data_2021 %>% count(CODGEO) %>% filter(n > 1)
# map_data_2011 %>% count(CODGEO) %>% filter(n > 1)


df_long <- bind_rows(
  map_data_2011 %>% mutate(annee = 0),
  map_data_2021 %>% mutate(annee = 1)
)

df_diff <- map_data_2021 %>%
  inner_join(map_data_2011, by = "CODGEO", suffix = c("_21", "_11")) %>%
  mutate(
    
    delta_NO2 = (MOY.NO2_21 - MOY.NO2_11),
    tx_NO2 = (MOY.NO2_21 - MOY.NO2_11)/MOY.NO2_11,
    
    delta_PM25 = (MOY.PM25_21 - MOY.PM25_11),
    tx_PM25 = (MOY.PM25_21 - MOY.PM25_11)/MOY.PM25_11,
    
    delta_PM10 = (MOY.PM10_21 - MOY.PM10_11),
    tx_PM10 = (MOY.PM10_21 - MOY.PM10_11)/MOY.PM10_11,
    
    delta_edu = High_edu_prop_21 - High_edu_prop_11,
    )


################################################################################

plot.delta.edu.ens <- ggplot(df_diff, aes(x = High_edu_prop_21, y = delta_NO2)) +
  geom_point(aes( size = POPULATION.x_21),alpha=0.10,colour = "#264653" )  +
  geom_smooth(method = "lm", aes(weight = POPULATION.x_21), colour ="#264653" ) +
  scale_color_manual(values = c("All" = "#264653",
                                "Rural"= "#BCC184",
                                "Urbain intermédiaire"= "#F4A261",
                                "Urbain dense"= "#C53D1B"))+
  scale_fill_manual(values = c("All" = "#264653",
                               "Rural"= "#BCC184",
                               "Urbain intermédiaire"= "#F4A261",
                               "Urbain dense"= "#C53D1B"))+  
  # scale_y_continuous(range = c(-10, 0), guide = "none") +
  labs(
    x = "Proportion de personne avec un haut niveau d'education",
    y = "Δ NO2 (2021 - 2011)",
    title = "Évolution de l'éducation et du NO2 par commune",
    size = "Population"
  ) +
  theme_minimal()

plot.delta.edu.urb <- ggplot(df_diff, aes(x = High_edu_prop_21, y = delta_NO2, colour = LIBDENS_21, 
                    fill = LIBDENS_21)) +
  geom_point(aes(size = POPULATION.x_21) , alpha=0.2) +
  geom_smooth(method = "lm", aes(weight = POPULATION.x_21)) +
  scale_color_manual(values = c("All" = "#264653",
                                "Rural"= "#BCC184",
                                "Urbain intermédiaire"= "#F4A261",
                                "Urbain dense"= "#C53D1B"))+
  scale_fill_manual(values = c("All" = "#264653",
                               "Rural"= "#BCC184",
                               "Urbain intermédiaire"= "#F4A261",
                               "Urbain dense"= "#C53D1B"))+  
  # scale_y_continuous(range = c(-10, 0), guide = "none") +
  labs(
    x = "Proportion de personne avec un haut niveau d'education",
    y = "Δ NO2 (2021 - 2011)",
    title = "Évolution de l'éducation et du NO2 par commune",
    size = "Population"
  ) +
  theme_minimal()

ggsave(filename = "2_Results/Evolution/delta_edu_ensemble.png", plot.delta.edu.ens, width = 8, height = 5, dpi = 300)
ggsave(filename = "2_Results/Evolution/delta_edu_by_urbanicity.png", plot.delta.edu.urb, width = 8, height = 5, dpi = 300)

################################################################################

plot.tx.edu.ens <- ggplot(df_diff, aes(x = High_edu_prop_21, y = tx_NO2)) +
  geom_point(aes( size = POPULATION.x_21),alpha=0.10,colour = "#264653" )  +
  geom_smooth(method = "lm", aes(weight = POPULATION.x_21), colour ="#264653" ) +
  scale_color_manual(values = c("All" = "#264653",
                                "Rural"= "#BCC184",
                                "Urbain intermédiaire"= "#F4A261",
                                "Urbain dense"= "#C53D1B"))+
  scale_fill_manual(values = c("All" = "#264653",
                               "Rural"= "#BCC184",
                               "Urbain intermédiaire"= "#F4A261",
                               "Urbain dense"= "#C53D1B"))+  
  # scale_y_continuous(range = c(-10, 0), guide = "none") +
  labs(
    x = "Proportion de personne avec un haut niveau d'education",
    y = "Δ NO2 (2021 - 2011)",
    title = "Évolution de l'éducation et du NO2 par commune",
    size = "Population"
  ) +
  theme_minimal()

plot.tx.edu.urb <- ggplot(df_diff, aes(x = High_edu_prop_21, y = tx_NO2, colour = LIBDENS_21, 
                                          fill = LIBDENS_21)) +
  geom_point(aes(size = POPULATION.x_21) , alpha=0.2) +
  geom_smooth(method = "lm", aes(weight = POPULATION.x_21)) +
  scale_color_manual(values = c("All" = "#264653",
                                "Rural"= "#BCC184",
                                "Urbain intermédiaire"= "#F4A261",
                                "Urbain dense"= "#C53D1B"))+
  scale_fill_manual(values = c("All" = "#264653",
                               "Rural"= "#BCC184",
                               "Urbain intermédiaire"= "#F4A261",
                               "Urbain dense"= "#C53D1B"))+  
  # scale_y_continuous(range = c(-10, 0), guide = "none") +
  labs(
    x = "Proportion de personne avec un haut niveau d'education",
    y = "Δ NO2 (2021 - 2011)",
    title = "Évolution de l'éducation et du NO2 par commune",
    size = "Population"
  ) +
  theme_minimal()

ggsave(filename = "2_Results/Evolution/tx_edu_ensemble.png", plot.tx.edu.ens, width = 8, height = 5, dpi = 300)
ggsave(filename = "2_Results/Evolution/tx_edu_by_urbanicity.png", plot.tx.edu.urb, width = 8, height = 5, dpi = 300)
