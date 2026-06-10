##############################
####-------Evolution-------###
##############################


library(questionr)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ncdf4)
library(readxl)
library(terra)
library(COGugaison)

rm(list = ls())

###---Données insee---###
#2021
Insee_EDU_C_2021 <- readRDS("0_input/INSEE_EDU/Commune/Insee_C_2021_COG2024.rds")
INSEE_Densite_2021 <- readRDS("0_input/Population/Densite_2021_COG2024.rds")

###---Données INERIS---###

INERIS_Air_2011 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2011.rds")
INERIS_Air_2012 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2012.rds")
INERIS_Air_2013 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2013.rds")
INERIS_Air_2014 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2014.rds")
INERIS_Air_2015 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2015.rds")
INERIS_Air_2016 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2016.rds")
INERIS_Air_2017 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2017.rds")
INERIS_Air_2018 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2018.rds")
INERIS_Air_2019 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2019.rds")
INERIS_Air_2020 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2020.rds")
INERIS_Air_2021 <-  readRDS("0_input/INERIS/Cartotheque/Commune/INERIS_Air_2021.rds")

INERIS_list <- list(
  `2011` = INERIS_Air_2011,
  `2012` = INERIS_Air_2012,
  `2013` = INERIS_Air_2013,
  `2014` = INERIS_Air_2014,
  `2015` = INERIS_Air_2015,
  `2016` = INERIS_Air_2016,
  `2017` = INERIS_Air_2017,
  `2018` = INERIS_Air_2018,
  `2019` = INERIS_Air_2019,
  `2020` = INERIS_Air_2020,
  `2021` = INERIS_Air_2021
)

INERIS_NO2 <- purrr::reduce(
  names(INERIS_list),
  function(acc, year) {
    df <- INERIS_list[[year]] %>%
      select(CODGEO, MOY.NO2) %>%
      rename(!!paste0("MOY.NO2_", year) := MOY.NO2)
    full_join(acc, df, by = "CODGEO")
  },
  .init = data.frame(CODGEO = unique(INERIS_list[["2011"]]$CODGEO))  
)

df_final <- left_join(Insee_EDU_C_2021, INERIS_NO2, by = "CODGEO")
df_final <- left_join(df_final, INSEE_Densite_2021, by = "CODGEO")

df_final <- df_final %>%
  mutate(tercile_edu = ntile(tercile_edu, 3),
         tercile_edu = factor(tercile_edu,
                              levels = 1:3,
                              labels = c("Faible", "Moyen", "Élevé"))) %>%
  filter(!is.na(tercile_edu)) 


df_long <- df_final %>%
  select(CODGEO, LIBDENS, starts_with("MOY.NO2_")) %>%
  pivot_longer(cols = starts_with("MOY.NO2_"),
               names_to = "annee",
               names_prefix = "MOY.NO2_",
               values_to = "NO2") %>%
  mutate(annee = as.integer(annee))

df_plot <- df_long %>%
  group_by(annee, LIBDENS) %>%
  summarise(
    NO2_moy = mean(NO2, na.rm = TRUE),
    ic_low  = mean(NO2, na.rm = TRUE) - 1.96 * sd(NO2, na.rm = TRUE) / sqrt(sum(!is.na(NO2))),
    ic_high = mean(NO2, na.rm = TRUE) + 1.96 * sd(NO2, na.rm = TRUE) / sqrt(sum(!is.na(NO2))),
    .groups = "drop"
  )

df_ensemble <- df_long %>%
  group_by(annee) %>%
  summarise(
    NO2_moy = mean(NO2, na.rm = TRUE),
    ic_low  = mean(NO2, na.rm = TRUE) - 1.96 * sd(NO2, na.rm = TRUE) / sqrt(sum(!is.na(NO2))),
    ic_high = mean(NO2, na.rm = TRUE) + 1.96 * sd(NO2, na.rm = TRUE) / sqrt(sum(!is.na(NO2))),
    .groups = "drop"
  )
df_plot <- bind_rows(df_plot, df_ensemble)

ggplot(df_plot, aes(x = annee, y = NO2_moy, color = LIBDENS, fill = LIBDENS, group = LIBDENS)) +
  geom_ribbon(aes(ymin = ic_low, ymax = ic_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # scale_color_manual(values = c("Urbain" = "#C53D1B", "Moyen" = "#F4A261", "Élevé" = "#BCC184", "Ensemble" = "#264653")) +
  # scale_fill_manual(values  = c("Faible" = "#C53D1B", "Moyen" = "#F4A261", "Élevé" = "#BCC184", "Ensemble" = "#264653")) +
  scale_x_continuous(breaks = 2011:2021) +
  labs(
    title = "Évolution de la concentration en NO2 par niveau d'éducation",
    subtitle = "Terciles basés sur la proportion de hauts diplômés (High_prop_edu)",
    x = "Année",
    y = "Concentration moyenne NO2 (µg/m³)",
    color = "Tercile éducation",
    fill  = "Tercile éducation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

