########################
###-----Chainage-----###
########################

rm(list=ls())
graphics.off()
set.seed(22071998)

library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(scales)

# ── 1. Données ────────────────────────────────────────────────────────────────

Residuals_all   <- readRDS("2_Results/Regression/All_resid_air_pollution.rds")
communes_sf     <- st_read("0_Input/Contour_commune/2024/COMMUNE.shp")
map_data        <- readRDS("0_input/map_data_COG2024_C_2021.rds") |>
  st_drop_geometry() |>
  distinct(CODGEO, .keep_all = TRUE)

# ── 2. Constantes ─────────────────────────────────────────────────────────────

REG_LABELS <- c(
  "1" = "Guadeloupe", "2" = "Martinique", "3" = "Guyane",
  "4" = "La Réunion", "6" = "Mayotte", "11" = "Île-de-France",
  "24" = "Centre-Val de Loire", "27" = "Bourgogne-Franche-Comté",
  "28" = "Normandie", "32" = "Hauts-de-France", "44" = "Grand Est",
  "52" = "Pays de la Loire", "53" = "Bretagne", "75" = "Nouvelle-Aquitaine",
  "76" = "Occitanie", "84" = "Auvergne-Rhône-Alpes",
  "93" = "Provence-Alpes-Côte d'Azur", "94" = "Corse"
)

EXPOSURE_META <- list(
  MOY.NO2  = list(label = "NO2",  palette = "RdYlGn", limits = c(-10, 10), direction = -1),
  MOY.PM25 = list(label = "PM25", palette = "RdYlGn", limits = c(-6,   6), direction = -1),
  MOY.PM10 = list(label = "PM10", palette = "RdYlGn", limits = c(-10, 10), direction = -1)
)

MISSING_CODGEO <- c("60694","85165","85212","55239","55139","55307","55039","55050","55189")

COLS_DROP <- c("ID","NOM_M","INSEE_COM","STATUT","INSEE_CAN",
               "INSEE_REG","INSEE_ARR","INSEE_DEP","SIREN_EPCI")

# ── 3. Préparation des communes (fait une seule fois) ─────────────────────────

communes_base <- communes_sf |>
  mutate(CODGEO = as.character(INSEE_COM)) |>
  select(-any_of(COLS_DROP))

# Villes > 160 000 hab (centroïdes) — calculé une seule fois
villes_sf <- map_data |>
  filter(Pop_tot > 160000) |>
  select(CODGEO, NOM, Pop_tot) |>
  inner_join(communes_base, by = "CODGEO") |>
  st_as_sf() |>
  st_centroid() |>
  mutate(long = st_coordinates(geometry)[,1],
         lat  = st_coordinates(geometry)[,2])

# ── 4. Fonction de production d'une carte ─────────────────────────────────────

make_map <- function(sf_data, exposure, meta, model_id, urbanicity, sociale) {
  
  col      <- paste0("residual_", meta$label)
  title    <- glue::glue("Résidus {sociale} × {meta$label} — {model_id} / {urbanicity}")
  leg_name <- glue::glue("**Résidus {sociale} et {meta$label}**")
  
  ggplot(sf_data) +
    geom_sf(aes(fill = .data[[col]]), color = NA) +
    geom_sf(data = villes_sf, colour = "black", size = 0.5) +
    geom_label_repel(
      data        = villes_sf,
      aes(x = long, y = lat, label = NOM.x),
      fill        = alpha("white", 0.5),
      color       = "black",
      label.size  = 0,
      box.padding = 0.1, point.padding = 0.1,
      max.overlaps = Inf, size = 1.5
    ) +
    coord_sf(expand = FALSE) +
    scale_fill_distiller(
      palette   = meta$palette,
      direction = meta$direction,
      oob       = squish,
      na.value  = "grey90",
      name      = leg_name,
      guide     = guide_colorbar(
        title.position = "top", title.hjust = 0,
        label.theme    = element_text(size = 6),
        direction      = "horizontal",
        title.theme    = element_markdown(size = 8)
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#fbf9f4", color = NA),
      legend.position  = "bottom",
      legend.key.height = unit(0.15, "cm"),
      legend.key.width  = unit(0.8,  "cm"),
      plot.title        = element_markdown(hjust = 0.5, face = "bold")
    )
}

# ── 5. Pipeline principal ──────────────────────────────────────────────────────

# Toutes les combinaisons présentes dans les données
combos <- Residuals_all |>
  filter(Sociale == "High_edu_prop", norm == FALSE) |>
  distinct(model, Urbanicity) 

# Padding des CODGEO manquants (une ligne par exposure)
padding <- expand_grid(
  CODGEO   = MISSING_CODGEO,
  Exposure = names(EXPOSURE_META)
) |> mutate(residual = NA_real_)

walk2(combos$model, combos$Urbanicity, function(mod, urb) {
  
  message("▶ ", mod, " / ", urb)
  
  # — Résidus filtrés & pivotés en large
  resid_wide <- Residuals_all |>
    filter(model == mod, Urbanicity == urb,
           Sociale == "High_edu_prop", norm == FALSE,
           Exposure %in% names(EXPOSURE_META)) |>
    select(CODGEO, Exposure, residual) |>
    distinct(CODGEO, Exposure, .keep_all = TRUE) |> 
    bind_rows(padding) |>                        # ajoute communes manquantes
    pivot_wider(names_from  = Exposure,
                values_from = residual,
                names_prefix = "residual_") |>
    rename_with(~ gsub("residual_MOY\\.", "residual_", .x))
  # → colonnes : residual_NO2, residual_PM25, residual_PM10
  
  # — Jointure géo
  sf_data <- communes_base |>
    inner_join(map_data,    by = "CODGEO") |>
    inner_join(resid_wide,  by = "CODGEO") |>
    mutate(LIBREG = REG_LABELS[as.character(REG)])
  
  # — Génération & sauvegarde de chaque carte
  iwalk(EXPOSURE_META, function(meta, exposure_key) {
    col <- paste0("residual_", meta$label)
    if (!col %in% names(sf_data)) return(invisible())
    
    p <- make_map(sf_data, exposure_key, meta,
                  model_id = mod, urbanicity = urb, sociale = "High_edu_prop")
    
    out_path <- file.path(
      "2_results/Map/Residus",
      glue::glue("map_{meta$label}_{mod}_{urb}_edu_2021.png")
    )
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    ggsave(out_path, p, width = 5, height = 5, dpi = 300)
    message("  ✔ ", basename(out_path))
  })
})

message("✅ Toutes les cartes ont été générées.")
