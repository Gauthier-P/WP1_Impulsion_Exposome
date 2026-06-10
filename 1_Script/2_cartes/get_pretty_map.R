library(sf)
library(terra)
library(tidyverse)
library(stars)
library(rayshader)
library(rayrender)
library(MetBrewer)
library(colorspace)
library(RColorBrewer)
library(viridisLite)
library(R.utils)

rm(list=ls())
# graphics.off()

###---Données merged---###

map_data_2024 <- readRDS("0_input/map_data_COG2024_C_2021.rds")
# map_union <- st_union(map_data_2024)

map_data_2024 <-select(map_data_2024,c(MOY.NO2))
# map_simple <- st_simplify(map_data_2024, dTolerance = 50)
# map_data_2024 <- map_simple
bb <- st_bbox(map_data_2024)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(map_data_2024))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(map_data_2024))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(map_data_2024))

height <- st_distance(bottom_left, top_left)

if (width > height){
  w_ratio <-1
  h_ratio <- height/width
}else{
  h_ratio <-1
  w_rato <- width/height
}

size <- 3000
rast <- st_rasterize(map_data_2024, 
                     nx =floor(size*w_ratio),
                     ny =floor(size*h_ratio))

mat <- matrix(rast$MOY.NO2,
              nrow = floor(size*w_ratio),
              ncol = floor(size*h_ratio))

c1 <- rev(met.brewer("Hiroshige"))
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2.2)(256)
swatchplot(texture)

# colors = brewer.pal(n=9, name = "PuRd")
# 
# texture <- grDevices::colorRampPalette(colors, bias = 3)(256)
# swatchplot(texture)
# 
# texture <- viridis(256, option = "plasma")
# swatchplot(texture)

mat %>% 
  height_shade(texture = texture) %>%
  plot_3d(heightmap = mat,
          zscale = 0.25/3,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -18, phi = 55, zoom = 0.76)

outfile <- "2_results/Map/final_beautiful_veryhighres.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  
  if(!file.exists(outfile)){
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive= FALSE,
    lightdirection = 280,
    lightaltitude = c(20,80),
    lightcolor = c(c1[9], "white"),
    lightintensity = c(600,200),
    samples = 150,
    width = 4000,
    height = 4000,
  )
  end_time <- Sys.time()
  diff <- end_time- start_time
  cat(crayon::cyan(diff), "\n")
  
}
# ggsave("2_results/Map/beautiful.png", map, width = 5, height = 5, bg = "#fbf9f4", dpi = 300)
library(grid)

legend_file <- "2_results/Map/legend_NO2.png"

# plage de valeurs
val_min <- min(mat, na.rm = TRUE)
val_max <- max(mat, na.rm = TRUE)

png(legend_file, width = 500, height = 2000, bg = "transparent")

grid.newpage()

# matrice de dégradé
legend_matrix <- matrix(seq(val_min, val_max, length.out = 256), ncol = 1)

# affichage du dégradé
grid.raster(
  as.raster(matrix(rev(texture), ncol = 1)),
  x = 0.4,
  width = 0.3,
  height = 0.9,
  interpolate = TRUE
)

# graduations
ticks <- pretty(c(val_min, val_max), n = 6)
tick_pos <- (ticks - val_min) / (val_max - val_min)

for(i in seq_along(ticks)){
  grid.text(
    label = round(ticks[i], 1),
    x = 0.8,
    y = 0.05 + 0.9 * tick_pos[i],
    gp = gpar(fontsize = 50)
  )
}


dev.off()
