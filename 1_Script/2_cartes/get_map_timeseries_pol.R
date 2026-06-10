library(magick)
rm(list=ls())
#loading images 
NO2_2011   <- image_read("2_Results/Map/2011/Air_pollution/map_NO2_2011.png")
NO2_2015   <- image_read("2_Results/Map/2015/Air_pollution/map_NO2_2015.png")
NO2_2021   <- image_read("2_Results/Map/2021/Air_pollution/map_NO2_2021.png")

PM25_2011 <- image_read("2_Results/Map/2011/Air_pollution/map_PM25_2011.png")
PM25_2015 <- image_read("2_Results/Map/2015/Air_pollution/map_PM25_2015.png")
PM25_2021 <- image_read("2_Results/Map/2021/Air_pollution/map_PM25_2021.png")

PM10_2011 <- image_read("2_Results/Map/2011/Air_pollution/map_PM10_2011.png")
PM10_2015 <- image_read("2_Results/Map/2015/Air_pollution/map_PM10_2015.png")
PM10_2021 <- image_read("2_Results/Map/2021/Air_pollution/map_PM10_2021.png")


add_title <- function(img, title) {
  image_annotate(
    img,
    text = title,
    size = 70,
    gravity = "north",
    location = "+0+0",
    color = "black",
    boxcolor = "#fbf9f4",
    font = "Arial-Bold"
  )
}
######
NO2_2011_evo <- add_title(NO2_2011, "2011")
NO2_2015_evo <- add_title(NO2_2015, "2015")
NO2_2021_evo <- add_title(NO2_2021, "2021")

PM25_2011_evo <- add_title(PM25_2011, "2011")
PM25_2015_evo <- add_title(PM25_2015, "2015")
PM25_2021_evo <- add_title(PM25_2021, "2021")

PM10_2011_evo <- add_title(PM10_2011, "2011")
PM10_2015_evo <- add_title(PM10_2015, "2015")
PM10_2021_evo <- add_title(PM10_2021, "2021")

NO2 <- image_append(c(NO2_2011_evo, NO2_2015_evo,NO2_2021_evo))
PM25 <- image_append(c(PM25_2011_evo, PM25_2015_evo,PM25_2021_evo))
PM10 <- image_append(c(PM10_2011_evo, PM10_2015_evo,PM10_2021_evo))

image_write(NO2, "2_Results/Map/Air_pollution/NO2_2011_2021.jpg")
image_write(PM25, "2_Results/Map/Air_pollution/PM25_2011_2021.jpg")
image_write(PM10, "2_Results/Map/Air_pollution/PM10_2011_2021.jpg")

######

pol_2011 <- image_append(c(NO2_2011, PM25_2011,PM10_2011))
pol_2015 <- image_append(c(NO2_2015, PM25_2015,PM10_2015))
pol_2021 <- image_append(c(NO2_2021, PM25_2021,PM10_2021))

image_write(pol_2011, "2_Results/Map/Air_pollution/pol_2011.jpg")
image_write(pol_2015, "2_Results/Map/Air_pollution/pol_2015.jpg")
image_write(pol_2021, "2_Results/Map/Air_pollution/pol_2021.jpg")


