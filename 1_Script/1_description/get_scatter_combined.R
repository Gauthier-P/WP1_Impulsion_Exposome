library(magick)
rm(list=ls())

#loading images 
NO2.edu.A  <- image_read("2_Results/Regression/Scatterplot/High_edu_prop/MOY.NO2_weighted.png")
PM25.edu.A <- image_read("2_Results/Regression/Scatterplot/High_edu_prop/MOY.PM25_weighted.png")
PM10.edu.A <- image_read("2_Results/Regression/Scatterplot/High_edu_prop/MOY.PM10_weighted.png")

NO2.rev.A  <- image_read("2_Results/Regression/Scatterplot/Q221/MOY.NO2_weighted.png")
PM25.rev.A <- image_read("2_Results/Regression/Scatterplot/Q221/MOY.PM25_weighted.png")
PM10.rev.A <- image_read("2_Results/Regression/Scatterplot/Q221/MOY.PM10_weighted.png")

combined_edu <- image_append(c(NO2.edu.A, PM25.edu.A,PM10.edu.A), stack =T)
combined_rev <- image_append(c(NO2.rev.A, PM25.rev.A,PM10.rev.A), stack =T)

image_write(combined_edu, "2_Results/Regression/scatter_edu_NO2_PM25_PM10.jpg")
image_write(combined_rev, "2_Results/Regression/scatter_rev_NO2_PM25_PM10.jpg")
