library(terra)
library(sf)
library(raster)

ls <- list.files("/Volumes/Z_vld/sfica_proiect_NE/Surf_Temp_Daily_1Km_v6/LST_Day_1km", pattern = ".tif", full.names = T)
head(ls)
tail(ls)
dat <- seq(as.Date("2000-02-24"), as.Date("2002-12-31"), by = "days")

r <- terra::rast(ls)
 
