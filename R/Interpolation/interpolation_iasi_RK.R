library(xlsx)
library(tidyr)
library(dplyr)
library(sf)
library(sp)
library(terra)
library(raster)
library(rgdal)
source("~/Documente/vlad_R/krige1_functii.R")

# dem <- terra::rast("~/Documente/vlad_R/R_meteo/mnt_ro/mozaic_ro.tif")
# newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"
# dem <- terra::project(dem,newproj1)
# mask <- read_sf("Limite/Limite.shp")%>%filter(Drawings=="Iasi")
# 
# dem <- crop(dem,mask)
# writeRaster(dem,"grids/dem_Iasi.tif")

##########
demm <- brick("grids/dem_Iasi_modis_res.tif")
dem <- readGDAL("grids/dem_Iasi_modis_res.tif")
dem$alt <- dem$band1
dem$band1 <- NULL
dem$lc <- readGDAL("grids/interpolation_explicatory_vars/land_cover_resampled.tif")$band1

coord <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
gg <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 2) ## Daily tmax, Daily Tmean, Daily Tmin
gg <- gg%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
      mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)

#gg <- gg%>%filter(Time == "2015-07-23")%>%na.omit()
zile <- unique(gg$Time)

rk <- rast() 

for(z in 1:length(zile)){
  
  print(zile[z])
  tf <- gg%>%filter(Time == zile[z])%>%na.omit()
  df <- as.data.frame(tf, xy = F)
  coordinates(tf) = c("Longitudine","Latitudine")
  proj4string(tf) = CRS("+init=epsg:4326")
  ov <- sp::over(tf,dem)
  tf$alt <- ov$alt
  tf$lc <- ov$lc
  lm <- lm(Temp_mean~alt,data =tf)
  s.lm <- step(lm)
  #summary(s.lm)$adj.r.squared
  #print(paste(s.lm$call$formula)[3])
  dem2 <- brick(dem)
  r <- predict(dem2,s.lm)
  tt_reg <- as(r,"SpatialGridDataFrame")
  tf$res <- s.lm$residuals
  rbf_tt <- krige1(res~1,tf, dem, model=v)
  tt_reg@data[,'res'] <- rbf_tt[,1]
  tt_reg@data[,"tt"]<-tt_reg@data[,"layer"]+tt_reg@data[,'res']
  rst <- rast(tt_reg["tt"])
  terra::time(rst)<- zile[z]
  rk <- c(rk,rst)
  
}

writeCDF(rk,"grids/test_interpolare_RK_tmean_anual_2018.nc", overwrite = T)



