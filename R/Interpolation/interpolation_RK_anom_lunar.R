library(xlsx)
library(tidyr)
library(dplyr)
library(sf)
library(sp)
library(terra)
library(raster)
library(rgdal)
library(lubridate)

source("~/Documente/vlad_R/krige1_functii.R")
### the lst modis for resample 
mod.month <- rast("grids/interpolation_explicatory_vars/MODIS_ALL_monthly_mean_2018_names.nc")
### dem 
dem <- rast("grids/dem_Iasi_modis_res.tif")
## land cover
lc <- rast("grids/interpolation_explicatory_vars/land_cover_resampled1.tif")
vars <- c(dem,lc)
#### stations 
coord <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
gg <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx", sheet = 2)
gg <- gg%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
  mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)%>%na.omit()

gg.l <- gg%>%group_by(Year,Month,Statie,Altitudine,Latitudine,Longitudine)%>%
        summarise(Tmean = mean(Temp_mean))

luni <- 1:12

for ( l in 1:length(luni)){
  
  print(luni[l])
  modis.f <- mod.month[[l]]
  vars1 <- c(vars,modis.f)
  names(vars1) <- c("alt","lc", "modis")
  tf <- gg.l%>%filter(Month == luni[l])%>%na.omit()
  df <- tf
  tf.sf <- st_as_sf(tf, coords = c("Longitudine","Latitudine"), crs = 4326)
  
  tf.ex <- terra::extract(vars1, tf.sf,bind = T, ID = F)%>%as.data.frame()
  
  tf.ex[is.na(tf.ex)] <- 6.5
  
  lm <- lm(Tmean~lc+modis,data = tf.ex)###linear regression 
  s.lm <- step(lm, trace = 0)
  ##summary(s.lm)
  dem2 <- dem
  rec <- terra::predict(dem2,s.lm) ###estimeaza cf ecuatiei de regresie
  
  
}

############## method 2 

source("~/Documente/vlad_R/krige1_functii.R")

# dem <- terra::rast("~/Documente/vlad_R/R_meteo/mnt_ro/mozaic_ro.tif")
# newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"
# dem <- terra::project(dem,newproj1)
# mask <- read_sf("Limite/Limite.shp")%>%filter(Drawings=="Iasi")
# 
# dem <- crop(dem,mask)
# writeRaster(dem,"grids/dem_Iasi.tif")

for(l in 1:length(luni)){
  
  modis.f <- mod.month[[l]]
  writeRaster(modis.f,paste0("grids/interpolation_explicatory_vars/MODIS/",month.name[l],".tif"))
  
}

##########
demm <- brick("grids/dem_Iasi_modis_res.tif")
dem <- readGDAL("grids/dem_Iasi_modis_res.tif")
dem$alt <- dem$band1
dem$band1 <- NULL
dem$lc <- readGDAL("grids/interpolation_explicatory_vars/land_cover_resampled1.tif")$band1

coord <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
gg <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 2) ## Daily tmax, Daily Tmean, Daily Tmin
gg <- gg%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
  mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)

gg.l <- gg%>%group_by(Year,Month,Statie,Altitudine,Latitudine,Longitudine)%>%
  summarise(Tmean = mean(Temp_mean))

rk <- rast() 
luni <- 1:12

for(l in 1:length(luni)){
  
  print(luni[l])
  tf <- gg.l%>%filter(Month == luni[l])%>%na.omit()
  dem$modis <- readGDAL(paste0("grids/interpolation_explicatory_vars/MODIS/",month.name[l],".tif"))$band1
  df <- as.data.frame(tf, xy = F)
  coordinates(tf) = c("Longitudine","Latitudine")
  proj4string(tf) = CRS("+init=epsg:4326")
  ov <- sp::over(tf,dem)
  tf$alt <- ov$alt
  tf$lc <- ov$lc
  tf$modis <- ov$modis
  df <- as.data.frame(tf)
  df[is.na(df)] <- 6.5
  lm <- lm(Tmean~alt+modis+lc,data =df)
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
  writeRaster(rst, paste0("grids/interpolation_explicatory_vars/RK_monthly/",month.name[l],".tif"))
  # terra::time(rst)<- luni[l]
  # rk <- c(rk,rst)
  
}

writeCDF(rk,"grids/test_interpolare_RK_tmean_anual_2018.nc", overwrite = T)





