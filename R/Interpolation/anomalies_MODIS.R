library(dplyr)
library(terra)
library(sf)
library(lubridate)

mod.day <- rast("grids/MODIS/nc/filled/LST_Day_1km/Iasi/MOD11A1_Day_2000-2020/MOD11A1Day_2000-2020.tif")
zile_nebul <- read.csv("tabs/MODIS/Iasi/MOD11A1/Day_cloud_pixels.csv")
zile_nebul <- zile_nebul%>%filter(frecventa <=50)%>%dplyr::select(timp)
terra::time(mod.day) <- as.Date(names(mod.day), "X%Y.%m.%d")
mod.day <- mod.day[[which(time(mod.day) %in% c(as.Date(zile_nebul$timp)) &  year(time(mod.day)) =="2018")]]#& month(time(modis.grids)) =="1"

#writeCDF(mod.day, "grids/interpolation_tests/MOD_Day_201801.nc")

mod.month.day <- tapp(mod.day, index = "months", fun = mean)-273.15

mod.night <- rast("grids/MODIS/nc/filled/LST_Night_1km/Iasi/MOD11A1_Night_2000-2020/MOD11A1_Night_2000-2020.tif")
night_nebul <- read.csv("tabs/MODIS/Iasi/MOD11A1/Night_cloud_pixels.csv")
night_nebul <- night_nebul%>%filter(frecventa <=50)%>%dplyr::select(timp)
terra::time(mod.night) <- as.Date(names(mod.night), "X%Y.%m.%d")
mod.night <- mod.night[[which(time(mod.night) %in% c(as.Date(night_nebul$timp)) &  year(time(mod.night)) =="2018")]]#& month(time(modis.grids)) =="1"
#writeCDF(mod.night, "grids/interpolation_tests/MOD_Night_201801.nc")
mod.month.night <- tapp(mod.night, index = "months", fun = mean)-273.15

mod.month.mean <- (mod.month.day + mod.month.night)/2
plot(mod.month.night)
plot(mod.month.day)
plot(mod.month.mean)
### 
writeCDF(mod.month.day,"grids/interpolation_explicatory_vars/MOD11A1_Day_monthly_mean_2018.nc", overwrite = T)
writeCDF(mod.month.night,"grids/interpolation_explicatory_vars/MOD11A1_Night_monthly_mean_2018.nc", overwrite = T)
writeCDF(mod.month.mean,"grids/interpolation_explicatory_vars/MOD11A1_mean_monthly_mean_2018.nc", overwrite = T)


###########MYD
myd.day <- rast("grids/MODIS/nc/filled/LST_Day_1km/Iasi/MYD11A1_Day_2000-2020/MYD11A1Day_2000-2020.tif")
zile_nebul <- read.csv("tabs/MODIS/Iasi/MYD11A1/Day_cloud_pixels.csv")
zile_nebul <- zile_nebul%>%filter(frecventa <=50)%>%dplyr::select(timp)
terra::time(myd.day) <- as.Date(names(myd.day), "X%Y.%m.%d")
myd.day <- myd.day[[which(time(myd.day) %in% c(as.Date(zile_nebul$timp)) & year(time(myd.day)) =="2018")]]#& month(time(modis.grids)) =="1"
#writeCDF(myd.day, "grids/interpolation_tests/MYD_Day_201801.nc")
myd.month.day <- tapp(myd.day, index = "months", fun = mean)-273.15

myd.night <- rast("grids/MODIS/nc/filled/LST_Night_1km/Iasi/MYD11A1_Night_2000-2020/MYD11A1_Night_2000-2020.tif")
night_nebul <- read.csv("tabs/MODIS/Iasi/MYD11A1/Night_cloud_pixels.csv")
night_nebul <- night_nebul%>%filter(frecventa <=50)%>%dplyr::select(timp)
terra::time(myd.night) <- as.Date(names(myd.night), "X%Y.%m.%d")
myd.night <- myd.night[[which(time(myd.night) %in% c(as.Date(night_nebul$timp)) & year(time(myd.night)) =="2018")]]#& month(time(modis.grids)) =="1"
#writeCDF(myd.night, "grids/interpolation_tests/MYD_Night_201801.nc")
myd.month.night <- tapp(myd.night, index = "months", fun = mean)-273.15

myd.month.day

myd.month.mean <- (myd.month.day + myd.month.night)/2

plot(myd.month.night)
plot(myd.month.day)
plot(myd.month.mean)

### 
writeCDF(myd.month.day,"grids/interpolation_explicatory_vars/MYD11A1_Day_monthly_mean_2018.nc", overwrite = T)
writeCDF(myd.month.night,"grids/interpolation_explicatory_vars/MYD11A1_Night_monthly_mean_2018.nc", overwrite = T)
writeCDF(myd.month.mean,"grids/interpolation_explicatory_vars/MYD11A1_mean_monthly_mean_2018.nc", overwrite = T)

modis.day <- (mod.month.day + myd.month.day)/2
modis.night <- (mod.month.night + myd.month.night)/2
modis.mean <- (modis.day+modis.night)/2
plot(modis.day)
plot(modis.night)
plot(modis.mean)

writeCDF(modis.day,"grids/interpolation_explicatory_vars/MODIS_Day_monthly_mean_2018.nc", overwrite = T)
writeCDF(modis.night,"grids/interpolation_explicatory_vars/MODIS_DAY_monthly_mean_2018.nc", overwrite = T)
writeCDF(modis.mean,"grids/interpolation_explicatory_vars/MODIS_ALL_monthly_mean_2018.nc", overwrite = T)


### land cover 
lc <- rast("grids/interpolation_explicatory_vars/Land_cover/MCD12Q1.006_LC_Type5_doy2018001_aid0001.tif")
plot(lc)
plot(tf, add = T)
text(df$Longitudine, df$Latitudine, labels = df$Statie, col = "red", 2,1,cex = 1)

lookup_table <- c(7, 4, 6, 9, 0)  # Numeric codes or factors
labels <- c("crops", "forest", "grass", "urban", "water")  # Corresponding character labels

lc_classified <- classify(lc, lookup_table, labels)

# Replace factor levels with character labels
lc_classified <- lc_classified[[1]]  # Convert to a single-layer raster
levels(lc_classified) <- labels

lc_classified1 <- resample(lc_classified,mod[[1]])
ext(lc_classified1) <- ext(mod[[1]])

writeRaster(lc1, "grids/interpolation_explicatory_vars/land_cover_resampled.tif", overwrite = T)

####
lc1 <- resample(lc,mod[[1]])
ext(lc_classified1) <- ext(mod[[1]])

writeRaster(lc1, "grids/interpolation_explicatory_vars/land_cover_resampled1.tif")
