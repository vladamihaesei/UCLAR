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
### dem 
dem.rg <- readGDAL("grids/dem_Iasi_modis_res.tif")

#### stations 
coord <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
gg <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx", sheet = 2)
gg <- gg%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
  mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)%>%na.omit()

### select the days
zile <- unique(gg$Time)
### for each month
luni <- 1:12
### generate empty raster to add them in the loop 
rr <- rast()

for(m in 1:length(luni)){
  
  print(luni[m])
  zile.f <- zile[month(zile)== luni[m]]
  rr1 <- rast()

  monthly_RK <- rast(paste0("grids/interpolation_explicatory_vars/RK_monthly/",month.name[m],".tif"))
  #plot(monthly_RK)
  
  for(z in 1:length(zile.f)){
    
    print(zile.f[z])
    tf <- gg%>%filter(Time == zile.f[z])%>%na.omit()
    df <- as.data.frame(tf, xy = F)
    tf.sf <- st_as_sf(tf,coords = c("Longitudine","Latitudine"), crs= 4326)
    
    month.df <- terra::extract(monthly_RK, tf.sf, bind = T, ID = F)%>%as.data.frame()
    
    df.rt <- df%>%left_join(month.df[c(5,9)])%>%mutate(anom = Temp_mean-tt)
    
    coordinates(df.rt) = c("Longitudine","Latitudine")
    proj4string(df.rt) = CRS("+init=epsg:4326")
    ##### kriging residuals
    ress <- krige1(anom~1,df.rt, dem.rg, model=v)
    
    tt_reg <- as(dem,"SpatialGridDataFrame")
    
    tt_reg@data[,"anom"] <- ress[,1]
    
    anom <- rast(tt_reg["anom"])
    rst2 <- monthly_RK+anom
    rr1 <- c(rr1,rst2)
    
  }
  rr <- c(rr,rr1)
  
}

writeCDF(rr,"grids/test_interpolare_anom_MODIS_tmean_anual_2018.nc", overwrite = T)




