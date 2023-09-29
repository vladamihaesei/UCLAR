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
dem <- readGDAL("grids/dem_Iasi_modis_res.tif")
dem$alt <- dem$band1
dem$band1 <- NULL
dem$lc <- readGDAL("grids/interpolation_explicatory_vars/land_cover_resampled.tif")$band1

#dem.rg <- readGDAL("grids/dem_Iasi_modis_res.tif")
## land cover
lc <- rast("grids/interpolation_explicatory_vars/land_cover_resampled.tif")
vars <- c(dem,lc)
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
  #m=7
  monthly_RK <- rast(paste0("grids/interpolation_explicatory_vars/RK_monthly/",month.name[m],".tif"))
  #plot(monthly_RK)
  
  for(z in 1:length(zile.f)){
    
    print(zile.f[z])
    tf <- gg%>%filter(Time == zile.f[z])%>%na.omit()
    df <- as.data.frame(tf, xy = F)
    coordinates(tf) = c("Longitudine","Latitudine")

    proj4string(tf) = CRS("+init=epsg:4326")
    ov <- sp::over(tf,dem)
    tf$alt <- ov$alt
    tf$lc <- ov$lc
    lm <- lm(Temp_mean~alt,data = tf)
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

    #terra::time(rst) <- zile[z]
  
    month.df <- terra::extract(monthly_RK, tf.sf, bind = T, ID = F)%>%as.data.frame()
    
    df.rt <- df%>%left_join(month.df[c(5,9)])%>%mutate(anom = Temp_mean-tt)
    
    coordinates(df.rt) = c("Longitudine","Latitudine")
    proj4string(df.rt) = CRS("+init=epsg:4326")
    ##### kriging residuals
    ress <- krige1(anom~1,df.rt, dem.rg, model=v)
    
    #rst <- rec + anomalies### adaugare anomalii 
    
    #tt_reg@data[,"tt"] <- tt_reg@data[,"layer"]+tt_reg@data[,'res']
    
    tt_reg@data[,"anom"] <- ress[,1]
    tt_reg@data[,"tt1"] <- tt_reg@data[,"tt"]+tt_reg@data[,'anom']
    
    rst2 <- rast(tt_reg["tt1"])
 
    #terra::time(rst) <- zile.f[z]### set the time 
    rr1 <- c(rr1,rst2)
    #plot(rr1)
  }
  
  rr <- c(rr,rr1)
  
}

writeCDF(rr,"grids/test_interpolare_anom_MODIS_tmean_anual_2018.nc", overwrite = T)




