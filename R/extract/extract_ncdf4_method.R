library(ncdf4)
library(lubridate)
library(dplyr)
#### citeste limita punct
pct <- read.csv("tabs/Orase_punct-central.csv", sep = ";")

### listeaza fisierele raster
files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/dailymerged",pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- paste0("/",unique(files.split[,10]))
#### incepe loop pentru extragere punct

for( i in 1:nrow(pct)){
  
  punct <- pct[i,]
  pct.lat <- round(punct$Lat,1)
  pct.lon <- round(punct$Lon,1) 
  nume <- punct$Oras
  
  for( j in 1:length(params)){
    
    r <- rast(files[j])
    timp <- as.Date(time(r))
    
    rr <- nc_open(files[j])
    lat <- round(ncvar_get(rr,"latitude"),1) %>%as.vector()
    lon <- round(ncvar_get(rr,"longitude"),1)%>%as.vector()
    var <- ncvar_get(rr,"d2m")%>% as.vector()
    time <- ncvar_get(rr,"time")%>%as.vector()
    
    df <- data.frame(lon,lat,year = year(timp[1]), month = month(timp[1]),day = day(timp[1]),var)%>%filter(lat==pct.lat & lon ==pct.lon)#%>%dplyr::select(-c(lat,lon))%>%arrange(year,month,day)
    df
    
  }
  
  write.csv(df, paste0("/Volumes/Z_vld/sfica_proiect_NE/era5land/tabs/",nume,"_punct_daily_1981-2021.csv"))
  

}
###verificare
library(sf)
library(raster)
ln <- round(unique(pct$Lon),2)
lt <-  round(unique(pct$Lat),2)
points <- st_as_sf(x= pct, coords = c("Lon","Lat"), crs = CRS("+init=epsg:4326"))

files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily",pattern = ".nc", recursive = T,  full.names = T)
files
r <- rast(files[2])
plot(r[[200]])
plot(points,add = T, col = "black", size = .01)
writeRaster(r[[200]], "test.tif")
write_sf(points,"kmz/punct_central.shp")
plot()


