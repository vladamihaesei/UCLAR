library(terra)
library(sf)
library(lubridate)
library(dplyr)
library(tidyr)
library(raster)
source("sources.R")
####### puncte orase 
pct <- read.csv("tabs/Orase_punct-central.csv", sep = ";")
pct_sf <- st_as_sf(x= pct, coords = c("Lon","Lat"), crs = CRS("+init=epsg:4326"))

###### listeaza fisierele raster
files <- list.files(paste0(drive_d,"dailymerged"),pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- c("/dewtemperature_daily_", "/dewtemperature_dailymin_","/temperature_daily_","/temperature_dailymin_","/temperature_dailymax_", "/precipitation_daily_")

### obtine data frame cu data format
r <- rast(files[3])
timp <- as.Date(format(time(r), "%Y-%m-%d"))

#### incepe loop pentru fiecare punct/oras_central
for( i in 1:nrow(pct_sf)){
  
  punct <- pct_sf[i,]
  nume <- punct$Oras
  
  df1 <- data.frame(year = year(timp), month = month(timp), day = day(timp))
  #### incepe loop pentru fiecare parametru
  print(nume)
  
  for(j in 1:length(params)){
    
    print(params[j])
    files.sub <- grep(params[j], files,fixed = T, value = T)
    r <- rast(files.sub)
    #timp <- as.character(format(time(r), "%Y-%m-%d %H:%M:%S"))
    timp <- as.Date(format(time(r), "%Y-%m-%d"))
    tab <- data.frame(param = round(terra::extract(r,vect(punct)),2))%>%pivot_longer(-c(param.ID))
    tab <- tab%>%mutate(year = year(timp), month = month(timp), day = day(timp))%>%dplyr::select(year,month,day,value)
    names(tab)[4] <- params[j]
    df1 <- cbind(df1,tab[4])
  }
  
  write.csv(df1,paste0(drive_d,"tabs/punct_central/",nume,"_punct_central_daily_1981-2021.csv"))
  
}
