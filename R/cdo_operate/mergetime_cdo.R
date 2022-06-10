source("sources.R")
######## daily merged 

files <- list.files(paste0(drive_d,"daily"),pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- c("/dewtemperature_daily_", "/dewtemperature_dailymin_","/temperature_daily_","/temperature_dailymin_","/temperature_dailymax_", "/precipitation_daily_")

for(i in 1:length(params)){
  
  print(params[i])
  
  files.sub <- grep(params[i], files, fixed = T,value = T)
  length(files.sub)
  #system(paste0("cdo settaxis,1980-12-31 23:00:00,,1hour -cat ", gsub(",", " ", noquote(toString(files.sub)))," ", "/Volumes/Z_vld/sfica_proiect_NE/era5land/nc_merged/",params[i],"_daily_1981-2021.nc" ))
  system(paste0("cdo mergetime ", gsub(",", " ", noquote(toString(files.sub)))," ", drive_d,"dailymerged",params[i],"1981-2021.nc" ))
  
}

##verificare
library(terra)
library(sf)
limite <- read_sf("Limite/Limite.shp")
urban <- read_sf("Limite/Utilizare.shp")
vf <- rast(paste0(drive_d,"daily/precipitation_daily_1981-1991.nc"))
plot(vf[[1]])
plot(urban,add =T, col = "transparent")
plot(limite,add =T, col = "transparent")
