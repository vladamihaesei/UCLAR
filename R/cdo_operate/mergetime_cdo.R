
######## daily merged 
files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily",pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- paste0("/",unique(files.split[,10]))

for(i in 1:length(params)){
  
  print(params[i])
  
  files.sub <- grep(params[i], files, fixed = T,value = T)
  
  #system(paste0("cdo settaxis,1980-12-31 23:00:00,,1hour -cat ", gsub(",", " ", noquote(toString(files.sub)))," ", "/Volumes/Z_vld/sfica_proiect_NE/era5land/nc_merged/",params[i],"_daily_1981-2021.nc" ))
  system(paste0("cdo mergetime ", gsub(",", " ", noquote(toString(files.sub)))," ", "/Volumes/Z_vld/sfica_proiect_NE/era5land/dailymerged",params[i],"_daily_1981-2021.nc" ))
  
}

###verificare 
library(terra)
library(sf)
limite <- read_sf("Limite/Limite.shp")
urban <- read_sf("Limite/Utilizare.shp")
vf <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/dailymerged/temperature_daily_1981-2021.nc")
plot(vf[[1]])
plot(urban,add =T, col = "transparent")
plot(limite,add =T, col = "transparent")
