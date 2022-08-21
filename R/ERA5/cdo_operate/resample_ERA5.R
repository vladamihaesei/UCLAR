### reseample remap  nearest neighbour 
files <- list.files(paste0(drive_d,"dailymerged"),pattern = ".nc", recursive = T,  full.names = T)
files <- grep("lcc_daily|tcc_daily", files, value = T)
system(paste0("cdo remapnn, ",drive_d,"dailymerged/temperature_daily_1981-2021.nc ",drive_d," dailymerged/lcc_daily_1981-2021.nc ", drive_d,"dailymerged/lcc_dailyresampled_1981-2021.nc" ))
system(paste0("cdo remapnn, ",drive_d,"dailymerged/temperature_daily_1981-2021.nc ",drive_d," dailymerged/tcc_daily_1981-2021.nc ", drive_d,"dailymerged/tcc_dailyresampled_1981-2021.nc" ))

### verificare 
library(terra)
g <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/dailymerged/lcc_dailyresampled_1981-2021.nc")
g
plot(g[[1]])
