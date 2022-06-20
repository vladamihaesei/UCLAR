######## calculeaza zilnice din orare - foloseste CDO pentru timp mai scurt 
### pentru temperatura  - se converteste in grade celsius si dupa se shifteaza timpul cu o secunda pentru a face media zilnica corect 
### the values for 00:00:00 to 00:59:59 are stored in 01:00:00, which means that 00:00:00 actually has the values of the last timestep of the previous day.
### See the documentation https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation#ERA5Land:datadocumentation-Temporalfrequency
source("sources.R")
files <- list.files(paste0(drive_d,"scripts"),pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- paste0("/",unique(files.split[,11]))

for(i in 1:length(params)){
  
  print(params[i])
  files.sub <- grep(params[i], files, value = T)
  oper <- ifelse(params[i]=="/precipitation", "cdo -L -b F64 -mulc,1000 -selhour,00:00","cdo -b F64 -daymean -shifttime,-1sec -subc,273.15")
  oper1 <- ifelse(params[i]=="/precipitation", "cdo -L -b F64 -mulc,1000 -selhour,00:00","cdo -b F64 -daymax -shifttime,-1sec -subc,273.15")
  oper2 <- ifelse(params[i]=="/precipitation", "cdo -L -b F64 -mulc,1000 -selhour,00:00","cdo -b F64 -daymin -shifttime,-1sec -subc,273.15")
  
  for ( j in 1:length(files.sub)){
    
    nume <- strsplit(files.sub[j], "/")[[1]][9]
    
    #system(paste0("cdo settaxis,1980-12-31 23:00:00,,1hour -cat ", gsub(",", " ", noquote(toString(files.sub)))," ", "/Volumes/Z_vld/sfica_proiect_NE/era5land/nc_merged/",params[i],"_daily_1981-2021.nc" ))
    system(paste0(oper," ", files.sub[j]," ", paste0(drive_d,"daily/"), gsub("hourly","daily",nume)))
    system(paste0(oper1," ", files.sub[j]," ", paste0(drive_d,"daily/"), gsub("hourly","dailymax",nume)))
    system(paste0(oper2," ", files.sub[j]," ", paste0(drive_d,"daily/"), gsub("hourly","dailymin",nume)))
  }
  
}

########verificare
### horuly
vf <- rast(paste0(drive_d,"scripts/python/precipitation_hourly_1981-1991.nc"))
plot(vf[[46:58]])
### daily 
vf1 <- brick(paste0(drive_d,"daily/precipitation_dailymax_1981-1991.nc"))
plot(vf1[[3654:3670]])

vf2 <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily/temperature_daily_1981-1991.nc")
plot(vf2[[1:16]])

vf3 <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily/precipitation_daily_1981-1991.nc")
plot(vf3[[17:30]])

###### daily 
vf4 <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/scripts/python/precip_daysum_conv.nc")
plot(vf4[[210]])
plot(limite.sub, add = T, col = "transparent")

v <- brick(vf[[1]])
vf5 <-raster:: brick("~/Downloads/test_era5land.nc")
vf5 <- resample(vf5, v, method = "ngb")
vf5[is.na(v)] <- NA

plot(vf5)
plot(vf1)
