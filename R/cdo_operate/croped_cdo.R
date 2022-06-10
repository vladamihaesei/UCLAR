### drive windows sau mac 
drive_d <- ifelse(Sys.info()[1] == "Darwin", "/Volumes/vld_z", "/D/")

### citeste limitele 
limite <- read_sf("Limite/Limite.shp")

files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily",pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- unique(files.split[,10])

for( i in 1:nrow(limite)){
  
  limite.sub <- limite[i,]
  g <- as.numeric(st_bbox(limite.sub))
  gg <- paste0(g[1],",",g[3],",",g[2],",",g[4])
  nume <- limite.sub$Drawings
  path <- paste0("/Volumes/Z_vld/sfica_proiect_NE/era5land/croped/",nume,"/")
  files.sub <- grep("dewtemperature", files, invert = T, value = T)
  files.sub <- grep("temperature", files.sub, invert = F, value = T)
  if(!dir.exists(path)) dir.create(path, recursive = T)
  
  for(j in 1:length(files.sub)){
    
    print(files.sub[j])
    den <- strsplit(files.sub[j],"/")[[1]][7]
    system(paste0("cdo remapnn," ,"gridded4.nc"," ",files.sub[j], " ","tmp/tmp.nc"))
    
    system(paste0("cdo -b F64 -sellonlatbox," , "tmp/tmp.nc", " ",path,den))
    
  }
  unlink(list.files("tmp",full.names = T))
}

### verificare 
### resample 


rr <- rast("gridded4.nc")
plot(rr[[1]])
rr <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily/temperature_daily_1981-1991.nc")
sr <- terra::rast(nrow = 5000, ncol = 5000)
terra::ext(sr) <- ext(rr[[1]])
sr <- terra::resample(rr[[1]], sr, method = "near")
writeCDF(sr,"gridded.nc")
plot(rr[[1]])
plot(limite.sub, add =T)
rs <- rast("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily/temperature_daily_1981-1991.nc")

writeCDF(rs[[1:4]], "/Volumes/Z_vld/sfica_proiect_NE/era5land/in_test.nc")
####dd 



