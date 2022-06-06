library(terra)
library(sf)

### drive windows sau mac 
drive_d <- ifelse(Sys.info()[1] == "Darwin", "/Volumes/vld_z", "/D/")

### citeste limitele 
limite <- read_sf("Limite/Limite.shp")

files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/scripts",pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- unique(files.split[,11])

for( i in 1:nrow(limite)){
  
  limite.sub <- limite[i,]
  nume <- limite.sub$Drawings
  files.sub <- grep("dewtemperature", files, invert = T, value = T)
  files.sub <- grep("temperature", files.sub, invert = F, value = T)
  df <- NULL
  for(j in 1:length(files.sub)){
    print(files.sub[j])
    r <- rast(files.sub[j])
   
    timp <- as.character(format(time(r), "%Y-%m-%d %H:%M:%S"))
    
    
    for(l in 1:nlyr(r)){
      
      print(timp[l])
      r1 <- terra::mask(r[[l]],vect(limite.sub))
      
      #tab <- terra::extract(r[[l]],vect(limite.sub), fun = mean, na.rm =T)[,2]
      tab <- data.frame(data = timp[l], param = round(exactextractr::exact_extract(r[[l]],limite.sub,fun = 'mean') -273.15, 2))
      
      df <- rbind(tab,df)
      
    }
    
  }
  
  write.csv(paste0("/Volumes/Z_vld/sfica_proiect_NE/era5land/tabs/"),nume[i],"all_temperature_hourly_1981-2021.csv")
  
}

####
plot(r1)
plot(limite.sub,add= T, col = "transparent")
writeRaster(r[[l]], "test.tif")
