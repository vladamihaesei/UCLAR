library(terra)
library(raster)

### trebuie sa creezi un
files <- list.files("tmp", pattern = ".nc",full.names = T, recursive = T) ### afiseasaza toate fisierele format nc ### schimbat calea



#### lista cu orasele

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for ( o in 1:length(orase)){
  
  print(orase[o])
  fl <- grep(orase[o],files, value = T)
  vars <- c("LST_Day_1km", "LST_Night_1km", "QC_Day", "QC_Night")
  
  for( v in 1:length(vars)){
    print(vars[v])
   
    r <- rast(fl,subds = vars[v])
    #r <- raster::brick(fl,varname = vars[v])
    
    for(i in 1:nlyr(r)){
      
      r11 <- terra::rast(r[[i]])
      r1 <- raster::brick(r[[i]])
      timp <- format(as.Date(time(r11)),"%Y%m%d")
      #timp <- format(raster::getZ(r1), "%Y%m%d")
      print(timp)
      
      path10 <- paste0("grids/",orase[o],"/Surf_Temp_Daily_1Km_v6/",vars[v],"/") ### aici de umblat  doar la "grids/"
      
      if (!dir.exists(path10)) dir.create(path10,recursive = T) ### aici se defineste calea 
      
      raster::writeRaster(r1, paste0(path10, timp, ".tif"),overwrite = T)
      #terra::writeRaster(r1, paste0(path10, timp, ".tif"), overwrite = T, gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
      
    }
    
  }
  
}

### verificare

fg <- terra::rast("grids/Falticeni/Surf_Temp_daily_1km_v6/LST_Day_1km/20190105.tif")
plot(fg)

fg1 <- raster::brick(files, varname = "LST_Night_1km")
plot(fg1)
