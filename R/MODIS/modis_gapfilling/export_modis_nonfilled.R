library(terra)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

orase <- "Suceava2"

for( i in 1:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/MODIS/nc/nonfilled/MOD11A1/",orase[i]), full.names = T, pattern = "nc")
  
  files1 <- list.files(paste0("grids/MODIS/nc/nonfilled/MYD11A1/",orase[i]), full.names = T, pattern = "nc")
  
  rst.day <- terra::rast(files, subds = "LST_Day_1km")
  rst.night <- terra::rast(files, subds = "LST_Night_1km")
  
  rst1.day <- terra::rast(files1,subds = "LST_Day_1km")
  rst1.night <- terra::rast(files1,subds = "LST_Night_1km")
  
  ### MOD
  for(j in 1:nlyr(rst.day)){
    
    nume <- as.Date(terra::time(rst.day[[j]]))
    print(nume)
    out <- paste0("grids/MODIS/tif/nonfilled/MOD11A1/LST_Day_1km/",orase[i],"/")
    if(!dir.exists(out)) dir.create(out, recursive = T)
    terra::writeRaster(rst.day[[j]],paste0(out,nume,".tif"), overwrite = T)
    
  }
  
  for(j in 1:nlyr(rst.night)){
    
    nume <- as.Date(terra::time(rst.night[[j]]))
    print(nume)
    out <- paste0("grids/MODIS/tif/nonfilled/MOD11A1/LST_Night_1km/",orase[i],"/")
    if(!dir.exists(out)) dir.create(out, recursive = T)
    terra::writeRaster(rst.night[[j]],paste0(out,nume,".tif"), overwrite = T)
    
  }
  
  #### MYD
  for(jj in 1:nlyr(rst1.day)){
    
    nume1 <- as.Date(terra::time(rst1.day[[jj]]))
    print(nume1)
    out1 <- paste0("grids/MODIS/tif/nonfilled/MYD11A1/LST_Day_1km/",orase[i],"/")
    if(!dir.exists(out1)) dir.create(out1, recursive = T)
    terra::writeRaster(rst1.day[[jj]],paste0(out1,nume1,".tif"),overwrite  = T)
    
  }
  
  for(jj in 1:nlyr(rst1.night)){
    
    nume1 <- as.Date(terra::time(rst1.night[[jj]]))
    print(nume1)
    out1 <- paste0("grids/MODIS/tif/nonfilled/MYD11A1/LST_Night_1km/",orase[i],"/")
    if(!dir.exists(out1)) dir.create(out1, recursive = T)
    terra::writeRaster(rst1.night[[jj]],paste0(out1,nume1,".tif"),overwrite  = T)
    
  }
  
}



###### MYD
for( i in 9:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/MODIS/nc/filled/LST_Day_1km/",orase[i],"/MYD11A1Day_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
  
  files1 <- list.files(paste0("grids/MODIS/nc/filled/LST_Night_1km/",orase[i],"/MYD11A1Night_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
  
  files <- files[1]### elimina extensia aux.xml
  files1 <- files1[1]### elimina extensia aux.xml
  
  rst <- terra::rast(files)
  rst1 <- terra::rast(files1)
  
  for(j in 1:nlyr(rst)){
    
    nume <- as.Date(names(rst[[j]]), "X%Y.%m.%d")
    out <- paste0("grids/MODIS/tif/filled/MYD11A1/LST_Day_1km/",orase[i],"/")
    if(!dir.exists(out)) dir.create(out, recursive = T)
    terra::writeRaster(rst[[j]],paste0(out,nume,".tif"), overwrite = T)
    
  }
  
  for(jj in 1:nlyr(rst1)){
    
    nume1 <- as.Date(names(rst1[[jj]]), "X%Y.%m.%d")
    out1 <- paste0("grids/MODIS/tif/filled/MYD11A1/LST_Night_1km/",orase[i],"/")
    if(!dir.exists(out1)) dir.create(out1, recursive = T)
    terra::writeRaster(rst1[[jj]],paste0(out1,nume1,".tif"),overwrite  = T)
    
  }
  
}
