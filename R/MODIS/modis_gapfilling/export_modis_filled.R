library(terra)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

##### MOD11A1
for( i in 1:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/MODIS/nc/filled/LST_Day_1km/",orase[i],"/MOD11A1Day_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
  
  
  files1 <- list.files(paste0("grids/MODIS/nc/filled/LST_Night_1km/",orase[i],"/MOD11A1Night_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
  
  files <- files[1]### elimina extensia aux.xml
  files1 <- files1[1]### elimina extensia aux.xml
  
  rst <- terra::rast(files)
  rst1 <- terra::rast(files1)
  
  for(j in 1:nlyr(rst)){
    
    nume <- as.Date(names(rst[[j]]), "X%Y.%m.%d")
    print(nume)
    out <- paste0("grids/MODIS/tif/filled/MOD11A1/LST_Day_1km/",orase[i],"/")
    if(!dir.exists(out)) dir.create(out, recursive = T)
    terra::writeRaster(rst[[j]],paste0(out,nume,".tif"), overwrite = T)
    
  }
  
  for(jj in 1:nlyr(rst1)){
    
    nume1 <- as.Date(names(rst1[[jj]]), "X%Y.%m.%d")
    print(nume1)
    out1 <- paste0("grids/MODIS/tif/filled/MOD11A1/LST_Night_1km/",orase[i],"/")
    if(!dir.exists(out1)) dir.create(out1, recursive = T)
    terra::writeRaster(rst1[[jj]],paste0(out1,nume1,".tif"),overwrite  = T)
    
  }
  
}

###### MYD
for( i in 9:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/MODIS/nc/filled/LST_Day_1km/",orase[i],"/MYD11A1_Day_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
  
  files1 <- list.files(paste0("grids/MODIS/nc/filled/LST_Night_1km/",orase[i],"/MYD11A1_Night_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
  
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
