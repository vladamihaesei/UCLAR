library(terra)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")


for( i in 1:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/filled/",orase[i]), pattern = ".tif", recursive = T, full.names = T)
  
  for( j in 1:length(files)){
    
    print(files[j])
    
    rst <- terra::rast(files[j])
    
    
    for(k in 1:nlyr(rst)){
    
      nume <- strsplit(names(rst[[k]]), "_")[[1]][3]
      out <- paste0("grids/LANDSAT/filled/",orase[i],"/")
      if(!dir.exists(out)) dir.create(out, recursive = T)
      terra::writeRaster(rst[[k]],paste0(out,nume,".tif"))
      
    }
  
  }
  
}

### verificare 
vf <- terra::rast("grids/LANDSAT/filled/Barlad/20131125.tif")
plot(vf)
