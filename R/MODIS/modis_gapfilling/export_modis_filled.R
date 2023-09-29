library(terra)

orase <- c("Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")#"Barlad","Bacau",

orase <- "Suceava2"
##### MOD11A1 & MYD11A1
for( i in 1:length(orase)){
  
  print(orase[i])
  types <- c("MOD11A1","MYD11A1")
  for(k in 1:length(types)){
    
    dn <- c("Day")#"Night",
    
    for(l in 1:length(dn)){
      
      files <- list.files(paste0("grids/MODIS/nc/filled/LST_",dn[l],"_1km/",orase[i],"/",types[k],"_",dn[l],"_2000-2020"), pattern = ".tif", recursive = T, full.names = T)
      
      rst <- terra::rast(files)
      
      for(jj in 1:nlyr(rst)){
        
        nume <- as.Date(names(rst[[jj]]), "X%Y.%m.%d")
        print(nume)
        out <- paste0("grids/MODIS/tif/filled/",types[k],"/","LST_",dn[l],"_1km/",orase[i],"/")
        if(!dir.exists(out)) dir.create(out, recursive = T)
        terra::writeRaster(rst[[jj]],paste0(out,nume,".tif"),overwrite  = T)
        
      }
    }
  }
  
}


