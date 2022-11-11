library(dplyr) 
library(terra)
library(ggplot2)
### rasterele de analizat 

nume <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for(n in 1:length(nume)){
  
  print(nume[n])
  dn <- c("Night", "Day")
  
  for(d in 1:length(dn)){
  
    print(dn[d])
    mod <- c("MOD11A1","MYD11A1")
    for(m in 1:length(mod)){
      
      print(mod[m])
      
      files.sub1 <- list.files(path = paste0("grids/MODIS/tif/nonfilled/",mod[m],"/LST_",dn[d],"_1km/",nume[n]), recursive = T, full.names = T, pattern = ".tif")
      files.sub1 <- grep("aux.json",files.sub1, invert = T, value = T)
    
      dats <- as.Date(do.call(rbind,strsplit(files.sub1,"/|_|.tif"))[,10], format("%Y-%m-%d"))
      
      tab <- NULL
      
      for(i in 1:length(files.sub1)){
        
        print(dats[i])
        rss <- terra::rast(files.sub1[i])
        frec <- (sum(is.na(terra::values(rss)))*100)/length(terra::values(rss))
        dat <- dats[i]
        t <- data.frame(timp = dat, frecventa = frec, tip = mod[m], Day_night = dn[d])
        tab <- rbind(tab,t)
     
      }

      out.path <- paste0("tabs/MODIS/",nume[n],"/",mod[m],"/")
      if(!dir.exists(out.path)) dir.create(out.path,recursive = T)
      write.csv(tab,paste0(out.path, dn[d],"_cloud_pixels.csv"))
      
    }
    
    
  }
  
}


