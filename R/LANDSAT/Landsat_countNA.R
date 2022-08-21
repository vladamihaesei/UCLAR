library(terra)
library(dplyr)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for (p in 1:length(orase)){
  
  fl <- list.files(paste0("/Volumes/Z_vld/sfica_proiect_NE/Landsat/Landsat_",orase[o],"_LST_T-20220725T095615Z-001/Landsat_",orase[o],"_LST_T/"), full.names = T, pattern = ".tif")
  
  ## tasteaza in consola p=1, dupa incepe sa rulezi de la fl, adica linia 9 ### nu da for la toata bucla
  #ruleaza linia de dedesubt - unique(do.call(rbind, strsplit(fl,"/|_|.tif"))) - scoate diezul. Dupa ce ruleaza
  # trebuie schimbat numarul 20 din linia 17. In functie de locatia fisierelor. 
 
  #unique(do.call(rbind, strsplit(fl,"/|_|.tif"))) 
  
  dats <- as.Date(unique(do.call(rbind, strsplit(fl,"/|_|.tif"))[,20]), format = "%Y%m%d")
  dats1 <- sort(dats)
  
  f <- terra::rast(fl)
  terra::time(f) <- dats
  names(f) <- dats
  ff <- f[[order(terra::time(f))]]
  
  tab <- NULL
  
  for(i in 1:nlyr(ff)){
    
    print(dats1[i])
    rss <- ff[[i]]
    frec <- (sum(is.na(values(rss)))*100)/length(values(rss))
    dat <- terra::time(rss)
    t <- data.frame(timp = dat, frecventa = frec, tip = "Landsat")
    tab <- rbind(tab,t)
    
  }
  
  tab <- tab%>%dplyr::mutate(year  = format(timp,"%Y"),
                      month = format(timp,"%m"),
                      dens = frecventa/sum(frecventa)*100)
  write.csv(tab,paste0("tabs/LANDSAT/countNA_",orase[o],".csv"), row.names = F)
  
}



h$density = h$counts/sum(h$counts)*100


