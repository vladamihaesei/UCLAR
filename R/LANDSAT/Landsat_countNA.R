library(terra)
library(dplyr)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")
### metoda 1
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
  ## incepe loop-ul de calcul pentru nr de pixeli lipsa pentru fiecare zi 
  for(i in 1:nlyr(ff)){
    
    print(dats1[i])
    rss <- ff[[i]]
    frec <- (sum(is.na(values(rss)))*100)/length(values(rss))
    dat <- terra::time(rss)
    t <- data.frame(timp = dat, frecventa = frec, tip = "Landsat")
    tab <- rbind(tab,t)
    
  }
  
  tab <- tab%>%dplyr::mutate(year = format(timp,"%Y"),
                      month = format(timp,"%m"))
  
  write.csv(tab,paste0("tabs/LANDSAT/countNA_",orase[o],".csv"), row.names = F)## exporta fisier csv cu frecventa acoperirii
  
  #### aici poti modifica pragul
  tab.f <- tab%>%dplyr::filter(frecventa >=80) ### filtru pentru lipsuri mai mari sau egali de 80%
  
  zile <-  as.character(gsub(",", "|",noquote(toString(format(tab.f$timp,"%Y%m%d")))))### aducerea la format de lista a datelor tip caracter 
  ### ca sa intelegi linia de mai sus trebuie sa o rulezi pe bucatele. 
  zile1 <- gsub(" ","", zile)## sterge spatiul intre ele
  
  fl.sub <- grep(paste0(zile1), fl, value =T, invert =TRUE)### selecteaza fisierele de interes. aici zilele cu acoperire mai mica de 80. 
  #Functia invert de mai sus  trebuie setata ca True; ea inverseaza selectia. Adica selecteaza fisierele cu lipsuri <=80%
  out <- paste0("grids/LANDSAT/filled/Selection/",orase[p],"/") ## folderul unde se vor copia fisierele
  if (!dir.exists(out)) dir.create(out, recursive = T)### creaza folderul daca acesta nu exista
  file.copy(fl.sub, out)## aici sunt copiate fisierele de interes
  
}

# 
# #### metoda 2 lipeste fiecarui fisier tif frecventa 
# for (p in 1:length(orase)){
#   
#   fl <- list.files(paste0("/Volumes/Z_vld/sfica_proiect_NE/Landsat/Landsat_",orase[o],"_LST_T-20220725T095615Z-001/Landsat_",orase[o],"_LST_T/"), full.names = T, pattern = ".tif")
#   
#   ## tasteaza in consola p=1, dupa incepe sa rulezi de la fl, adica linia 9 ### nu da for la toata bucla
#   #ruleaza linia de dedesubt - unique(do.call(rbind, strsplit(fl,"/|_|.tif"))) - scoate diezul. Dupa ce ruleaza
#   # trebuie schimbat numarul 20 din linia 17. In functie de locatia fisierelor. 
#   
#   #unique(do.call(rbind, strsplit(fl,"/|_|.tif"))) 
#   
#   dats <- as.Date(unique(do.call(rbind, strsplit(fl,"/|_|.tif"))[,20]), format = "%Y%m%d")
#   dats1 <- sort(dats)
#   
#   f <- terra::rast(fl)
#   terra::time(f) <- dats
#   names(f) <- dats
#   ff <- f[[order(terra::time(f))]]
#   
#   tab <- NULL
#   ## incepe loop-ul de calcul a nr de pixeli lipsa pentru fiecare zi 
#   for(i in 1:nlyr(ff)){
#     
#     print(dats1[i])
#     rss <- ff[[i]]
#     den <- strsplit(terra::sources(rss),"/|.tif")[[1]][8]### denumirea trebuie sa fie ultima de dupa "/". Eu am pus pozitia 8. La tine poate sa difere
#     frec <- (sum(is.na(values(rss)))*100)/length(values(rss)) ## calculeaza frecventa
#     frec1 <- as.character(round(frec,digits = 0))## rotunjeste 
#     dat <- terra::time(rss)### selecteaza data din raster
#     t <- data.frame(timp = dat, frecventa = frec, tip = "Landsat")## creaza un data frame sau un tabel
#     tab <- rbind(tab,t)## adauga tabelul in loop
#     writeRaster(rss,paste0("grids/LANDSAT/filled/Rescriere/",orase[p],den,"_CC_",frec1,".tif"))## exporta rasterul cu frecventa nebulozitatii lipite in denumirea sa
#     
#   }
#   
#   tab <- tab%>%dplyr::mutate(year = format(timp,"%Y"),
#                              month = format(timp,"%m"))
#   
#   write.csv(tab,paste0("tabs/LANDSAT/countNA_",orase[o],".csv"), row.names = F)## exporta fisier csv cu frecventa acoperirii
#   
# }