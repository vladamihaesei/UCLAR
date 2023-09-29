library(dplyr)
library(tidyr)
library(seas)
library(lubridate)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan","Pascani",
           "Onesti","PiatraNeamt","Radauti","Roman","Suceava","Vaslui") #

calcul_mean <- function(orase){
  for(o in 1:length(orase)){
    
    print(orase[o])
    
    ####each  season 
    sezon = c("NDJFMA","MJJAS","AprOct")
    
    for(s in 1:length(sezon)){
      
      print(sezon[s])
      night_day <- c("Night", "Day")
      
      for(d in 1: length(night_day)){
        
        print(night_day[d])
        df <- NULL
        tt <- NULL
        mod <- c("MOD11A1","MYD11A1")
        
        for(m in 1:length(mod)){
          
          print(mod[m])
          files <- list.files(paste0("grids/MODIS/nc/filled/LST_",night_day[d],"_1km/",orase[o],"/",mod[m],"_",night_day[d],"_2000-2020"),recursive = T, full.names = T, pattern = ".tif")[1]
          tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/",mod[m],"/",night_day[d],"_","cloud_pixels.csv"))%>%mutate(sez = mkseas(as.Date(timp), width = "DJF"), 
                                                                                                                       sez1 = case_when(month(timp) %in% c("1","2","3","4","11","12")~"NDJFMA",
                                                                                                                                        month(timp) %in% c("5","6","7","8","9")~"MJJAS",
                                                                                                                                        month(timp)%in% c("10","4")~"AprOct"))

          frec <- ifelse(sezon[s]== "NDJFMA",20, 70)
          
          t1 <- tabs%>%filter(sez1 == sezon[s] & frecventa <= frec)
          tt <- rbind(tt,t1) 
          r <- terra::rast(files)
          r <- r -273.15
          terra::time(r) <- as.Date(names(r),"X%Y.%m.%d")
          r.sub <- r[[which(format(terra::time(r),"%Y-%m-%d") %in% c(t1$timp))]]
          
          df.r <- as.data.frame(r.sub, xy =T)%>%pivot_longer(-c(x,y), names_to = "ind", values_to = "values")%>%
            mutate(tip = mod[m], per = night_day[d], seas = sezon[s])
          df.r$ind <- as.Date(df.r$ind, format = "X%Y.%m.%d")
          df <- rbind(df,df.r)
        }
        
        ### calculeaza media ponderata; se complica treaba 
        df.myd <- tt%>%filter(frecventa <= frec & tip == "MYD11A1")  ## selecteaza pragul de nebulozitate
        df.mod <- tt%>%filter(frecventa <= frec & tip == "MOD11A1") ## selecteaza pragul de nebulozitate 
        
        df.s1 <- df%>%group_by(x,y,per,tip)%>%summarise(mean.seas = mean(values))
        
        df.seas <- df.s1%>%summarise(mean.seas = ifelse(tip == "MYD11A1",mean.seas*((nrow(df.myd)/nrow(tt))),mean.seas*((nrow(df.mod)/nrow(tt)))))%>%
          group_by(x,y,per)%>%summarise(values.mean = sum(mean.seas))
        ### exporta ca tif 
        path.out <- paste0("grids/MODIS/filled_selection/", orase[o],"/")
        if(!dir.exists(path.out)) dir.create(path.out, recursive = T)
        rr <- terra::rast(df.seas[c(1,2,4)], type="xyz",crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        terra::writeRaster(rr,paste0(path.out,night_day[d],"_",sezon[s],".tif"), overwrite = T) 
        
      }
      
    }
    
  }
}

calcul_mean(orase = orase)

#### verificare 

vf <- terra::rast(list.files("grids/MODIS/filled_selection/Pascani/", pattern = ".tif", full.names = T))


terra::plot(vf)

#### pascani problem 

gr <- terra::rast("grids/MODIS/nc/nonfilled/MYD11A1/Pascani/MYD11A1_PAS.nc", subds = "LST_Day_1km")

terra::plot(gr[[1:5]])

gr1 <- terra::rast("grids/MODIS/nc/filled/LST_Day_1km/Pascani/MYD11A1_Day_2000-2020/MYD11A1Day_2000-2020.tif")

terra::plot(gr1[[1:5]])



