library(dplyr)
library(tidyr)
library(seas)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for(o in 1:length(orase)){
  
  print(orase[o])
  night_day <- c("Night", "Day")

  for(d in 1: length(night_day)){
    
    print(night_day[d])
    df <- NULL
    tt <- NULL
    mod <- c("MOD11A1","MYD11A1")
    
    for(m in 1:length(mod)){
      
      print(mod[m])
      files <- list.files(paste0("grids/MODIS/nc/filled/LST_",night_day[d],"_1km/",orase[o],"/",mod[m],"_",night_day[d],"_2000-2020"),recursive = T, full.names = T, pattern = ".tif")[1]
      tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/",mod[m],"/",night_day[d],"_","cloud_pixels.csv"))
      t <- tabs%>%filter(frecventa <= 20)
      tt <- rbind(tt,t) 
      r <- terra::rast(files)
      r <- r -273.15
      terra::time(r) <- as.Date(names(r),"X%Y.%m.%d")
      r.sub <- r[[which(format(terra::time(r),"%Y-%m-%d") %in% c(t$timp))]]
      
      df.r <- as.data.frame(r.sub, xy =T)%>%pivot_longer(-c(x,y), names_to = "ind", values_to = "values")%>%
        mutate(tip = mod[m], per = night_day[d])
      df.r$ind <- as.Date(df.r$ind, format = "X%Y.%m.%d")
      df <- rbind(df,df.r)
      
    }
    
    ### calculeaza media ponderata se complica treaba 
    df.myd <- tt%>%filter(frecventa <= 20 & tip == "MYD11A1")  ## selecteaza pragul de nebulozitate
    df.mod <- tt%>%filter(frecventa <= 20 & tip == "MOD11A1") ## selecteaza pragul de nebulozitate 
    
    df.s1 <- df%>%group_by(x,y,ind,per,tip)%>%summarise(values.mean.myd = ifelse(tip == "MYD11A1",mean(values*((nrow(df.myd)/nrow(tt)))),mean(values*((nrow(df.mod)/nrow(tt))))))%>%
      group_by(x,y,ind,per)%>%summarise(values.mean = sum(values.mean.myd))
    df.seas <- df.s1%>%mutate(seas = mkseas(ind, width = "DJF"))%>%group_by(x,y,per,seas)%>% summarise(mean.seas = mean(values.mean))
    
    ####each  season 
    sez = c("DJF","MAM","JJA","SON")
    for(s in 1:length(sez)){
      
      print(sez[s])
      test <- df.seas%>%filter(seas==sez[s])
      ### exporta ca tif 
      path.out <- paste0("grids/MODIS/filled_selection/", orase[o],"/")
      if(!dir.exists(path.out)) dir.create(path.out, recursive = T)
      rr <- terra::rast(test[c(1,2,5)], type="xyz",crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      terra::writeRaster(rr,paste0(path.out,night_day[d],"_",sez[s],".tif"), overwrite = T) 
      
    }
    
  }

}

#### verificare 

vf <- terra::rast(list.files("grids/MODIS/filled_selection/Vaslui/", pattern = ".tif", full.names = T))


