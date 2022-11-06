library(ggplot2)
library(dplyr)
library(tidyr)
orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for(o in 1:length(orase)){
  
  print(orase[o])
  night_day <- c("Night", "Day")
  df <- NULL
  for(d in 1: length(night_day)){
    print(night_day[d])
    mod <- 
    for(m in 1:length(mod)){
      
      files <- list.files(paste0("grids/MODIS/nc/filled/LST_",night_day[d],"_1km/",orase[o]), recursive = T, full.names = T, pattern = ".tif")
      files <- grep(".aux.xml", files, value = T, invert = T)
      files <- grep(mod[m], files, value = T)
      #### testare 
      
      rst <- terra::plot(terra::rast(files))
      
      tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/",mod[m],"/",night_day[d],"_","cloud_pixels.csv"))
      
      t <- tabs%>%filter(frecventa <= 20)
      
      r <- terra::rast(files)
      r <- r -273.15
      terra::time(r) <- as.Date(names(r),"X%Y.%m.%d")
      r.sub <- r[[which(format(terra::time(r),"%Y-%m-%d") %in% c(t$timp))]]
      df.r <- as.data.frame(r.sub, xy =T)%>%pivot_longer(-c(x,y), names_to = "ind", values_to = "values")%>%
        mutate(tip = mod[m], per = night_day[d])
      df <- rbind(df,df.r)
    }
    
    
  }
  gg <- ggplot(df)+
    geom_raster(aes(x = x , y = y, fill = values))+
    coord_sf(xlim = c(24.96,25.140), ylim = c(45.20, 45.320), expand = F)+
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
}


vf <- raster::raster("grids/MODIS/tif/nonfilled/MOD11A1/LST_Night_1km/Barlad/2002-07-05.tif")
raster::plot(vf)

         