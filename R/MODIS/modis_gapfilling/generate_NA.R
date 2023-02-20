library(remotes)
#remotes::install_github("ffilipponi/rtsa")
#install.packages("https://github.com/ffilipponi/rtsa.git")
#install.packages("https://github.com/marchtaylor/sinkr")
#install_github("marchtaylor/sinkr")
library(sinkr) 
library(rtsa)
library(rgdal)
library(raster)
library(sp)
library(dplyr)

library(lubridate)

orase <- c("Bacau","Iasi","Botosani","PiatraNeamt") ## Pascani 

for (i in 1:length(orase)){
  
  print(orase[i])
  
  types <- c("MOD11A1","MYD11A1")
  
  for(j in 1:length(types)){
    
    dn <- c("Day","Night")
    
    for (d in 1:length(dn)){
      
      files <- list.files(paste0("grids/MODIS/nc/nonfilled/",types[j],"/",orase[i]), full.names = T, pattern = ".nc")
      r <- raster::stack(files, varname = paste0("LST_",dn[d],"_1km"))## citeste cu raster, cateodata salveaza data time
      #rr <- terra::rast(files, subds = "LST_Day_1km")## citeste cu terra, cateodata salveaza data time 
      
      tabs  <- read.csv(paste0("tabs/MODIS/",orase[i],"/",types[j],"/",dn[d],"_","cloud_pixels.csv"))
      # tabs$timp <- as.Date(tabs$timp)
      # t <- tabs%>%filter(frecventa <= 0.5& year(tabs$timp)>2019 & year(tabs$timp) < 2021) 

      t <- tabs%>%filter(frecventa <= 0.5&timp%in%c("2020-08-06","2020-08-29","2020-07-29")) 
      timp <- t$timp
      r.sub <- r[[which(format(raster::getZ(r),"%Y-%m-%d") %in% c(timp))]]
  
      for(n in 1:nlayers(r.sub)){
        
        rt <- r.sub[[n]]
        tmp <- getZ(rt)
        
        # Plot the masked raster to visualize the gaps
        xmin <- 26.35
        xmax <- 26.42
        ymin <- 46.91
        ymax <- 46.97
        
        # create a mask of the region to set as NA
        mask <- raster(rt)
        extent(mask) <- c(xmin, xmax, ymin, ymax)
        mask[] <- 1
        mask <- mask > 0
        polg <- rasterToPolygons(mask, dissolve = T)## creaza polygon pentru harta 
        
        mask <- raster::raster("grids/MODIS/tif/CopyOfnonfilled/MOD11A1/LST_Day_1km/Botosani/2020-01-25.tif")
        # set the values in the region to NA
        rt[is.na(mask)] <- NA
        plot(rt)
        
        out <- paste0("grids/MODIS/tif/CopyOfnonfilled/",types[j],"/LST_",dn[d],"_1km/",orase[i],"/")
        raster::writeRaster(rt, paste0(out,tmp,".tif"),overwrite = T)
       
      } 
    }
  }
  
}

rc <- raster::raster("grids/MODIS/tif/CopyOfnonfilled/MOD11A1/LST_Day_1km/Bacau/2020-02-16.tif")

# Plot the masked raster to visualize the gaps
### Piatra Neamt 
# xmin <- 26.35
# xmax <- 26.42
# ymin <- 46.91
# ymax <- 46.97
