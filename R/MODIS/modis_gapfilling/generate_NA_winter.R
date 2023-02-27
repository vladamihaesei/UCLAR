#library(remotes)
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
      tabs$timp <- as.Date(tabs$timp)
      tf <- tabs%>%filter(year(timp) >= 2019 & year(timp) <= 2021 &month(timp) %in% c("1","2","3","12","11") & frecventa <= 0.1)
      tf <- tf[c(round(runif(12, min=1, max=nrow(tf)))),]
      timp <- as.character(tf$timp)
      
      write.csv(tf,paste0("tabs/MODIS/",orase[i],"/",types[j],"/",dn[d],"_","nocloud_days_winter.csv"), row.names = F) 
  
      tabs1 <- read.csv(paste0("tabs/MODIS/",orase[i],"/","MYD11A1","/","Day","_","cloud_pixels.csv")) 
      t.mask <- tabs1%>%filter(year(timp) >= 2019 & year(timp) <= 2021 &month(timp) %in% c("1","2","3","12","11")& frecventa>= 22&frecventa <= 26)
      t.mask <- t.mask[1,]
      timp1 <- t.mask$timp
      r.sub <- r[[which(format(raster::getZ(r),"%Y-%m-%d") %in% c(timp))]]
      
      ### masca pentru nori 
      mask <- raster::raster(paste0("grids/MODIS/tif/CopyOfnonfilled/","MYD11A1","/LST_Day_1km/",orase[i],"/",timp1,".tif"))
      aut <- paste0("grids/MODIS/tif/mask/",orase[i],"/")
      if (!dir.exists(aut)) dir.create(aut)
      writeRaster(mask,paste0(aut,timp1,".tif"), overwrite = T)
      for(n in 1:nlayers(r.sub)){
        rt <- r.sub[[n]]
        tmp <- getZ(rt)
        # set the values in the region to NA
        rt[is.na(mask)] <- NA
        #plot(rt)
        out <- paste0("grids/MODIS/tif/CopyOfnonfilled/",types[j],"/LST_",dn[d],"_1km/",orase[i],"/")
        raster::writeRaster(rt, paste0(out,tmp,".tif"),overwrite = T)
      }
    }
  }
  
}

#rc <- raster::raster("grids/MODIS/tif/CopyOfnonfilled/MOD11A1/LST_Day_1km/Bacau/2020-02-16.tif")


# 
# # Plot the masked raster to visualize the gaps
# xmin <- 26.35
# xmax <- 26.42
# ymin <- 46.91
# ymax <- 46.97
# 
# # create a mask of the region to set as NA
# mask <- raster(rt)
# extent(mask) <- c(xmin, xmax, ymin, ymax)
# mask[] <- 1
# mask <- mask > 0
# polg <- rasterToPolygons(mask, dissolve = T)## creaza polygon pentru harta 
# 
# Plot the masked raster to visualize the gaps
### Piatra Neamt 
# xmin <- 26.35
# xmax <- 26.42
# ymin <- 46.91
# ymax <- 46.97
