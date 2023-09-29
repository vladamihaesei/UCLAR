library(terra)
library(dplyr)
library(raster)
orase <- c("Iasi","Bacau","Botosani","PiatraNeamt") ## Pascani #

for (i in 1:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/LANDSAT/data_2/",orase[i], "/summer"), full.names = T, pattern = ".tif")
  length(files)
  rr <- raster::stack(files)## citeste cu raster, cateodata salveaza data time
  rr.ter <- terra::rast(files)## citeste cu terra, cateodata salveaza data time 
  dats <- as.Date(do.call(rbind, strsplit(files,"_"))[,4], format = "%Y%m%d")
  terra::time(rr.ter) <- dats
  rr <- raster::setZ(rr,dats, name = "time")
  tabs1 <- read.csv(paste0("tabs/LANDSAT/countNA_",orase[i], "_summer.csv"))%>%dplyr::arrange(frecventa)%>%filter(frecventa<=1)
  tabs1.f <- tabs1[1,]
  rt <- rr[[which(getZ(rr) %in% as.Date(tabs1.f$timp))]]
  rt.ti <- rr.ter[[which(time(rr.ter) %in% as.Date(tabs1.f$timp))]]
  
  nume <- strsplit(sources(rt.ti),"/")[[1]][12]
  print(nume)
  rtt <- raster::projectRaster(rt, crs = "+proj=longlat +datum=WGS84", method = "bilinear")
  rtt
  # Plot the masked raster to visualize the gaps
   xmin <- 26.9
   xmax <- 26.98
   ymin <- 46.55
   ymax <- 46.60
   
   # create a mask of the region to set as NA
   mask <- raster(rtt)
   extent(mask) <- c(xmin, xmax, ymin, ymax)
   mask[] <- 1
   mask <- mask > 0
   mask <- raster::projectRaster(mask, crs = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", method = "bilinear")
   
   rt[mask] <- NA
   plot(rt)
  #polg <- rasterToPolygons(mask, dissolve = T)## creaza polygon pentru harta 
   
  #grids/LANDSAT/data_2/Bacau/summer/LC08_183028_20150709_algn.tif" masca Bacau cu NA 
  #grids/LANDSAT/data_2/Botosani/summer/LT05_183027_20070703.tif masca Botosani cu NA, adica cu nori
  #grids/LANDSAT/data_2/PiatraNeamt/summer/LT05_183027_19990713_algn.tif Piatra Neamt cu NA  
  
  #grids/LANDSAT/data_2/Iasi/winter/LT05_182027_20061216_algn.tif Iasi cu NA - Iarna
  #grids/LANDSAT/data_2/Iasi/summer/LC08_182027_20180608_algn.tif Iasi cu NA - Vara
  #mask <- list.files(paste0("grids/LANDSAT/masks/",orase[i]), full.names = T, pattern = ".tif")
  #mask.r <- terra::rast(mask)
  #mask.r1 <- raster::raster(mask)
  # set the values in the region to NA
  #plot(rt)
  #rt[is.na(mask.r1)] <- NA
  #plot(rt)
  out <- paste0("grids/LANDSAT/CopyOfdata_2/",orase[i],"/summer/",nume)
  out
  #terra::writeRaster(rt, out,overwrite = T)
  terra::writeRaster(rt,out, overwrite= T)
  raster::writeRaster(rt,out, overwrite  = T)
}


# Plot the masked raster to visualize the gaps
### Piatra Neamt 
# xmin <- 26.35
# xmax <- 26.42
# ymin <- 46.91
# ymax <- 46.97
