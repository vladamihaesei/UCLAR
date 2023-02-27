library(remotes)
#remotes::install_github("ffilipponi/rtsa")
#install.packages("https://github.com/ffilipponi/rtsa.git")
#install.packages("https://github.com/marchtaylor/sinkr")
#install_github("marchtaylor/sinkr")
library(sinkr) 
library(rtsa)
library(rgdal)
library(raster)

orase <- c("Bacau","Iasi","Botosani","PiatraNeamt") ## Pascani 

for (i in 1:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("grids/LANDSAT/data_2/",orase[i], "/summer"), full.names = T, pattern = ".tif")
  r <- raster::stack(files)## citeste cu raster, cateodata salveaza data time
  rr <- terra::rast(files)## citeste cu terra, cateodata salveaza data time 
  rt <- r[[7]]
  split.nume <- strsplit(files[7],"/|_")[[1]]
  nume <- paste0(split.nume[7],"_",
                 split.nume[8],"_",split.nume[9],"_",split.nume[10])
  print(nume)
  #rt <- raster::projectRaster(rt, crs = "+proj=longlat +datum=WGS84", method = "bilinear")
  # Plot the masked raster to visualize the gaps
  # xmin <- 27.58
  # xmax <- 27.78
  # ymin <- 47.1
  # ymax <- 47.24
  # 
  # # create a mask of the region to set as NA
  # mask <- raster(rt)
  # extent(mask) <- c(xmin, xmax, ymin, ymax)
  # mask[] <- 1
  # mask <- mask > 0
  #polg <- rasterToPolygons(mask, dissolve = T)## creaza polygon pentru harta 
  
  #grids/LANDSAT/data_2/Bacau/summer/LC08_183028_20150709_algn.tif" masca Bacau cu NA 
  #grids/LANDSAT/data_2/Botosani/summer/LE07_183027_20020830.tif masca Botosani cu NA, adica cu nori
  #grids/LANDSAT/data_2/PiatraNeamt/summer/LT05_183027_19990713_algn.tif Piatra Neamt cu NA  
  #grids/LANDSAT/data_2/PiatraNeamt/summer/LT05_183027_19990713_algn.tif Piatra Neamt cu NA 
  #grids/LANDSAT/data_2/Iasi/winter/LT05_182027_20061216_algn.tif Iasi cu NA - Iarna
  #grids/LANDSAT/data_2/Iasi/summer/LC08_182027_20180608_algn.tif Iasi cu NA - Vara

  mask <- raster::raster("grids/LANDSAT/data_2/Iasi/summer/LC08_182027_20180608_algn.tif")
  # set the values in the region to NA
  rt[is.na(mask)] <- NA
  plot(rt)
  out <- paste0("grids/LANDSAT/Copydata2/",orase[i],"/summer/",nume)
  raster::writeRaster(rt, out,overwrite = T)
 
}

# Plot the masked raster to visualize the gaps
### Piatra Neamt 
# xmin <- 26.35
# xmax <- 26.42
# ymin <- 46.91
# ymax <- 46.97
