library(sf)
library(terra)
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

ls <- list.files("grids/LANDSAT/CopyOfdata_2/Suceava2", pattern = ".tif", full.names = T, recursive = T)
ls <- grep("aux.xml|.ovr", ls, invert =T, value =T)
r <- terra::rast(ls)
date <- as.Date(do.call(rbind, strsplit(names(r),"_"))[,2], format = "%Y%m%d")

terra::time(r) <- date
r
terra::writeCDF(r, "grids/LANDSAT/CopyOfdata_2/Suceava2/landsat_agg_2019-2020.nc")

#### 
### citirea rasterului exportat 

r1 <- raster::stack("grids/LANDSAT/CopyOfdata_2/Suceava2/landsat_agg_2019-2020.nc")# citeste cu raster, cateodata salveaza data time

dates <- as.Date(names(r1),"X%Y.%m.%d")

r1[is.na(r1)]<- NA

rst.tem1 <- rts::rts(r1, dates)

rasterts_dineof1 <- rtsa.gapfill(rst.tem1, rastermask = NULL, method="dineof")

out1 <- paste0("grids/LANDSAT/filled/Suceava/")

write.rts(rasterts_dineof1, paste0(out1,"/","Landsat"), overwrite = T)

### export each layer from the above 

rt <- terra::rast("grids/LANDSAT/filled/Suceava/Landsat/Landsat.tif")
dates  <- as.Date(names(rt),format("X%Y.%m.%d"))
nl <- nlyr(rt)

for (i in 1:nlyr(rt)){
  
  print(dates[i])
  rt.sub <- rt[[i]]
  dates.s <- dates[i]
  terra::writeRaster(rt.sub,paste0("grids/LANDSAT/filled/tif/Suceava/",dates.s,".tif"), overwrite = T)
  
}


