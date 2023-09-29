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

orase <- c("Iasi","Bacau","Botosani","PiatraNeamt") ## Pascani 

##### MOD11A1
for (i in 1:length(orase)){
  
  print(orase[i])
  types <- c("summer", "winter")
  
  for(j in 1:length(types)){
    
    print(types[j])
    files <- list.files(paste0("grids/LANDSAT/CopyOfdata_2/",orase[i],"/",types[j]), full.names = T, pattern = ".tif")
    files <- files[c(1:40,101)]
    #rr1 <- terra::rast(files)# citeste cu raster, cateodata salveaza data time
    r1 <- raster::stack(files)# citeste cu raster, cateodata salveaza data time
    
    dates <- as.Date(do.call(rbind,strsplit(files,"/|_|.tif"))[,9],"%Y%m%d")
    
    r1[is.na(r1)]<- NA
    
    rst.tem1 <- rts(r1, dates)
    
    rasterts_dineof1 <- rtsa.gapfill(rst.tem1, rastermask = NULL, method="dineof")
    
    out1 <- paste0("grids/LANDSAT/filled_validation/",orase[i])
    
    if (!dir.exists(out1)) dir.create(out1, recursive = T)
    write.rts(rasterts_dineof1, paste0(out1,"/",types[j]), overwrite = T)
    
  }
  
}

### 
vf <- raster::brick("grids/LANDSAT/filled_validation/Iasi/summer/summer.tif") 
fl <- grep("2019|2020", fl,value = T, invert = T)
file.remove(fl)
