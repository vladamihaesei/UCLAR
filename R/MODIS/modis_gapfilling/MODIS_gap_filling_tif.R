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

orase <- c("Bacau","Botosani","Iasi","PiatraNeamt") #

##### MOD11A1
for (i in 1:length(orase)){
  
  print(orase[i])
  nr <- c(1,2)
  types <- c("MOD11A1","MYD11A1")
  
  for(j in 1:length(types)){
    
    dn <- c("Day","Night")
    
    for(d in 1:length(dn)){
      
    files <- list.files(paste0("grids/MODIS/tif/CopyOfnonfilled/",types[j],"/LST_",dn[d],"_1km/",orase[i]), full.names = T, pattern = ".tif")
    files <- grep(".aux.json", files,value = T, invert = T)
    files <- grep("2019-|2020-|2021-", files, value = T)
    
    rr1 <- terra::rast(files)# citeste cu raster, cateodata salveaza data time
    r1 <- raster::stack(files)# citeste cu raster, cateodata salveaza data time
    
    dates <- as.Date(do.call(rbind,strsplit(sources(rr1),"/|_|.tif"))[,17])
    
    #r[is.na(r)]<- NA
    r1[is.na(r1)]<- NA
    
    # rst.tem <- rts(r, as.Date(terra::time(rr)))
    
    rst.tem1 <- rts(r1, dates)
    
    #rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")
    rasterts_dineof1 <- rtsa.gapfill(rst.tem1, rastermask = NULL, method="dineof")
    
    #out <- paste0("grids/MODIS/nc/filled/LST_Day_1km/",orase[i],"/")
    out1 <- paste0("grids/MODIS/nc/filled_validation/LST_",dn[d],"_1km/",orase[i],"/")
    
    #if (!dir.exists(out)) dir.create(out, recursive = T)
    if (!dir.exists(out1)) dir.create(out1, recursive = T)
    
    #write.rts(rasterts_dineof, paste0(out,"MOD11A1_Day_2000-2020"))t
    write.rts(rasterts_dineof1, paste0(out1,types[j],"_",dn[d],"_2019-2021"), overwrite = T)
    
    }
  }
  
}

### 

fl <- list.files("grids/MODIS/tif/CopyOfnonfilled/", full.names = T, recursive = T)
fl <- grep("2019|2020", fl,value = T, invert = T)
file.remove(fl)
