library(remotes)
remotes::install_github("ffilipponi/rtsa")
#install.packages("https://github.com/ffilipponi/rtsa.git")
install.packages("https://github.com/marchtaylor/sinkr")
#install_github("marchtaylor/sinkr")
library(sinkr) 
library(rtsa)
library(rgdal)
library(raster)
library(sp)

orase <- c("Barlad","Bacau","Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui") ## Pascani 

for (i in 1:length(orase)){
  
  print(orase[i])
  
  types <- c("MOD11A1","MYD11A1")
  
  for(j in 1:length(types)){
    
    dn <- c("Day","Night")
    
    for (d in 1:length(dn)){
      
      files <- list.files(paste0("grids/MODIS/nc/nonfilled/",types[j],"/",orase[i]), full.names = T, pattern = "nc")
      r <- raster::stack(files, varname = paste0("LST_",dn[d],"_1km"))## citeste cu raster, cateodata salveaza data time
      #rr <- terra::rast(files, subds = "LST_Day_1km")## citeste cu terra, cateodata salveaza data time 
      
      tabs  <- read.csv(paste0("tabs/MODIS/",orase[i],"/",types[j],"/",dn[d],"_","cloud_pixels.csv"))
      
      t <- tabs%>%filter(frecventa <= 0.5) 
      
      r.sub <- r[[which(format(raster::getZ(r),"%Y-%m-%d") %in% c(t$timp))]]
      
      cc <- c(10,20,30,40,50,60,70,80,90,95) ### grad cer acoperit
      
      for (c in 1:length(cc)){
        
        ss <- sampleRandom(r[[1]], ncell(r)-((ncell(r)*cc[c])/100), na.rm=TRUE, xy=TRUE)
        
        r4 <- rasterFromXYZ(ss)
        
        r.sub[is.na(r4)] <- NA
        
        dates <- raster::getZ(r)### citeste data cu raster
        #dates <- terra::time(rr)##  citeste data cu terra
        
        rst.tem <- rts(r, dates)
        
        rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")
        
        out <- paste0("grids/MODIS/nc/filled_validation/CC",cc[c],"_LST_",dn[d],"_1km/",orase[i],"/")
        
        if (!dir.exists(out)) dir.create(out, recursive = T)
        
        write.rts(rasterts_dineof, paste0(out,types[j],"_",dn[d],"_CC90_2000-2020"))
        
      }
    }
  }
  
}

