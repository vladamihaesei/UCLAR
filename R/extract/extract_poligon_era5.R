library(terra)
library(sf)
library(lubridate)
### drive windows sau mac 
drive_d <- ifelse(Sys.info()[1] == "Darwin", "/Volumes/vld_z", "/D/")

### citeste limitele 
limite <- read_sf("Limite/Limite.shp")

files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/dailymerged",pattern = ".nc", recursive = T,  full.names = T)
files <- grep("10m_v_component",files,invert = T, value = T)## elimina vantul 
files.split <- do.call(rbind, strsplit(files, "\\/|_"))
params <- paste0("/",unique(files.split[,10]))
#params <- params[2]

r <- rast(files[2])
r <- r[[1:3]]
timp <- as.Date(format(time(r), "%Y-%m-%d"))

#### extragere poligon 
for( i in 1:nrow(limite)){
  
  limite.sub <- limite[i,]
  nume <- limite.sub$Drawings
  print(nume)
  df <- data.frame(year = year(timp), month = month(timp), day = day(timp))
  for(j in 1:length(params)){
    
    print(params[j])
    files.sub <- grep(params[j], files,fixed = T, value = T)
    r <- rast(files.sub)
    r <- r[[1:3]]
    #timp <- as.character(format(time(r), "%Y-%m-%d %H:%M:%S"))
    timp <- as.Date(format(time(r), "%Y-%m-%d"))
    
    df1 <- NULL
    for(l in 1:nlyr(r)){
        
        print(timp[l])
        #tab <- terra::extract(r[[l]],vect(limite.sub), fun = mean, na.rm =T)[,2]
        tab <- data.frame(year = year(timp[l]), month = month(timp[l]), day = day(timp[l]),  param = round(exactextractr::exact_extract(r[[l]],limite.sub,fun = 'mean'),2))
        names(tab)[4] <- params[j]
        df1 <- rbind(df1,tab)
      
    }
    
    df <- cbind(df, df1[4]) 
  }
  
  write.csv(df,paste0("/Volumes/Z_vld/sfica_proiect_NE/era5land/tabs/",nume,"_all_daily_1981-2021.csv"))
  
}

####### verificare

### hourly 
files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/scripts/python",pattern = ".nc", recursive = T,  full.names = T)
files
r <- rast(files[11])
plot(r)
#### daily 
files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/era5land/daily",pattern = ".nc", recursive = T,  full.names = T)
files
r <- rast(files[8])
plot(r)
