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

scene <- c("182028","183027", "183028")

for (i in 1:length(scene)){
  
  print(orase[i]) #pun in consola i=nr.oras apoi run...
  #files <- list.files(paste0("E:/1UCLAR/Date_Lansat/proba_gapfill/Bacau/",orase[i],"/"), full.names = T, pattern = ".tif")
  
  files <- list.files(paste0("grids/LANDSAT/Toate"), full.names = T, recursive = T,pattern = ".tif")

  files1 <- grep(scene[i], files, value = T)
  
  dates <- unique(as.Date(unique(do.call(rbind, strsplit(files1,"/|_|.tif"))[,6]),"%Y%m%d"))
  ani1 <- unique(format(dates,"%m"))
  ani <- sort(ani1)
  for (j in 1:length(dates)){
    
    print(ani[j])
    
    files.sub <- files1[format(dates, "%m") == ani[j]]
    files.sub
    #files.sub <- files[which(format(dates,"%Y")==ani[i])]
    
    rss <- raster::stack(files.sub)
    #rss <- parallel::mclapply(files.sub, function(i){raster::stack(r(i)}, mc.cores = 5)
  
    rss[is.na(rss)]<- NA

    dates1 <- as.Date(format(as.Date(do.call(rbind,strsplit(files.sub,"_|/|.tif"))[,6], "%Y%m%d"), "%Y-%m-%d"))
   
    rst.tem <- rts(rss, dates1)
    
    # using dineof interpolation and raster mask
    rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")
    
    out <- paste0("E:/1UCLAR/Date_Lansat/proba_gapfill/Bacau/",orase[i],"/")
    if (!dir.exists(out)) dir.create(out, recursive = T)
    
    write.rts(rasterts_dineof, paste0(out,ani[j]))
   

}
