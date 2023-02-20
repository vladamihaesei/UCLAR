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
library(doParallel)
library(parallel)

orase <- c("182028", "183028")

for (i in 1:length(orase)){
  print(orase[i])
  files <- list.files(paste0("/home/claudiu/UCLAR/60ccstep1/MoinCom/",orase[i],"/"), full.names = T, pattern = ".tif")
  #unique(do.call(rbind, strsplit(files,"/|_|.tif"))) 
  dates <- unique(as.Date(unique(do.call(rbind, strsplit(files,"/|_|.tif"))[,11]),"%Y%m%d"))
  
  ani <- list(a = c('01'), b= c('02'), c = c('03'), d=c('04'), e=c('05'), f=c('06'), g=c('07'), h=c('08'), i=c('09'), j=c('10'), k=c('11'), l=c('12'))
 
  for (j in 1:length(ani)){
    print(ani[[j]])
    files.sub <- files[format(dates, "%m") %in% c(ani[[j]])]
    rss <- raster::stack(files.sub)
    rss[is.na(rss)]<- NA
    #unique(do.call(rbind, strsplit(files,"/|_|.tif"))) 
    dates1 <- as.Date(format(as.Date(do.call(rbind,strsplit(files.sub,"_|/|.tif"))[,11], "%Y%m%d"), "%Y-%m-%d"))
    
    rst.tem <- rts(rss, dates1)
    
    # using dineof interpolation and raster mask
    no_cores <- detectCores()
    clust <- makeCluster(no_cores) 
    base <- 3
    #Note that this line is required so that all cores in cluster have this variable available
    clusterExport(clust, "base")
    
    rasterts_dineof <-parallel::mclapply(clust,rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof"))
    out <- paste0("/home/claudiu/UCLAR/60ccstep2_fill/MoinCom/",orase[i],"/")
    if (!dir.exists(out)) dir.create(out, recursive = T)
    
    write.rts(rasterts_dineof, paste0(out,ani[j]))
    endCluster() 
  }
  
}
