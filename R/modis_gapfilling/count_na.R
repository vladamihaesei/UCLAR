library(dplyr)
library(terra)
library(ggplot2)
### rasterele de analizat 

all <- list.files(path = "tmp/", pattern = "*.tif", recursive = T, full.names = T )

nume <-  unique(do.call(rbind,strsplit(all,"/"))[,3])
dn <- c("Day","Night")
for(n in 1:length(nume)){
  
  for(d in 1:length(dn)){
    
  
    files <- list.files(path = paste0("tmp/",nume[n],"/Surf_Temp_Daily_1Km_v6/LST_",dn[d],"_1km"), pattern = "*.tif", recursive = T, full.names = T )
    files.sub <- grep("MOD11A1",files, value =T)## Aqua
    files.sub1 <- grep("MYD11A1",files, value =T)##Terra 
    
    dats <- as.Date(paste0(do.call(rbind,strsplit(files.sub,"/|_"))[,15],"-",do.call(rbind,strsplit(files.sub,"/|_|.tif"))[,16]), format("%Y-%j"))
    dats1 <- as.Date(paste0(do.call(rbind,strsplit(files.sub1,"/|_"))[,15],"-",do.call(rbind,strsplit(files.sub1,"/|_|.tif"))[,16]), format("%Y-%j"))
    
    tab <- NULL
    
    for(i in 1:length(files.sub)){
      
      print(dats[i])
      rss <- terra::rast(files.sub[i])
      frec <- (sum(is.na(values(rss)))*100)/length(values(rss))
      dat <- dats[i]
      t <- data.frame(timp = dat, frecventa = frec, tip = "MOD11A1")
      tab <- rbind(tab,t)
      
    }
    
    
  }

  
  
}



tab1 <- NULL

for(i in 1:length(files.sub1)){
  
  print(dats1[i])
  rss <- terra::rast(files.sub1[i])
  frec <- (sum(is.na(values(rss)))*100)/length(values(rss))
  dat <- dats[i]
  t <- data.frame(timp = dat, frecventa = frec,tip = "MYD11A1")
  tab1 <- rbind(tab1,t)
  
}

tot <- rbind(tab,tab1)

tot <- tot%>%mutate(year  = format(timp,"%Y"),
                    month = format(timp,"%m"))
write.csv(tot,"tabs/count_na_Botosani.csv", row.names =F)

tot.f <- tot%>%filter(!frecventa %in%c(0,100))

g  <- ggplot(tot.f, aes(x= frecventa))+
  geom_histogram(color="darkblue", fill="lightblue")+facet_wrap(~tip)+theme_bw()+
  scale_x_continuous(expand = c(0, 0))+scale_y_continuous(expand = c(0, 0))
g
png("png/histrogam_countNA_Baotosani.png", width = 1500, height = 1300, res = 200)
g
dev.off()

g1 <- ggplot(tot.f, aes(x= frecventa))+
  geom_histogram(color="darkblue", fill="lightblue")+facet_grid(month~tip)+theme_bw()+
scale_x_continuous(expand = c(0, 0))+scale_y_continuous(expand = c(0, 0))

png("png/histrogam_monthly_countNA_Barlad.png", width = 1800, height = 1400, res = 230)
g1
dev.off()
