library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(seas)
library(RColorBrewer)
library(ggspatial)
library(sf)
library(metR)
library(tidyr)
library(xlsx)
library(openxlsx)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

df1 <- NULL
df21 <- NULL
for(o in 1:length(orase)){
  
  print(orase[o])
  night_day <- c("Day","Night")
  prag <- 50
  prag1 <- 100
  for(p in 1:length(prag)){
    
    print(prag[p])
    tt <- NULL
    tt1 <- NULL
    
    for(d in 1: length(night_day)){
      
      print(night_day[d])
      mod <- c("MOD11A1","MYD11A1")
      for(m in 1:length(mod)){
        print(mod[m])
        
        files <- list.files(paste0("grids/MODIS/nc/filled/LST_",night_day[d],"_1km/",orase[o],"/",mod[m],"_",night_day[d],"_2000-2020"),recursive = T, full.names = T, pattern = ".tif")[1]
        
        tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/",mod[m],"/",night_day[d],"_","cloud_pixels.csv"))
        
        t <- tabs%>%filter(frecventa <= prag[p])
        tt <- rbind(tt,t) 
      
        
        t1 <- tabs%>%filter(frecventa <= prag1[p])
        tt1 <- rbind(tt1,t1) 
        
      }
      
    }
    
  }
  
   df <- tt%>% count(tip,Day_night, format(as.Date(timp),"%m"))%>%pivot_wider(names_from = c("tip","Day_night"),values_from = n)
   df$orase <- orase[o]
   df1 <- rbind(df,df1)
   #names(df1)[1] <- "Luni"
   
   df2 <- tt1%>% count(tip,Day_night, format(as.Date(timp),"%m"))%>%pivot_wider(names_from = c("tip","Day_night"),values_from = n)
   df2$orase <- orase[o]
   df21 <- rbind(df2,df21)
   # names(df21)[1] <- "Luni"
   # 
}

names(df1) <- c("Luni","MOD11A1_Day_CC_50%", "MOD11A1_Night_CC_50%", "MYD11A1_Day_CC_50%", "MYD11A1_Night_CC_50%","orase")
names(df21)<- c("Luni","MOD11A1_Day_CC_all", "MOD11A1_Night_CC_all", "MYD11A1_Day_CC_all", "MYD11A1_Night_CC_all","orase")

dataset_names <- list('CC_50' = df1, 'CC_all' = df21)
openxlsx::write.xlsx(dataset_names, file = 'tabs/MODIS/count_images.xlsx')


write.xlsx(df1,"tabs/MODIS/count_images_all1.xlsx",sheetName = "50_CC")
write.xlsx(df1,"tabs/MODIS/count_images_all1.xlsx",sheetName = "100_CC")
