library(tidyr)
library(RColorBrewer)
library(ggpointdensity)
library(Metrics)
library(terra)
library(sinkr)
library(rtsa)
library(dplyr)
library(tidyr)
orase <- c("Bacau","Botosani","PiatraNeamt") #

df <- NULL
for (i in 1:length(orase)){
  df1 <- NULL
  print(orase[i])
  
  types <- c("MYD11A1")

  for(j in 1:length(types)){
    
    df2 <- NULL
    dn <- c("Day","Night")
    for (d in 1:length(dn)){
      
      t.m <- read.csv(paste0("tabs/MODIS/",orase[i],"/",types[j],"/",dn[d],"_nocloud_days_winter.csv"))
      zile <- c(t.m$timp)
      
      rmis <- list.files(path = paste0("grids/MODIS/tif/CopyOfnonfilled/",types[j],"/LST_",dn[d],"_1km/",orase[i]), pattern=paste0(zile, collapse="|"), full.names = T)
      rmis <- terra::rast(grep(pattern="tif.aux.json",rmis, invert = T, value =T)) -273.15
      
      rorg <- list.files(path = paste0("grids/MODIS/tif/nonfilled/",types[j],"/LST_",dn[d],"_1km/",orase[i]), pattern=paste0(zile, collapse="|"), full.names = T)
      rorg <- terra::rast(grep(pattern="tif.aux.json",rorg, invert = T, value =T)) -273.15
      
      rorg[!is.na(rmis)] <- NA
    
     # rfill <- rts(paste0("grids/MODIS/nc/filled_validation/LST_",dn[d],"_1km/",orase[i],"/",types[j],"_",dn[d],"_2018-2021"))
      rfill <-  terra::rast(paste0("grids/MODIS/nc/filled_validation/LST_",dn[d],"_1km/",orase[i],"/",types[j],"_",dn[d],"_2019-2021/",types[j],"_",dn[d],"_2019-2021.tif"))
      
      dats <- seq(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "days")  
      #dats <- dats[!dats=="2020-02-29"]
      terra::time(rfill) <- dats
      rfill.sel <- rfill[[which(time(rfill) %in%c(as.Date(zile)))]]-273.15
      rfill.sel[!is.na(rmis)] <- NA
      
      t1 <- as.data.frame(rorg,xy =T)%>%pivot_longer(-c(x,y), names_to = "ind_org", values_to = "org")%>%na.omit()%>%dplyr::select(org)
      t2 <- as.data.frame(rfill.sel, xy = T)%>%pivot_longer(-c(x,y), names_to = "ind_fill", values_to = "fill")%>%na.omit()%>%dplyr::select(fill)
      tt <- cbind(t1,t2[1:length(nrow(t1)),])
      #tt <- left_join(t1,t2)
      
      ff <- tt%>%dplyr::mutate(Oras = orase[i], Tip = types[j], Dn = dn[d])
      
      lm_eqn <- function(tt){
        m <- lm(y ~ x, tt);
        m <- cor(tt$x,tt$y);
        eq <- substitute(italic(r)^2~"="~r2, 
                         list(#a = format(unname(coef(m)[1]), digits = 2),
                           #b = format(unname(coef(m)[2]), digits = 2),
                           r2 = format(m, digits = 2)))
        as.character(as.expression(eq));
      }
     df2 <- rbind(df2,ff)
    }
    df1 <- rbind(df2,df1)
  }
  df <- rbind(df1,df)
}

statt <- df%>%group_by(Oras,Dn)%>%summarise(RMSE = rmse(org,fill), R= cor(org,fill), BIAS= bias(org,fill))
statt
dns <- ggplot(tt, aes(x=x, y=y)) + geom_smooth(method = "lm", se = FALSE)+geom_pointdensity(show.legend = F) + scale_color_viridis_c(n)+
  
  geom_text(x = 6.5, y = 10, label = lm_eqn(tt), parse = TRUE)+theme_bw()+xlab("Originale (°C)")+ylab("Reconstruite (°C)")+theme(text = element_text(size=14.),
                                                                                                                                 axis.text = element_text(size = 15.5))

png(paste0("png/MODIS/AIC_article/scatterplot_validation",".png"), height = 800, width = 1000, res = 230)
dns
dev.off()
system(paste0("convert -trim ", "png/MODIS/AIC_article/scatterplot_validation",".png"," png/MODIS/AIC_article/scatterplot_validation",".png"))

system(paste0("convert -trim ", "png/MODIS/AIC_article/dineof_scatterplot_validation",".png"," png/MODIS/AIC_article/dineof_scatterplot_validation",".png"))


