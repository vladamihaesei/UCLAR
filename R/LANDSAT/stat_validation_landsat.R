library(tidyr)
library(RColorBrewer)
library(ggpointdensity)
library(Metrics)
library(terra)
library(sinkr)
library(rtsa)
library(dplyr)
library(tidyr)
library(ggplot2)
orase <- c("Iasi","Bacau","Botosani","PiatraNeamt") #

df <- NULL
for (i in 1:length(orase)){
  
  df1 <- NULL
  print(orase[i])
  types <- c("summer", "winter")
  for(j in 1:length(types)){
    
    print(types[j])
    fs <- list.files(paste0("grids/LANDSAT/days_gaps/",orase[i],"/",types[j]), full.names = T)[1]
    rmis <- terra::rast(grep(".tif.aux.json", value =T, invert= T,list.files(paste0("grids/LANDSAT/days_gaps/",orase[i],"/",types[j]), full.names = T))[1]) -273.15
    rmis
    zile <- do.call(rbind,strsplit(fs,"/"))[,6]
    zile1 <- as.Date(do.call(rbind,strsplit(fs,"/|_"))[,9],"%Y%m%d")
    rorg <- terra::rast(list.files(path = paste0("grids/LANDSAT/data_2/",orase[i],"/",types[j]), pattern=paste0(zile, collapse="|"), full.names = T)[1])-273.15
    rorg1 <- list.files(path = paste0("grids/LANDSAT/data_2/",orase[i],"/",types[j]), full.names = T)
    #rorg1 <- rorg1[c(1:40,101)]
    #rorg1 <- terra::rast(rorg1)
    rorg[!is.na(rmis)] <- NA
    
    #rfill <- rts(paste0("grids/LANDSAT/filled_validation/",orase[i],"/",types[j],"/"))
    rfill <-  terra::rast(paste0("grids/LANDSAT/filled_validation/",orase[i],"/",types[j],"/",types[j],".tif"))- 273.15
    
    dats <- as.Date(do.call(rbind,strsplit(rorg1,"/|_"))[,9],"%Y%m%d")
    
    #dats <- dats[!dats=="2020-02-29"]
    terra::time(rfill) <- dats
    rfill.sel <- rfill[[which(time(rfill) %in%c(as.Date(zile1)))]]
    rfill.sel[!is.na(rmis)] <- NA
    
    t1 <- as.data.frame(rorg,xy =T)%>%pivot_longer(-c(x,y), names_to = "ind_org", values_to = "org")%>%na.omit()%>%dplyr::select(x,y,org)
    t2 <- as.data.frame(rfill.sel, xy = T)%>%pivot_longer(-c(x,y), names_to = "ind_fill", values_to = "fill")%>%na.omit()%>%dplyr::select(x,y,fill)
    #tt <- cbind(t1,t2)
    tt <- left_join(t1,t2)
    
    ff <- tt%>%dplyr::mutate(Oras = orase[i], Tip = types[j])
    # lm_eqn <- function(tt){
    #   m <- lm(y ~ x, tt);
    #   m <- cor(tt$x,tt$y);
    #   eq <- substitute(italic(r)^2~"="~r2, 
    #                    list(#a = format(unname(coef(m)[1]), digits = 2),
    #                      #b = format(unname(coef(m)[2]), digits = 2),
    #                      r2 = format(m, digits = 2)))
    #   as.character(as.expression(eq));
    # }
    df1 <- rbind(ff,df1)
    
  }
  
  df <- rbind(df1,df)
}

statt <- df%>%na.omit()%>%group_by(factor(Oras, levels = orase <- c("Iasi","Bacau","PiatraNeamt","Botosani")),Tip)%>%summarise(RMSE = rmse(org,fill), R= cor(org,fill), BIAS= bias(org,fill))
statt

lm_eqn <- function(tt){
  m <- lm(fill ~ org, tt);
  m <- cor(tt$fill,tt$org);
  eq <- substitute(italic(r)^2~"="~r2, 
                   list(#a = format(unname(coef(m)[1]), digits = 2),
                     #b = format(unname(coef(m)[2]), digits = 2),
                     r2 = format(m, digits = 2)))
  as.character(as.expression(eq));
}

dns <- ggplot(tt, aes(x=org, y=fill)) + geom_smooth(method = "lm", se = FALSE)+geom_pointdensity(show.legend = F) + scale_color_viridis_c(n)+
  
 theme_bw()+xlab("Originale (°C)")+ylab("Reconstruite (°C)")+theme(text = element_text(size=14.),axis.text = element_text(size = 15.5))
#geom_text(x = -22.5, y = -9, label = lm_eqn(tt), parse = TRUE)
png(paste0("png/LANDSAT/AIC_article/scatterplot_validation_landsat",".png"), height = 1000, width = 1200, res = 210)
dns
dev.off()
system(paste0("convert -trim ", "png/LANDSAT/AIC_article/scatterplot_validation_landsat",".png"," png/LANDSAT/AIC_article/scatterplot_validation_landsat",".png"))

max(tt$fill)

