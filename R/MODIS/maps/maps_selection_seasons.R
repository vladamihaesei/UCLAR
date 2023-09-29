library(ggplot2)
library(dplyr)
library(tidyr)
library(seas)
library(RColorBrewer)
library(ggspatial)
library(sf)
library(metR)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")
uat <- readRDS("shp/uat_ro.rds")%>%filter(county %in% c("Vaslui","Iași","Neamț","Botoșani","Suceava","Bacău"))
uat.f <- uat%>%filter(name == "Pașcani")

#### pentru legenda
#### culori all seasons 
rmean <- colorRampPalette(rev(c(brewer.pal(9,"RdYlBu"))), interpolate="linear")
brks.mean <- seq(-4,28.8, by = .2)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-4.5,28.5)
den <- "°C"

for(o in 1:length(orase)){
  
  print(orase[o])
  night_day <- c("Day","Night")
  prag <- 50
  
  for(p in 1:length(prag)){
    
    print(prag[p])
    df <- NULL
    tt <- NULL
 
  for(d in 1: length(night_day)){
    
    print(night_day[d])
    mod <- c("MOD11A1","MYD11A1")
    for(m in 1:length(mod)){
      print(mod[m])
      
      files <- list.files(paste0("grids/MODIS/nc/filled/LST_",night_day[d],"_1km/",orase[o],"/",mod[m],"_",night_day[d],"_2000-2020"),recursive = T, full.names = T, pattern = ".tif")[1]
      
      tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/",mod[m],"/",night_day[d],"_","cloud_pixels.csv"))
      
      t <- tabs%>%filter(frecventa <= prag[p])
      tt <- rbind(tt,t) 
      
      r <- terra::rast(files)
      r <- r-273.15
      terra::time(r) <- as.Date(names(r),"X%Y.%m.%d")
      r.sub <- r[[which(format(terra::time(r),"%Y-%m-%d") %in% c(t$timp))]]
      df.r <- as.data.frame(r.sub, xy =T)%>%pivot_longer(-c(x,y), names_to = "ind", values_to = "values")%>%
        mutate(tip = mod[m], per = night_day[d])
      df.r$ind <- as.Date(df.r$ind, format = "X%Y.%m.%d")
      df <- rbind(df,df.r)
      
    }

  }

  ### calculeaza media ponderata se complica treaba 
  df.myd <- tt%>%filter(frecventa <= prag[p] & tip == "MYD11A1")
  df.mod <- tt%>%filter(frecventa <= prag[p] & tip == "MOD11A1")
  
  df.seas <- df%>%mutate(seas = mkseas(ind, width = "DJF"))%>%group_by(x,y,per,seas,tip)%>% summarise(mean.seas = mean(values))
  
  df.seas1 <- df.seas%>%group_by(x,y,per,seas,tip)%>%
    summarise(mean.seas = ifelse(tip == "MYD11A1",mean.seas*((nrow(df.myd)/nrow(tt))),mean.seas*((nrow(df.mod)/nrow(tt)))))%>%
    group_by(x,y,seas,per)%>%summarise(values.mean = sum(mean.seas))

  gg <- ggplot(df.seas1)+
    
    geom_contour_fill(aes(x,y,z= values.mean, fill = stat(level)),breaks = MakeBreaks(1))+
    #geom_contour2( aes(x,y,z = mean.seas),binwidth = 5,skip = .2)+
    #metR::geom_text_contour(aes(x,y,z = mean.seas),stroke  = 0.1,skip = .1,rotate = T,check_overlap = T,size = 8)+
    geom_sf(data = uat, fill= "transparent",color = "black", size = 0.4)+
    coord_sf(xlim = c(min(df.seas$x),max(df.seas$x)), ylim = c(min(df.seas$y), max(df.seas$y)), expand = F)+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    scale_alpha(guide = "none") +
    scale_fill_divergent_discretised(high = "#b2182b",mid = "#ffffbf",low = "#225ea8",name = "°C",midpoint = 5)+
    ##scale_fill_distiller(palette="RdYlBu", direction=-1, na.value="transparent", limits=lim.mean, breaks=brks.mean, 
      #                   guide=guide_colorsteps(ticks=TRUE, show.limits = TRUE), oob=squish) +
    # scale_fill_stepsn(colours = cols.mean, name = den,
    #                   breaks = brks.mean,
    #                   limits =lim.mean)+
    #scale_fill_divergent_discretised(colours = cols.mean, name = den, breaks = brks.mean,limits =lim.mean )+
    
    guides(fill = guide_colourbar(barwidth = 47.0, barheight = 0.8, title.position = "right",
                                  label.theme = element_text(size =12))) +
    
    scale_linetype_manual(values=c("twodash")) +xlab("")+ylab("")+theme_bw()+
    theme(legend.position = "bottom",
          legend.title=element_text(size=12.0),
          text = element_text(size=12.5),
          axis.text.x = element_text(angle = 90),
          strip.background = element_rect(colour = "black", fill = "white"),
          panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
          panel.border = element_rect(colour = "black", fill = "transparent"))+facet_grid(per~seas)
  
  png(paste0("png/MODIS/maps/praguri/all_seasons_mean_pond_CC_",prag[p],"_",orase[o],".png"), height = 1600, width = 2600, res = 220)
  print(gg)
  dev.off()
  system(paste0("convert -trim ", "png/MODIS/maps/praguri/all_seasons_mean_pond_CC_",prag[p],"_",orase[o],".png"," png/MODIS/maps/praguri/all_seasons_mean_pond_CC_",prag[p],"_",orase[o],".png"))
  
  # ####each  season 
  # sez = c("DJF","MAM","JJA","SON")
  # 
  # for(s in 1:length(sez)){
  #   
  #   print(sez[s])
  #   test <- df.seas%>%filter(seas==sez[s])
  #   
  #   gg1 <- ggplot(test)+
  #     
  #     geom_contour_fill(aes(x,y,z= mean.seas, fill = stat(level)),breaks = MakeBreaks(1))+
  #     #geom_contour2( aes(x,y,z = mean.seas),binwidth = 5,skip = .2)+
  #     #metR::geom_text_contour(aes(x,y,z = mean.seas),stroke  = 0.1,skip = .1,rotate = T,check_overlap = T,size = 8)+
  #     geom_sf(data = uat, fill= "transparent",color = "black", size = 0.4)+
  #     coord_sf(xlim = c(min(df.seas$x)-.02,max(df.seas$x)+.02), ylim = c(min(df.seas$y)-.02, max(df.seas$y)+.02), expand = F)+
  #     scale_x_discrete(expand = c(0, 0))+
  #     scale_y_discrete(expand = c(0, 0))+
  #     scale_alpha(guide = "none") +
  #     scale_fill_divergent_discretised(high = "#b2182b",mid = "#ffffbf",low = "#225ea8",name = "°C",midpoint = 14)+
  #     #scale_fill_distiller(palette="RdYlBu", direction=-1, na.value="transparent", limits=lim.mean, breaks=brks.mean, 
  #     #                   guide=guide_colorsteps(ticks=TRUE, show.limits = TRUE), oob=squish) +
  #     # scale_fill_stepsn(colours = cols.mean, name = den,
  #     #                   breaks = brks.mean,
  #     #                   limits =lim.mean)+
  #     #scale_fill_divergent_discretised(colours = cols.mean, name = den, breaks = brks.mean,limits =lim.mean )+
  #     guides(fill = guide_colourbar(barwidth =40.0, barheight = 0.8, title.position = "right",
  #                                   label.theme = element_text(size =12)))+
  #     scale_linetype_manual(values=c("twodash")) +xlab("")+ylab("")+theme_bw()+
  #     theme(legend.position = "bottom",
  #           legend.title=element_text(size=12.0),
  #           text = element_text(size=12.5),
  #           axis.text.x = element_text(angle = 90),
  #           strip.background = element_rect(colour = "black", fill = "white"),
  #           panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
  #           panel.border = element_rect(colour = "black", fill = "transparent"))+facet_wrap(~per)
  #   png(paste0("png/MODIS/maps/season_",sez[s],"_",orase[o],".png"), height = 1800, width = 2200, res = 220 )
  #   print(gg1)
  #   dev.off()
  #   system(paste0("convert -trim ", "png/MODIS/maps/season_",sez[s],"_",orase[o],".png"," png/MODIS/maps/season_",sez[s],"_",orase[o],".png"))
  #   
  # }
  
  }
  
}


