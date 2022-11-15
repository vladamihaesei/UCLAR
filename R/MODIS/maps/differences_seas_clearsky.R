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
uat.f <- uat%>%filter(name == "Bacău")

#### pentru legenda
#### culori all seasons 
rmean <- colorRampPalette(rev(c(brewer.pal(11,"RdBu"))), interpolate="linear")
brks.mean <- seq(-4.8,3.8, by = .3)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-5,4.2)
den <- "°C"

for(o in 1:length(orase)){
  
  print(orase[o])
  
  night_day <- c("Day","Night")

  prag <- seq(0,90,10)
  for( p in 1:length(prag)){
    
    df <- NULL
    df1 <- NULL
    tt <- NULL
    tt1 <- NULL
    print(as.character(prag[p]))
  
  for(d in 1: length(night_day)){
    
    print(night_day[d])
    mod <- c("MOD11A1","MYD11A1")
    for(m in 1:length(mod)){
      
      print(mod[m])
      files <- list.files(paste0("grids/MODIS/nc/filled/LST_",night_day[d],"_1km/",orase[o],"/",mod[m],"_",night_day[d],"_2000-2020"),recursive = T, full.names = T, pattern = ".tif")[1]
      tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/",mod[m],"/",night_day[d],"_","cloud_pixels.csv"))
      
      t <- tabs%>%filter(frecventa <= prag[p])
      t1 <- tabs%>%filter(frecventa <= 0) 
      tt <- rbind(tt,t) 
      tt1 <- rbind(tt1,t1)
      
      r <- terra::rast(files)
      r <- r-273.15
      terra::time(r) <- as.Date(names(r),"X%Y.%m.%d")
      
      r.sub <- r[[which(format(terra::time(r),"%Y-%m-%d") %in% c(t$timp))]]
      r.sub1 <- r[[which(format(terra::time(r),"%Y-%m-%d") %in% c(t1$timp))]]
     
      df.r <- as.data.frame(r.sub, xy =T)%>%pivot_longer(-c(x,y), names_to = "ind", values_to = "values")%>%
        mutate(tip = mod[m], per = night_day[d])
      df.r1 <- as.data.frame(r.sub1, xy =T)%>%pivot_longer(-c(x,y), names_to = "ind", values_to = "values")%>%
        mutate(tip = mod[m], per = night_day[d])
      
      df.r$ind <- as.Date(df.r$ind, format = "X%Y.%m.%d")
      df.r1$ind <- as.Date(df.r1$ind, format = "X%Y.%m.%d")
      
      df <- rbind(df,df.r)
      df1 <- rbind(df1,df.r1)
      
    }
    
  }
  
  #### verificare 
  # g1 <- df%>%filter(ind == "2003-08-12"& tip =="MOD11A1",per == "Night"& x ==27.6 & y == 46.2)
  # g2 <- df%>%filter(ind == "2003-08-12"& tip =="MYD11A1", per == "Night"& x ==27.6 & y == 46.2)
  
  # vf.r <- terra::rast("grids/MODIS/tif/nonfilled/MYD11A1/LST_Night_1km/Barlad/2003-08-12.tif")
  # plot(vf.r)
  
  ### calculeaza media ponderata se complica treaba 
  df.myd <- tt%>%filter(frecventa <= prag[p] & tip == "MYD11A1")
  df.mod <- tt%>%filter(frecventa <= prag[p] & tip == "MOD11A1")
  
  df.myd1 <- tt1%>%filter(frecventa <= 0 & tip == "MYD11A1")
  df.mod1 <- tt1%>%filter(frecventa <= 0 & tip == "MOD11A1")

  # df.s1 <- df%>%group_by(x,y,ind,per,tip)%>%summarise(values.mean.myd = ifelse(tip == "MYD11A1",values*((nrow(df.myd)/nrow(tt))),values*((nrow(df.mod)/nrow(tt)))))%>%
  #   group_by(x,y,ind,per)%>%summarise(values.mean = sum(values.mean.myd))
  # 
  #df.pond1 <- df%>%filter(tip == "MYD11A1")%>%group_by(x,y,ind,per,tip)%>%summarise(values.mean.myd = values*((nrow(df.mod)/nrow(tt))))
  #df.pond2 <- df%>%filter(tip == "MOD11A1")%>%group_by(x,y,ind,per,tip)%>%summarise(values.mean.mod = values*((nrow(df.mod)/nrow(tt))))
  
  #df.s2 <- df%>%group_by(x,y,ind,per,tip)%>%summarise(values.mean = mean(values))
  #df.s3 <- df%>%group_by(x,y,ind,per)%>%summarise(values.mean = mean(values))
  
  df.seas <- df%>%mutate(seas = mkseas(ind, width = "DJF"))%>%group_by(x,y,per,seas,tip)%>% summarise(mean.seas = mean(values))
  
  df.seas.clear <- df1%>%mutate(seas = mkseas(ind, width = "DJF"))%>%group_by(x,y,per,seas,tip)%>% summarise(mean.seas = mean(values))
  
  #df.seass <- df.s1%>%mutate(seas = mkseas(ind, width = "DJF"))%>%group_by(x,y,per,seas)%>% summarise(mean.seas = mean(values.mean))
  #df.seass1 <- df.s3%>%mutate(seas = mkseas(ind, width = "DJF"))%>%group_by(x,y,per,seas)%>% summarise(mean.seas = mean(values.mean))
  
  df.seas1 <- df.seas%>%group_by(x,y,per,seas,tip)%>%
    summarise(mean.seas = ifelse(tip == "MYD11A1",mean.seas*((nrow(df.myd)/nrow(tt))),mean.seas*((nrow(df.mod)/nrow(tt)))))%>%
    group_by(x,y,seas,per)%>%summarise(values.mean = sum(mean.seas))
  
  df.seas.clear1 <- df.seas.clear%>%group_by(x,y,per,seas,tip)%>%
    summarise(mean.seas = ifelse(tip == "MYD11A1",mean.seas*((nrow(df.myd1)/nrow(tt1))),mean.seas*((nrow(df.mod1)/nrow(tt1)))))%>%
    group_by(x,y,seas,per)%>%summarise(values.mean.clear = sum(mean.seas))
  
  diff <- df.seas1%>%left_join(df.seas.clear1)%>%mutate(diff= values.mean - values.mean.clear)
  
  gg <- ggplot(diff)+
  
    geom_contour_fill(aes(x,y,z= diff, fill = stat(level)),breaks = MakeBreaks(.3))+
    #geom_contour2( aes(x,y,z = mean.seas),binwidth = 5,skip = .2)+
    #metR::geom_text_contour(aes(x,y,z = mean.seas),stroke  = 0.1,skip = .1,rotate = T,check_overlap = T,size = 8)+
    geom_sf(data = uat, fill= "transparent",color = "black", size = 0.4)+
    coord_sf(xlim = c(min(df.seas$x)-.01,max(df.seas$x)+.01), ylim = c(min(df.seas$y)-.01, max(df.seas$y)+.01), expand = F)+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    scale_alpha(guide = "none") +
    scale_fill_divergent_discretised(high = "#67001f",mid = "#f7f7f7",low = "#2166ac",name = "°C",midpoint = 0)+
    ##scale_fill_distiller(palette="RdYlBu", direction=-1, na.value="transparent", limits=lim.mean, breaks=brks.mean, 
    #                   guide=guide_colorsteps(ticks=TRUE, show.limits = TRUE), oob=squish) +
    # scale_fill_stepsn(colours = cols.mean, name = den,
     #                  breaks = brks.mean,
      #                 limits =lim.mean)+
    #scale_fill_divergent_discretised(colours = cols.mean, name = den, breaks = brks.mean,limits =lim.mean )+
    
    guides(fill = guide_colourbar(barwidth = 45.0, barheight = 0.8, title.position = "right",
                                  label.theme = element_text(size =12))) +
    scale_linetype_manual(values=c("twodash")) +xlab("")+ylab("")+theme_bw()+
    theme(legend.position = "bottom",
          legend.title=element_text(size=12.0),
          text = element_text(size=12.5),
          axis.text.x = element_text(angle = 90),
          strip.background = element_rect(colour = "black", fill = "white"),
          panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
          panel.border = element_rect(colour = "black", fill = "transparent"))+facet_grid(per~seas)
  
  png(paste0("png/MODIS/maps/diff/all_seasons_diff_",prag[p],"_",orase[o],".png"), height = 1600, width = 2600, res = 220)
  print(gg)
  dev.off()
  system(paste0("convert -trim ", "png/MODIS/maps/diff/all_seasons_diff_",prag[p],"_",orase[o],".png"," png/MODIS/maps/diff/all_seasons_diff_",prag[p],"_",orase[o],".png"))
  
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
  #}
  
  }
  
}


