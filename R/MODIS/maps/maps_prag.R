library(ggplot2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggspatial)
library(sf)
library(metR)

orase <- c("Bacau", "Botosani","Iasi","PiatraNeamt")

for( o in 1:length(orase)){
  
  print(orase[o])
  files <- list.files(paste0("grids/MODIS/season/",orase[o]), full.names = T)
  filess <- grep("Day", files, value = T)
  prag <- c("0", "10","70")
  
  for(p in 1:length(prag)){
    
  files.s <- grep(paste0("_",prag[p],".tif"),filess, value = T)
  
  files.prob <- grep(paste0("_",prag[1],".tif"),filess, value = T)
 
  sezon <- c("DJF","MAM","JJA","SON")
  
  for( s in 1:length(sezon)){
    
  files.s1 <- grep(sezon[s], files.s, value = T)
  rr <- terra::rast(files.s1)%>%as.data.frame(xy=T)
  
  files.prob1 <- grep(sezon[s], files.prob, value = T)
  files.prob11 <-  files.prob1[1]
  rr.prob <- terra::rast(files.prob11)%>%as.data.frame(xy=T)
  mn <- median(rr.prob$values.mean)
  
  gg <- ggplot(rr)+
    
    geom_contour_fill(aes(x,y,z= values.mean, fill = stat(level)),breaks = MakeBreaks(.4))+
    #geom_contour2( aes(x,y,z = mean.seas),binwidth = 5,skip = .2)+
    #metR::geom_text_contour(aes(x,y,z = mean.seas),stroke  = 0.1,skip = .1,rotate = T,check_overlap = T,size = 8)+
    geom_sf(data = uat, fill= "transparent",color = "black", size = 0.4)+
    coord_sf(xlim = c(min(rr$x),max(rr$x)), ylim = c(min(rr$y), max(rr$y)), expand = F)+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    scale_alpha(guide = "none") +
    scale_fill_divergent_discretised(high = "#b2182b",mid = "#ffffbf",low = "#225ea8",name = "°C",midpoint = mn)+
    ##scale_fill_distiller(palette="RdYlBu", direction=-1, na.value="transparent", limits=lim.mean, breaks=brks.mean,
    #                   guide=guide_colorsteps(ticks=TRUE, show.limits = TRUE), oob=squish) +
    # scale_fill_stepsn(colours = cols.mean, name = den,
    #                   breaks = brks.mean,
    #                   limits =lim.mean)+
    #scale_fill_divergent_discretised(colours = cols.mean, name = den, breaks = brks.mean,limits =lim.mean )+
    
    guides(fill = guide_colourbar(barwidth = .6, barheight = 22.0, title.position = "right",
                                  label.theme = element_text(size =10))) +
    
    scale_linetype_manual(values=c("twodash")) +xlab("")+ylab("")+theme_bw()+
    theme(legend.position = "right",
          legend.title=element_text(size=12.0),
          text = element_text(size=12.5),
          axis.text.x = element_text(angle = 90),
          strip.background = element_rect(colour = "black", fill = "white"),
          panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
          panel.border = element_rect(colour = "black", fill = "transparent"))

  out <- paste0("png/MODIS/maps/AIC/",orase[o],"/")
  
  if(!dir.exists(out)) dir.create(out)
  
  png(paste0(out,sezon[s],"_day_",prag[p],".png"), height = 1600, width = 1600, res = 220)
  print(gg)
  dev.off()
  system(paste0("convert -trim ", out,sezon[s],"_day_",prag[p],".png"," ",out,sezon[s] ,"_day_",prag[p],".png"))
  }
  } 
}
