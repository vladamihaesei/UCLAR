library(terra)
library(dplyr)
library(ggplot2)
library(tmap)
library(sf)
library(tidyr)
library(RColorBrewer)
library(ggpointdensity)
library(Metrics)

shp <- sf::read_sf("Limite/Utilizare.shp")
rmis <- terra::rast("grids/LANDSAT/days_gaps/Iasi/winter/LT05_182027_20100125_algn.tif") -273.15
rorg <- terra::rast("grids/LANDSAT/data_2/Iasi/winter/LT05_182027_20100125_algn.tif") -273.15
rorg1 <- list.files("grids/LANDSAT/data_2/Iasi/winter/",pattern = ".tif")

#rorg[!is.na(rmis)] <- NA
rfill <- terra::rast("grids/LANDSAT/filled_validation/Iasi/winter/winter.tif")
dats <- as.Date(do.call(rbind,strsplit(rorg1,"_"))[,3],"%Y%m%d")
terra::time(rfill) <- dats

rfill.sel <- rfill[[which(time(rfill) == "2010-01-25")]]-273.15
#rfill.sel[!is.na(rmis)] <- NA

r.pl <- c(rmis,rorg,rfill.sel)

newproj1 <- "+proj=longlat +datum=WGS84 +no_defs"

r.pl <- terra::project(r.pl, newproj1)

names(r.pl) <- c("Nuages Artificiels","Originale","Remplis")

df <- r.pl%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y), names_to = "ind")

#gg <- ggplot()+
#    geom_raster(data= df, aes(x = x, y = y, fill = value), interpolate = T)+
#   facet_wrap(~factor(ind, levels = c("Originale","Remplis","Nuages Artificiels")), nrow = 2)

##### legenda culori 
rmean <- colorRampPalette(rev(c(brewer.pal(9,"RdYlBu"))), interpolate="linear")
brks.mean <- seq(-23.5,-8., by = 1.5)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(-25.0,-6.5)
den <- "°C"

gg <- ggplot(df)+
  geom_raster(data= df, aes(x = x, y = y, fill = value), interpolate = F)+
  #geom_contour_fill(aes(x,y,z= value, fill = stat(level)),breaks = MakeBreaks(1))+
  #geom_contour2( aes(x,y,z = mean.seas),binwidth = 5,skip = .2)+
  #metR::geom_text_contour(aes(x,y,z = mean.seas),stroke  = 0.1,skip = .1,rotate = T,check_overlap = T,size = 8)+
  geom_sf(data = shp, fill= "transparent",color = "black", size = 0.65)+
  coord_sf(xlim = c(min(df$x)-.01,max(df$x)+.01), ylim = c(min(df$y)-.01, max(df$y)+.01), expand = F)+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  scale_alpha(guide = "none") +
  #scale_fill_divergent_discretised(high = "#b2182b",mid = "#ffffbf",low = "#225ea8",name = "°C",midpoint = 5)+
  ##scale_fill_distiller(palette="RdYlBu", direction=-1, na.value="transparent", limits=lim.mean, breaks=brks.mean, 
  #                   guide=guide_colorsteps(ticks=TRUE, show.limits = TRUE), oob=squish) +
  scale_fill_stepsn(colours = cols.mean, name = den, na.value="white",
                    breaks = brks.mean,
                    limits =lim.mean)+
  #scale_fill_divergent_discretised(colours = cols.mean, name = den, breaks = brks.mean,limits =lim.mean )+
  
  guides(fill = guide_colourbar(barwidth = 25.0, barheight = 0.8, title.position = "right",
                                label.theme = element_text(size =12))) +
  scale_linetype_manual(values=c("twodash")) +xlab("")+ylab("")+theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12.0),
        text = element_text(size=12.5),
        axis.text.x = element_text(angle = 90),
        strip.background = element_rect(colour = "black", fill = "white"),
        panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"))+facet_wrap(~factor(ind, levels = c("Originale","Remplis","Nuages Artificiels")), nrow = 2)

png(paste0("png/LANDSAT/AIC_article/dineof_validation_landsat",".png"), height = 1600, width = 2600, res = 220)
gg
dev.off()
system(paste0("convert -trim ", "png/LANDSAT/AIC_article/dineof_validation_landsat",".png"," png/LANDSAT/AIC_article/dineof_validation_landsat",".png"))





