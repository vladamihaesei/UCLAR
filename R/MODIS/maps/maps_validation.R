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
rmis <- terra::rast("grids/MODIS/tif/CopyOfnonfilled/MYD11A1/LST_Day_1km/Iasi/2020-01-24.tif") -273.15
rorg <- terra::rast("grids/MODIS/tif/filled/MYD11A1/LST_Day_1km/Iasi/2020-01-24.tif") -273.15
#rorg[!is.na(rmis)] <- NA
rfill <- terra::rast("grids/MODIS/nc/filled_validation/LST_Day_1km/Iasi/MYD11A1_Day_2020-2021/MYD11A1_Day_2020-2021.tif")
dats <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "days")
terra::time(rfill) <- dats

rfill.sel <- rfill[[which(time(rfill) == "2020-01-24")]]-273.15
#rfill.sel[!is.na(rmis)] <- NA

r.pl <- c(rmis,rorg,rfill.sel)
names(r.pl) <- c("Nuages Artificiels","Originale","Remplis")

df <- r.pl%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y), names_to = "ind")

#gg <- ggplot()+
 #    geom_raster(data= df, aes(x = x, y = y, fill = value), interpolate = T)+
  #   facet_wrap(~factor(ind, levels = c("Originale","Remplis","Nuages Artificiels")), nrow = 2)

##### legenda culori 
rmean <- colorRampPalette(rev(c(brewer.pal(9,"RdYlBu"))), interpolate="linear")
brks.mean <- seq(4,10., by = 1.2)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(2.8,11.2)
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

png(paste0("png/MODIS/AIC_article/dineof_validation",".png"), height = 1600, width = 2600, res = 220)
gg
dev.off()
system(paste0("convert -trim ", "png/MODIS/AIC_article/dineof_validation",".png"," png/MODIS/AIC_article/dineof_validation",".png"))

#plot(rmis, breaks = seq(2,15,1))
#plot(rorg,breaks = seq(2,15,1))
#terra::plot(rfill.sel, breaks = seq(2,15,1))

dn <- c("Day","Night")
