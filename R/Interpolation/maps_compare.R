library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(dplyr)
library(tidyr)

### stations 
st <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx", sheet = 2)
st <- st%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
  mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)%>%na.omit()
st.jan <- st%>%filter(Month == 1)%>%st_as_sf(coords = c("Longitudine","Latitudine"), crs = 4326)
st.jul <- st%>%filter(Month == 7)%>%st_as_sf(coords = c("Longitudine","Latitudine"), crs = 4326)

#### RK 
rk <- rast("grids/test_interpolare_RK_tmean_anual_2018.nc")
names(rk) <- as.Date(time(rk))
rk.jan <- rk[[which(month(time(rk)) == "1")]]%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")
rk.jul <- rk[[which(month(time(rk)) == "7")]]%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")

### anomalies
anom <- rast("grids/test_interpolare_anom_MODIS_tmean_anual_2018.nc")
terra::time(anom) <- terra::time(rk)

names(anom) <- as.Date(time(anom))

anom.jan <- anom[[which(month(time(anom)) == "1")]]%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")
anom.jul <- anom[[which(month(time(anom)) == "7")]]%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")


###colors for the legend 
rmean <- colorRampPalette(c(rev(brewer.pal(9,"YlGnBu")), brewer.pal(9,"YlOrRd")), interpolate="linear")
brks.mean <- seq(14.5,26.5, by = .5)
cols.mean <- rmean(length(brks.mean) - 1)
lim.mean <- c(14,27)
den <- "°C"

rmean1 <- colorRampPalette(c(rev(brewer.pal(9,"YlGnBu")), brewer.pal(3,"YlOrRd")), interpolate="linear")
brks.mean1 <- seq(-11,10, by = .5)
cols.mean1 <- rmean1(length(brks.mean1) - 1)
lim.mean1 <- c(-11.5,10.5)

gg <- ggplot()+
  
      geom_raster(data = rk.jan, mapping = aes(x,y,fill = value))+
      # scale_fill_stepsn(colours = cols.mean, name = den,
      #                breaks = brks.mean,
      #                limits =lim.mean) +
      scale_fill_stepsn(colours = cols.mean1, name = den,
                    breaks = brks.mean1,
                    limits =lim.mean1)+
     geom_sf(st.jan, mapping = aes(geometry= geometry), col = "black", size = 1.4)+
     #geom_sf_text(st.jan,mapping = aes(geometry = geometry, label = Statie))+
     #geom_sf_label(st.jan, mapping = aes(geometry = geometry, label = round(Temp_mean),1), nudge_y = -1)+
     coord_sf(xlim = c(27.44167,27.84167), ylim = c(47.05833,47.25833))+
     guides(fill = guide_colourbar(barwidth = .8, barheight = 42.8, title.position = "right",
                                label.theme = element_text(size = 7.5)),na.value = "white") +
     
  theme(legend.position = "right",
        strip.background = element_rect(colour = "black", fill = "white"),
        panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"),
        legend.background = element_rect(fill = "white"),
        #axis.text.x = element_text(angle = 90),
        legend.title=element_text(size=14.5),
        legend.text=element_text(size=17.5),text = element_text(size=10.5))+
     facet_wrap(~ind)

png("png/test_interpolare/maps_interpolation_rk_jan.png", width = 2700, height = 2700, res = 220)
gg
dev.off()
system(paste0("convert -trim ","png/test_interpolare/maps_interpolation_rk_jan.png"," png/test_interpolare/maps_interpolation_rk_jan.png"))

####### monthly multianual 

rk.mean <- tapp(rk, index = "months", fun = mean)
names(rk.mean) <- month.abb
rk.mean <- rk.mean%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")

anom.mean <- tapp(anom, index = "months", fun = mean)#%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")
names(anom.mean) <- month.abb
anom.mean <- anom.mean%>%as.data.frame(xy=T)%>%pivot_longer(-c(x,y),names_to = "ind")

rmean10 <- colorRampPalette(c(rev(brewer.pal(9,"YlGnBu")), brewer.pal(9,"YlOrRd")), interpolate="linear")
brks.mean10 <- seq(-1,24, by = .5)
cols.mean10 <- rmean1(length(brks.mean10) - 1)
lim.mean10 <- c(-1.5,24.5)

gg <- ggplot()+
  
  geom_raster(data = rk.mean, mapping = aes(x,y,fill = value))+
  scale_fill_stepsn(colours = cols.mean10, name = den,
                    breaks = brks.mean10,
                    limits =lim.mean10) +
  # scale_fill_stepsn(colours = cols.mean1, name = den,
  #               breaks = brks.mean1,
  #               limits =lim.mean1) +
  
  geom_sf(st.jan, mapping = aes(geometry= geometry), col = "black", size = 1.4)+
  #geom_sf_text(st.jan,mapping = aes(geometry = geometry, label = Statie))+
  #geom_sf_label(st.jan, mapping = aes(geometry = geometry, label = round(Temp_mean),1), nudge_y = -1)+
  coord_sf(xlim = c(27.44167,27.84167), ylim = c(47.05833,47.25833))+
  guides(fill = guide_colourbar(barwidth = .8, barheight = 42.8, title.position = "right",
                                label.theme = element_text(size = 7.5)),na.value = "white") +
  xlab("")+ylab("")+
  theme(legend.position = "right",
        strip.background = element_rect(colour = "black", fill = "white"),
        panel.background = element_rect(fill = "#E4E5E9"),#EAF7FA
        panel.border = element_rect(colour = "black", fill = "transparent"),
        legend.background = element_rect(fill = "white"),
        #axis.text.x = element_text(angle = 90),
        legend.title=element_text(size=14.5),
        legend.text=element_text(size=17.5),text = element_text(size=10.5))+
  facet_wrap(~factor(ind,levels = month.abb))

gg

