library(dplyr)
library(tidyr)
library(terra)
library(lubridate)
library(sf)
library(ggplot2)
#### modis 
# mod.day <- rast("grids/interpolation_tests/MOD_Day_201801.nc")-273.15
# mod.night <-rast("grids/interpolation_tests/MOD_Night_201801.nc")-273.15
# myd.day <- rast("grids/interpolation_tests/MYD_Day_201801.nc")-273.15
# myd.night <- rast("grids/interpolation_tests/MYD_Night_201801.nc")-273.15

#### RK 
rk <- rast("grids/test_interpolare_RK_tmean_anual_2018.nc")
### anomalies
anom <- rast("grids/test_interpolare_anom_MODIS_tmean_anual_2018.nc")

### valoarea la statie 
coord <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
coord.sf <- st_as_sf(coord, coords =  c("Longitudine","Latitudine"), crs = 4326)

gg <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 2) ## Daily tmax, Daily Tmean, Daily Tmin
gg <- gg%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
  mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)%>%arrange(Statie)

gg.sf <- st_as_sf(gg, coords = c("Longitudine","Latitudine"), crs = 4326)


rk.df <- terra::extract(rk,coord.sf,bind =T,ID=F)%>%as.data.frame()%>%
  pivot_longer(-c(Statie, Altitudine),
               names_to = "Temp_rk", values_to = "Temp_mean_RK")%>%
  dplyr::select(-c(Temp_rk))%>%mutate(Time = rep(seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day"),15))

ranm.df <- terra::extract(anom,coord.sf,bind =T,ID=F)%>%as.data.frame()%>%
  pivot_longer(-c(Statie, Altitudine), names_to = "Temp_rk", values_to = "Temp_mean_Ranom")%>%dplyr::select(-c(Temp_rk))%>%
  mutate(Time = rep(seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day"),15))

r.tot <- cbind(rk.df,ranm.df[3])%>%arrange(Statie)
r.tot1 <- r.tot%>%left_join(gg)%>%mutate(Rk_dif = Temp_mean_RK - Temp_mean, 
                                         Ranom_dif = Temp_mean_Ranom - Temp_mean)

write.csv(r.tot1, "tabs/extract_interpolation.csv", row.names = F)
##### graphs 

st <- r.tot1%>%na.omit()

bp <- ggplot(st, aes(x= factor(Month), y=Rk_dif, fill=factor(Month))) + 
  geom_boxplot()+facet_wrap(~Statie)+
  labs(title="Regreesion Kriging vs Observed",x="Months", y = "°C")+ theme_bw()

png("png/test_interpolare/box_plot_RK_2018.png", height = 1200, width = 1600, res= 200)
bp 
dev.off()

bp1 <- ggplot(st, aes(x = factor(Month), y = Ranom_dif, fill = factor(Month))) + 
  geom_boxplot()+facet_wrap(~Statie)+
  labs(title="Regresion Anomalies vs Observed",x="Months", y = "°C")+theme_bw()

png("png/test_interpolare/box_plot_Ranom_2018.png", height = 1200, width = 1600, res =200)
bp1 
dev.off()

###########
r.tot1
gg.lin <- ggplot()+
       geom_line(data = r.tot1, mapping = aes(x= Time,y=Rk_dif), col = "red")+
       geom_line(data = r.tot1, mapping = aes(x= Time,y=Ranom_dif), col = "green")+
       ylab("°C")+xlab("Time")+#scale_x_date(date_breaks = "15 days", date_labels = "%m")+
       facet_wrap(~Statie)
gg.lin       
  
png("png/test_interpolare/geom_line_diff_2018.png", height = 1200, width = 1600, res =200)
gg.lin
dev.off()    
  
###RMSE+MAE+R
library(Metrics)
library(tableHTML)

sts <- r.tot1%>%group_by(Statie)%>%summarise(RMSE_RK = rmse(Temp_mean,Temp_mean_RK),
                                             RMSE_DA = rmse(Temp_mean,Temp_mean_Ranom),
                                             
                                             MSE_RK = mse(Temp_mean,Temp_mean_RK),
                                             MSE_DA = mse(Temp_mean,Temp_mean_Ranom),
                                             
                                             BIAS_RK = bias(Temp_mean,Temp_mean_RK),
                                             BIAS_DA = bias(Temp_mean,Temp_mean_Ranom),
                                             
                                             PC_BIAS_RK = percent_bias(Temp_mean,Temp_mean_RK),
                                             PC_BIAS_DA = percent_bias(Temp_mean,Temp_mean_Ranom)
                                             )%>%mutate(across(where(is.numeric), ~round(., 3)))

library(tableHTML)
tableHTML(sts)
#and to export in a file
write_tableHTML(tableHTML(sts), file = 'tabs/sts.html')



#### taylor diagram 
library(openair)

r.tot1 <- r.tot1[c(1,3,4,5,10)]
r.tot1 <- r.tot1[c(1,3,2,4,5)]

r.long <- r.tot1%>%pivot_longer(-c(Statie,Time,Temp_mean), names_to = "indicator", values_to = "value")

TaylorDiagram(r.long, obs = "Temp_mean", mod = "value", group = "indicator")


head(mydata)




