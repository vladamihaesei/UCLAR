library(ggplot2)
library(dplyr)
library(sf)

uat <- readRDS("/Volumes/Z_vld/D/2022/prepclim2/www/data/shp/uat_ro.rds")%>%filter(county == "Iași" & name == "Iași")

vars <- c("tasAdjust","tasmaxAdjust","tasminAdjust")

for (i in 1:length(vars)){
  
  data <- read.csv(paste0("tabs/scenarii/uat_anomalies_annual_",vars[i],"_rcp45_1971_2100.csv"), sep = ";")#$anomalies$`95060`
  data1 <- read.csv(paste0("tabs/scenarii/uat_anomalies_annual_",vars[i],"_rcp85_1971_2100.csv"),sep = ";")#$anomalies$`95060`
  
  dataa <- rbind(data,data1)
  dataa$data <- as.Date(dataa$data)
  
  gg <- ggplot(dataa) + 
    geom_ribbon(data= filter(data, format(as.Date(data),"%Y")>=2020),aes(x = as.Date(data), ymax = scen_anom_max, ymin = scen_anom_min), alpha = 0.6,fill = "#e4650c")+ 
    geom_ribbon(data=filter(data1, format(as.Date(data),"%Y")>=2020),aes(x = as.Date(data), ymax = scen_anom_max, ymin = scen_anom_min), alpha = 0.4,fill = "red" ) +
    scale_x_date(breaks = c(as.Date("1971-01-01"), seq(as.Date("2020-01-01"), as.Date("2100-12-31"), by = "20 years")), date_labels = "%Y",expand = c(.01,.01)) +
    geom_bar(aes(x = as.Date(data), y = obs_anom, fill = symb, group =1),
             stat = "identity", width = 500, show.legend = F)  + 
    geom_line(data= filter(data, format(as.Date(data),"%Y")>=2020),aes(as.Date(data), scen_anom_mean), color = "#e4650c",size = 0.9) +
    geom_line(data= filter(data1, format(as.Date(data),"%Y")>=2020),aes(as.Date(data), scen_anom_mean),color = "red", size = 0.9) +
    
    scale_y_continuous(expand = c(0,0),
                       paste0("Anomaly [°C]")
    ) + #breaks = seq(-8,8, 2.0),limits = c(-4,8)) +  
    
    scale_fill_manual(values = c("blue","red")) +
    labs( x = "") + theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) 
  gg
  png(paste0("png/",vars[i],"_ribbon.png"), width = 2600, height = 1200, res = 250)
  print(gg)
  dev.off()
  system(paste0("convert -trim ", "png/",vars[i],"_ribbon.png"," png/",vars[i],"_ribbon.png"))
  
}

#data <- readRDS(paste0("tabs/scenarii/uat_anomalies_annual_",vars[i],"_rcp45_1971_2100.rds"))$anomalies$`95060`
#data1 <- readRDS(paste0("tabs/scenarii/uat_anomalies_annual_",vars[i],"_rcp85_1971_2100.rds"))$anomalies$`95060`
#write.csv(data,paste0("tabs/scenarii/uat_anomalies_annual_",vars[i],"_rcp45_1971_2100.csv"),row.names = F)
#write.csv(data1,paste0("tabs/scenarii/uat_anomalies_annual_",vars[i],"_rcp85_1971_2100.csv"),row.names = F)



