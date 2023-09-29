library(ggplot2)
library(dplyr)

df <- NULL
df1 <- NULL
orase <- c("Bacau", "Botosani","Iasi","PiatraNeamt")

for(o in 1:length(orase)){
  
  nocl <- read.csv(paste0("tabs/MODIS/",orase[o],"/allseas_0.csv"))
  tabs <- read.csv(paste0("tabs/MODIS/",orase[o],"/allseas_70.csv"))
  tabs$dif <- tabs$values.mean - nocl$values.mean
  tabs$orase <- orase[o]
  df <- rbind(tabs,df)
  
}

gg.bx <- ggplot(df)+
         geom_boxplot(aes(x = orase, y = dif, color = orase))+
         xlab("")+ylab("°C")+theme_bw()+
         theme(legend.position = "none",
          legend.title=element_text(size=12.0),
          text = element_text(size=12.5),
          axis.text.x = element_text(angle = 90),
          strip.background = element_rect(colour = "black", fill = "white"),
          panel.background = element_rect(fill = "white"),#EAF7FA
          panel.border = element_rect(colour = "black", fill = "transparent"))+facet_grid(per~factor(seas, levels = c("DJF","MAM","JJA","SON")))

png("png/MODIS/box_plots_differences_70p.png", height = 1700, width = 1600, res = 220)
gg.bx
dev.off()
system(paste0("convert -trim ", "png/MODIS/box_plots_differences_70p.png"," png/MODIS/box_plots_differences_70p.png"))
 
