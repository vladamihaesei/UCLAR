library(ggplot2)
library(dplyr)
library(scales)
orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")
for(o in 1:length(orase)){
  
  print(orase[o])
  
  files <- list.files(paste0("tabs/MODIS/",orase[o]), pattern = ".csv", recursive = T, full.names = T)
  
  for(i in 1:length(files)){
    
    d <- read.csv(files[i])%>%mutate(densit  = case_when(frecventa == 0~"0.0",
                                                         frecventa > 0 & frecventa <= 10~"1-10",
                                                         frecventa >10 & frecventa <= 20~"10.1-20.0",
                                                         frecventa >20 & frecventa <= 30~"20.1-30.0",
                                                         frecventa >30 & frecventa <= 40~"30.1-40.0",
                                                         frecventa >40 & frecventa <= 50~"40.1-50.0",
                                                         frecventa >50 & frecventa <= 60~"50.1-60.0",
                                                         frecventa >60 & frecventa <= 70~"60.1-70.0",
                                                         frecventa >70 & frecventa <= 80~"70.1-80.0",
                                                         frecventa >80 & frecventa <= 90~"80.1-90.0",
                                                         frecventa >90 & frecventa <= 100~"90.1-100.0"))
    
    tip <- unique(d$tip)
    dn <- unique(d$Day_night)
    g <- ggplot(d) + 
      geom_bar(aes(x = factor(densit),y = (..count..)/sum(..count..)), color="darkblue", fill="lightblue",position = 'dodge', stat='identity') +
      #geom_text(aes(x = factor(densit),y = (..count..)/sum(..count..),label= round(frecventa,1)), position=position_dodge(width=0.9), vjust=-0.25)+
      xlab("Cloud Coverage [%]")+ylab("Frequency [%]")+
      scale_y_continuous(labels=percent,expand = c(0.001, 0.001))+
      scale_x_discrete(expand = c(0.05, 0.05))+theme(axis.text.x = element_text(angle = 90))+theme_bw()
    path.out <- paste0("png/MODIS/histogram_cloud/",orase[o],"/")
    if(!dir.exists(path.out)) dir.create(path.out)
    png(paste0(path.out, "hist_",tip,"_",dn,".png"), width = 1800, height = 1400, res = 200)
    print(g)
    dev.off()
    
  }
  
}
  




