library(ggplot2)
library(dplyr)
library(scales)
orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for(o in 1:length(orase)){
  
  t <- read.csv(paste0("tabs/LANDSAT/countNA_",orase[o],".csv"))
  
  t <- t%>%mutate(densit  = case_when(frecventa == 0~"0.0",
                                      frecventa >0 & frecventa <= 10~"1-10",
                                      frecventa >10 & frecventa <=20~"10.1-20.0",
                                      frecventa >20 & frecventa <= 30~"20.1-30.0",
                                      frecventa >30 & frecventa <= 40~"30.1-40.0",
                                      frecventa >40 & frecventa <= 50~"40.1-50.0",
                                      frecventa >50 & frecventa <= 60~"50.1-60.0",
                                      frecventa >60 & frecventa <= 70~"60.1-70.0",
                                      frecventa >70 & frecventa <= 80~"70.1-80.0",
                                      frecventa >80 & frecventa <= 90~"80.1-90.0",
                                      frecventa >90 & frecventa <= 100~"90.1-100.0"
                  ))
  
  g <- ggplot(t, aes(x = factor(densit))) +  
    geom_bar(aes(y = (..count..)/sum(..count..)), color="darkblue", fill="lightblue") +
    xlab("Cloud Coverage [%]")+ylab("Frequency [%]")+
    scale_y_continuous(labels=percent,expand = c(0.0, 0.0))+theme_bw()+
    scale_x_discrete(expand = c(0.0, 0.0))
  
  g1 <- ggplot(t, aes(x = factor(densit))) +  
    geom_bar(aes(y = (..count..)/sum(..count..)), color="darkblue", fill="lightblue") +
    xlab("Cloud Coverage [%]")+ylab("Frequency [%]")+
    scale_y_continuous(labels=percent,expand = c(0.0, 0.0))+theme_bw()+
    scale_x_discrete(expand = c(0.0, 0.0))+facet_wrap(~month)+theme(axis.text.x = element_text(angle = 90,hjust = 0.1, vjust = 0.1))
  
  png("png/histrogam_countNA_LANDSAT_Barlad.png", width = 1500, height = 1300, res = 200)
  g
  dev.off()
  
  png("png/histrogam_countNA_LANDSAT_monthly_Barlad.png", width = 1900, height = 1500, res = 200)
  g1
  dev.off()
  
}


