library(ggplot2)
library(dplyr)
library(scales)
orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

for(o in 1:length(orase)){

  tab <- read.csv(paste0("tabs/MODIS/count_na_",orase[i],".csv"))
  
  t <- tab%>%mutate(densit  = case_when(frecventa == 0~"0.0",
                                        frecventa > 0 & frecventa <= 10~"1-10",
                                        frecventa >10 & frecventa <= 20~"10.1-20.0",
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
    scale_y_continuous(labels=percent,expand = c(0.001, 0.001))+theme_bw()+
    scale_x_discrete(expand = c(0.05, 0.05))+facet_wrap(~tip, nrow = 2)
  png("png/histrogam_countNA_MODIS_Barlad.png", width = 1600, height = 1300, res = 200)
  g
  dev.off()

}


# 
# #### monthly
# g1 <- ggplot(tab, aes(x= frecventa))+
#   geom_histogram(color="darkblue", fill="lightblue")+facet_grid(month~tip)+theme_bw()+
#   scale_x_continuous(expand = c(0, 0))+scale_y_continuous(expand = c(0, 0))
# 
# png("png/histrogam_monthly_countNA_Barlad.png", width = 1800, height = 1400, res = 230)
# g1
# dev.off()