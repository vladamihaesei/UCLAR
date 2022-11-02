library(ggplot2)
library(dplyr)
library(ggrepel)

df <- read.csv("tabs/ERA5/params_1981-2021_daily_v2.csv") %>% group_by(Year,Month,loc,param)%>%
  summarise(medie = ifelse(param !="Precipitation", mean(value, na.rm = T), sum(value,na.rm = T)))%>%group_by(Month,loc,param)%>%
  summarise(medie = mean(medie))

gh <- ggplot(df) +
  geom_line(aes(as.factor(Month), medie, group = loc,color = loc))+
  facet_wrap(~param, scales = "free_y",strip.position = "left",labeller = as_labeller(c(
                                                                Precipitation = "mm",Radiation ="J/m2",
                                                              
                                                                Tmax = "C",
                                                                Tmin = "C",
                                                                Tmed = "C"
                                                                ) ))+
               theme(legend.position = "none")
gh
strip.position = "left", 
labeller = as_labeller(c(A = "Currents (A)", V = "Voltage (V)") ) 

library(tidyverse)
library(reshape2)
x <- seq(0, 10, by = 0.1)
y1 <- sin(x)
y2 <- sin(x + pi / 4)
y3 <- cos(x)

my.df <-
  data.frame(
    time = x,
    currentA = y1,
    currentB = y2,
    voltage = y3
  )
my.df <- melt(my.df, id.vars = "time")
my.df$Unit <- as.factor(rep(c("A", "A", "V"), each = length(x)))
