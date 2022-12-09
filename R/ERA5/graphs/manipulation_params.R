library(dplyr)
library(openxlsx)
library(tidyr)

#### manipulare si modificare
t1 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 1) %>% mutate(param = "Radiation")
t2 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 2)
names(t2) <- names(t1)[1:19]
t2 <- t2 %>%mutate(param  = "Tmed") %>%na.omit()

t3 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 3) 
names(t3) <- names(t1)[1:19]
t3 <- t3 %>% mutate(param = "Tmax")%>%na.omit()

t4 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 4) 
names(t4) <- names(t1)[1:19]
t4 <- t4%>% mutate(param = "Tmin")%>%na.omit()

t5 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 5) 
names(t5) <- names(t1)[1:19]
t5 <- t5%>% mutate(param = "RH mean")%>%na.omit()

t6 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 6) 
names(t6) <- names(t1)[1:19]
t6 <- t6%>% mutate(param = "CC Total")%>%na.omit()

t7 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 8)
names(t7) <- names(t1)[1:19]
t7 <- t7%>% mutate(param = "Precipitation")%>%na.omit()

t8 <- openxlsx::read.xlsx("tabs/Data_compiled.xlsx", sheet = 9) 
names(t8) <- names(t1)[1:19]
t8 <- t8%>% mutate(param = "Wind speed")%>%na.omit()
### agregare fisiere
tot <- rbind(t1,t2,t3,t4,t5,t6,t7,t8)
i <- c(4:18)    
tot[,i] <- apply(tot[ , i], 2,            # Specify own function within apply
      function(x) as.numeric(as.character(x)))

tabs <- tot%>%pivot_longer(-c(Year,Month,Day,param), names_to = "loc","value")%>%mutate(Month = case_when(Month == 1~"Ianuarie",
                                                                                           Month == 2~"Februarie",
                                                                                           Month == 3~"Martie",
                                                                                           Month == 4~"Aprilie",
                                                                                           Month == 5~"Mai",
                                                                                           Month == 6~"Iunie",
                                                                                           Month == 7~"Iulie",
                                                                                           Month == 8~"August",
                                                                                           Month == 9~"Septembrie",
                                                                                           Month == 10~"Octombrie",
                                                                                           Month == 11~"Noiembrie",
                                                                                           Month == 12~"Decembrie"
                                                                                      ),
                                                                                      value = ifelse(param %in%c("Precipitation"),replace(value, value<=2.1,NA),value),
                                                                                      value = ifelse(param %in%c("CC Total"),replace(value, value>=0.78,NA),value),
                                                                                      value = ifelse(param %in%c("RH mean"),replace(value, value>=90,NA),value),
                                                                                      value = ifelse(param %in%c("CC Total"),replace(value, value<=0.1,NA),value),
                                                                                      value = ifelse(param %in%c("Radiation"),replace(value, value<=1.3,NA),value),
                                                                                      value = ifelse(param %in%c("Wind speed"),replace(value, value<=0.8,NA),value))%>%na.omit()%>%
                                                                                      mutate(value = ifelse(param=="Precipitation",log(value),value))


write.csv(tabs,"tabs/ERA5/params_1981-2021_daily.csv", row.names = F)

tabs.v1 <- tot%>%pivot_longer(-c(Year,Month,Day,param), names_to = "loc","value")%>%mutate(data = paste0(Year,"-",Month,"-",Day))

write.csv(tabs,"tabs/ERA5/params_1981-2021_daily.csv", row.names = F)
write.csv(tabs.v1,"tabs/ERA5/params_1981-2021_daily_v2.csv", row.names = F)





