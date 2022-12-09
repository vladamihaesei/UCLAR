library(ggplot2)
library(dplyr)
library(RColorBrewer)
require(patchwork)

df <- read.csv("tabs/ERA5/params_1981-2021_daily_v2.csv") %>% group_by(Year,loc,param)%>%
  summarise(medie = ifelse(param !="Precipitation", mean(value, na.rm = T), sum(value,na.rm = T)))%>%group_by(loc,param)%>%
  mutate(medie_91_2020 = mean(medie[Year >1990 & Year <=2020]),
    anom_91_2020 = ifelse(param!= "Precipitation", medie- medie_91_2020, (medie*100/medie_91_2020)-100))

df.sub <- df%>%filter(loc == "Vaslui")

col_strip <- brewer.pal(11, "RdBu")

gh <-  ggplot(df.sub,
              aes(x = Year, y = 1, fill = medie))+
  geom_tile() +
  scale_fill_gradientn(colors = rev(col_strip)) +
  guides(fill = guide_colorbar(barwidth = 1)) +
  theme_void()+facet_wrap(~param)

gg_l = lapply(df.sub, function(x) {
  ggplot(df.sub,
         aes(x = Year, y = .5, fill = anom_91_2020))+
    geom_tile() +
    scale_fill_gradientn(colors = rev(col_strip)) +
    guides(fill = guide_colorbar(barwidth = .5)) +
    theme_bw()+facet_wrap(~param, scales = "y_free")
})

# patchwork
png("png/raportare/warp_plots_anomalies.png", height = 2300, width = 2500, res = 240)
wrap_plots(gg_l, ncol = 3)
dev.off()
