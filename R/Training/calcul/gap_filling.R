library(remotes)
remotes::install_github("ffilipponi/rtsa")
install.packages("https://github.com/ffilipponi/rtsa.git")
install.packages("https://github.com/marchtaylor/sinkr")
install_github("marchtaylor/sinkr")
library(sinkr)
library(rtsa)
library(rgdal)
library(raster)
library(sp)
### rasterul de analizat 
files <- list.files(path = "/Volumes/Z_vld/sfica_proiect_NE/Surf_Temp_Daily_1Km_v6/LST_Day_1km/", pattern = "*.tif", recursive = T, full.names = T )


##rss <- stack(files)
rss <- raster::stack(files)
rss[is.na(rss)]<- NA
plot(rss)

df.r <- as.data.frame(rss, xy = T)

#rss[which(getValues(rss > -50))]<- NA
rst.tem <- rts(rss, seq(as.Date("2004-06-01"), as.Date("2004-06-10"), "days"))

# ## generate raster mask
# files <- list.files(path = "/Volumes/Z_vld/sfica_proiect_NE/Surf_Temp_Daily_1Km_v6/LST_Day_1km/", pattern = "*.tif", recursive = T, full.names = T )
# 
# files <- files[1153:1184]
# 
# raster_mask1 <- brick(files[6])
# raster_mask1 < raster_mask1[[1]]
# values(raster_mask1) <- 1 # set raster mask values
# raster_mask1[which(is.na(getValues(rss[[6]])))] <- 0 # set raster mask values
# plot(raster_mask1)

# using dineof interpolation and raster mask
rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")

write.rts(rasterts_dineof, "/Volumes/Z_vld/sfica_proiect_NE/")
