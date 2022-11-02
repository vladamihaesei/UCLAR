library(remotes)
#remotes::install_github("ffilipponi/rtsa")
#install.packages("https://github.com/ffilipponi/rtsa.git")
#install.packages("https://github.com/marchtaylor/sinkr")
#install_github("marchtaylor/sinkr")
library(sinkr)
library(rtsa)
library(rgdal)
library(raster)
library(sp)

orase <- c("Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi","MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

orase <- c(#"Barlad","Bacau", "Botosani","Dorohoi","Falticeni","Husi","Iasi",
           #"MoinComan",
           "Onesti","Pascani","PiatraNeamt","Radauti","Roman","Suceava","Vaslui")

##### MOD11A1
for (i in 1:length(orase)){
  
  print(orase[i])
  
  files <- list.files(paste0("grids/MODIS/nc/nonfilled/",orase[i]), full.names = T, pattern = "nc")[1]
  
  r <- raster::stack(files, varname = "LST_Day_1km")
  rr <- terra::rast(files, subds = "LST_Day_1km")
  r1 <- raster::stack(files, varname = "LST_Day_1km")
  rr1 <- terra::rast(files, subds = "LST_Night_1km")
  
  r[is.na(r)]<- NA
  r1[is.na(r1)]<- NA
  
  dates <- time(rr)
  dates1 <- time(rr1)
  
  rst.tem <- rts(r, as.Date(time(rr)))
  rst.tem1 <- rts(r1, as.Date(time(rr1)))
  
  rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")
  rasterts_dineof1 <- rtsa.gapfill(rst.tem1, rastermask = NULL, method="dineof")
  
  out <- paste0("grids/MODIS/nc/filled/LST_Day_1km/",orase[i],"/")
  out1 <- paste0("grids/MODIS/nc/filled/LST_Night_1km/",orase[i],"/")
  
  if (!dir.exists(out)) dir.create(out, recursive = T)
  if (!dir.exists(out1)) dir.create(out1, recursive = T)
  
  write.rts(rasterts_dineof, paste0(out,"MOD11A1Day_2000-2020"))
  write.rts(rasterts_dineof1, paste0(out1,"MOD11A1night_2000-2020"))
  
}

##### ##### MYD11A1

for (i in 1:length(orase)){
  
  print(orase[i])
  
  files <- list.files(paste0("grids/MODIS/nc/nonfilled/",orase[i]), full.names = T, pattern = "nc")[2]
  
  r <- raster::stack(files, varname = "LST_Day_1km")
  rr <- terra::rast(files, subds = "LST_Day_1km")
  r1 <- raster::stack(files, varname = "LST_Day_1km")
  rr1 <- terra::rast(files, subds = "LST_Night_1km")
  
  r[is.na(r)]<- NA
  r1[is.na(r1)]<- NA
  
  dates <- time(rr)
  dates1 <- time(rr1)
  
  rst.tem <- rts(r, as.Date(time(rr)))
  rst.tem1 <- rts(r1, as.Date(time(rr1)))
  
  rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")
  rasterts_dineof1 <- rtsa.gapfill(rst.tem1, rastermask = NULL, method="dineof")
  
  out <- paste0("grids/MODIS/nc/filled/LST_Day_1km/",orase[i],"/")
  out1 <- paste0("grids/MODIS/nc/filled/LST_Night_1km/",orase[i],"/")
  
  if (!dir.exists(out)) dir.create(out, recursive = T)
  if (!dir.exists(out1)) dir.create(out1, recursive = T)
  
  write.rts(rasterts_dineof, paste0(out,"MYD11A1Day_2000-2020"))
  write.rts(rasterts_dineof1, paste0(out1,"MYD11A1night_2000-2020"))
  
}
