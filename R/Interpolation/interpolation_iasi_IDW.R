library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(ggmap)
library(tmap)
#z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
#P <- readRDS(z)
coord <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
gg <- readxl::read_xlsx("tabs/Temp_Iasi_gridare_2023.xlsx",sheet = 2) ## Daily tmax, Daily Tmean, Daily Tmin
gg <- gg%>%pivot_longer(-c(Year,Month,Day,JDay), names_to = "Statie", values_to = "Temp_mean")%>%
  mutate(Time = as.Date(paste0(Year,"-",Month,"-",Day)))%>%filter(Year == 2018)%>%left_join(coord)

#gg <- gg%>%filter(Time == "2015-07-23")%>%na.omit()
zile <- unique(gg$Time)

ridw <- rast() 

r.t <- rast("grids/IDW/ziua1.tif")

for(z in 1:length(zile)){
  
  gg.f <- gg%>%filter(Time == zile[z])%>%na.omit()
  coordinates(gg.f) <- c("Longitudine","Latitudine")
  proj4string(gg.f) = CRS("+init=epsg:4326")
  #gg.f1 <- gg%>%filter(Time == zile[1])%>%na.omit()
  #coordinates(gg.f1) <- c("Longitudine","Latitudine")
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(gg.f1, "regular", n=85000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(gg.f) <- proj4string(gg.f) # Temp fix until new proj env is adopted
  proj4string(grd) <- proj4string(gg.f)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  P.idw <- gstat::idw(Temp_mean~1, gg.f, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to Texas
  r <- rast(P.idw)[[1]]
  r <- crop(r,r.t) 
  nrow(r) <- nrow(r.t)
  ncol(r) <- ncol(r.t)
  ext(r) <- ext(r.t)

  ridw <- c(ridw,r)
  
}

fg <- as.data.frame(r)
fg1 <- as.data.frame(ridw)

writeRaster(r, "grids/IDW/ziua1.tif")
# Plot


tm_shape(r) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="(°C)") + 
  tm_shape(gg.f) + tm_dots(size=0.2) + 
  tm_legend(legend.outside=TRUE)
