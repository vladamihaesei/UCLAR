library(terra)
library(sf)

mnt_iasi <- terra::rast("grids/dem_Iasi.tif")


coord <- readxl::read_xlsx("~/Downloads/Temp_Iasi_gridare_2023.xlsx",sheet = 4)
coord_sf <- sf::st_as_sf(coord,coords= c("Longitudine", "Latitudine"), crs= 4326 )

terra::plot(mnt_iasi)
terra::plot(coord_sf[1],add = T)
