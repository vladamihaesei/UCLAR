library(MODIStsp)
library(terra)
library(sf)

MODIStsp_get_prodnames()

MODIStsp_get_prodlayers("M*D11A1")$bandnames

limite <- read_sf("Limite/Limite.shp")

for (i in 1:length(limite)){
  
  limite1 <- limite[i,]
  nume <- limite1$Drawings
  bb <- st_bbox(limite1)
  
  path <- paste0('tmp/',nume) 
  if (!dir.exists(path)) dir.create(path, recursive = T)
  
  MODIStsp(gui             = FALSE,
           out_folder      = path,
           out_folder_mod  = path,
           selprod         = 'Surf_Temp_Daily_1Km (M*D11A1)',
           bandsel         = c('QC_Day','LST_Day_1km','LST_Night_1km'), 
           sensor          = 'Both',
           user            = 'vlad.amihaesei95@gmail.com' , # your username for NASA http server
           password        = 'Geografie10',  # your password for NASA http server
           start_date      = '2004.06.01', 
           end_date        = '2004.06.03', 
           download_server = 'http',
           #downloader      = 'aria2',
           verbose         = TRUE,
           bbox            = c(bb[1], bb[2], bb[3], bb[4]), #bbox of Latam
           spatmeth        = 'bbox',
           out_format      = 'GTiff',
           compress        = 'LZW',
           out_projsel     = 'User Defined',
           output_proj     = '+proj=longlat +datum=WGS84 +no_defs',
           delete_hdf      = T,
           parallel        = TRUE
  )
  
}
library(httr)
httr::GET("http://cran.r-project.org/Rlogo.jpg", config = httr::config(connecttimeout = 90))
GET("http://httpbin.org/delay/1", timeout(20))

GET("http://had.co.nz")

system("ping -c5 httpbin.org")

### verificare fisiere 
files <- list.files("/Volumes/Z_vld/sfica_proiect_NE/Surf_Temp_Daily_1Km_v6/LST_Day_1km", pattern = ".tif",full.names = T)

vf <- terra::rast(files)
plot(vf)
