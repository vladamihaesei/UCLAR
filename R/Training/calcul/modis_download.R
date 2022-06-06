library(MODIStsp)
library(terra)

MODIStsp_get_prodlayers("M*D11A1")$bandnames

MODIStsp_get_prodnames()

limite <- list(c(27.6933, 46.6140, 27.770, 46.6835),c(28.01, 46.6140, 27.770, 46.6835),c(28.01, 46.6140, 27.770, 46.6835),
               c(28.01, 46.6140, 27.770, 46.6835),c(28.01, 46.6140, 27.770, 46.6835),
               c(28.01, 46.6140, 27.770, 46.6835),c(28.01, 46.6140, 27.770, 46.6835))

for (i in 1:length(limite)){
  
  MODIStsp(gui             = FALSE,
           out_folder      = '/Volumes/Z_vld/sfica_proiect_NE/',
           out_folder_mod  = '/Volumes/Z_vld/sfica_proiect_NE/',
           selprod         = 'Surf_Temp_Daily_1Km (M*D11A1)',
           bandsel         = 'LST_Day_1km', 
           sensor          = 'Both',
           user            = 'vlad.amihaesei95@gmail.com' , # your username for NASA http server
           password        = 'Geografie10',  # your password for NASA http server
           start_date      = '2004.01.01', 
           end_date        = '2004.01.31', 
           verbose         = TRUE,
           bbox            = c(27.6933, 46.6140, 27.770, 46.6835), #bbox of Latam
           spatmeth        = 'bbox',
           out_format      = 'GTiff',
           compress        = 'LZW',
           out_projsel     = 'User Defined',
           output_proj     = '+proj=longlat +datum=WGS84 +no_defs',
           delete_hdf      = TRUE,
           parallel        = TRUE
  )
  
}