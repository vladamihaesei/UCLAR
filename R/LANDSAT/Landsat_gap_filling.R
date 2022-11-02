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

for (i in 1:length(orase)){
  
  print(orase[i])
  files <- list.files(paste0("/Volumes/Z_vld/sfica_proiect_NE/Landsat/Landsat_",orase[i],"_LST_T-20220725T095615Z-001/Landsat_",orase[i],"_LST_T"), full.names = T, pattern = ".tif")
  
  dates <- unique(as.Date(unique(do.call(rbind, strsplit(files,"/|_|.tif"))[,19]),"%Y%m%d"))
 
  ani <- list(a = c(1984,1985 ,1986 ,1987 ,1988 ,1989 ,1990), b= c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998),
              c = c(1999, 2000, 2001, 2002, 2003, 2004, 2005),d=c(2006, 2007, 2008, 2009, 2010, 2011),e=c(2012, 2013, 2014, 2015, 2016),
              f = c(2017, 2018, 2019, 2020, 2021))
  
  for (j in 1:length(ani)){
    
    print(ani[[j]])
    files.sub <- files[format(dates, "%Y") %in% c(ani[[j]])]
    #files.sub <- files[which(format(dates,"%Y")==ani[i])]
    rss <- raster::stack(files.sub)
    rss[is.na(rss)]<- NA

    dates1 <- as.Date(format(as.Date(do.call(rbind,strsplit(files.sub,"_|/|.tif"))[,19], "%Y%m%d"), "%Y-%m-%d"))
   
    rst.tem <- rts(rss, dates1)
    
    # using dineof interpolation and raster mask
    rasterts_dineof <- rtsa.gapfill(rst.tem, rastermask = NULL, method="dineof")
    
    out <- paste0("grids/filled/",orase[i],"/")
    if (!dir.exists(out)) dir.create(out, recursive = T)
    
    write.rts(rasterts_dineof, paste0(out,ani[j]))
    
  }

}
