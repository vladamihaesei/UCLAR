#### incepere plotare###
library(ggridges)
library(ggplot2)
library(dplyr)
theme_set(theme_ridges())

tabs <- read.csv("tabs/ERA5/params_1981-2021_daily.csv")%>%mutate(param = sub(" ","",param))
nume <- unique(tabs$loc)
params <- unique(tabs$param)

d <- data.frame(sign = c("J/m²","°C","°C","°C","%"," ","log(mm)","m/s"), 
                denumire = c("Radiația totală","Temperatura medie","Temperatura maximă","Temperatura minimă", "Umiditatea relativă",
                             "Nebulozitatea totală","Cantitatea de precipitații","Viteza vântului"))

cul <- list("1" = c("#fff7ec","#fdd49e","#fdbb84","#d7301f","#7f0000"),"2" = c("#3288bd", "#66c2a5", "#ffffbf", "#fdae61","#d53e4f"),
            "3" = c("#3288bd", "#66c2a5", "#ffffbf", "#fdae61","#d53e4f"),"4" = c("#3288bd", "#66c2a5", "#ffffbf", "#fdae61","#d53e4f"),
            "5" = c("#f7fcf0","#ccebc5","#7bccc4","#2b8cbe","#084081"),
            "6" = c("#ffffff","#d9d9d9","#737373","#000000"),
            "7" = c("#ffffd9","#7fcdbb","#1d91c0","#081d58"),
            "8" = c("#762a83","#e7d4e8","#d9f0d3","#1b7837"))

for(i in 1:length(params)){
  
  leg1 <- d$sign[i]
  leg2 <- d$denumire[i]
  print(leg2)
  
  for( j in 1:length(nume)){
    
    tf <- tabs%>%filter(loc== nume[j] & param == params[i])
    
    head(tf)
   
    if(leg1 == "°C"){
      
      gg <-  ggplot(tf,
                    aes(x = value, y = factor(Month,levels = rev(c("Ianuarie",   "Februarie" , "Martie"  ,   "Aprilie"   ,  "Mai"  ,     "Iunie"   ,   "Iulie" ,
                                                               "August" ,   "Septembrie" ,"Octombrie" ,  "Noiembrie" , "Decembrie"))))) +
        geom_density_ridges_gradient(
          aes(fill = ..x..), scale = 3.5, size = 0.5,rel_min_height = 0.01) +
        scale_fill_gradientn(colours = cul[[i]],name = d$sign[i], limits = c(-40,40))+
        labs(x ="", y = "", title = d$denumire[i],
             caption = "@UCLAR")+
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0))+ 
        theme(axis.text = element_text(size = 15),
              title = element_text(size = 15.5),
              legend.key.height =unit(1.6,"cm")
        )
        
      # theme_ridges(font_size = 15, grid = TRUE)
      
    }else{
      
      gg <-  ggplot(tf,
                    aes(x = value, y = factor(Month,levels = rev(c("Ianuarie",   "Februarie" , "Martie"  ,   "Aprilie"   ,  "Mai"  ,     "Iunie"   ,   "Iulie" ,
                                                               "August" ,   "Septembrie" ,"Octombrie" ,  "Noiembrie" , "Decembrie"))))) +
        geom_density_ridges_gradient(
          aes(fill = ..x..), scale = 3.5, size = 0.5,rel_min_height = 0.01) +
        scale_fill_gradientn(colours = cul[[i]],name = d$sign[i])+
       
        labs(x ="", y = "", title = d$denumire[i],
             caption = "@UCLAR")+
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0))+ 
        theme(axis.text = element_text(size = 15),
              title = element_text(size = 15.5),
              legend.key.height =unit(1.6,"cm")
        )
        #theme_ridges(font_size = 15, grid = TRUE)

    }
    
    path.out  <- paste0("png/raportare/splits/", nume[j])
    if(!dir.exists(path.out)) dir.create(path.out, recursive = T)
    png(paste0(path.out,"/",params[i],"_ridges.png"),height = 1600, width = 1800, res = 300 )
    print(gg)
    dev.off()
    system(paste0("convert -trim ",path.out,"/",params[i],"_ridges.png ",  path.out,"/",params[i],"_ridges.png"))

  }
  
}

##### 
library(grid)
library(png)
library(gridExtra)
library(magick)
library(cowplot)

files <- list.files("png/raportare/splits",full.names = T, recursive= T, pattern = ".png")

nume <- unique(do.call(rbind,strsplit(files,"/"))[,4])

for( n in 1:length(nume)){
  
  print(nume[n])
  files.sub <- grep(nume[n], files, value = T)
  files.long <- files.sub[c(3,5,6,7,4,1,2,8)]
  files.wide <- files.sub[c(3,5,6,7,4,1,2,8)]
  ### long 
  plots.long <- lapply(ll <- c(files.long),function(x){
    img <- image_read(x)
    rasterGrob(img, interpolate = FALSE)
  })
  
  #### wide
  plots.wide <- lapply(ll <- c(files.wide),function(x){
    img <- image_read(x)
    rasterGrob(img, interpolate = FALSE)
  })
  
  path.out1  <- paste0("png/raportare/merged/")
  if(!dir.exists(path.out1)) dir.create(path.out1, recursive = T)
  
  ########## long 
  png(paste0(path.out1,nume[n],"_plots_combined_long.png"),width=4200, height=4000, res = 500) 
  print(marrangeGrob(grobs = plots.long, nrow = 4, ncol = 4 , top = NULL))
  #print(grid.arrange(grobs = plots, nrow = 4, ncol = 4 ,top = NULL,  bottom = NULL))
  dev.off()
  system(paste0("convert -trim ",path.out1,nume[n],"_plots_combined_long.png ",  path.out1,nume[n],"_plots_combined_long.png"))
  
  ########### wide 
  png(paste0(path.out1,nume[n],"_plots_combined_wide.png"),width=4200, height=2100, res = 300) 
  print(marrangeGrob(grobs = plots.wide, nrow = 2, ncol = 4, top = NULL))
  #print(grid.arrange(grobs = plots.wide, nrow = 2, ncol = 4, bottom = NULL))
  dev.off()
  system(paste0("convert -trim ",path.out1,nume[n],"_plots_combined_wide.png ",  path.out1,nume[n],"_plots_combined_wide.png"))
  
}



