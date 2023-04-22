## A script that takes in the outputs a google earth engine script that outputs
## a folder of RGB tifs and plots them with a cruise based block and forest reserve 
## polygons. 

##Note 1: rewrote this with terra instead of raster and RSToolbox (deprecated or on its way out)
##Note 2: a little bit painful, this might be easier with ggplot2? Something to investigate.

library(terra)
library(tidyverse)

#Cutblock polygons
s <- "C:/Data/CruiseBilling/vectors/recentlyHarvested_CBB_v5.shp"

### Define column names that exist in the above shapefile.
obj_id <- 'Obj_ID2'
timber_mark <- 'TIMBER_MAR'
cutblock <- 'CUT_BLOCK_'
ff_id <- 'FFID'
disturbance_start <- 'DISTURBANC'
disturbance_end <- 'DISTURBA_1'
###

#Reserves polygons
r <- "C:/Data/CruiseBilling/vectors/recentlyHarvestedCBB_v5_reserves.shp"

#Output folder
finaldir <- 'C:/Data/CruiseBilling/gee/recentlyHarvested_CBB_v5_all_v3'

#Read in cutblock shapefile
dfs <- vect(s)

#Read reserves shapefile
dfr <- vect(r)

#get all folders 
dirs = list.dirs(finaldir,full.names = F, recursive = F)

for (f in dirs){
  print(f)
  ###get permit vector
  dfs_sub <- dfs[dfs[obj_id] %in% f, ]
  
  ###read in pre_mosaic
  d <-'tif'
  fl <- 'png'
  if (!dir.exists(file.path(finaldir,f,fl))) {dir.create(file.path(finaldir,f,fl))}
  imglist <- list.files(path = file.path(finaldir,f,d), pattern='.tif$') #$ means end of the string
  for (img in imglist){
    #read in raster
    b <- rast(file.path(finaldir,f,d,img))
    names(b) <- c('r','g','b')
    
    ###get reserves
    dfr_sub <- crop(dfr,ext(b))
    
    # Define names 
    qa_name <- paste0(strsplit(img,split=".",fixed=T)[[1]][[1]],'.png') 
    #export truecolor 
    qlname <- file.path(finaldir,f,fl,qa_name)
    
    #Plot title
    title <- paste0('Timbermark: ',dfs_sub[timber_mark],', ', 'Cutblock: ', dfs_sub[cutblock])
    
    # Plot
    png(qlname,units='in',width=8,height=10.5,res=200) #To open in a new window
    #windows(8.5,11);
    plot(b$r,legend=FALSE,main=title,cex.main=1.2)# need to plot one band first to get coords on x and y axes
    
    ## Plotting hell, need to figure out where to put the textbox
    cc <- par('usr') #get extent of the plot
    x_dist <- (cc[[2]] - cc[[1]]) * 0.03
    y_dist <- (cc[[4]] - cc[[3]]) * 0.03
    
    ## Data for text box
    #forest file id, district, disturbance start date, disturbance end date, area_ha, image date, sensor
    forest_file_id <- dfs_sub[ff_id]
    dstart <- dfs_sub[disturbance_start]
    dend <- dfs_sub[disturbance_end]
    area_ha <- round(expanse(dfs_sub,unit="ha"),digits=1)
    img_date <- strsplit(img,split="_",fixed=T)[[1]][[2]]
    sensor <- strsplit(strsplit(img,'_')[[1]][[3]],'.',fixed=T)[[1]][[1]]
    projection <- paste0(crs(b,describe=TRUE)$authority,':',crs(b,describe=TRUE)$code)
    
    ##aa <- substitute(paste(bold('p value'), " = 0.01"))
    str <- paste0('Forest File ID: ', forest_file_id, '\n',
                  'Disturbance Start Date: ', dstart, '\n',
                  'Disturbance End Date: ', dend, '\n',
                  'Area (ha): ', area_ha, '\n',
                  'Image Acquisition Date: ', img_date, '\n',
                  'Sensor: ', sensor, '\n',
                  'Projection: ', projection)
    #str <- expression(bold("Forest File ID:"))
    plot(b,add=TRUE) #plot rgb
    plot(dfs_sub,border='red',lwd=1,add=TRUE)#add cutblock boundary
    
    if (length(dfr_sub)!=0){
      #add reserve  
      plot(dfr_sub,border='yellow',lwd=1,add=TRUE)}else{}
    
    sbar(type='bar',divs=4,below='meters')#scale bar
    north(type=2)#north arrow
    legend("bottomright", inset=.02, title="Legend",
           c("permit","reserve"), fill = c("white","white"),border= c('red','yellow'), horiz=FALSE, cex=1.2)
    
    #add grey box
    
    textWidth <- strwidth(str, cex = 3/4)
    textHeight <- strheight(str, cex = 3/4)
    
    buffer <- min(x_dist,y_dist)/2
    boxX1 <- cc[[1]] + x_dist - buffer
    boxX2 <- cc[[1]] + x_dist + textWidth + buffer
    boxY1 <- cc[[4]] - y_dist + buffer
    boxY2 <- cc[[4]] - y_dist - textHeight - buffer
    
    #add text
    #text(str, x = cc[[1]] + x_dist, y = cc[[4]] - y_dist,adj=c(0,1),cex=3/4)
    rect(xleft = boxX1, ybottom = boxY1, xright = boxX2, ytop = boxY2, col = "white", border = "black", lwd = 1)
    text(str, x = cc[[1]] + x_dist, y = cc[[4]] - y_dist,adj=c(0,1),cex=3/4)
    dev.off()
  
}}
