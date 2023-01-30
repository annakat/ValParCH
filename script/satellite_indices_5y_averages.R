##################################################################
####Calculate running average indices for every 5 years #######
#### Based on: Swiss Data Cube Indices from https://yareta.unige.ch
### AS July 4 2022 ###

library(dplyr)
library(terra)

## Download index from https://yareta.unige.ch and select one, e.g. EVI
llx <- list.files("./results_EVI_1984_2021/", full.names = T)

## index statistics
idx <- c("mean", "med", "min", "max","range", "std")

## time period
years <- 1984:2021

## seasons
seasons <- c("MAM","JJA","SON", "DJF")
ss <- c("spring", "summer", "autumn", "winter")

## VP raster
VP_grid <- rast("./gis_pub/CRS_refgrid_AS/valparc_grid_LV95.tif") 

## Swiss grid LV95 25m resolution
VP_grid <- rast("./data/valparc_grid_LV95.tif") 

##############################
## Annual indices ###########
lll <- llx[grepl("annual",llx)]

### 5-year moving average ######
for(l in 1:length(idx)){
  idxx <- idx[l]
  (ll <- lll[grepl(idxx, lll)])
  
  for(j in 1:(length(years)-4)){ 
    yy <- years[j]:years[j+4]
    ras <- list()
    for (i in 1:5){
      ras[[i]] <- rast(ll[grep(paste0(yy[i],"_annual"),ll)])
    }
    bri <- rast(ras)
    ras_m <- mean(bri,na.rm=T)
    
    writeRaster(ras_m,paste0("./",yy[1],"_", yy[5],
                             "/meanEVI_annual",idxx,"_",yy[1],"_", yy[5],".tif"), 
                filetype="GTiff")
    
    ### Reproject to VP raster
    img_warp_CH_r <- project(ras_m,VP_grid,threads=TRUE)
    img_warp_CH_r_int <- round(img_warp_CH_r*10000,digits=0)
    
    writeRaster(img_warp_CH_r_int, 
                paste0("./",yy[1],"_", yy[5],
                       "/meanEVI_annual",idxx,"_",yy[1],"_", yy[5],".tif"),filetype="GTiff")
    saveRDS(img_warp_CH_r_int, paste0("./",yy[1],"_", yy[5],
                                      "/meanEVI_annual",idxx,"_",yy[1],"_", yy[5],".rds"))
  }
}

########################
#### Seasonal indices ##
### 5-year moving average 

for(k in 1:length(seasons)){ 
  seas <- seasons[k]
  ssx <- ss[k]
  (lll <- llx[grepl(ssx, llx)])
  
  for(l in 1:length(idx)){
    idxx <- idx[l]
    (ll <- lll[grepl(idxx, lll)])
    
    for(j in 1:1){ ### 
      yy <- years[j]:years[j+4]
      ras <- list()
      for (i in 1:5){
        ras[[i]] <- rast(ll[grep(paste(yy[i],ssx,sep="_"),ll)])
      }
      bri <- rast(ras)
      ras_m <- mean(bri,na.rm=T)
      
      writeRaster(ras_m,paste0("./",yy[1],"_", yy[5],
                               "/meanEVI_seasonal",idxx,"_",yy[1],"_", yy[5],"_", seas[1],".tif"), 
                  filetype="GTiff",overwrite=T)
      
      ### Reproject to VP raster
      img_warp_CH_r <- project(ras_m,VP_grid,threads=TRUE)
      img_warp_CH_r_int <- round(img_warp_CH_r*10000,digits=0) 
      
      writeRaster(img_warp_CH_r_int, 
                  paste0("./",yy[1],"_", yy[5],
                         "/meanLAI_seasonal",idxx,"_",yy[1],"_", yy[5],"_", 
                         seas[1],".tif"),filetype="GTiff",overwrite=T)
      saveRDS(img_warp_CH_r_int, paste0("./",yy[1],"_", yy[5],
                                        "/meanLAI_seasonal",idxx,"_",yy[1],"_", yy[5],"_", 
                                        seas[1],".rds"))
    }
  }
}

######### END ###########

  
  