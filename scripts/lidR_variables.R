# install.packages("lidR", lib = "/projappl/project_2003061/Rpackages/")
library(lidR)
library(sf)
library(raster)
library(tidyverse)

get_lidr_threads()
set_lidr_threads(future::availableCores())
get_lidr_threads()

max_z <- 40 # in meters

var_dir <- "/scratch/project_2007415/microclim_topo/"

tmpdir <- paste0("/scratch/project_2007415/temp/", stringi::stri_rand_strings(1, 10))
dir.create(tmpdir)
latest <- st_read("/appl/data/geo/mml/laserkeilaus/2008_latest/2008_latest.shp")

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area != "RAS")

for(area_name in rois$area){
  # area_name <- "EVO"
  if(!file.exists(paste0(var_dir, "/", area_name, "/lidR_zwimean.tif"))){
    print(area_name)
    
    unlink(list.files(tmpdir, pattern = "tif$", full.names = T))
    
    roi <- rois %>%
      filter(area == area_name) %>%
      st_transform(crs = st_crs(latest))
    
    pols <- st_make_grid(roi, cellsize = 3000) %>% 
      st_as_sf() %>% 
      mutate(id = 1:nrow(.))
    
    for(i in pols$id){
      # i <- 6
      
      roi_t <- pols %>% filter(id == i) %>% 
        st_buffer(10)
      
      latest_t <- latest[roi_t,]
      
      if(nrow(latest_t) > 0){
        
        lass <- readLAS(latest_t$path)
        lidR::st_crs(lass) <- sf::st_crs(roi_t)$epsg
        
        # CLIP THE LAS
        lass <- clip_roi(lass, roi_t)
        
        if(!is.null(lass)){
          if(lass@header@PHB$`Number of point records` > 1000){
            
            dtm <- grid_terrain(lass, 1, tin())
            
            lass <- normalize_height(lass, tin(), na.rm = TRUE)
            
            lass <- filter_poi(lass, Z <= max_z)
            
            chm <- grid_canopy(lass, res = 1, algorithm = dsmtin(max_edge = 5))
            
            chm[chm < 0] <- 0
            chm[is.na(chm)] <- 0
            
            roi_t <- pols %>% filter(id == i)
            
            chm <- crop(chm, roi_t, snap = "out")
            
            writeRaster(round(chm*100), paste0(tmpdir, "/lidR_chm_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            
            # CANOPY METRICS
            
            myMetrics = function (z, th = 2) 
            {
              n <- length(z)
              pzabovex <- lapply(th, function(x) {
                sum(z >= x)/n * 100
              })
              names(pzabovex) <- paste0("pzabove", th)
              
              metrics <- c(pzabovex)
              return(metrics)
            }
            
            mp <- grid_metrics(lass, ~myMetrics(Z, th = c(1,2,3,4,5,7,10,15,20)), res = 4)
            mp <- crop(mp, roi_t, snap = "out")
            
            writeRaster(round(mp[["pzabove1"]]), paste0(tmpdir, "/lidR_pzabove1_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove2"]]), paste0(tmpdir, "lidR_pzabove2_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove3"]]), paste0(tmpdir, "/lidR_pzabove3_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove4"]]), paste0(tmpdir, "/lidR_pzabove4_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove5"]]), paste0(tmpdir, "/lidR_pzabove5_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove7"]]), paste0(tmpdir, "/lidR_pzabove7_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove10"]]), paste0(tmpdir, "/lidR_pzabove10_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove15"]]), paste0(tmpdir, "/lidR_pzabove15_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(mp[["pzabove20"]]), paste0(tmpdir, "/lidR_pzabove20_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            
            
            # Density of points
            density <- grid_metrics(lass, ~sum(ReturnNumber == 1)/100, 10) # calculate density
            density <- crop(density, roi_t, snap = "out")
            
            writeRaster(round(density*100), paste0(tmpdir, "/lidR_density_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            
            myMetrics <- function (z, i) 
            {
              n <- length(z)
              zmax <- max(z)
              zmean <- mean(z)
              zsd = sd(z)
              zwimean = sum(z*i)/sum(i) # Mean elevation weighted by intensities
              zimean  = mean(z*i)       # Mean products of z by intensity
              zsqmean = sqrt(mean(z^2)) # Quadratic mean
              pzabovemean <- sum(z > zmean)/n * 100
              
              metrics <- list(zmax = zmax, zmean = zmean, zsd = zsd, pzabovemean = pzabovemean,
                              zwimean = zwimean,zimean = zimean, zsqmean = zsqmean,
                              zskew = (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2), 
                              zkurt = n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2))
              return(metrics)
            }
            
            metrics = grid_metrics(lass, ~myMetrics(Z, Intensity), 4)
            metrics <- crop(metrics, roi_t, snap = "out")
            
            metrics[["zmax"]][metrics[["zmax"]] < 0] <- 0
            metrics[["zmean"]][metrics[["zmean"]] < 0] <- 0
            metrics[["zwimean"]][metrics[["zwimean"]] < 0] <- 0
            metrics[["zimean"]][metrics[["zimean"]] < 0] <- 0
            
            metrics[["pzabovemean"]][metrics[["zmean"]] < 1] <- 0
            metrics[["zskew"]][metrics[["zmean"]] < 1] <- 0
            metrics[["zkurt"]][metrics[["zmean"]] < 1] <- 0
            
            
            writeRaster(round(metrics[["zmax"]]*100), paste0(tmpdir, "/lidR_zmax_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            writeRaster(round(metrics[["zmean"]]*100), paste0(tmpdir, "/lidR_zmean_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            writeRaster(round(metrics[["zsd"]]*100), paste0(tmpdir, "/lidR_zsd_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            writeRaster(round(metrics[["pzabovemean"]]), paste0(tmpdir, "/lidR_pzabovemean_",i,".tif"),
                        format = "GTiff", datatype = "INT1U", overwrite = T)
            writeRaster(round(metrics[["zwimean"]]*100), paste0(tmpdir, "/lidR_zwimean_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            writeRaster(round(metrics[["zimean"]]), paste0(tmpdir, "/lidR_zimean_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            writeRaster(round(metrics[["zsqmean"]]*100), paste0(tmpdir, "/lidR_zsqmean_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            writeRaster(round(metrics[["zskew"]]*100), paste0(tmpdir, "/lidR_zskew_",i,".tif"),
                        format = "GTiff", datatype = "INT2S", overwrite = T)
            writeRaster(round(metrics[["zkurt"]]*100), paste0(tmpdir, "/lidR_zkurt_",i,".tif"),
                        format = "GTiff", datatype = "INT2U", overwrite = T)
            
          } else {
            print("No layers!!!")
          }
        } else {
          print("No layers!!!")
        }
      } else {
        print("No layers!!!")
      }
    }
    
    #################################################################
    # Merge files
    vars <- unique(unlist(lapply(list.files(tmpdir, pattern = "tif$"), function(x) paste(head(unlist(str_split(x,"_")[[1]]), -1), collapse = "_"))))
    
    for(i in vars){
      
      f <- list.files(tmpdir, pattern = paste0(i,"_"), full.names = T)
      f <- f[f %in% paste0(tmpdir, "/", paste0(i,"_",1:nrow(pols)), ".tif")]
      
      rast.list <- lapply(f, terra::rast)
      
      rast.list <- terra::sprc(rast.list)
      rast.mosaic <- terra::mosaic(rast.list)
      
      rast.mosaic <- terra::crop(rast.mosaic, roi)
      
      names(rast.mosaic) <- i
      
      terra::writeRaster(round(rast.mosaic), paste0(var_dir, "/", area_name, "/",i,".tif"),
                         datatype = dataType(raster(f[1])), overwrite = T)
      unlink(list.files(tmpDir(), full.names = T))
      unlink(f)
    }
    
    unlink(list.files(tmpdir, pattern = "tif$", full.names = T))
    
  }
}

########################################################################
# The same with manually downloaded LiDAR data for areas with missing data

# Combine extentes of downloaded data
liddir <- "/scratch/project_2007415/microclim_lidar"

f <- list.files(liddir, pattern = ".laz$", recursive = T, full.names = T)

for(i in f){
  lass <- readLAS(i)
  p <- st_bbox(lass) %>% 
    st_as_sfc() %>% 
    st_as_sf() %>% 
    rename(geom = x) %>% 
    mutate(path = i)
  
  if(first(f) == i){
    latest <- p
  } else {
    latest <- bind_rows(latest, p)
  }
}

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area %in% c("ULV","OUL"))

for(area_name in rois$area){
  # area_name <- "OUL"
  
  print(area_name)
  
  unlink(list.files(tmpdir, pattern = "tif$", full.names = T))
  
  roi <- rois %>%
    filter(area == area_name) %>%
    st_transform(crs = st_crs(latest))
  
  pols <- st_make_grid(roi, cellsize = 3000) %>% 
    st_as_sf() %>% 
    mutate(id = 1:nrow(.))
  
  for(i in pols$id){
    # i <- 6
    
    roi_t <- pols %>% filter(id == i) %>% 
      st_buffer(10)
    
    latest_t <- latest[roi_t,]
    
    if(nrow(latest_t) > 0){
      
      lass <- readLAS(latest_t$path)
      lidR::st_crs(lass) <- sf::st_crs(roi_t)$epsg
      
      # CLIP THE LAS
      lass <- clip_roi(lass, roi_t)
      
      if(!is.null(lass)){
        if(lass@header@PHB$`Number of point records` > 1000){
          
          dtm <- grid_terrain(lass, 1, tin())
          
          lass <- normalize_height(lass, tin(), na.rm = TRUE)
          
          lass <- filter_poi(lass, Z <= max_z)
          
          chm <- grid_canopy(lass, res = 1, algorithm = dsmtin(max_edge = 5))
          
          chm[chm < 0] <- 0
          chm[is.na(chm)] <- 0
          
          roi_t <- pols %>% filter(id == i)
          
          chm <- crop(chm, roi_t, snap = "out")
          
          writeRaster(round(chm*100), paste0(tmpdir, "/lidR_chm_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          
          # CANOPY METRICS
          
          myMetrics = function (z, th = 2) 
          {
            n <- length(z)
            pzabovex <- lapply(th, function(x) {
              sum(z >= x)/n * 100
            })
            names(pzabovex) <- paste0("pzabove", th)
            
            metrics <- c(pzabovex)
            return(metrics)
          }
          
          mp <- grid_metrics(lass, ~myMetrics(Z, th = c(1,2,3,4,5,7,10,15,20)), res = 4)
          mp <- crop(mp, roi_t, snap = "out")
          
          writeRaster(round(mp[["pzabove1"]]), paste0(tmpdir, "/lidR_pzabove1_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove2"]]), paste0(tmpdir, "lidR_pzabove2_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove3"]]), paste0(tmpdir, "/lidR_pzabove3_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove4"]]), paste0(tmpdir, "/lidR_pzabove4_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove5"]]), paste0(tmpdir, "/lidR_pzabove5_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove7"]]), paste0(tmpdir, "/lidR_pzabove7_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove10"]]), paste0(tmpdir, "/lidR_pzabove10_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove15"]]), paste0(tmpdir, "/lidR_pzabove15_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(mp[["pzabove20"]]), paste0(tmpdir, "/lidR_pzabove20_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          
          
          # Density of points
          density <- grid_metrics(lass, ~sum(ReturnNumber == 1)/100, 10) # calculate density
          density <- crop(density, roi_t, snap = "out")
          
          writeRaster(round(density*100), paste0(tmpdir, "/lidR_density_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          
          myMetrics <- function (z, i) 
          {
            n <- length(z)
            zmax <- max(z)
            zmean <- mean(z)
            zsd = sd(z)
            zwimean = sum(z*i)/sum(i) # Mean elevation weighted by intensities
            zimean  = mean(z*i)       # Mean products of z by intensity
            zsqmean = sqrt(mean(z^2)) # Quadratic mean
            pzabovemean <- sum(z > zmean)/n * 100
            
            metrics <- list(zmax = zmax, zmean = zmean, zsd = zsd, pzabovemean = pzabovemean,
                            zwimean = zwimean,zimean = zimean, zsqmean = zsqmean,
                            zskew = (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2), 
                            zkurt = n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2))
            return(metrics)
          }
          
          metrics = grid_metrics(lass, ~myMetrics(Z, Intensity), 4)
          metrics <- crop(metrics, roi_t, snap = "out")
          
          metrics[["zmax"]][metrics[["zmax"]] < 0] <- 0
          metrics[["zmean"]][metrics[["zmean"]] < 0] <- 0
          metrics[["zwimean"]][metrics[["zwimean"]] < 0] <- 0
          metrics[["zimean"]][metrics[["zimean"]] < 0] <- 0
          
          metrics[["pzabovemean"]][metrics[["zmean"]] < 1] <- 0
          metrics[["zskew"]][metrics[["zmean"]] < 1] <- 0
          metrics[["zkurt"]][metrics[["zmean"]] < 1] <- 0
          
          
          writeRaster(round(metrics[["zmax"]]*100), paste0(tmpdir, "/lidR_zmax_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          writeRaster(round(metrics[["zmean"]]*100), paste0(tmpdir, "/lidR_zmean_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          writeRaster(round(metrics[["zsd"]]*100), paste0(tmpdir, "/lidR_zsd_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          writeRaster(round(metrics[["pzabovemean"]]), paste0(tmpdir, "/lidR_pzabovemean_",i,".tif"),
                      format = "GTiff", datatype = "INT1U", overwrite = T)
          writeRaster(round(metrics[["zwimean"]]*100), paste0(tmpdir, "/lidR_zwimean_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          writeRaster(round(metrics[["zimean"]]), paste0(tmpdir, "/lidR_zimean_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          writeRaster(round(metrics[["zsqmean"]]*100), paste0(tmpdir, "/lidR_zsqmean_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          writeRaster(round(metrics[["zskew"]]*100), paste0(tmpdir, "/lidR_zskew_",i,".tif"),
                      format = "GTiff", datatype = "INT2S", overwrite = T)
          writeRaster(round(metrics[["zkurt"]]*100), paste0(tmpdir, "/lidR_zkurt_",i,".tif"),
                      format = "GTiff", datatype = "INT2U", overwrite = T)
          
        } else {
          print("No layers!!!")
        }
      } else {
        print("No layers!!!")
      }
    } else {
      print("No layers!!!")
    }
  }
  
  #################################################################
  # Merge files
  vars <- unique(unlist(lapply(list.files(tmpdir, pattern = "tif$"), function(x) paste(head(unlist(str_split(x,"_")[[1]]), -1), collapse = "_"))))
  
  for(i in vars){
    
    f <- list.files(tmpdir, pattern = paste0(i,"_"), full.names = T)
    f <- f[f %in% paste0(tmpdir, "/", paste0(i,"_",1:nrow(pols)), ".tif")]
    
    rast.list <- lapply(f, terra::rast)
    
    rast.list <- terra::sprc(rast.list)
    rast.mosaic <- terra::mosaic(rast.list)
    
    rast.mosaic <- terra::crop(rast.mosaic, roi)
    
    names(rast.mosaic) <- i
    
    terra::writeRaster(round(rast.mosaic), paste0(var_dir, "/", area_name, "/",i,".tif"),
                       datatype = dataType(raster(f[1])), overwrite = T)
    unlink(list.files(tmpDir(), full.names = T))
    unlink(f)
  }
  
  unlink(list.files(tmpdir, pattern = "tif$", full.names = T))
  
}
