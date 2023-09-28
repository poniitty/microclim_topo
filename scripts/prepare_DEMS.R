library(terra)
library(tidyverse)
library(sf)

latest <- st_read("/appl/data/geo/mml/dem2m/2008_latest/dem2m.shp")

dem_dir <- "/scratch/project_2007415/microclim_topo/"

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area != "RAS")

# Create subfolders
for(area_name in rois$area){
  if(!dir.exists(paste0(dem_dir,"/",area_name))){
    dir.create(paste0(dem_dir,"/",area_name))
  }
}

# Mosaic 2m dems
for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name) %>%
    st_transform(crs = st_crs(latest))
  
  roi_t <- roi %>% 
    st_buffer(500)
  
  latest_t <- latest[roi_t,]
  
  #################################################################
  # Merge files
  
  rast.list <- lapply(latest_t$path, rast)
  
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list) %>% 
    round(., 3)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  names(rast.mosaic) <- "dem2m"
  
  # plot(rast.mosaic)
  
  writeRaster(rast.mosaic, paste0(dem_dir,"/", area_name, "/dem2m.tif"),
              overwrite = T)
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

# 10 m

latest <- st_read("/appl/data/geo/mml/dem10m/2019/dem10m.shp")

for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name) %>%
    st_transform(crs = st_crs(latest))
  
  roi_t <- roi %>% 
    st_buffer(3000)
  
  latest_t <- latest[roi_t,]
  
  #################################################################
  # Merge files
  
  rast.list <- lapply(latest_t$path, rast)
  
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list) %>% 
    round(., 3)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  names(rast.mosaic) <- "dem10m"
  
  # plot(rast.mosaic)
  
  writeRaster(rast.mosaic, paste0(dem_dir,"/", area_name, "/dem10m.tif"),
              overwrite = T)
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}
