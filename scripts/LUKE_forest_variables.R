library(terra)
library(tidyverse)
library(sf)

vmi_dir <- "/scratch/project_2007415/microclim_topo/"

vars <- list.files("/appl/data/geo/luke/vmi/2021/", pattern = "img$", full.names = T)

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area != "RAS") %>% 
  st_transform(crs = 3067)

for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name)
  
  roi_t <- roi %>% 
    st_buffer(3000)
  
  for(var in vars){
    print(var)
    
    r <- rast(var)
    
    r <- crop(r, roi_t)
    r[r > 32765] <- NA
    
    var_name <- gsub("_vmi1x_1721.img", "", tail(str_split(var, "/")[[1]], 1))
    
    # plot(r, main = var_name)
    names(r) <- paste0("VMI_", var_name)
    
    writeRaster(r, paste0(vmi_dir, "/", area_name, "/VMI_",var_name, ".tif"),
                datatype = ifelse(max(values(r), na.rm = T) < 256, "INT1U", "INT2S"),
                filetype = "GTiff", overwrite = T)
    
  }
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

# LUKE TWI

for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name)
  
  roi_t <- roi %>% 
    st_buffer(3000)
  
  r <- rast("/appl/data/geo/luke/twi/TWI_16m_Finland_NA_lakes_int.tif")
  
  r <- crop(r, roi_t)
  
  names(r) <- "luke_TWI"
  
  writeRaster(r, paste0(vmi_dir, "/", area_name, "/luke_TWI.tif"),
              datatype = "INT2U", filetype = "GTiff", overwrite = T)
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

#############################

for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name)
  
  roi_t <- roi %>% 
    st_buffer(500)
  
  # 0.5 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_00_5ha/DTW_00_5ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_00_5ha/", pols$location)
  
  rast.list <- lapply(f, rast)
  
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  # plot(rast.mosaic, main = area_name)
  names(rast.mosaic) <- "luke_dtw_0.5ha"
  
  writeRaster(round(rast.mosaic*100), paste0(vmi_dir, "/", area_name, "/luke_dtw_0.5ha.tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  # 1 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_01_0ha/DTW_01_0ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_01_0ha/", pols$location)
  
  rast.list <- lapply(f, rast)
  
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  # plot(rast.mosaic, main = area_name)
  names(rast.mosaic) <- "luke_dtw_1ha"
  
  writeRaster(round(rast.mosaic*100), paste0(vmi_dir, "/", area_name, "/luke_dtw_1ha.tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  # 4 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_04_0ha/DTW_04_0ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_04_0ha/", pols$location)
  
  rast.list <- lapply(f, rast)
  
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  # plot(rast.mosaic, main = area_name)
  names(rast.mosaic) <- "luke_dtw_4ha"
  
  writeRaster(round(rast.mosaic*100), paste0(vmi_dir, "/", area_name, "/luke_dtw_4ha.tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  # 10 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_10_0ha/DTW_10_0ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_10_0ha/", pols$location)
  
  rast.list <- lapply(f, rast)
  
  rast.list <- sprc(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  # plot(rast.mosaic, main = area_name)
  names(rast.mosaic) <- "luke_dtw_10ha"
  
  writeRaster(round(rast.mosaic*100), paste0(vmi_dir, "/", area_name, "/luke_dtw_10ha.tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}
