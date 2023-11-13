library(terra)
library(tidyverse)
library(sf)

latest <- st_read("/appl/data/geo/mml/dem2m/2008_latest/dem2m.shp")

dem_dir <- "/scratch/project_2007415/microclim_topo/"

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(!area %in% c("RAS","MAT","VIN"))

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

##################################################################
# Norwegian & Swedish sites

dem_dir <- "/scratch/project_2007415/microclim_topo/"

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area %in% c("RAS","MAT","VIN"))

# Create subfolders
for(area_name in rois$area){
  if(!dir.exists(paste0(dem_dir,"/",area_name))){
    dir.create(paste0(dem_dir,"/",area_name))
  }
}

# Vindelfjellen

area_name <- "VIN"

r <- rast("/scratch/project_2007415/microclim_topo/raw/VIN/hojddata2_mosaik.tif")

roi <- rois %>%
  filter(area == area_name) %>%
  st_transform(crs = st_crs(r))

roi_t <- roi %>% 
  st_buffer(500)

r <- terra::crop(r, roi_t) %>% 
  round(., 3)
names(r) <- "dem2m"
# plot(r)

writeRaster(r, paste0(dem_dir,"/", area_name, "/dem2m.tif"),
            overwrite = T)

unlink(list.files(tempdir(), full.names = T, recursive = T))

# 10m
r <- rast("/scratch/project_2007415/microclim_topo/raw/VIN/hojddata2_mosaik.tif")

roi <- rois %>%
  filter(area == area_name) %>%
  st_transform(crs = st_crs(r))

roi_t <- roi %>% 
  st_buffer(3000)

r <- terra::crop(r, roi_t) %>% 
  round(., 3)
r <- aggregate(r, 5, "mean") %>% 
  round(., 3)
names(r) <- "dem10m"
# plot(r)

writeRaster(r, paste0(dem_dir,"/", area_name, "/dem10m.tif"),
            overwrite = T)

unlink(list.files(tempdir(), full.names = T, recursive = T))

# Rastigaisa

area_name <- "RAS"

r <- rast("/scratch/project_2007415/microclim_topo/raw/RAS/RastiDem_2m.tif")

roi <- rois %>%
  filter(area == area_name) %>%
  st_transform(crs = st_crs(r))

roi_t <- roi %>% 
  st_buffer(500)

r <- terra::crop(r, roi_t) %>% 
  round(., 3)
names(r) <- "dem2m"
# plot(r)

writeRaster(r, paste0(dem_dir,"/", area_name, "/dem2m.tif"),
            overwrite = T)

unlink(list.files(tempdir(), full.names = T, recursive = T))

# 10m
r <- rast("/scratch/project_2007415/microclim_topo/raw/RAS/RastiDem_2m.tif")

roi <- rois %>%
  filter(area == area_name) %>%
  st_transform(crs = st_crs(r))

roi_t <- roi %>% 
  st_buffer(3000)

r <- terra::crop(r, roi_t) %>% 
  round(., 3)
r <- aggregate(r, 5, "mean") %>% 
  round(., 3)
names(r) <- "dem10m"
# plot(r)

writeRaster(r, paste0(dem_dir,"/", area_name, "/dem10m.tif"),
            overwrite = T)

unlink(list.files(tempdir(), full.names = T, recursive = T))


# Mattavarri

area_name <- "MAT"

r <- rast("/projappl/project_2003061/mattavarri_sampling/output/dem.tif")/100

roi <- rois %>%
  filter(area == area_name) %>%
  st_transform(crs = crs(r, proj = T))

roi_t <- roi %>% 
  st_buffer(500)

r <- terra::crop(r, roi_t) %>% 
  round(., 3)
names(r) <- "dem2m"
# plot(r)

writeRaster(r, paste0(dem_dir,"/", area_name, "/dem2m.tif"),
            overwrite = T)

unlink(list.files(tempdir(), full.names = T, recursive = T))

# 10m
r <- rast("/projappl/project_2003061/mattavarri_sampling/output/dem.tif")/100

roi <- rois %>%
  filter(area == area_name) %>%
  st_transform(crs = crs(r, proj = T))

roi_t <- roi %>% 
  st_buffer(3000)

r <- terra::crop(r, roi_t) %>% 
  round(., 3)
r <- aggregate(r, 5, "mean") %>% 
  round(., 3)
names(r) <- "dem10m"
# plot(r)

writeRaster(r, paste0(dem_dir,"/", area_name, "/dem10m.tif"),
            overwrite = T)

unlink(list.files(tempdir(), full.names = T, recursive = T))

