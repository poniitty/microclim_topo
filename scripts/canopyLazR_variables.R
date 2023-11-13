install.packages("lidR", lib = "/projappl/project_2003061/Rpackages/")
# Install canopyLazR from GitHub
# devtools::install_github("akamoske/canopyLazR", lib = "/projappl/project_2003061/Rpackages/")
# install.packages("rlas", lib = "/projappl/project_2003061/Rpackages/")
# install.packages("uuid", lib = "/projappl/project_2003061/Rpackages/")
# Load the library
library(canopyLazR, lib.loc = "/projappl/project_2003061/Rpackages/")
library(sf)
library(raster)
library(tidyverse)

tmpdir <- tempdir()

latest <- st_read("/appl/data/geo/mml/laserkeilaus/2008_latest/2008_latest.shp")
epsg <- st_crs(latest_t)$epsg

var_dir <- "/scratch/project_2007415/microclim_topo/"

# Prepocess study area polygons
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area != "RAS")

# rois <- rois %>% filter(area != "KIL") %>% filter(area != "RAS")

for(area_name in rois$area){
  # area_name <- "VAR"
  unlink(list.files(tmpdir, pattern = "tif$", full.names = T))
  
  roi <- rois %>%
    filter(area == area_name) %>%
    st_transform(crs = st_crs(latest))
  
  pols <- st_make_grid(roi, cellsize = 3000) %>% 
    st_as_sf() %>% 
    mutate(id = 1:nrow(.))
  
  for(i in pols$id){
    # i <- 1
    print(area_name)
    print(i)
    
    roi_t <- pols %>% filter(id == i) %>% 
      st_buffer(10)
    
    latest_t <- latest[roi_t,]
    
    if(nrow(latest_t) > 0){
      
      # Convert .laz or .las file into a voxelized lidar array
      laz.data <- laz.to.array(laz.file.path = latest_t$path, 
                               voxel.resolution = 10, 
                               z.resolution = 1,
                               use.classified.returns = TRUE)
      
      # Level the voxelized array to mimic a canopy height model
      level.canopy <- canopy.height.levelr(lidar.array = laz.data)
      
      # Estimate LAD for each voxel in leveled array
      lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy, 
                                   voxel.height = 1, 
                                   beer.lambert.constant = NULL)
      
      # Convert the LAD array into a single raster stack
      lad.raster <- lad.array.to.raster.stack(lad.array = lad.estimates, 
                                              laz.array = laz.data, 
                                              epsg.code = epsg)
      
      # Create a single LAI raster from the LAD raster stack
      lai.raster <- raster::calc(lad.raster, fun = sum, na.rm = TRUE)
      
      # Convert the list of LAZ arrays into a ground and canopy height raster
      grd.can.rasters <- array.to.ground.and.canopy.rasters(laz.data, epsg)
      
      # Calculate max LAD and height of max LAD
      max.lad <- lad.ht.max(lad.array = lad.estimates, 
                            laz.array = laz.data, 
                            ht.cut = 5, 
                            epsg.code = epsg)
      
      # Calculate the ratio of filled and empty voxels in a given column of the canopy
      empty.filled.ratio <- canopy.porosity.filled.ratio(lad.array = lad.estimates,
                                                         laz.array = laz.data,
                                                         ht.cut = 5,
                                                         epsg.code = epsg)
      
      # Calculate the volume of filled and empty voxles in a given column of the canopy
      empty.filled.volume <- canopy.porosity.filled.volume(lad.array = lad.estimates,
                                                           laz.array = laz.data,
                                                           ht.cut = 5,
                                                           xy.res = 10,
                                                           z.res = 1,
                                                           epsg.code = epsg)
      
      # Calculate the within canopy rugosity
      within.can.rugosity <- rugosity.within.canopy(lad.array = lad.estimates,
                                                    laz.array = laz.data,
                                                    ht.cut = 5,
                                                    epsg.code = epsg)
      
      # Calculate the heights of various LAD quantiles
      ht.quantiles <- lad.quantiles(lad.array = lad.estimates,
                                    laz.array = laz.data,
                                    ht.cut = 5,
                                    epsg.code = epsg)
      
      # Calculate various canopy volume metrics from Lefsky
      can.volume <- canopy.volume(lad.array = lad.estimates,
                                  laz.array = laz.data,
                                  ht.cut = 5,
                                  xy.res = 10,
                                  z.res = 1,
                                  epsg.code = epsg)
      
      # We can calculate the depth of the euphotic zone by dividing by the volume of the voxel
      euphotic.depth <- can.volume$euphotic.volume.column.raster / ( 10 * 10 * 1)
      
      # Calculate the top of canopy rugosity volume
      toc.rugos <- toc.rugosity(chm.raster = grd.can.rasters$chm.raster,
                                xy.res = 10,
                                z.res = 1)
      
      # Plot the lai raster
      plot(lai.raster)
      writeRaster(round(lai.raster*100), paste0(tmpdir, "/canopyLazR_LAI_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      
      # Plot the canopy height model raster
      plot(grd.can.rasters$chm.raster)
      writeRaster(round(grd.can.rasters$chm.raster*100), paste0(tmpdir, "/canopyLazR_CHM_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      # Plot the max LAD raster
      plot(max.lad$max.lad.raster)
      writeRaster(round(max.lad$max.lad.raster*1000), paste0(tmpdir, "/canopyLazR_maxLAD_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      # Plot the height of max LAD raster
      plot(max.lad$max.lad.ht.raster)
      writeRaster(round(max.lad$max.lad.ht.raster*100), paste0(tmpdir, "/canopyLazR_HmaxLAD_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      
      # Plot filled voxel volume raster
      plot(empty.filled.volume$filled.raster)
      writeRaster(round(empty.filled.volume$filled.raster), paste0(tmpdir, "/canopyLazR_filledVoxelVolume_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      # Plot the height of the 10th quantile
      plot(ht.quantiles$quantile.10.raster)
      writeRaster(round(ht.quantiles$quantile.10.raster)*100, paste0(tmpdir, "/canopyLazR_H10quant_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      # Plot the height of the 25th quantile
      plot(ht.quantiles$quantile.25.raster)
      writeRaster(round(ht.quantiles$quantile.25.raster)*100, paste0(tmpdir, "/canopyLazR_H25quant_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      # Plot the height of the 50th quantile
      plot(ht.quantiles$quantile.50.raster)
      writeRaster(round(ht.quantiles$quantile.10.raster)*100, paste0(tmpdir, "/canopyLazR_H10quant_",i,".tif"),
                  format = "GTiff", datatype = "INT2U", overwrite = T)
      
      # Plot the height of the 75th quantile
      plot(ht.quantiles$quantile.75.raster)
      
      # Plot the height of the 90th quantile
      plot(ht.quantiles$quantile.90.raster)
      
      # Plot the height of the mean LAD
      plot(ht.quantiles$mean.raster)
      
      # Plot the volume of the euphotic zone for each column
      plot(can.volume$euphotic.volume.column.raster)
      
      # Plot the total leaf area in the euphotic zone for each column
      plot(can.volume$euphotic.tla.column.raster)
      
      # Plot the depth of the euphotic zone
      plot(euphotic.depth)
      
      # Plot the volume of the oligophotic zone for each column
      plot(can.volume$oligophotic.volume.column.raster)
      
      # Plot the total leaf area in the oligophotic zone for each column
      plot(can.volume$oligophotic.tla.column.raster)
      
      # Plot the volume of the empty space within a 3x3 moving window
      plot(can.volume$empty.canopy.volume.raster)
      
      # Plot the volume of the euphotic zone within a 3x3 moving window
      plot(can.volume$filled.canopy.euphotic.raster)
      
      # Plot the volume of the oligophotic zone within a 3x3 moving window
      plot(can.volume$filled.canopy.oligophotic.raster)
      
      # Plot the total leaf area of the euphotic zone within a 3x3 moving window
      plot(can.volume$filled.canopy.euphotic.tla.raster)
      
      # Plot the total leaf area of the oligophotic zone within a 3x3 moving window
      plot(can.volume$filled.canopy.oligophotic.tla.raster)
      
      # Plot the top of canopy rugosity volume
      plot(toc.rugos)
      
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
    
    
    rast.list <- lapply(f, raster)
    rast.list$fun <- mean
    rast.list$tolerance <- 0.5
    rast.mosaic <- do.call(mosaic,rast.list)
    
    names(rast.mosaic) <- i
    
    writeRaster(rast.mosaic, paste0(var_dir, "/", area_name, "/",i,".tif"),
                format = "GTiff", datatype = dataType(raster(f[1])), overwrite = T)
    
  }
  
  unlink(list.files(tmpdir, pattern = "tif$", full.names = T))
}
