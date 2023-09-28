library(Rsagacmd)
library(terra)
library(sf)
library(tidyverse)

saga_version("saga_cmd")
saga <- saga_gis(cores = future::availableCores(),
                 raster_backend = "terra")

dem_dir <- "/scratch/project_2007415/microclim_topo/"
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area != "RAS")

for(i in rois$area){
  # i <- "ULV"
  print(i)
  
  # read DEMs
  dem2 <- rast(paste0(dem_dir, "/", i, "/dem2m.tif"))
  dem10 <- rast(paste0(dem_dir, "/", i, "/dem10m.tif"))
  
  dem2_filled <- saga$ta_preprocessor$fill_sinks_xxl_wang_liu(elev = dem2, minslope = 0.01)
  
  # SWI
  if(!file.exists(paste0(dem_dir, "/", i, "/swi_suction256.tif"))){
    print("Calculating SAGA wetness indices...")
    # SWI, suction 2
    swi <- saga$ta_hydrology$saga_wetness_index(dem = dem2_filled, suction = 2, 
                                                area_type = 2, slope_type = 0,)
    writeRaster(round(swi$twi*100), paste0(dem_dir, "/", i, "/swi_suction2.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # SWI, suction 16
    swi <- saga$ta_hydrology$saga_wetness_index(dem = dem2_filled, suction = 16, 
                                                area_type = 2, slope_type = 0,)
    writeRaster(round(swi$twi*100), paste0(dem_dir, "/", i, "/swi_suction16.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # SWI, suction 64
    swi <- saga$ta_hydrology$saga_wetness_index(dem = dem2_filled, suction = 64, 
                                                area_type = 2, slope_type = 0,)
    writeRaster(round(swi$twi*100), paste0(dem_dir, "/", i, "/swi_suction64.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # SWI, suction 128
    swi <- saga$ta_hydrology$saga_wetness_index(dem = dem2_filled, suction = 128, 
                                                area_type = 2, slope_type = 0,)
    writeRaster(round(swi$twi*100), paste0(dem_dir, "/", i, "/swi_suction128.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # SWI, suction 256
    swi <- saga$ta_hydrology$saga_wetness_index(dem = dem2_filled, suction = 256, 
                                                area_type = 2, slope_type = 0,)
    writeRaster(round(swi$twi*100), paste0(dem_dir, "/", i, "/swi_suction256.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
  }
  
  
  # SLOPE 2m
  if(!file.exists(paste0(dem_dir, "/", i, "/aspect.tif"))){
    slp <- saga$ta_morphometry$slope_aspect_curvature(elevation = dem2)
    slp$slope <- 180/pi*slp$slope
    slp$aspect <- 180/pi*slp$aspect
    names(slp$slope) <- "slope"
    names(slp$aspect) <- "aspect"
    writeRaster(round(slp$slope*100), paste0(dem_dir, "/", i, "/slope.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    writeRaster(round(slp$aspect*100), paste0(dem_dir, "/", i, "/aspect.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  
  # TPI
  if(!file.exists(paste0(dem_dir, "/", i, "/tpi1000.tif"))){
    print("Calculating TPIs...")
    # 10
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem2, radius_max = 10)
    names(tpi) <- "tpi10"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi10.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 20
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem2, radius_max = 20)
    names(tpi) <- "tpi20"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi20.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 30
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem2, radius_max = 30)
    names(tpi) <- "tpi30"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi30.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 50
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem2, radius_max = 50)
    names(tpi) <- "tpi50"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi50.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 100
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem10, radius_max = 100)
    names(tpi) <- "tpi100"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi100.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 200
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem10, radius_max = 200)
    names(tpi) <- "tpi200"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi200.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 300
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem10, radius_max = 300)
    names(tpi) <- "tpi300"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi300.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    # 500
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem10, radius_max = 500)
    names(tpi) <- "tpi500"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi500.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT4S")
    # 1000
    tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem10, radius_max = 1000)
    names(tpi) <- "tpi1000"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/tpi1000.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT4S")
  }
  
  # Multi-scale TPI
  if(!file.exists(paste0(dem_dir, "/", i, "/multi_tpi50.tif"))){
    # Multi-scale TPI local
    tpi <- saga$ta_morphometry$multi_scale_topographic_position_index_tpi(dem = dem2, 
                                                                          scale_max = 50)
    names(tpi) <- "multi_tpi50"
    writeRaster(round(tpi*100), paste0(dem_dir, "/", i, "/multi_tpi50.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
  }
  
  
  # Wind indices
  if(!file.exists(paste0(dem_dir, "/", i, "/windexp5000.tif"))){
    print("Calculating wind indices...")
    # 500 m
    wind <- saga$ta_morphometry$wind_exposition_index(dem = dem2, maxdist = 0.5,
                                                      step = 30)
    names(wind) <- "windexp500"
    writeRaster(round(wind*1000), paste0(dem_dir, "/", i, "/windexp500.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    # 1000 m
    wind <- saga$ta_morphometry$wind_exposition_index(dem = dem2, maxdist = 1,
                                                      step = 30)
    names(wind) <- "windexp1000"
    writeRaster(round(wind*1000), paste0(dem_dir, "/", i, "/windexp1000.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    # 5000 m
    wind <- saga$ta_morphometry$wind_exposition_index(dem = dem10, maxdist = 5,
                                                      step = 30)
    names(wind) <- "windexp5000"
    writeRaster(round(wind*1000), paste0(dem_dir, "/", i, "/windexp5000.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  
  # Diurnal Anisotropic Heat
  if(!file.exists(paste0(dem_dir, "/", i, "/diurnalaniheat.tif"))){
    ha <- saga$ta_morphometry$diurnal_anisotropic_heat(dem = dem10)
    names(ha) <- "diurnalaniheat"
    writeRaster(round(ha*1000), paste0(dem_dir, "/", i, "/diurnalaniheat.tif"),
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
  }
  
  # Mass balance index
  if(!file.exists(paste0(dem_dir, "/", i, "/mbi.tif"))){
    ha <- saga$ta_morphometry$mass_balance_index(dem = dem10)
    names(ha) <- "mbi"
    writeRaster(round(ha*1000), paste0(dem_dir, "/", i, "/mbi.tif"),
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
  }
  
  # Relative Heights and Slope Positions
  if(!file.exists(paste0(dem_dir, "/", i, "/mid_slope_position.tif"))){
    ha <- saga$ta_morphometry$relative_heights_and_slope_positions(dem = dem10)
    names(ha$ho) <- "slope_height"
    writeRaster(round(ha$ho*100), paste0(dem_dir, "/", i, "/slope_height.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    names(ha$hu) <- "valley_depth"
    writeRaster(round(ha$hu*100), paste0(dem_dir, "/", i, "/valley_depth.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    names(ha$nh) <- "norm_height"
    writeRaster(round(ha$nh*1000), paste0(dem_dir, "/", i, "/norm_height.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    names(ha$sh) <- "stand_height"
    writeRaster(round(ha$sh*100), paste0(dem_dir, "/", i, "/stand_height.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT4U")
    names(ha$ms) <- "mid_slope_position"
    writeRaster(round(ha$ms*1000), paste0(dem_dir, "/", i, "/mid_slope_position.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  
  # Terrain ruggedness index
  if(!file.exists(paste0(dem_dir, "/", i, "/tri10.tif"))){
    ha <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem = dem2, radius = 10)
    names(ha) <- "tri10"
    writeRaster(round(ha*100), paste0(dem_dir, "/", i, "/tri10.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  
  # Vector ruggedness index
  if(!file.exists(paste0(dem_dir, "/", i, "/vri10.tif"))){
    ha <- saga$ta_morphometry$vector_ruggedness_measure_vrm(dem = dem2, radius = 10)
    names(ha) <- "vri10"
    writeRaster(round(ha*10000), paste0(dem_dir, "/", i, "/vri10.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  
  # Morphometric Protection Index
  if(!file.exists(paste0(dem_dir, "/", i, "/mpi2000.tif"))){
    ha <- saga$ta_morphometry$morphometric_protection_index(dem = dem10)
    names(ha) <- "mpi2000"
    writeRaster(round(ha*10000), paste0(dem_dir, "/", i, "/mpi2000.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  # Upslope and Downslope Curvature
  if(!file.exists(paste0(dem_dir, "/", i, "/curv_downslope_local.tif"))){
    ha <- saga$ta_morphometry$upslope_and_downslope_curvature(dem = dem10)
    names(ha$c_local) <- "curv_local"
    writeRaster(round(ha$c_local*1000), paste0(dem_dir, "/", i, "/curv_local.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    names(ha$c_up) <- "curv_upslope"
    writeRaster(round(ha$c_up*1000), paste0(dem_dir, "/", i, "/curv_upslope.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    names(ha$c_up_local) <- "curv_upslope_local"
    writeRaster(round(ha$c_up_local*1000), paste0(dem_dir, "/", i, "/curv_upslope_local.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    names(ha$c_down) <- "curv_downslope"
    writeRaster(round(ha$c_down*1000), paste0(dem_dir, "/", i, "/curv_downslope.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
    names(ha$c_down_local) <- "curv_downslope_local"
    writeRaster(round(ha$c_down_local*1000), paste0(dem_dir, "/", i, "/curv_downslope_local.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2S")
  }
  
  # Topographic Openness
  if(!file.exists(paste0(dem_dir, "/", i, "/openness_neg.tif"))){
    ha <- saga$ta_lighting$topographic_openness(dem = dem10, radius = 5000)
    names(ha$pos) <- "openness_pos"
    writeRaster(round(ha$pos*1000), paste0(dem_dir, "/", i, "/openness_pos.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
    names(ha$neg) <- "openness_neg"
    writeRaster(round(ha$neg*1000), paste0(dem_dir, "/", i, "/openness_neg.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  
  
  # Valley depth
  if(!file.exists(paste0(dem_dir, "/", i, "/valley_depth.tif"))){
    ha <- saga$ta_channels$valley_depth(elevation = dem10)
    names(ha$valley_depth) <- "valley_depth"
    writeRaster(round(ha$valley_depth*100), paste0(dem_dir, "/", i, "/valley_depth.tif"), 
                filetype = "GTiff", overwrite = T, datatype = "INT2U")
  }
  unlink(list.files(tempdir(), full.names = T))
  
  # Radiation
  if(!file.exists(paste0(dem_dir, "/", i, "/pisr_12.tif"))){
    print("Calculating monthly PISR layers...")
    if(!file.exists(paste0(dem_dir, "/", i, "/svf.tif"))){
      svi <- saga$ta_lighting$sky_view_factor(dem = dem10, radius = 3000)
      svi$svf <- resample(svi$svf, dem2)
      
      names(svi$svf) <- "svf"
      writeRaster(round(svi$svf*1000), paste0(dem_dir, "/", i, "/svf.tif"), 
                  filetype = "GTiff", overwrite = T, datatype = "INT2U")
    }
    
    svf <- rast(paste0(dem_dir, "/", i, "/svf.tif"))/1000
    
    for(month in 1:12){
      if(!file.exists(paste0(dem_dir, "/", i, "/pisr_",month,".tif"))){
        print(month)
        
        unlink(list.files(tempdir(), full.names = T))
        
        rad <- saga$ta_lighting$potential_incoming_solar_radiation(grd_dem = dem2,
                                                                   grd_svf = svf,
                                                                   period = 1, 
                                                                   location = 1,
                                                                   day = paste0("2020-",month,"-15"),
                                                                   hour_step = 1)
        
        writeRaster(round(rad$grd_total*1000), paste0(dem_dir, "/", i, "/pisr_",month,".tif"), 
                    datatype = "INT2U", overwrite = T)
      }
    }
  }
  
  unlink(list.files(tempdir(), full.names = T))
}
