library(sf)
library(tidyverse)

vect_dir <- "/scratch/project_2007415/microclim_vector/"
rois <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area != "RAS")

# Create subfolders
for(area_name in rois$area){
  if(!dir.exists(paste0(vect_dir,"/",area_name))){
    dir.create(paste0(vect_dir,"/",area_name))
  }
}

# Waters

list.files("/appl/data/geo/mml/maastotietokanta/2023/gpkg", full.names = T)
st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-vakavesi_23-03-02.gpkg")
st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-virtavesi_23-03-02.gpkg")

p <- bind_rows(read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-vakavesi_23-03-02.gpkg", "jarvi"),
               read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-virtavesi_23-03-02.gpkg", "virtavesialue"))


for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  waters <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  waters <- waters %>% st_transform(crs = st_crs(aoi))
  
  st_write(waters, paste0(vect_dir, "/", area_name, "/waters.gpkg"), append = FALSE)
}


# Smaller rivers

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-virtavesi_23-03-02.gpkg", "virtavesikapea")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  rivers <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  rivers <- rivers %>% st_transform(crs = st_crs(aoi))
  
  st_write(rivers, paste0(vect_dir, "/", area_name, "/rivers.gpkg"), append = FALSE)
}


# Roads

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-tie_23-03-02.gpkg")

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-tie_23-03-02.gpkg", "tieviiva")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  roads <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  roads <- roads %>% st_transform(crs = st_crs(aoi))
  
  st_write(roads, paste0(vect_dir, "/", area_name, "/roads.gpkg"), append = FALSE)
}


# buildings

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-rakennus_23-03-02.gpkg")

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-rakennus_23-03-02.gpkg", "rakennus")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  buildings <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  buildings <- buildings %>% st_transform(crs = st_crs(aoi))
  
  st_write(buildings, paste0(vect_dir, "/", area_name, "/buildings.gpkg"), append = FALSE)
}


# Wetland

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-suo_23-03-02.gpkg")

p <- bind_rows(read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-suo_23-03-02.gpkg", "suo"),
               read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-suo_23-03-02.gpkg", "soistuma"))

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  wetlands <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  wetlands <- wetlands %>% st_transform(crs = st_crs(aoi))
  
  st_write(wetlands, paste0(vect_dir, "/", area_name, "/wetlands.gpkg"), append = FALSE)
}

# Rocky outcrops

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-kallio_23-03-02.gpkg")

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-kallio_23-03-02.gpkg", "kallioalue")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  rock <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  rock <- rock %>% st_transform(crs = st_crs(aoi))
  
  st_write(rock, paste0(vect_dir, "/", area_name, "/rock.gpkg"), append = FALSE)
}

plot(st_geometry(rock))

# Meadows

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg")

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg", "niitty")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  meadows <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  meadows <- meadows %>% st_transform(crs = st_crs(aoi))
  
  st_write(meadows, paste0(vect_dir, "/", area_name, "/meadows.gpkg"), append = FALSE)
}

# Agricultural fields

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg")

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg", "maatalousmaa")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  fields <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  fields <- fields %>% st_transform(crs = st_crs(aoi))
  
  st_write(fields, paste0(vect_dir, "/", area_name, "/fields.gpkg"), append = FALSE)
}


# boulderfiels

st_layers("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg")

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg", "kivikko")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  boulderfiels <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  boulderfiels <- boulderfiels %>% st_transform(crs = st_crs(aoi))
  
  st_write(boulderfiels, paste0(vect_dir, "/", area_name, "/boulderfiels.gpkg"), append = FALSE)
}

# cliff

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg", "jyrkanne")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  cliff <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  cliff <- cliff %>% st_transform(crs = st_crs(aoi))
  
  st_write(cliff, paste0(vect_dir, "/", area_name, "/cliff.gpkg"), append = FALSE)
}

plot(st_geometry(cliff))

# springs

p <- read_sf("/appl/data/geo/mml/maastotietokanta/2023/gpkg/MTK-muut_23-03-02.gpkg", "lahde")

for(area_name in rois$area){
  print(area_name)
  
  aoi <- rois %>% 
    filter(area == area_name) %>% 
    st_buffer(3000)
  
  springs <- st_crop(p, aoi %>% st_transform(crs = st_crs(p)))
  springs <- springs %>% st_transform(crs = st_crs(aoi))
  
  st_write(springs, paste0(vect_dir, "/", area_name, "/springs.gpkg"), append = FALSE)
}
