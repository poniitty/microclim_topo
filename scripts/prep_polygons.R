library(tidyverse)
library(sf)

list.files("GIS/")

# Pallas

p <- read_csv("GIS/PAL_sites.csv") %>% 
  st_as_sf(coords = c("Easting","Northing"), crs = 3067)

pol <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area == "PAL")

plot(st_geometry(p))
plot(st_geometry(pol), add = T)

rois <- bind_rows(st_read("GIS/area_polygons_all.gpkg") %>% 
            filter(area != "PAL"),
          st_bbox(p) %>% 
            st_as_sfc() %>% 
            st_as_sf() %>% 
            mutate(area = "PAL",
                   full_name = "pallas") %>% 
            rename(geom = x))

# ULV

p <- read_csv("GIS/ULV_sites.csv") %>% 
  st_as_sf(coords = c("Easting","Northing"), crs = 3067)

pol <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area == "ULV")

plot(st_geometry(p))
plot(st_geometry(pol), add = T)

rois <- bind_rows(rois %>% 
                    filter(area != "ULV"),
                  st_bbox(p) %>% 
                    st_as_sfc() %>% 
                    st_as_sf() %>% 
                    mutate(area = "ULV",
                           full_name = "ulvinsalo") %>% 
                    rename(geom = x))

# Oulanks

p <- st_read("GIS/Kemppinen_Niittynen_logger_locations_OULANKA.gpkg") %>% 
  st_transform(crs = 3067)

pol <- st_read("GIS/area_polygons_all.gpkg") %>% 
  filter(area == "OUL")

plot(st_geometry(p))
plot(st_geometry(pol), add = T)

rois <- bind_rows(rois %>% 
                    filter(area != "OUL"),
                  st_bbox(p) %>% 
                    st_as_sfc() %>% 
                    st_as_sf() %>% 
                    mutate(area = "OUL",
                           full_name = "oulanka") %>% 
                    rename(geom = x))


rois %>% st_write("GIS/area_polygons_all.gpkg", append=FALSE)



rois <- bind_rows(st_read("GIS/area_polygons_all.gpkg") %>% 
                    filter(area %in% c("AIL","MAL","SAA","RAR")) %>% 
                    st_transform(crs = st_crs(latest)) %>% 
                    st_bbox() %>% 
                    st_as_sfc() %>% st_as_sf() %>% 
                    mutate(area = "KIL",
                           full_name = "kilpisjarvi") %>% 
                    rename(geom = x),
                  st_read("GIS/area_polygons_all.gpkg") %>% 
                    filter(!(area %in% c("AIL","MAL","SAA","RAR"))) %>% 
                    st_transform(crs = st_crs(latest)))

rois <- bind_rows(rois,
                  st_read("GIS/evo.gpkg") %>% 
                    mutate(area = "EVO", full_name = "evo") %>% 
                    st_transform(crs = st_crs(latest)),
                  st_read("GIS/oulanka.gpkg") %>% 
                    mutate(area = "OUL", full_name = "oulanka") %>% 
                    st_transform(crs = st_crs(latest)) %>% 
                    rename(geom = geometry),
                  st_read("GIS/pallas.gpkg") %>% 
                    mutate(area = "PAL", full_name = "pallas") %>% 
                    st_transform(crs = st_crs(latest)),
                  st_read("GIS/ulvinsalo.gpkg") %>% 
                    mutate(area = "ULV", full_name = "ulvinsalo") %>% 
                    st_transform(crs = st_crs(latest)))

rois %>% st_write("GIS/area_polygons_all.gpkg", append=FALSE)


################################################################
# VIN & MAT

rois <- bind_rows(st_read("/projappl/project_2007415/repos/MICROCLIMATES/data/site_coordinates.gpkg") %>% 
                    filter(area == "VIN") %>% 
                    st_transform(3067) %>% 
                    st_bbox() %>% 
                    st_as_sfc() %>% st_as_sf() %>% 
                    mutate(area = "VIN",
                           full_name = "vindelfjell") %>% 
                    rename(geom = x),
                  st_read("/projappl/project_2007415/repos/MICROCLIMATES/data/site_coordinates.gpkg") %>% 
                    filter(area == "MAT") %>% 
                    st_transform(3067) %>% 
                    st_bbox() %>% 
                    st_as_sfc() %>% st_as_sf() %>% 
                    mutate(area = "MAT",
                           full_name = "mattavarri") %>% 
                    rename(geom = x),
                  st_read("GIS/area_polygons_all.gpkg") %>% 
                    st_transform(3067))

rois %>% st_write("GIS/area_polygons_all.gpkg", append=FALSE)
