library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

library(tidyverse)


# Data --------------------------------------------------------------------

gi_point <- st_read("/Users/yang/Documents/GIS/NYC/DEP_GI_Assets_Public/DEP_GI_Assets_Public.shp")

land_cover <- raster::raster("/Users/yang/Documents/GIS/NYC/Land_Cover/NYC_2017_LiDAR_LandCover.img")

sewershed <- st_read("/Users/yang/Documents/GIS/NYC/drainage/Sewershed/Sewershed.shp")


# Preprocessing -----------------------------------------------------------

gi_point <- gi_point %>%
  mutate(sewershed = st_within(gi_point, sewershed) %>% unlist())

# Plot --------------------------------------------------------------------

gi_point$Sewer_Type %>% table()

# Analyze distance between points -----------------------------------------

gi_distance_m <- st_distance(gi_point)



# Raster-vector interactions ----------------------------------------------

gi_point_buffer <- gi_point[1,] %>% 
  st_buffer(10)

mask(land_cover, gi_point_buffer)

raster::extract(land_cover, gi_point[1:5,], buffer = 1000)


