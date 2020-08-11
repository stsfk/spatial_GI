library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(tidyverse)

# Data --------------------------------------------------------------------

# GI
gi_point <- st_read("/Users/yang/Documents/GIS/NYC/DEP_GI_Assets_Public/DEP_GI_Assets_Public.shp")

# land cover
land_cover <- raster::raster("/Users/yang/Documents/GIS/NYC/Land_Cover/NYC_2017_LiDAR_LandCover.img")

# sewershed
sewershed <- st_read("/Users/yang/Documents/GIS/NYC/drainage/Sewershed/Sewershed.shp")

# Elements
#  1. building
building <- st_read("/Users/yang/Documents/GIS/NYC/elements/Building/building.shp")
#  2. curb
curb <- st_read("/Users/yang/Documents/GIS/NYC/elements/CURB/CURB.shp")
#  3. foreverwild
foreverwild <- st_read("/Users/yang/Documents/GIS/NYC/elements/ForeverWild_20170801/ForeverWild_20170801.shp")
#  4. hydrography
hydrography <- st_read("/Users/yang/Documents/GIS/NYC/elements/HYDRO/HYDROGRAPHY.shp")
#  5. catch basin
catch_basin <- st_read("/Users/yang/Documents/GIS/NYC/elements/NYC_Catch_Basins/NYC_Catch_Basins/NYCDEP_Catch_Basins.shp")
#  6. outfall
outfall <- st_read("/Users/yang/Documents/GIS/NYC/elements/NYC_Outfalls/NYCDEP_Outfalls.shp")
#  7. park
park <- st_read("/Users/yang/Documents/GIS/NYC/elements/PARK/PARK.shp")
#  8. road
road <- st_read("/Users/yang/Documents/GIS/NYC/elements/ROADBED/ROADBED.shp")
#  9. sidewalk
sidewalk <- st_read("/Users/yang/Documents/GIS/NYC/elements/SIDEWALK/SIDEWALK.shp")
#  10. MS4
MS4 <- st_read("/Users/yang/Documents/GIS/NYC/elements/MS4OpenData.gdb/")


# Preprocessess -----------------------------------------------------------



# reproject

