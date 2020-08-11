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
#  10. MS4 areas
MS4_area <- st_read("/Users/yang/Documents/GIS/NYC/elements/MS4OpenData.gdb/", layer = "MS4DRAINAGEAREAS")
#  11. MS4 outfalls
MS4_outfall <- st_read("/Users/yang/Documents/GIS/NYC/elements/MS4OpenData.gdb/", layer = "MS4OUTFALLS")
#  12. drainage area type
drainage_type <- st_read("/Users/yang/Documents/GIS/NYC/drainage/combined_separate_sewer/combined_separate_sewer.shp")
#  13. CSO
cso <- st_read("/Users/yang/Documents/GIS/NYC/drainage/CSOs_2019/CSOs_2019.shp")
#  14. CSOshed
cso_shed <- st_read("/Users/yang/Documents/GIS/NYC/drainage/DEP_CSOsheds/DEP_CSOsheds.shp")
#  15. subsewershed
subsewershed <- st_read("/Users/yang/Documents/GIS/NYC/drainage/Subsewersheds/Subsewersheds.shp")


# Preprocessess -----------------------------------------------------------

# reprojecting
ref_crs <- st_crs(land_cover)
gi_point <- st_transform(gi_point, ref_crs)
sewershed <- st_transform(sewershed, ref_crs)

# elements
building <- st_transform(building, ref_crs) #1
curb <- st_transform(curb, ref_crs) #2
foreverwild <- st_transform(foreverwild, ref_crs) #3
hydrography <- st_transform(hydrography, ref_crs) #4
catch_basin <- st_transform(catch_basin, ref_crs) #5
outfall <- st_transform(outfall, ref_crs) #6
park <- st_transform(park, ref_crs) #7
road <- st_transform(road, ref_crs) #8
sidewalk <- st_transform(sidewalk, ref_crs) #9
MS4_area <- st_transform(MS4_area, ref_crs) #10
MS4_outfall <- st_transform(MS4_outfall, ref_crs) #11
drainage_type <- st_transform(drainage_type, ref_crs) #12
cso <- st_transform(cso, ref_crs) #13
cso_shed <- st_transform(cso_shed, ref_crs) #13
subsewershed <- st_transform(subsewershed, ref_crs) #13

