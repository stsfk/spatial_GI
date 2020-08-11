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

# Group GIs into two types
# ROWEB unknown
gi_type <- gi_point %>% 
  count(Asset_Type) %>%
  arrange(desc(n)) %>%
  pull(Asset_Type)

green_gi_type <- c(
  "ROWB", "ROWGS", "ROWRG", "ROWSGS", "Rain Garden", 
  "ROW Infiltration Basin with Grass Top", "Green Roof", "ROW Infiltration Basin with Combination of Concrete and Grass Top", "Engineered Soil Tree Pit", "Rooftop Farm",
  "Combined Blue/Green Roof"
)

other_gi_type <- setdiff(gi_type, green_gi_type)

gi_point <- gi_point %>%
  mutate(green = Asset_Type %in% green_gi_type)

# GIs for later analysis
gi_full <- gi_point
gi_green <- gi_point %>%
  dplyr::filter(green)
gi_other <- gi_point %>%
  dplyr::filter(!green)

gi <- gi_green # subset selected for analysis

# nature area -------------------------------------------------------------


# drainage ----------------------------------------------------------------

# preprocess: correct names
cso_outfall_mismatched <- setdiff(cso_shed$PRIMARY_OU, cso$outfall_id)
cso_outfall_new <- c("BB-006", "BB-019", "BB-020", "BB-031", "BB-038", "BB-044", "BB-046", "BB-047", "HP-839", "JAM-003A", "NCB-019", "NCM-050", 
                     "NCQ-027", "NR-043", "OH-061", "OH-063", "OH-066", "OH-023", "OH-606", "TI-074", "TI-075", "TI-076", "TI-013", "TI-604",    
                     "TI-621", "WI-073", "WIM-028", "WIM-029")

for (i in seq_along(cso_outfall_mismatched)){
  cso_shed$PRIMARY_OU[cso_shed$PRIMARY_OU == cso_outfall_mismatched[i]] <- cso_outfall_new[i]
}

# preprocess: join CSOshed with the same name
cso_shed_2join_outfalls <- cso_shed %>% count(PRIMARY_OU) %>% dplyr::filter(n > 1) %>% pull(PRIMARY_OU)
for (i in seq_along(cso_shed_2join_outfalls)){
  cso_shed_2join_outfall <- cso_shed_2join_outfalls[i]
  
  out <- cso_shed %>%
    dplyr::filter(PRIMARY_OU == cso_shed_2join_outfall) %>%
    summarise(PRIMARY_OU = PRIMARY_OU[1])
  
  cso_shed <- cso_shed %>%
    dplyr::filter(PRIMARY_OU != cso_shed_2join_outfall) %>%
    bind_rows(out)
}

# preprocess: join cso_shed and CSO, drop rows without flow information
cso_process <- cso_shed %>%
  left_join(cso %>% st_drop_geometry(), by = c("PRIMARY_OU" = "outfall_id")) %>%
  drop_na(contains("volume")|contains("events")) # 340 cso_shed with cso record
  
# plot: get CSO number and volume each year
data_plot <- cso_process %>%
  gather(item, value, contains("volume")|contains("events")) %>%
  mutate(year = str_extract(item, "[0-9]+"),
         item = str_extract(item, "[a-z]+")) %>%
  spread(item, value)

ggplot(data_plot, aes(events, volume)) +
  geom_point()+
  facet_wrap(~factor(year), scales = "free") +
  labs(x = "# of events",
       y = "CSO volume",
       title = "CSO number and volume of each CSO outfall in each year") +
  theme_bw()

# preprocess: compute volume per area
#   million US gallons = 133680.556 cubic feet
cso_process <- cso_process %>%
  gather(item, value, contains("volume")|contains("events")) %>%
  mutate(year = str_extract(item, "[0-9]+"),
         item = str_extract(item, "[a-z]+")) %>%
  spread(item, value) %>%
  mutate(area = as.numeric(st_area(geometry)),
         cso_depth = volume*133680.556/area) %>%
  select(PRIMARY_OU, area, year, cso_depth, volume, events, everything()) 

# preprocess: compute volume per area

get_gi_metrics <- function(gi, cso_shed_shape, item = "Asset_Area", fn){
  # Get the metrics "item" of GIs within cso_shed_shape, return value summarized by fn 
  gi[cso_shed_shape, ] %>%
    pull(item) %>%
    fn()
}

out <- tibble(
  PRIMARY_OU = cso_process$PRIMARY_OU %>% unique(),
  gi_area = 0
)
for (i in 1:nrow(out)){
  cso_shed_shape <- cso_process %>%
    dplyr::filter(PRIMARY_OU == out$PRIMARY_OU[[i]])
  out$gi_area[i] <- get_gi_metrics(gi, cso_shed_shape, "Asset_Area", sum)  # gi_full may be used here
}

cso_process <- cso_process %>%
  left_join(out, by = "PRIMARY_OU")

# analysis

ggplot(cso_process, aes(volume, gi_area)) +
  geom_point()+
  facet_wrap(~factor(year), scales = "free") +
  labs(y = "GI area [ft2]",
       x = "CSO volume [million gallon]",
       title = "CSO volume vs. GI areas in each year") +
  theme_bw()

