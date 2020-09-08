library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(stringr)

library(tidyverse)
library(ggalluvial)

# Data --------------------------------------------------------------------

# GI
gi_point <- st_read("/Users/yang/Documents/GIS/NYC/DEP_GI_Assets_Public/DEP_GI_Assets_Public.shp")

# NYC
nyc <- st_read("/Users/yang/Documents/GIS/NYC/nybb_20c/nybb.shp")

# land cover
land_cover <- raster::raster("/Users/yang/Documents/GIS/NYC/Land_Cover/NYC_2017_LiDAR_LandCover.img")

# Preprocessess -----------------------------------------------------------

# reprojecting
ref_crs <- st_crs(land_cover)
gi_point <- st_transform(gi_point, ref_crs)
gi_point_raw <- gi_point # backup

nyc <- st_transform(nyc, ref_crs)

# Alluvial plot -----------------------------------------------------------


# 1.Borough
#   Some GI points has na values in borough, fix the naming issue
for (i in 1:nrow(nyc)){
  ind <- gi_point$Asset_ID %in% gi_point[nyc[i,],]$Asset_ID
  gi_point$Borough[ind] <- nyc$BoroName[i]
}
gi_point %>%
  count(Borough) %>%
  arrange(desc(n))

gi_point$Borough %>% is.na() %>% sum() # one missing, it is in Brooklyn, checked with QGIS

gi_point$Borough[is.na(gi_point$Borough)] <- "Brooklyn"

# 2. Waterbody
gi_point$Waterbody %>% is.na() %>% sum() # no NA values
gi_point %>%
  count(Waterbody) %>%
  arrange(desc(n))

# 3. Status
gi_point$Status_Gro %>% is.na() %>% sum() # no NA values
gi_point %>%
  count(Status_Gro) %>%
  arrange(desc(n))

# 4. Combined or separate
gi_point$Sewer_Type %>% is.na() %>% sum() # no NA values
gi_point %>%
  count(Sewer_Type) %>%
  arrange(desc(n))

ind <- -str_detect(gi_point$Sewer_Type, "^Combined")
gi_point$Sewer_Type[ind] <- "Separate or other type"


# prepare plot
data_plot <- gi_point %>%
  group_by(Borough, Waterbody, Status_Gro, Sewer_Type) %>%
  summarise(area = sum(Asset_Area),
            number = n()) %>%
  ungroup() %>%
  mutate(Borough = factor(Borough, levels = c("Queens", "Brooklyn", "Bronx", "Manhattan", "Staten Island")),
         Status_Gro = factor(Status_Gro, levels = c("Constructed", "In Construction", "Final Design")),
         Waterbody = factor(Waterbody, levels = c("Jamaica Bay and Tributaries", "Flushing Creek", "Newtown Creek",
                                                  "East River / Open Waters", "Flushing Bay", "Bronx River", "Westchester Creek",
                                                  "Hutchinson River", "Gowanus Canal", "Coney Island Creek")))

ggplot(data = data_plot,
       aes(axis1 = Borough, axis2 = Waterbody, axis3 = Status_Gro,
           y = number)) +
  scale_x_discrete(limits = c("Borough", "Waterbody", "Status"), expand = c(.05, .05)) +
  labs(y = "Number of GIs") +
  geom_alluvium(aes(fill = Sewer_Type), color = "black") +
  geom_stratum(width = 1/3) + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")



