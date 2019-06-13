library(sf)
library(raster)
library(dplyr)
library(stringr) # for working with strings (pattern matching)

library(spData)

methods(class = "sf") # methods for sf objects, first 12 shown

dim(world) # it is a 2 dimensional object, with rows and columns
#> [1] 177  11
nrow(world) # how many rows?
#> [1] 177
ncol(world) # how many columns?
#> [1] 11

sel_area = world$area_km2 < 10000
summary(sel_area) # a logical vector
#>    Mode   FALSE    TRUE 
#> logical     170       7
small_countries = world[sel_area, ]

world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)
#> [1] "data.frame"

world_agg2 = aggregate(world["pop"], by = list(world$continent),
                       FUN = sum, na.rm = TRUE)
class(world_agg2)

world %>%
  group_by(continent) %>%
  summarise(pop = sum(pop, na.rm = T)) %>%
  arrange(desc(pop)) %>%
  slice(1:3) %>%
  st_drop_geometry()

world_coffee = left_join(world, coffee_data)
class(world_coffee)

names(world_coffee)
plot(world_coffee["coffee_production_2017"])

elev = raster(nrows = 6, ncols = 6, res = 0.5,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5, 
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)

r_stack = stack(elev, grain)
names(r_stack) = c("elev", "grain")

raster::subset(r_stack, "elev")
r_stack[["elev"]]
r_stack$elev
r_stack[1]

# Exercises ---------------------------------------------------------------

















