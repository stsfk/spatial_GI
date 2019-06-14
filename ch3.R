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

library(spData)
data(us_states)
data(us_states_df)

# 1
us_states_name <- us_states %>%
  dplyr::select(NAME) %>%
  rename(name = NAME) %>%
  st_drop_geometry()

# 2
us_states_pop <- us_states %>%
  select(contains("pop"))

# 3
us_states %>% 
  filter(REGION == "Midwest")

us_states %>%
  filter(REGION == "West",
         AREA <= units::set_units(5000000, "km^2"))

us_states %>%
  filter(REGION == "South",
         AREA <= units::set_units(150000, "km^2"),
         total_pop_15 > 7e6)

# 4
us_states$total_pop_15 %>% sum()

# 5
us_states %>%
  count(REGION) %>%
  plot()

# 6
us_states %>%
  group_by(REGION) %>%
  summarise(pop = sum(total_pop_15))

# 7
us_states %>%
  left_join(us_states_df, by = c(NAME = "state")) %>%
  class()

# 8
us_states_df %>%
  anti_join(us_states, by = c("state" = "NAME"))

# 9
us_states %>%
  left_join(us_states_df, by = c(NAME = "state")) %>%
  mutate(pop_den = total_pop_15/AREA)

# 10
us_states %>%
  left_join(us_states_df, by = c(NAME = "state")) %>%
  transmute(
    NAME = NAME,
    pop_den15 = as.numeric(total_pop_15 / AREA),
    pop_den10 = as.numeric(total_pop_10 / AREA),
    pop_den_change = as.numeric(pop_den15 - pop_den10)
  ) %>%
  plot()

# 11
temp <- names(us_states) %>%
  tolower()
us_states %>%
  setNames(temp)

# 12
us_states_sel <- us_states %>%
  left_join(us_states_df, by = c(NAME = "state")) %>%
  dplyr::select(median_income_15) %>%
  rename(Income = median_income_15)

# 13
us_states %>%
  left_join(us_states_df, by = c(NAME = "state")) %>%
  transmute(
    NAME = NAME,
    REGION = REGION,
    income_change = as.numeric(median_income_15 - median_income_10)
  ) %>%
  group_by(REGION) %>%
  summarise(mean = mean(income_change)) %>%
  arrange(mean)

# 14
ras <- raster(nrows = 9, ncols = 9, res = 0.5,
              xmn = 0, xmx = 4.5, ymn = 0, ymx = 4.5,
              vals = sample(x = 1:20, 81, replace = T))
ras %>% dim()
plot(ras)

# 15
modal(ras)

# 16
library(RQGIS)
data(dem, package = "RQGIS")
plot(dem)

hist(dem)
boxplot(dem)




