library(sf)
library(raster)
library(dplyr)
library(spData)

canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

nz_height[canterbury, , op = st_disjoint]

plot(nz_height)

## Spatial operation vector data

#1 Spatial subsetting

canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

canterbury_height3 = nz_height %>%
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))

#2 Topological relations 
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")  # convert type of the sf

(st_intersects(p, a) %>% lengths )> 0

st_intersects(p, a, sparse = FALSE)

st_disjoint(p, a, sparse = FALSE)[, 1]

st_within(p, a, sparse = FALSE)[, 1]

st_touches(p, a, sparse = FALSE)[, 1]

sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
lengths(sel) > 0

#3 Spatial joining

set.seed(2018) # set seed for reproducibility
(bb_world = st_bbox(world)) # the world's bounds

random_df = tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)  

random_points = random_df %>% 
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS


world_random = world[random_points, ]
nrow(world_random)
random_joined = st_join(random_points, world["name_long"])

# non-overlapping joins
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)

z = st_join(cycle_hire_P, cycle_hire_osm_P,
            join = st_is_within_distance, dist = 20)

z = z %>% 
  group_by(id) %>% 
  summarize(capacity = mean(capacity))

nrow(z) == nrow(cycle_hire)

#5 spatial data aggregation

nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)

nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))

agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones,
                           extensive = TRUE)
agg_aw$value


# Distance relations

nz_heighest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_heighest, canterbury_centroid)

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)
#> Units: [m]
#>        [,1]  [,2]
#> [1,] 123537 15498
#> [2,]  94283     0
#> [3,]  93019     0

plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)

## Spatial operation on raster data

#1 Spatial subsetting

id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]

raster::extract(elev, data.frame(x = 0.1, y = 0.1))

clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip] %>% plot()

# create raster mask
rmask = elev 
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)  # values to access the values of ratsterr

# spatial subsetting
elev[rmask, drop = FALSE]           # with [ operator
mask(elev, rmask)                   # with mask()
overlay(elev, rmask, fun = "max")   # with overlay

#2 map algerbra

#3 Local operations
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)
elev + elev
elev^2
log(elev)
elev > 5

#4  Focal operations

r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

#5 Zonal operations
z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()
z
#>   zone mean
#> 1    1 17.8
#> 2    2 18.5
#> 3    3 19.2


#6 global operations and distance

#7 map algebra

#8 Merging rasters
aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)
aut_ch = merge(aut, ch)








