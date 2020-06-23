library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

#2 Geometric operations on vector data

seine_simp = st_simplify(seine, dTolerance = 2000) 

us_states2163 = st_transform(us_states, 2163)
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000)  # 100 km

# proportion of points to retain (0-1; default 0.05)
us_states2163$AREA = as.numeric(us_states2163$AREA)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01,
                                          keep_shapes = TRUE)
#3 Centroids
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

#4 Affine transformation
nz_shift = nz_sfc + c(0, 100000)

nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc

rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc

nz_scale_sf = st_set_geometry(nz, nz_scale)

#5 Clipping

b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text

x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area

bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)
plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"))

sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy1 = p[sel_p_xy]
p_xy2 = p[x_and_y]
identical(p_xy1, p_xy2)

#6 union
regions = aggregate(
  x = us_states[, "total_pop_15"],
  by = list(us_states$REGION),
  FUN = sum,
  na.rm = TRUE
)
regions2 = us_states %>% 
  group_by(REGION) %>%
  summarize(pop = sum(total_pop_15, na.rm = TRUE))
identical(regions, regions2)

us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)

#7 type transformation
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))

linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2, multipoint_3)

multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2), 
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring((multilinestring_list))
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))


linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2 %>% plot()


##3 Geometirc operation on raster data

#1 geometric inserctions
data("elev", package = "spData")
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]

#2 extend and origin
data(elev, package = "spData")
elev_2 = extend(elev, c(1, 2), value = 1000)

plot(elev_2)

elev_3 = elev + elev_2

elev_4 = extend(elev, elev_2)






