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






