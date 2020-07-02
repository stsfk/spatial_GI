library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

london = data.frame(lon = -0.1, lat = 51.5) %>% 
  st_as_sf(coords = c("lon", "lat"))
st_is_longlat(london)

london_geo = st_set_crs(london, 4326)
st_is_longlat(london_geo)

london_buff_no_crs = st_buffer(london, dist = 1)

london_buff_no_crs = st_buffer(london, dist = 1)
london_buff = st_buffer(london_geo, dist = 1)

london_proj = data.frame(x = 530000, y = 180000) %>% 
  st_as_sf(coords = 1:2, crs = 27700)

london2 = st_transform(london_geo, 27700)

st_distance(london2, london_proj)






