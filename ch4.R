library(sf)
library(raster)
library(dplyr)
library(spData)

canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

nz_height[canterbury, , op = st_disjoint]


