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