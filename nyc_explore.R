library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(tidyverse)

# Data --------------------------------------------------------------------

gi_point <- st_read("/Users/yang/Documents/GIS/NYC/DEP_GI_Assets_Public/DEP_GI_Assets_Public.shp")

land_cover <- raster::raster("/Users/yang/Documents/GIS/NYC/Land_Cover/NYC_2017_LiDAR_LandCover.img")

sewershed <- st_read("/Users/yang/Documents/GIS/NYC/drainage/Sewershed/Sewershed.shp")


# Preprocessing -----------------------------------------------------------

gi_point <- gi_point %>%
  mutate(sewershed = st_within(gi_point, sewershed) %>% unlist())

gi_point %>%
  ggplot(aes(sewershed)) +
  geom_histogram()

gi_point %>%
  count(sewershed)

gi_sub <- gi_point %>%
  filter(sewershed == 6)

# Plot --------------------------------------------------------------------

plot(gi_sub)

# Analyze distance between points -----------------------------------------

gi_distance_m <- st_distance(gi_sub)

gi_sub %>%
  count(Asset_Type)

# only analysis ROWB in one sewershed
ROWB_ind <- (gi_sub$Asset_Type == "ROWB") %>% which()
ROWB_distance_m <- gi_distance_m[ROWB_ind, ROWB_ind]

get_distance <- function(x){
  
  out <- ROWB_distance_m[x,] %>% 
    as.numeric()
  
  list(out[out!=0])
}

gi_sub <- gi_sub %>%
  filter(Asset_Type == "ROWB") %>%
  mutate(distance = sapply(1:length(ROWB_ind), get_distance))


# minimal distance

minimal_distance <- sapply(gi_sub$distance, min)

gi_sub <- gi_sub %>%
  mutate(minimal_distance = minimal_distance)

ggplot(gi_sub, aes(minimal_distance)) +
  geom_histogram(bins = 40) +
  theme_bw()+
  labs(title = "Distribution of the distance to the nearest GI")


nearest_n <- function(n){
  # average distance between the nearest n GIs
  sapply(gi_sub$distance, function(x) sort(x)[1:n]) %>% 
    colMeans()
}

gi_sub <- gi_sub %>%
  mutate(nearest_5 = nearest_n(5),
         nearest_10 = nearest_n(10),
         nearest_20 = nearest_n(20),
         nearest_50 = nearest_n(50))


data_plot <- gi_sub %>%
  gather(item, average_distance, nearest_5:nearest_50) %>%
  mutate(item, factor(item, levels = c("nearest_5", "nearest_10", "nearest_20", "nearest_50")))

ggplot(data_plot, aes(average_distance)) +
  geom_histogram(bins = 40) +
  facet_wrap(~item) +
  theme_bw() +
  labs(title = "Distribution of average distance to the neighboring GIs")


# how many GIs within a radius

how_many <- function(d){
  # count the number of GIs within a radius
  sapply(gi_sub$distance, function(x) (x <= d) %>% sum)
}


data_plot <- gi_sub
for (i in 1:9){
  d <- 100 * i
  
  col_name <- paste0(d, "ft")
  data_plot[col_name] <- how_many(d)
}

data_plot <- data_plot %>%
  gather(distance, number, `100ft`:`900ft`)

ggplot(data_plot, aes(distance, number))+
  geom_boxplot()



# Raster-vector interactions ----------------------------------------------

gi_sample <- sample(1:1700, 5)

surrounding <- raster::extract(land_cover, gi_sub[1:5,], buffer = 50)

land_cover_template <- tibble(
  gi_id = rep(1, 8), 
  land_cover_id = as.character(1:8),
  land_cover_type = c(
    "Tree Canopy",
    "Grass or Shrubs",
    "Bare Soil",
    "Water",
    "Buildings",
    "Roads",
    "Other Impervious",
    "Railroads"
  )
)

data_plot <- vector("list", 5)
for (i in 1:5){
  surrounding_summary <- surrounding[[i]] %>% 
    table()
  surrounding_summary <- 
    tibble(
      land_cover_id = names(surrounding_summary),
      cover_percent = surrounding_summary/length(surrounding[[i]]) * 100
      )
  
  out <- land_cover_template
  
  data_plot[[i]] <- land_cover_template %>%
    left_join(surrounding_summary, by = "land_cover_id") %>%
    mutate(cover_percent = replace(cover_percent, is.na(cover_percent), 0),
           gi_id = i)
}

data_plot <- data_plot %>%
  bind_rows()

ggplot(data_plot, aes(land_cover_type,cover_percent, fill = land_cover_type)) +
  geom_bar(stat="identity") +
  facet_wrap(~gi_id)






