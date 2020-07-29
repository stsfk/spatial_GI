library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data

library(tidyverse)

library(ggridges)
theme_set(theme_minimal())

# Data --------------------------------------------------------------------

gi_point <- st_read("/Users/yang/Documents/GIS/NYC/DEP_GI_Assets_Public/DEP_GI_Assets_Public.shp")

land_cover <- raster::raster("/Users/yang/Documents/GIS/NYC/Land_Cover/NYC_2017_LiDAR_LandCover.img")

# land_cover2 <- raster::raster("/Volumes/WD Data/data/nyc/Land_Cover/NYC_2017_LiDAR_LandCover.img")

sewershed <- st_read("/Users/yang/Documents/GIS/NYC/drainage/Sewershed/Sewershed.shp")

dem <- raster("/Volumes/WD Data/data/nyc/NYC_DEM/NYC_DEM_1ft_Float_2/DEM_LiDAR_1ft_2010_Improved_NYC.img")

st_crs(land_cover)$proj4string

# Preprocessing -----------------------------------------------------------

units::set_units(st_area(sewershed),m^2) # area of sewershed

# group GIs into different sewershed
gi_point <- gi_point %>%
  mutate(sewershed = st_within(gi_point, sewershed) %>% 
           unlist())

gi_point %>%
  ggplot(aes(sewershed)) +
  geom_histogram()

gi_point %>%
  count(sewershed)

# analysis GIs in subcatchment 6

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
  tibble(n_GIs = sapply(gi_sub$distance, function(x) (x <= d) %>% sum),
         d = as.character(d)) %>%
    dplyr::select(d, n_GIs)
}

data_plot <- lapply(c(1:10)*100, how_many) %>%
  bind_rows() %>%
  mutate(d = factor(d, levels = as.character(c(1:10)*100)))

ggplot(data_plot, aes(x = n_GIs, y = d, fill = factor(stat(quantile)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", scale = 0.6, calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles") +
  labs(title = "Distriubtion of numbers of neighbouring GIs within a radius of each GI",
       x = "# of neighbouring GIs",
       y = "Distance [feet]")


data_plot2 <-  lapply(c(1:100)*10, how_many) %>%
  bind_rows() %>%
  mutate(d = as.numeric(d))

data_plot2 <- data_plot2 %>%
  group_by(d) %>%
  summarize_all(list( ~ min(.), q25 = ~ quantile(., 0.25), q50 = ~ quantile(., 0.5), q75 = ~ quantile(., 0.75), ~max(.))) %>%
  gather(statistics, value, -d)

ggplot(data_plot2, aes(d, value, color = statistics, shape = statistics, linetype = statistics)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Numbers of neighbouring GIs within a radius of each GI for different quantiles",
       y = "# of neighbouring GIs",
       x = "Distance [feet]")


# Direct access -----------------------------------------------------------

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

create_line <- function(i, j){
  point1 <- gi_sub$geometry[i] %>% unlist()
  point2 <- gi_sub$geometry[j] %>% unlist()
  
  rbind(point1, point2) %>%
    st_linestring() %>% 
    st_sfc(crs = projection(gi_sub)) %>% 
    st_sf()
}

create_line_nearest <- function(i){
  
  j <- which.min(gi_sub$distance[i][[1]])
  out <- create_line(i, j)
  out$id <- i
  
  out %>%
    dplyr::select(id, everything())
}

get_path <- function(i){
  line <- create_line_nearest(i)
  raster::extract(land_cover, line, 
                    along = TRUE, cellnumbers = TRUE)
}

temp <- lapply(1:20, get_path)

temp <- lapply(1:20, function(x) temp[[x]] %>% unlist)


temp <- lapply(1:20, function(x) temp[[x]][temp[[x]]<10])


data_plot3 <- gi_sub %>%
  slice(1:20) %>% 
  mutate(direct_path = temp)

data_plot3 <- data_plot3 %>%
  select(GI_ID, direct_path) %>%
  unnest(direct_path)

ggplot(data_plot3, aes(direct_path, fill = GI_ID)) +
  geom_histogram(position = "dodge") +
  labs(x = "land use",
       y = "count")

ggplot(data_plot3, aes(x = direct_path, y = GI_ID)) + 
  stat_density_ridges(geom = "density_ridges_gradient", scale = 0.6, calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE) + 
  geom_density_ridges(
    stat = "binline", bins = 20, scale = 0.95,
    draw_baseline = FALSE
  )

# Raster-vector interactions ----------------------------------------------
n_sample <- 1000
gi_sample <- sample(nrow(gi_sub), n_sample)

surrounding <- raster::extract(land_cover, gi_sub[gi_sample,], buffer = 300)

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

data_plot <- vector("list", n_sample)
for (i in 1:n_sample){
  surrounding_summary <- surrounding[[i]] %>% 
    table()
  surrounding_summary <- 
    tibble(
      land_cover_id = names(surrounding_summary),
      cover_percent = surrounding_summary/length(surrounding[[i]]) * 100
      )
  
  out <- land_cover_template
  
  out <- land_cover_template %>%
    left_join(surrounding_summary, by = "land_cover_id") %>%
    select(land_cover_type, cover_percent) %>%
    mutate(cover_percent = replace(cover_percent, is.na(cover_percent), 0),
           cover_percent = as.double(cover_percent))
  
  temp <- as_tibble(out[,2] %>% t())
  names(temp) <- out$land_cover_type 
  
  temp <- temp %>%
    mutate(gi_id = i) %>%
    dplyr::select(gi_id, everything())
  
  data_plot[[i]] <- temp
}

data_process <- data_plot %>%
  bind_rows()
x <- data_process[,-1]
cl <- kmeans(x, 3, nstart = 10)


ks <- 1:50
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(x, k, nstart = 10)
  cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b")



d <- dist(data_process[, -1])
hcl <- hclust(d)
hcl

plot(hcl)

abline(h = 25, col = "red")


ggplot(data_plot, aes(land_cover_type,cover_percent, fill = land_cover_type)) +
  geom_bar(stat="identity") +
  facet_wrap(~gi_id)

km <- kmeans(x, centers = 5, nstart = 10)
hcl <- hclust(d)
table(km$cluster, cutree(hcl, k = 5))



hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)

