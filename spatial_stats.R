library(rgdal)
library(maptools)
library(raster)
library(spatstat)


# data

gi_point <- readOGR("/Users/yang/Documents/GIS/NYC/DEP_GI_Assets_Public/DEP_GI_Assets_Public.shp")

land_cover <- raster::raster("/Users/yang/Documents/GIS/NYC/Land_Cover/NYC_2017_LiDAR_LandCover.img")

water <- readOGR("/Users/yang/Documents/GIS/NYC/elements/HYDRO/HYDROGRAPHY.shp")

sewershed <- readOGR("/Users/yang/Documents/GIS/NYC/drainage/Sewershed/Sewershed.shp")


# preprocess
gi_point <-  as(gi_point, "ppp")
sewershed <-  as(sewershed, "owin")
water <-  as(water, "owin")

starbucks <- gi_point
ma <- sewershed
pop <- land_cover

marks(gi_point) <- NULL

Window(gi_point) <- sewershed

plot(gi_point, main=NULL, cols=rgb(0,0,0,.2), pch=20)


Q <- quadratcount(gi_point, nx= 6, ny=3)

plot(gi_point, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid


# Compute the density for each quadrat
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

starbucks.km <- rescale(starbucks, 1000, "km")
ma.km <- rescale(ma, 1000, "km")
pop.km    <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")


mean(nndist(starbucks.km, k=1))

mean(nndist(starbucks.km, k=2))

ANN <- apply(nndist(starbucks.km, k=1:100),2,FUN=mean)
plot(ANN ~ eval(1:100), type="b", main=NULL, las=1)


K <- Kest(starbucks.km)
plot(K, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))


L <- Lest(starbucks.km, main=NULL)
plot(L)

plot(L, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

g  <- pcf(starbucks.km)
plot(g)

ann.p <- mean(nndist(starbucks.km, k=1))
ann.p

n     <- 50               # Number of simulations
ann.r <- vector(length = n) # Create an empty object to be used to store simulated ANN values
for (i in 1:n){
  rand.p   <- rpoint(n=starbucks.km$n, win=ma.km)  # Generate random point locations
  ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}

ann.r  <- ann.r[ann.r !=0]

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

