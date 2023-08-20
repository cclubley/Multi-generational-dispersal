library(dplyr)
library(plyr)
library(geosphere)
library(sdmpredictors)

# Read in the csv data
mydata_100 <- read.csv("./csv file/Sensitivity_100.csv")
mydata_1000 <- read.csv("./csv file/Sensitivity_1000.csv")
mydata_10000 <- read.csv("./csv file/Sensitivity_10000.csv")
mydata_100000 <- read.csv("./csv file/Sensitivity_100000.csv")

# Load raster layer for CRS projection
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)

# Calculate distance between start and end point
#100
mydata_100_source <- data.frame(mydata_100$Source_lon, mydata_100$Source_lat)
colnames(mydata_100_source) <- c("Lon", "Lat")
mydata_100_sink <- data.frame(mydata_100$Sink_lon, mydata_100$Sink_lat)
colnames(mydata_100_sink) <- c("Lon", "Lat")

mydata_100_source <- SpatialPointsDataFrame(
  coords=mydata_100_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100_source)

mydata_100_sink <- SpatialPointsDataFrame(
  coords=mydata_100_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100_sink)

mydata_100 <- mydata_100 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_100_source)) {
  gDists <- suppressWarnings(distm(mydata_100_source[i,], mydata_100_sink[i,], fun=distHaversine)/1000)
  mydata_100$Dist_total[i] <- gDists}

#1000
mydata_1000_source <- data.frame(mydata_1000$Source_lon, mydata_1000$Source_lat)
colnames(mydata_1000_source) <- c("Lon", "Lat")
mydata_1000_sink <- data.frame(mydata_1000$Sink_lon, mydata_1000$Sink_lat)
colnames(mydata_1000_sink) <- c("Lon", "Lat")

mydata_1000_source <- SpatialPointsDataFrame(
  coords=mydata_1000_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_1000_source)

mydata_1000_sink <- SpatialPointsDataFrame(
  coords=mydata_1000_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_1000_sink)

mydata_1000 <- mydata_1000 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_1000_source)) {
  gDists <- suppressWarnings(distm(mydata_1000_source[i,], mydata_1000_sink[i,], fun=distHaversine)/1000)
  mydata_1000$Dist_total[i] <- gDists}

#10000
mydata_10000_source <- data.frame(mydata_10000$Source_lon, mydata_10000$Source_lat)
colnames(mydata_10000_source) <- c("Lon", "Lat")
mydata_10000_sink <- data.frame(mydata_10000$Sink_lon, mydata_10000$Sink_lat)
colnames(mydata_10000_sink) <- c("Lon", "Lat")

mydata_10000_source <- SpatialPointsDataFrame(
  coords=mydata_10000_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_10000_source)

mydata_10000_sink <- SpatialPointsDataFrame(
  coords=mydata_10000_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_10000_sink)

mydata_10000 <- mydata_10000 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_10000_source)) {
  gDists <- suppressWarnings(distm(mydata_10000_source[i,], mydata_10000_sink[i,], fun=distHaversine)/1000)
  mydata_10000$Dist_total[i] <- gDists}

#100000
mydata_100000_source <- data.frame(mydata_100000$Source_lon, mydata_100000$Source_lat)
colnames(mydata_100000_source) <- c("Lon", "Lat")
mydata_100000_sink <- data.frame(mydata_100000$Sink_lon, mydata_100000$Sink_lat)
colnames(mydata_100000_sink) <- c("Lon", "Lat")

mydata_100000_source <- SpatialPointsDataFrame(
  coords=mydata_100000_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100000_source)

mydata_100000_sink <- SpatialPointsDataFrame(
  coords=mydata_100000_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100000_sink)

mydata_100000 <- mydata_100000 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_100000_source)) {
  gDists <- suppressWarnings(distm(mydata_100000_source[i,], mydata_100000_sink[i,], fun=distHaversine)/1000)
  mydata_100000$Dist_total[i] <- gDists}

# Get just the distances
ks_100 <- mydata_100$Dist_total
ks_1000 <- mydata_1000$Dist_total
ks_10000 <- mydata_10000$Dist_total
ks_100000 <- mydata_100000$Dist_total

# Carry out the Kolmogorov-Smirnov test
print("100 vs 1000")
ks.test(ks_100, ks_1000)
print("1000 vs 10000")
ks.test(ks_1000, ks_10000)
print("10000 vs 100000")
ks.test(ks_10000, ks_100000)

setwd("./ECDF plot")

# Plot
plot(ecdf(ks_100), col="dark grey")
plot(ecdf(ks_1000), add=TRUE, col="tan1")
plot(ecdf(ks_10000), add=TRUE, col="aquamarine3")
plot(ecdf(ks_100000), add=TRUE, col="purple")
