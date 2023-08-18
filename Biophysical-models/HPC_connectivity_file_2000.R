# Load packages
library(ncdf4)
library(reshape2)
library(stringr)
library(tm)
library(dplyr)
library(tidyr)
library(sdmpredictors)
library(sp)
library(rgdal)
library(rgeos)

print("Connectivity file - 28 days")

# Load csv data
mydata <- read.csv("./csv file/2000_28_Random_100000_passive_2D_1deg_settled.csv")

# Load raster layer
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# Load ocean grid
setwd("./Coast_grid")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("/gpfs/home/xbx21mvu/Post-process")

# Find the polygon IDs of the points from the ocean grid
mg_poly <- over(spawning_data, ocean_grid)

# Add to the original data frame
mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
print("Number of NA values:")
sum(is.na(mg$ID))

# Remove rows containing NA values
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Edit data for connectivity file
source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Settled <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

# Write to file
write.csv(connectivity_data, "./Connectivity/2000_28_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)

print("Connectivity file - 25 days")

# Load csv data
mydata <- read.csv("./csv file/2000_25_Random_100000_passive_2D_1deg_settled.csv")

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# Find the polygon IDs of the points from the ocean grid
mg_poly <- over(spawning_data, ocean_grid)

# Add to the original data frame
mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
print("Number of NA values:")
sum(is.na(mg$ID))

# Remove rows containing NA values
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Edit data for connectivity file
source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Settled <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

# Write to file
write.csv(connectivity_data, "./Connectivity/2000_25_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)

print("Connectivity file - 17 days")

# Load csv data
mydata <- read.csv("./csv file/2000_17_Random_100000_passive_2D_1deg_settled.csv")

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# Find the polygon IDs of the points from the ocean grid
mg_poly <- over(spawning_data, ocean_grid)

# Add to the original data frame
mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
print("Number of NA values:")
sum(is.na(mg$ID))

# Remove rows containing NA values
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Edit data for connectivity file
source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Settled <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

# Write to file
write.csv(connectivity_data, "./Connectivity/2000_17_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)

print("Connectivity file - 14 days")

# Load csv data
mydata <- read.csv("./csv file/2000_14_Random_100000_passive_2D_1deg_settled.csv")

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# Find the polygon IDs of the points from the ocean grid
mg_poly <- over(spawning_data, ocean_grid)

# Add to the original data frame
mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
print("Number of NA values:")
sum(is.na(mg$ID))

# Remove rows containing NA values
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Edit data for connectivity file
source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Settled <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

# Write to file
write.csv(connectivity_data, "./Connectivity/2000_14_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)
