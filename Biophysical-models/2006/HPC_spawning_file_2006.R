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

print("Spawning locations - 28 days")

# Load csv data
mydata <- read.csv("./csv file/2006_28_Random_100000_passive_2D_1deg_settled.csv")

# Load raster layer
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)

# Load ocean grid
setwd("./Coast_grid")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("/gpfs/home/xbx21mvu/Post-process")

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

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

# Crop ocean grid
mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

# Extract polygon IDs
polygonID <- mg_grid$ID 

# Get centroids 
centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

# Convert 
centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

# Merge
grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2007_28_1_deg.csv", row.names = FALSE)

# Load csv data
mydata <- read.csv("./csv file/2006_25_Random_100000_passive_2D_1deg_settled.csv")

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

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

# Crop ocean grid
mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

# Extract polygon IDs
polygonID <- mg_grid$ID 

# Get centroids 
centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

# Convert 
centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

# Merge
grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2007_25_1_deg.csv", row.names = FALSE)

print("Spawning locations - 17 days")

# Load csv data
mydata <- read.csv("./csv file/2006_17_Random_100000_passive_2D_1deg_settled.csv")

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

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

# Crop ocean grid
mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

# Extract polygon IDs
polygonID <- mg_grid$ID 

# Get centroids 
centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

# Convert 
centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

# Merge
grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2007_17_1_deg.csv", row.names = FALSE)

print("Spawning locations - 14 days")

# Load csv data
mydata <- read.csv("./csv file/2006_14_Random_100000_passive_2D_1deg_settled.csv")

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

# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

# Crop ocean grid
mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

# Extract polygon IDs
polygonID <- mg_grid$ID 

# Get centroids 
centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

# Convert 
centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

# Merge
grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2007_14_1_deg.csv", row.names = FALSE)
