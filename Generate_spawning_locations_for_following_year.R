# The following script takes a .csv file and processes it to generate a new set of 
# spawning locations for the following year. It uses the same spatial grid that 
# is used in the 'Connectivity_file_annotated_script' and that was used to 
# generate the initial spawning locations for the year 2000.

# Load the required packages ----------------------------------------------
{ library(ncdf4)
library(reshape2)
library(stringr)
library(tm)
library(dplyr)
library(tidyr)
library(sdmpredictors)
library(sp)
library(rgdal)
library(rgeos)
  }

# Generate new spawning locations from the 28 day PLD output --------------

# First load the .csv data that was created for a 28 day PLD
mydata <- read.csv("./csv file/2000_28_Random_100000_passive_2D_1deg_settled.csv")

# Load the raster layer that was used to create the spatial grid, and use 
# it to convert the .csv data to a spatial points data frame with the same 
# projection as the grid.
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)
# Convert the data to a spdf using the raster projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# Load the spatial grid
ocean_grid <- readOGR(dsn=".", layer="coast_grid")

# Overlay the spdf on the grid using the 'over' function, which will assign 
# each point within the spdf (both the source and sinkn locations of particles)
# a polygon ID which corresponds to the grid polygon within which it falls
mg_poly <- over(spawning_data, ocean_grid)

# Add this information to the original data frame
mg <- as.data.frame(mg_poly) # convert the spdf back to a data frame
spawning_data$ID <- mg$ID # add the polygon IDs to the original data frame
mg <- as.data.frame(spawning_data) # re-name
mg$Lon.1 <- NULL # remove excess variables
mg$Lat.1 <- NULL

# How many values did not fall over the grid, and therefore
# were assigned an ID of NA
print("Number of NA values:")
sum(is.na(mg$ID))

# These rows represent particles that did not settle within the realm of the 
# spatial grid within their alloted PLD. Remove these particles from the data
# frame under the assumption that they did not reach suitable settlement habitat
# in time
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Double check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Convert projection of the new data 
spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg) # remove unneeded data

# Crop the spatial grid to only those polygons containing data, so that the 
# remaining polygons represent the source and sink locations of particles in the
# model output
mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

# Extract the polygon IDs of these remaining polygons
polygonID <- mg_grid$ID 

# Retrieve the centroids (lat and lon) of each polygon
centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

# Convert the data to a data frame format, and rename the columns 
centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

# Merge the centroids and polygon IDs so that for each polygon you have the 
# latitude and longitude information
grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

# Write the .csv file
write.csv(grid_centres, "./New spawning locations/Spawning_locations_2000_28_1_deg.csv", row.names = FALSE)

# This new file now contains the spawning information for the following year. 
# The source and sink locations have been used to inform these new spawning 
# locations, under the assumption that particles at sink locations 'survive' to
# contribute to propagule production the following year. 

# Repeat the above process for the remaining PLDs (25, 17 and 14 days), 
# and then manually combine the spawning locations in excel so that there is one
# .csv spawning file for the following year with no repeat locations.

# Generate new spawning locations from the 25 day PLD output --------------

print("Spawning locations - 25 days")

mydata <- read.csv("./csv file/2000_25_Random_100000_passive_2D_1deg_settled.csv")

spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

mg_poly <- over(spawning_data, ocean_grid)

mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL

print("Number of NA values:")
sum(is.na(mg$ID))

mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

sum(is.na(mg$ID))

spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

polygonID <- mg_grid$ID 

centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2000_25_1_deg.csv", row.names = FALSE)

# Generate new spawning locations from the 17 day PLD output --------------

print("Spawning locations - 17 days")

mydata <- read.csv("./csv file/2000_17_Random_100000_passive_2D_1deg_settled.csv")

spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

mg_poly <- over(spawning_data, ocean_grid)

mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL

print("Number of NA values:")
sum(is.na(mg$ID))

mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

sum(is.na(mg$ID))

spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

polygonID <- mg_grid$ID 

centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2000_17_1_deg.csv", row.names = FALSE)

# Generate new spawning locations from the 14 day PLD output --------------

print("Spawning locations - 14 days")

mydata <- read.csv("./csv file/2000_14_Random_100000_passive_2D_1deg_settled.csv")

spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

mg_poly <- over(spawning_data, ocean_grid)

mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
print("Number of NA values:")
sum(is.na(mg$ID))

mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

sum(is.na(mg$ID))

spawning_data <- SpatialPointsDataFrame(
  coords=mg[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mg)
rm(mg)

mg_grid <- crop(ocean_grid, spawning_data)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(spawning_data))),]
rm(spawning_data)

polygonID <- mg_grid$ID 

centroids <- getSpPPolygonsLabptSlots(mg_grid) 
rm(mg_grid)

centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

grid_centres <- cbind(centroids, polygonID)
rm(centroids, polygonID)

write.csv(grid_centres, "./New spawning locations/Spawning_locations_2000_14_1_deg.csv", row.names = FALSE)
