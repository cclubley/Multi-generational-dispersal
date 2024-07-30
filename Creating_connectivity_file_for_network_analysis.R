# The following script takes a .csv file and processes it to make the data suitable 
# for connectivity analysis using graph network. It uses the same 
# spatial grid that was used to generate spawning locaitons to assign the source 
# and final location of each particle a 'polygon ID'. In this way, spatial bias
# is controlled for by looking at the connectivity between defined areas rather 
# than exact locations. The 'polygon IDs' assigned here later go on to form the 
# nodes in the graph networks.

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

# Assign a polygon ID to each particle ------------------------------------

# First load the .csv data that was created for a 28 day PLD
mydata <- read.csv("./csv file/2000_28_Random_100000_passive_2D_1deg_settled.csv")

# Load the raster layer that was used to create the spatial grid, and use 
# it to convert the .csv data to a spatial points data frame with the same 
# projection as the grid.
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE) # Load the raster data
# Convert the data to a spdf using the raster projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# Load the spatial grid
ocean_grid <- readOGR(dsn=".", layer="coast_grid")

# Overlay the spdf on the grid using the 'over' function, which will assign 
# each point within the spdf (both the source and final locations of particles)
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
# spatial grid within their allocated PLD. Remove these particles from the data
# frame under the assumption that they did not reach suitable settlement habitat
# in time
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Double check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Identify the 'source ID' and 'sink ID' ----------------------------------

# So that the data is of use for graph network theory, Label the 
# particles so that it is clear which polygon ID corresponds to the particle
# source, and which corresponds to the particle sink.

# Filter the source and sink locations to two separate data frames
source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

# Remove the data that is not of use and rename the resulting columns
# Note that the coordinates now become 'source_lon' and source_lat', the time 
# becomes 'start_time' and 'ID' becomes 'Source_ID' - this is to distinguish 
# these columns from the particle sink data
source$Distance <- NULL
source$Settled <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

# Do the same for the sink locations
sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

# And then re-combine the source and sink data, now with separate columns for 
# information on the source and sink data for each particle. The source and sink 
# data should line up as the data frame wasn't restructured at any point, but 
# to double check the number in the 'Particle' and 'PArticle2' column should be
# the same. Once you are happy that this has worked properly, the 'Particle2' 
# column can be removed
connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

# Finally, write the .csv file
write.csv(connectivity_data, "./Connectivity/2000_28_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)

# Now repeat the process for the remaining PLDs: 25, 17 and 14 days

# Create a connectivity file for a 25 day PLD -----------------------------

print("Connectivity file - 25 days")

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

# Create a connectivity for a 17 day PLD ----------------------------------

print("Connectivity file - 17 days")

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

write.csv(connectivity_data, "./Connectivity/2000_17_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)

# Create a connectivity file for a 14 day PLD -----------------------------

print("Connectivity file - 14 days")

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

write.csv(connectivity_data, "./Connectivity/2000_14_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)
