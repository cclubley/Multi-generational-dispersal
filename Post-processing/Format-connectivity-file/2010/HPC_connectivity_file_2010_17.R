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

print("Connectivity file - 17 days")

print("Loading data...")
# Load csv data
mydata <- read.csv("./csv file/2010_17_Random_100000_passive_2D_1deg_settled.csv")

print("Loading raster layer...")
# Load raster layer
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)

print("Converting data projection...")
# Convert projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

print("Loading ocean grid...")
# Load ocean grid
setwd("./Coast_grid")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("/gpfs/home/xbx21mvu/Post-process")

print("Finding polygon IDs...")
# Find the polygon IDs of the points from the ocean grid
mg_poly <- over(spawning_data, ocean_grid)

print("Adding IDs to data frame...")
# Add to the original data frame
mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
print("Number of NA values:")
sum(is.na(mg$ID))

print("Removing NA values...")
# Remove rows containing NA values
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Check that this has dealt with all of the NA values
sum(is.na(mg$ID))

print("Separating source and sink...")
# Edit data for connectivity file
# Create index for deletion
ind <- seq(1, nrow(mg), by=2)
# Keep for source
source <- mg[ind, ]
# Delete for sink
sink <- mg[-ind, ]

print("Editing source column names...")
source$Distance <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

print("Editing sink column names...")
sink$Connectivity <- NULL
sink$Particle <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Sink_ID")

print("Binding source and sink...")
connectivity_data <- cbind(source, sink)

print("Writing file...")
# Write to file
write.csv(connectivity_data, "./Connectivity/2010_17_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)
