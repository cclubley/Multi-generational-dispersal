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

print("Connectivity file - 14 days")

print("Loading data...")
# Load csv data
mydata <- read.csv("./csv file/2003_14_Random_100000_passive_2D_1deg_settled.csv")

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

print("Filtering unneeded IDs...")
# Filter out unneeded IDs
connectivity_data <- connectivity_data[!(connectivity_data$Source_ID==364|connectivity_data$Source_ID==365|connectivity_data$Source_ID==366
                               |connectivity_data$Source_ID==367|connectivity_data$Source_ID==412|connectivity_data$Source_ID==413|connectivity_data$Source_ID==414
                               |connectivity_data$Source_ID==415|connectivity_data$Source_ID==460|connectivity_data$Source_ID==461|connectivity_data$Source_ID==493
                               |connectivity_data$Source_ID==494|connectivity_data$Source_ID==505|connectivity_data$Source_ID==506|connectivity_data$Source_ID==541
                               |connectivity_data$Source_ID==553|connectivity_data$Source_ID==554|connectivity_data$Source_ID==601|connectivity_data$Source_ID==602
                               |connectivity_data$Source_ID==649|connectivity_data$Source_ID==650|connectivity_data$Source_ID==651|connectivity_data$Source_ID==653
                               |connectivity_data$Source_ID==654|connectivity_data$Source_ID==655|connectivity_data$Source_ID==656|connectivity_data$Source_ID==697
                               |connectivity_data$Source_ID==698|connectivity_data$Source_ID==699|connectivity_data$Source_ID==700|connectivity_data$Source_ID==703
                               |connectivity_data$Source_ID==704|connectivity_data$Source_ID==750|connectivity_data$Source_ID==835|connectivity_data$Source_ID==836
                               |connectivity_data$Source_ID==1308|connectivity_data$Source_ID==1309|connectivity_data$Source_ID==1355|connectivity_data$Source_ID==1356
                               |connectivity_data$Source_ID==1357|connectivity_data$Source_ID==1403|connectivity_data$Source_ID==1404), ]

print("Writing file...")
# Write to file
write.csv(connectivity_data, "./Connectivity/2003_14_Random_100000_passive_2D_1deg_connectivity.csv", row.names = FALSE)