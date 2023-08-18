# Load packages
library(ncdf4)
library(reshape2)
library(stringr)
library(tm)
library(dplyr)
library(sdmpredictors)
library(sp)
library(rgdal)
library(rgeos)
library(zoo)
library(tidyr)

# Load netCDF file
mydata <- nc_open("./Model outputs/2001_Random_100000_passive_2D_1deg_output_ADDITIONAL_RELEASES_25DAY.nc")

# Get variables
lat <- ncvar_get(mydata,"lat")
lon <- ncvar_get(mydata,"lon")
time <- ncvar_get(mydata, "time")
dist <- ncvar_get(mydata, "distance")

# Get time units
tunits <- ncatt_get(mydata,"time","units")
time_origin <- mydata$var$time$units
time_origin2 <- str_sub(time_origin, 15, 24)
time_origin3 <- str_sub(time_origin, 26, 33)
time_origin <- paste(time_origin2, " ", time_origin3)

rm(time_origin2, time_origin3, tunits)
nc_close(mydata)
rm(mydata)

# Fill NA values with last observation
lat <- na.locf(na.locf(lat), fromLast=T)
lon <- na.locf(na.locf(lon), fromLast=T)
time <- na.locf(na.locf(time), fromLast=T)
dist <- na.locf(na.locf(dist), fromLast=T)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# 25 * 24 = 600 rows
# Get last row
lat_last <- lat[600, ]
lon_last <- lon[600, ]
time_last  <- time[600, ]
dist_last <- dist[600, ]

# Bind first
lat_first <- as.data.frame(lat_first)
lon_first <- as.data.frame(lon_first)
time_first <- as.data.frame(time_first)
dist_first <- as.data.frame(dist_first)

first <- cbind(lat_first, lon_first, time_first, dist_first)
first$Particle <- paste(1:nrow(first)) 
first$Connectivity <- "Source"
colnames(first) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_first, lon_first, time_first, dist_first)

# Bind last
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)

last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

# Combine first and last
mydata <- rbind(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

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
write.csv(connectivity_data, "./Connectivity/2001_25_Random_100000_passive_2D_1deg_connectivity__ADDITIONAL.csv", row.names = FALSE)

