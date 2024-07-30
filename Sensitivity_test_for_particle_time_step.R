{library(ncdf4)
  library(reshape2)
  library(stringr)
  library(tm)
  library(dplyr)
  library(sdmpredictors)
  library(sp)
  library(zoo)
  library(plyr)
  library(geosphere)
  library(tidyr)
  library(sf)
  library(ggplot2)
}

# 1 minute ts -------------------------------------------------------------

# Load the NetCDF  output of the biophysical model
mydata <- nc_open("./2000_1mints_output_nc.nc")

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

# Remove NA values
lat <- na.omit(lat)
lon <- na.omit(lon)
time <- na.omit(time)
dist <- na.omit(dist)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# Get last row
lat_last <- lat[nrow(lat), ]
lon_last <- lon[nrow(lon), ]
time_last  <- time[nrow(time), ]
dist_last <- dist[nrow(dist), ]

rm(lat, lon, time, dist)

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
rm(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)
rm(time_origin)

write.csv(mydata, "./2000_1mints_output_csv.csv", row.names = FALSE)

rm(mydata)

# 5 minute ts -------------------------------------------------------------

# Load the NetCDF  output of the biophysical model
mydata <- nc_open("./2000_5mints_output_nc.nc")

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

# Remove NA values
lat <- na.omit(lat)
lon <- na.omit(lon)
time <- na.omit(time)
dist <- na.omit(dist)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# Get last row
lat_last <- lat[nrow(lat), ]
lon_last <- lon[nrow(lon), ]
time_last  <- time[nrow(time), ]
dist_last <- dist[nrow(dist), ]

rm(lat, lon, time, dist)

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
rm(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)
rm(time_origin)

write.csv(mydata, "./2000_5mints_output_csv.csv", row.names = FALSE)

# 5 second ts -------------------------------------------------------------

# Load the NetCDF  output of the biophysical model
mydata <- nc_open("./2000_5sects_output_nc.nc")

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

# Remove NA values
lat <- na.omit(lat)
lon <- na.omit(lon)
time <- na.omit(time)
dist <- na.omit(dist)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# Get last row
lat_last <- lat[nrow(lat), ]
lon_last <- lon[nrow(lon), ]
time_last  <- time[nrow(time), ]
dist_last <- dist[nrow(dist), ]

rm(lat, lon, time, dist)

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
rm(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)
rm(time_origin)

write.csv(mydata, "./2000_5sects_output_csv.csv", row.names = FALSE)

# 30 second ts -------------------------------------------------------------

# Load the NetCDF  output of the biophysical model
mydata <- nc_open("./2000_30sects_output_nc.nc")

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

# Remove NA values
lat <- na.omit(lat)
lon <- na.omit(lon)
time <- na.omit(time)
dist <- na.omit(dist)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# Get last row
lat_last <- lat[nrow(lat), ]
lon_last <- lon[nrow(lon), ]
time_last  <- time[nrow(time), ]
dist_last <- dist[nrow(dist), ]

rm(lat, lon, time, dist)

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
rm(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)
rm(time_origin)

write.csv(mydata, "./2000_30sects_output_csv.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# ---------------------- Change to connectivity format --------------------
# -------------------------------------------------------------------------
# Assign a polygon ID to each particle ------------------------------------

# First load the .csv data that was created for a 28 day PLD
mydata <- read.csv("./2000_1mints_output_csv.csv")

# I then load the raster layer that was used to create the spatial grid, and use 
# it to convert the .csv data to a spatial points data frame with the same 
# projection as the grid.
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE) # Load the raster data
# Convert the data to a spdf using the raster projection
spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

# I then load the spatial grid
setwd("D:/OneDrive back up (27-02-2024)/Chapters/(3) Dispersal Limitation/(3) Methods/Modelling/Original release file/Shapefiles/Ocean Grid/1 degree")
ocean_grid <- st_read(dsn=".", layer="ocean_grid")
setwd("C:/Users/au760477/OneDrive - Aarhus universitet/Admin/Publications/In_prep/Clubley et al. (2024) STOTEN/Second revision/Timestep")

ocean_grid <- as(ocean_grid, "Spatial")

# I overlay the spdf on the grid using the 'over' function, which will assign 
# each point within the spdf (both the source and sink locations of particles)
# a polygon ID which corresponds to the grid polygon within which it falls
mg_poly <- over(spawning_data, ocean_grid)

# I then add this information to the original data frame
mg <- as.data.frame(mg_poly) # convert the spdf back to a data frame
spawning_data$ID <- mg$ID # add the polygon IDs to the original data frame
mg <- as.data.frame(spawning_data) # re-name
mg$Lon.1 <- NULL # remove excess variables
mg$Lat.1 <- NULL

# I then want to know how many values did not fall over the grid, and therefore
# were assigned an ID of NA
sum(is.na(mg$ID))

# These rows represent particles that did not settle within the realm of the 
# spatial grid within their alloted PLD. I remove these particles from the data
# frame under the assumption that they did not reach suitable settlement habitat
# in time
mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

# Double check that this has dealt with all of the NA values
sum(is.na(mg$ID))

# Identify the 'source ID' and 'final ID' ----------------------------------

# So that the data is of use for graph network theory, I need to label the 
# particles so that it is clear which polygon ID corresponds to the particle
# source, and which corresponds to the particle sink.

# First I filter the source and sink locations to two separate data frames
source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

# I then remove the data that is not of use and rename the resulting columns
# Note that the coordinates now become 'source_lon' and source_lat', the time 
# becomes 'start_time' and 'ID' becomes 'Source_ID' - this is to distinguish 
# these columns from the particle sink data
source$Distance <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

# Do the same for the sink locations
sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

# And then re-combine the source and sink data, now with separate columns for 
# information on the source and sink data for each particle. The source and sink 
# data should line up as I didn't restructure the data frame at any point, but 
# to double check the number in the 'Particle' and 'PArticle2' column should be
# the same. Once you are happy that this has worked properly, the 'Particle2' 
# column can be removed
connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL

# Finally, write the .csv file
write.csv(connectivity_data, "./2000_1mints_output_connectivity.csv", row.names = FALSE)

# Create a connectivity file for 5 min ts -----------------------------

mydata <- read.csv("./2000_5mints_output_csv.csv")

spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

mg_poly <- over(spawning_data, ocean_grid)

mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
sum(is.na(mg$ID))

mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

sum(is.na(mg$ID))

source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

# Write to file
write.csv(connectivity_data, "./2000_5mints_output_connectivity.csv", row.names = FALSE)

# Create a connectivity for a 17 day PLD ----------------------------------

mydata <- read.csv("./2000_5sects_output_csv.csv")

spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

mg_poly <- over(spawning_data, ocean_grid)

mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
sum(is.na(mg$ID))

mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

sum(is.na(mg$ID))

source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

write.csv(connectivity_data, "./2000_5sects_output_connectivity.csv", row.names = FALSE)

# Create a connectivity file for a 14 day PLD -----------------------------

mydata <- read.csv("./2000_30sects_output_csv.csv")

spawning_data <- SpatialPointsDataFrame(
  coords=mydata[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata)

mg_poly <- over(spawning_data, ocean_grid)

mg <- as.data.frame(mg_poly)
spawning_data$ID <- mg$ID
mg <- as.data.frame(spawning_data)
mg$Lon.1 <- NULL
mg$Lat.1 <- NULL
sum(is.na(mg$ID))

mg <- mg %>% group_by(Particle) %>% filter(!any(is.na(ID)))

sum(is.na(mg$ID))

source <- filter(mg, Connectivity=="Source")
sink <- filter(mg, Connectivity=="Sink")

source$Distance <- NULL
source$Connectivity <- NULL
colnames(source) <- c("Source_lat", "Source_lon", "Start_time", "Particle", "Source_ID")

sink$Connectivity <- NULL
colnames(sink) <- c("Sink_lat", "Sink_lon", "End_time", "Distance", "Particle2", "Sink_ID")

connectivity_data <- cbind(source, sink)
connectivity_data$Particle2 <- NULL
connectivity_data$Settled <- NULL

write.csv(connectivity_data, "./2000_30sects_output_connectivity.csv", row.names = FALSE)

# Sensitivity test --------------------------------------------------------

# Read in the csv data
mydata_1min <- read.csv("./2000_1mints_output_connectivity.csv")
mydata_5min <- read.csv("./2000_5mints_output_connectivity.csv")
mydata_5sec <- read.csv("./2000_5sects_output_connectivity.csv")
mydata_30sec <- read.csv("./2000_30sects_output_connectivity.csv")

# Load raster layer for CRS projection
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)

# Calculate distance between start and end point
# 1 min
mydata_1min_source <- data.frame(mydata_1min$Source_lon, mydata_1min$Source_lat)
colnames(mydata_1min_source) <- c("Lon", "Lat")
mydata_1min_sink <- data.frame(mydata_1min$Sink_lon, mydata_1min$Sink_lat)
colnames(mydata_1min_sink) <- c("Lon", "Lat")

mydata_1min_source <- SpatialPointsDataFrame(
  coords=mydata_1min_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_1min_source)

mydata_1min_sink <- SpatialPointsDataFrame(
  coords=mydata_1min_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_1min_sink)

mydata_1min <- mydata_1min %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_1min_source)) {
  gDists <- suppressWarnings(distm(mydata_1min_source[i,], mydata_1min_sink[i,], fun=distHaversine)/1000)
  mydata_1min$Dist_total[i] <- gDists}

#5 min
mydata_5min_source <- data.frame(mydata_5min$Source_lon, mydata_5min$Source_lat)
colnames(mydata_5min_source) <- c("Lon", "Lat")
mydata_5min_sink <- data.frame(mydata_5min$Sink_lon, mydata_5min$Sink_lat)
colnames(mydata_5min_sink) <- c("Lon", "Lat")

mydata_5min_source <- SpatialPointsDataFrame(
  coords=mydata_5min_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_5min_source)

mydata_5min_sink <- SpatialPointsDataFrame(
  coords=mydata_5min_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_5min_sink)

mydata_5min <- mydata_5min %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_5min_source)) {
  gDists <- suppressWarnings(distm(mydata_5min_source[i,], mydata_5min_sink[i,], fun=distHaversine)/1000)
  mydata_5min$Dist_total[i] <- gDists}

#5 sec
mydata_5sec_source <- data.frame(mydata_5sec$Source_lon, mydata_5sec$Source_lat)
colnames(mydata_5sec_source) <- c("Lon", "Lat")
mydata_5sec_sink <- data.frame(mydata_5sec$Sink_lon, mydata_5sec$Sink_lat)
colnames(mydata_5sec_sink) <- c("Lon", "Lat")

mydata_5sec_source <- SpatialPointsDataFrame(
  coords=mydata_5sec_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_5sec_source)

mydata_5sec_sink <- SpatialPointsDataFrame(
  coords=mydata_5sec_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_5sec_sink)

mydata_5sec <- mydata_5sec %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_5sec_source)) {
  gDists <- suppressWarnings(distm(mydata_5sec_source[i,], mydata_5sec_sink[i,], fun=distHaversine)/1000)
  mydata_5sec$Dist_total[i] <- gDists}

#30sec
mydata_30sec_source <- data.frame(mydata_30sec$Source_lon, mydata_30sec$Source_lat)
colnames(mydata_30sec_source) <- c("Lon", "Lat")
mydata_30sec_sink <- data.frame(mydata_30sec$Sink_lon, mydata_30sec$Sink_lat)
colnames(mydata_30sec_sink) <- c("Lon", "Lat")

mydata_30sec_source <- SpatialPointsDataFrame(
  coords=mydata_30sec_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_30sec_source)

mydata_30sec_sink <- SpatialPointsDataFrame(
  coords=mydata_30sec_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_30sec_sink)

mydata_30sec <- mydata_30sec %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_30sec_source)) {
  gDists <- suppressWarnings(distm(mydata_30sec_source[i,], mydata_30sec_sink[i,], fun=distHaversine)/1000)
  mydata_30sec$Dist_total[i] <- gDists}

# Get just the distances
ks_1min <- mydata_1min$Dist_total
ks_5min <- mydata_5min$Dist_total
ks_5sec <- mydata_5sec$Dist_total
ks_30sec <- mydata_30sec$Dist_total

# Carry out the Kolmogorov-Smirnov test
print("5 min vs 1 min")
ks.test(ks_5min, ks_1min) # p = 0.3982
print("1 min vs 30 sec")
ks.test(ks_1min, ks_30sec) # 0.9496
print("30 sec vs 5 sec")
ks.test(ks_30sec, ks_5sec) # 0.8185

# Plot
plot(ecdf(ks_5min), col="dark grey")
plot(ecdf(ks_1min), add=TRUE, col="tan1")
plot(ecdf(ks_30sec), add=TRUE, col="aquamarine3")
plot(ecdf(ks_5sec), add=TRUE, col="purple")

# Density plot ------------------------------------------------------------

ks_1min <- as.data.frame(ks_1min)
ks_5min <- as.data.frame(ks_5min)
ks_5sec <- as.data.frame(ks_5sec)
ks_30sec <- as.data.frame(ks_30sec)

# Add a particle column
ks_1min$TS <- "1min"
ks_5min$TS <- "5min"
ks_5sec$TS <- "5sec"
ks_30sec$TS <- "30sec"

# Change column names
colnames(ks_1min) <- c("Distance", "TS")
colnames(ks_5min) <- c("Distance", "TS")
colnames(ks_5sec) <- c("Distance", "TS")
colnames(ks_30sec) <- c("Distance", "TS")

# Merge
total_data <- rbind(ks_1min, ks_5min, ks_5sec, ks_30sec)
total_data$TS <- as.factor(total_data$TS)

# ggplot theme
theme_marine <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype="blank"))
}

# Density plot
medians <- ddply(total_data, "TS", summarise, rating.median = median(Distance))

ggplot(total_data, aes(x=Distance, group=TS, fill=TS))+
  geom_density(alpha=.4)+
  scale_fill_manual(values=c("dark grey", "tan1", "aquamarine3", "dark green"))+
  geom_vline(data=medians, aes(xintercept=rating.median, colour=TS), linetype="longdash", size=1)+
  scale_colour_manual(values=c("dark grey", "tan1", "aquamarine3", "dark green"))+
  xlab("Distance travelled (km)") +
  ylab("Density")+
  theme_marine()


